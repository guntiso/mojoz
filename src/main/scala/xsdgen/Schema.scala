package xsdgen

import scala.io.Source

case class DbTable(name: String, comment: String, cols: Seq[DbCol])
case class DbCol(
  name: String,
  dbType: String,
  nullable: Boolean,
  comment: String)

case class XsdType(name: String, length: Option[Int],
  totalDigits: Option[Int], fractionDigits: Option[Int]) {
  def this(name: String) = this(name, None, None, None)
  def this(name: String, length: Int) =
    this(name, Some(length), None, None)
  def this(name: String, totalDigits: Int, fractionDigits: Int) =
    this(name, None, Some(totalDigits), Some(fractionDigits))
}
case class ExTypeDef(name: String, comment: String, cols: Seq[ExColDef])
case class ExColDef(
  name: String,
  xsdType: Option[XsdType],
  nullable: Option[Boolean],
  comment: String)

case class Entity(name: String, comment: String, cols: Seq[XsdCol])
case class XsdCol(
  name: String,
  xsdType: XsdType,
  nullable: Boolean,
  comment: String)

object SchemaConventions {
  // TODO default comment for id col?
  def isBooleanName(name: String) =
    name.startsWith("is_") || name.startsWith("has_")
  def isDateName(name: String) = name.endsWith("_date")
  def isIdRefName(name: String) = name.endsWith("_id")
  def isTypedName(name: String) =
    isBooleanName(name) || isDateName(name) || isIdRefName(name)
  def fromExternal(typeDef: ExTypeDef): Entity = {
    Entity(typeDef.name, typeDef.comment, typeDef.cols.map(fromExternal(_)))
  }
  def fromExternal(col: ExColDef): XsdCol = {
    col match {
      case ExColDef("id", xsdType, nullable, comment) =>
        XsdCol("id", xsdType getOrElse new XsdType("long"),
          nullable getOrElse false, comment)
      case ExColDef(name, xsdType, nullable, comment) if isBooleanName(name) =>
        XsdCol(name, xsdType getOrElse new XsdType("boolean"),
          nullable getOrElse true, comment)
      case ExColDef(name, xsdType, nullable, comment) if isDateName(name) =>
        XsdCol(name, xsdType getOrElse new XsdType("date"),
          nullable getOrElse true, comment)
      // FIXME typename not defined, len defined
      case x =>
        XsdCol(x.name, x.xsdType getOrElse new XsdType("string", 256),
          x.nullable getOrElse true, x.comment)
    }
  }
  def toExternal(typeDef: Entity): ExTypeDef =
    ExTypeDef(typeDef.name, typeDef.comment, typeDef.cols map toExternal)

  def toExternal(col: XsdCol): ExColDef = {
    val nullOpt = (col.name, col.nullable) match {
      case ("id", false) => None
      case (_, true) => None
      case (_, nullable) => Some(nullable)
    }
    val typeOpt = (col.name, col.xsdType.name, col.xsdType.length) match {
      case ("id", "long", _) => None
      case (name, "long", _) if isIdRefName(name) => None
      case (name, "boolean", _) if isBooleanName(name) => None
      case (name, "date", _) if isDateName(name) => None
      case (name, "string", None) if !isTypedName(name) => None
      case (name, "string", Some(len)) if !isTypedName(name) =>
        Some(new XsdType(null.asInstanceOf[String], len))
      case _ => Option(col.xsdType)
    }
    ExColDef(col.name, typeOpt, nullOpt, col.comment)
  }
}

object Schema {
  val scriptFileName = "../db/schema.sql" // TODO use resources!
  val yamlFileName = "../db/schema.yaml" // TODO use resources!
  def loadFromFile: List[DbTable] = loadFromFile(scriptFileName)
  def loadFromFile(scriptFileName: String) = {
    val lines = Source.fromFile(scriptFileName, "UTF-8").getLines.toList
    var tableName = ""
    var tableComment = ""
    var tables: List[DbTable] = Nil
    var cols: List[DbCol] = Nil
    var colComments = scala.collection.mutable.HashMap[String, String]()
    def flush() =
      if (tableName != "") {
        val commentedCols = cols.reverse.map(col =>
          col.copy(comment = colComments.getOrElse(col.name, null)))
        tables = DbTable(tableName, tableComment, commentedCols) :: tables
        tableName = ""
        tableComment = ""
        cols = Nil
        colComments.clear()
      }
    lines.foreach(_.trim.split("[\\s]+").toList match {
      case "create" :: "table" :: dTable :: tail =>
        flush()
        tableName = dTable.replace("(", "")
      case "constraint" :: tail =>
      case "comment" :: "on" :: "table" :: dTable :: "is" :: tail =>
        val dComment = tail.mkString(" ")
        tableComment = dComment.substring(1, dComment.length - 2)
      case "comment" :: "on" :: "column" :: dTableAndCol :: "is" :: tail =>
        val dComment = tail.mkString(" ")
        val colComment = dComment.substring(1, dComment.length - 2)
        val colName = dTableAndCol.split("\\.")(1)
        colComments(colName) = colComment
      case _ :: Nil =>
      case colName :: tail =>
        var dType = tail.mkString(" ")
        dType = dType.substring(0, dType.length - 1) // remove comma
        var nullable = colName != "id" // XXX
        if (dType.endsWith(" not null")) {
          dType = dType.replace(" not null", "")
          nullable = false
        }
        cols = DbCol(colName, dType, nullable, null) :: cols
      case _ =>
    })
    flush()
    tables.reverse
  }
  def toEntity(table: DbTable) = {
    val xsdCols = table.cols.map(col => ExColDef(
      col.name, Option(xsdType(col)), Option(col.nullable), col.comment))
    SchemaConventions.fromExternal(
      ExTypeDef(table.name, table.comment, xsdCols))
  }
  def entities = loadFromFile map toEntity
  def entities(scriptFileName: String) =
    loadFromFile(scriptFileName) map toEntity
  // TODO min-value, max-value?
  def integerOrSubtype(len: Int) =
    if (len > 18) new XsdType("integer", None, Some(len.toInt), None)
    else if (len > 9) new XsdType("long", None, Some(len.toInt), None)
    else new XsdType("int", None, Some(len.toInt), None)
  def xsdType(col: DbCol) = (col.name, col.dbType) match {
    case ("id", _) => new XsdType("long")
    case (name, _) if name endsWith "_id" => new XsdType("long")
    case (_, t) => t.split("[\\s\\'\\(\\)\\,]+").toList match {
      case "date" :: tail => new XsdType("date")
      case "timestamp" :: tail => new XsdType("dateTime")
      case "varchar2" :: len :: Nil => new XsdType("string", len.toInt)
      case "varchar2" :: len :: char :: Nil => new XsdType("string", len.toInt)
      case "char" :: tail => new XsdType("boolean")
      case "numeric" :: len :: Nil =>
        integerOrSubtype(len.toInt)
      case "numeric" :: len :: "0" :: Nil =>
        integerOrSubtype(len.toInt)
      case "numeric" :: len :: frac :: Nil =>
        new XsdType("decimal", len.toInt, frac.toInt)
      case "blob" :: Nil => new XsdType("base64Binary")
      case x => throw new RuntimeException("Unexpected db type: " + col.dbType)
    }
  }
  def dbNameToXsdName(dbName: String) = {
    dbName.split("[_\\.]+").map(_.toLowerCase match {
      case "usr" => "user"
      case "grp" => "group"
      case "rle" => "role"
      case x => x
    }).map(_.capitalize) mkString
  }
  def xsdNameToDbName(xsdName: String) = {
    ElementName.get(xsdName).split("[\\-\\_]").map(_.toLowerCase match {
      case "user" => "usr"
      case "group" => "grp"
      case "role" => "rle"
      case x => x
    }).mkString("_") match {
      case x if x endsWith "_2" => x.replace("_2", "2") // XXX dirty fix phone_2
      case x => x
    }
  }
  private lazy val md = entities.map(e => (e.name, e)).toMap
  def tableDef(typeDef: XsdTypeDef): Entity =
    // TODO get line, file info from xsd type def
    if (typeDef.xtnds == null)
      md.get(typeDef.table) getOrElse
        sys.error("table not found: " + typeDef.table +
          ", type def: " + typeDef.name)
    else tableDef(XsdGen.td(typeDef.xtnds))

  def getCol(typeDef: XsdTypeDef, f: XsdFieldDef) = {
    val tableMd = tableDef(typeDef)
    val cols = tableMd.cols.map(c => (c.name, c)).toMap // TODO cache col map for all tables!
    val colName = xsdNameToDbName(f.name)
    try {
      (if (f.table == typeDef.table) cols
      else md(f.table).cols.map(c => (c.name, c)).toMap)(colName)
    } catch {
      case ex: Exception =>
        // TODO print filename, lineNr, colNr, too!
        throw new RuntimeException(
          "Problem finding column (typeDef: " + typeDef.name
            + ", column: " + f.table + "." + colName +
            (if (f.tableAlias == null) ""
            else " (table alias " + f.tableAlias + ")") + ")", ex)
    }
  }

  def toYamlColDef(colDef: ExColDef) = {
    import colDef._
    val t = colDef.xsdType getOrElse new XsdType(null, None, None, None)
    val td = List(
      Some(name.padTo(20, " ").mkString),
      Some(colDef.nullable map (b => if (b) "?" else "!") getOrElse (" ")),
      Option(t.name),
      t.length,
      t.totalDigits,
      t.fractionDigits).flatMap(x => x) mkString " "
    // TODO max row length! Wrap comments
    // TODO format fixed width columns for better readability
    if (comment == null || comment == "") td else td + ": " + comment
  }
  def toYaml(entity: Entity): String =
    toYaml(SchemaConventions.toExternal(entity))
  def toYaml(entity: ExTypeDef): String =
    List(Some(entity.name).map("name: " + _),
      Option(entity.comment).filter(_ != "").map("comment: " + _),
      Some("columns:"),
      Option(entity.cols.map(f => "- " + toYamlColDef(f)).mkString("\n")))
      .flatMap(x => x).mkString("\n")
  def toYaml: String = (entities map toYaml).mkString("\n---\n\n")
}

package xsdgen

import scala.io.Source

case class DbTable(name: String, comment: String, cols: Seq[DbCol])
case class DbCol(name: String, dbType: String, comment: String)

case class XsdType(name: String, length: Option[Int],
  totalDigits: Option[Int], fractionDigits: Option[Int]) {
  def this(name: String) = this(name, None, None, None)
  def this(name: String, length: Int) =
    this(name, Some(length), None, None)
  def this(name: String, totalDigits: Int, fractionDigits: Int) =
    this(name, None, Some(totalDigits), Some(fractionDigits))
}
case class XsdCol(name: String, xsdType: XsdType, comment: String)
case class Entity(name: String, comment: String, cols: Seq[XsdCol])

object Schema {
  val scriptFileName = "../db/schema.sql" // TODO use resources!
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
          DbCol(col.name, col.dbType, colComments.getOrElse(col.name, null)))
        tables = DbTable(tableName, tableComment, commentedCols) :: tables
        tableName = ""
        tableComment = ""
        cols = Nil
        colComments.clear()
      }
    lines.foreach(_.trim.split("\\s").toList match {
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
        dType = dType.substring(0, dType.length - 1)
        cols = DbCol(colName, dType, null) :: cols
      case _ =>
    })
    flush()
    tables.reverse
  }
  def toEntity(table: DbTable) = {
    val xsdCols = table.cols.map(col =>
      XsdCol(col.name, xsdType(col), col.comment))
    Entity(table.name, table.comment, xsdCols)
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
  def tableDef(typeDef: XsdTypeDef) =
    if (typeDef.xtnds == null) md(typeDef.table) else md(XsdGen.td(typeDef.xtnds).table)
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
}

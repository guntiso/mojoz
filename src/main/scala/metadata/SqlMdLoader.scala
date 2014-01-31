package metadata

import java.io.File
import scala.io.Source
import scala.math.max
import scala.annotation.tailrec

case class DbTableDef(
  name: String,
  comment: String,
  cols: Seq[DbColumnDef],
  pk: Option[DbIndex])
case class DbColumnDef(
  name: String,
  dbType: String,
  nullable: Boolean,
  dbDefault: String,
  check: String,
  comment: String)

trait SqlLinesSource {
  def lines: Seq[String]
}

trait FileSqlLinesSource extends SqlLinesSource {
  def sqlPath: String
  override def lines = Source.fromFile(sqlPath, "UTF-8").getLines.toList
}

trait ResourceSqlLinesSource extends SqlLinesSource {
  def sqlPath: String = "/schema.sql"
  override def lines = Source.fromInputStream(
    getClass.getResourceAsStream(sqlPath))(io.Codec("UTF-8")).getLines.toList
}

trait TableDefSource {
  val entities: List[TableDef]
}

trait SqlMdLoader extends TableDefSource { this: SqlLinesSource =>
  def load = loadLines(lines)
  def loadLines(lines: Seq[String]) = {
    var tableName = ""
    var tableComment = ""
    var tables: List[DbTableDef] = Nil
    var cols: List[DbColumnDef] = Nil
    var colComments = scala.collection.mutable.HashMap[String, String]()
    var primaryKey: Option[DbIndex] = None
    def flush() =
      if (tableName != "") {
        val commentedCols = cols.reverse.map(col =>
          col.copy(comment = colComments.getOrElse(col.name, null)))
        val pkCols = primaryKey.map(_.cols) getOrElse Nil
        val readyCols = commentedCols.map(col =>
          if (pkCols contains col.name) col.copy(nullable = false) else col)
        val tableDef =
          DbTableDef(tableName, tableComment, readyCols, primaryKey)
        tables = tableDef :: tables
        tableName = ""
        tableComment = ""
        cols = Nil
        colComments.clear()
        primaryKey = None
      }
    lines.map(_.trim).mkString("\r\n").split("\\;\\r\\n").flatMap {
      case x if x startsWith "comment" => x :: Nil
      case x => x.split("\\r\\n")
    }.map {
      _.split("\\-\\-").toList.headOption getOrElse "" // ignore comments
    }.foreach(_.trim.split("[\\s]+").toList match {
      case "create" :: "table" :: dTable :: tail =>
        flush()
        tableName = dTable.replace("(", "")
      case "constraint" :: pkName :: "primary" :: "key" :: pkCols =>
        val cleanPkCols = pkCols.map(
          _.replace("(", "").replace(")", "").replace(",", "").trim)
          .filter(_ != "")
        primaryKey = Some(DbIndex(pkName, cleanPkCols))
      case "comment" :: "on" :: "table" :: dTable :: "is" :: tail =>
        val dComment = tail.mkString(" ")
        tableComment = dComment.substring(1, dComment.length - 1)
      case "comment" :: "on" :: "column" :: dTableAndCol :: "is" :: tail =>
        val dComment = tail.mkString(" ")
        val colComment = dComment.substring(1, dComment.length - 1)
        val colName = dTableAndCol.split("\\.")(1)
        colComments(colName) = colComment
      case _ :: Nil =>
      case colName :: tail =>
        var dType = tail.mkString(" ")
        dType = dType.substring(0, dType.length - 1) // remove comma
        var nullable = colName != "id" // XXX
        if (dType.endsWith(" not null") || dType.contains(" not null ")) {
          dType = dType.replace(" not null", "")
          nullable = false
        }
        var dDefault: String = null
        if (dType.contains(" default ")) {
          val Df = " default "
          val iv = dType.indexOf(Df) + Df.length
          val ive = dType.indexOf(" ", iv) match {
            case neg if neg < iv => dType.length
            case i => i
          }
          dDefault = dType.substring(iv, ive).trim
          dType = dType.replace(Df + dDefault, "")
        }
        var check: String = null
        if (dType.contains(" check ")) {
          val i = dType.indexOf(" check ")
          check = dType.substring(i).trim
          dType = dType.substring(0, i).trim
        }
        cols = DbColumnDef(
          colName, dType, nullable, dDefault, check, null) :: cols
      case _ =>
    })
    flush()
    tables.reverse
  }
  def checkToEnum(check: String) =
    // TODO properly, regexp at least
    Option(check).map(_ split "[\\s'\\(\\),]+")
      .map(_.toList.filter(_ != ""))
      .map {
        case "check" :: _ :: "in" :: tail => tail
        case x => Nil
      }.filter(_.size > 0).orNull
  def toEntity(table: DbTableDef) = {
    val xsdCols = table.cols.map(col => ExFieldDef(col.name,
      Option(xsdType(col)), Option(col.nullable), col.dbDefault,
      checkToEnum(col.check), col.comment))
      .map(col =>
        if (col.xsdType.get.name != "string" && col.enum != null)
          col.copy(enum = null)
        else col)
    MdConventions.fromExternal(
      ExTypeDef(table.name, table.comment, xsdCols, table.pk))
  }
  override val entities = load map toEntity
  // TODO min-value, max-value?
  def integerOrSubtype(len: Int) =
    if (len > 18) new XsdType("integer", None, Some(len.toInt), None, false)
    else if (len > 9) new XsdType("long", None, Some(len.toInt), None, false)
    else new XsdType("int", None, Some(len.toInt), None, false)
  def xsdType(col: DbColumnDef) = (col.name, col.dbType) match {
    case ("id", _) => new XsdType("long")
    case (name, t) if name.endsWith("_id") && !t.startsWith("varchar2") =>
      new XsdType("long")
    case (_, t) => t.split("[\\s\\'\\(\\)\\,]+").toList match {
      case "date" :: tail => new XsdType("date")
      case "timestamp" :: tail => new XsdType("dateTime")
      case "varchar2" :: len :: Nil => new XsdType("string", len.toInt)
      case "varchar2" :: len :: "char" :: Nil => new XsdType("string", len.toInt)
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
}

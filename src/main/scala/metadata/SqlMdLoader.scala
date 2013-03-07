package metadata

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

object SqlMdLoader {
  val scriptFileName = "../db/schema.sql" // TODO use resources!
  def loadFromFile: List[DbTableDef] = loadFromFile(scriptFileName)
  def loadFromFile(scriptFileName: String) = {
    val lines = Source.fromFile(scriptFileName, "UTF-8").getLines.toList
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
        val tableDef =
          DbTableDef(tableName, tableComment, commentedCols, primaryKey)
        tables = tableDef :: tables
        tableName = ""
        tableComment = ""
        cols = Nil
        colComments.clear()
        primaryKey = None
      }
    lines.foreach(_.trim.split("[\\s]+").toList match {
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
          // FIXME db default value
          dDefault = dType.substring(iv, ive).trim
          nullable = false
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
  def toEntity(table: DbTableDef) = {
    val xsdCols = table.cols.map(col => ExFieldDef(col.name,
      Option(xsdType(col)), Option(col.nullable), col.dbDefault, col.comment))
    MdConventions.fromExternal(
      ExTypeDef(table.name, table.comment, xsdCols, table.pk))
  }
  val entities = loadFromFile map toEntity
  def entities(scriptFileName: String) =
    loadFromFile(scriptFileName) map toEntity
  // TODO min-value, max-value?
  def integerOrSubtype(len: Int) =
    if (len > 18) new XsdType("integer", None, Some(len.toInt), None, false)
    else if (len > 9) new XsdType("long", None, Some(len.toInt), None, false)
    else new XsdType("int", None, Some(len.toInt), None, false)
  def xsdType(col: DbColumnDef) = (col.name, col.dbType) match {
    case ("id", _) => new XsdType("long")
    case (name, _) if name endsWith "_id" => new XsdType("long")
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

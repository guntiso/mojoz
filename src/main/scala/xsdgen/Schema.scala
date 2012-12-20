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
  val scriptFileName = "..\\db\\schema.sql"
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
  def xsdType(col: DbCol) = (col.name, col.dbType) match {
    case ("id", _) => new XsdType("long")
    case (name, _) if name endsWith "_id" => new XsdType("long")
    case (_, t) => t.split("[\\s\\'\\(\\)\\,]+").toList match {
      case "date" :: tail => new XsdType("date")
      case "timestamp" :: tail => new XsdType("dateTime")
      case "varchar2" :: len :: Nil => new XsdType("string", len.toInt)
      case "char" :: tail => new XsdType("boolean")
      case "numeric" :: len :: Nil =>
        new XsdType("integer", None, Some(len.toInt), None)
      case "numeric" :: len :: "0" :: Nil =>
        new XsdType("integer", None, Some(len.toInt), None)
      case "numeric" :: len :: frac :: Nil =>
        new XsdType("decimal", len.toInt, frac.toInt)
      case "blob" :: Nil => new XsdType("base64Binary")
      case x => throw new RuntimeException("Unexpected db type: " + col.dbType)
    }
  }
  def dbNameToXsdName(dbName: String) = {
    dbName.split("_").map(_.toLowerCase match {
      case "usr" => "user"
      case "grp" => "group"
      case "rle" => "role"
      case x => x
    }).map(_.capitalize) mkString
  }
  def xsdNameToDbName(xsdName: String) = {
    ElementName.get(xsdName).split("\\-").map(_.toLowerCase match {
      case "user" => "usr"
      case "group" => "grp"
      case "role" => "rle"
      case x => x
    }).mkString("_")
  }
}

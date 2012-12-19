package xsdgen

import scala.io.Source

case class DbTable(name: String, comment: String, cols: Seq[DbCol])
case class DbCol(name: String, dbType: String, comment: String)

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
}

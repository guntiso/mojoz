package metadata

import xsdgen.ElementName

case class XsdType(name: String, length: Option[Int],
  totalDigits: Option[Int], fractionDigits: Option[Int]) {
  def this(name: String) = this(name, None, None, None)
  def this(name: String, length: Int) =
    this(name, Some(length), None, None)
  def this(name: String, totalDigits: Int, fractionDigits: Int) =
    this(name, None, Some(totalDigits), Some(fractionDigits))
}
case class DbIndex(
  name: String,
  cols: Seq[String])
case class TableDef(
  name: String,
  comment: String,
  cols: Seq[ColumnDef],
  pk: Option[DbIndex])
case class ColumnDef(
  name: String,
  xsdType: XsdType,
  nullable: Boolean,
  dbDefault: String,
  comment: String)

object Metadata {
  val entities = SqlMdLoader.entities
  private lazy val md = entities.map(e => (e.name, e)).toMap
  val nameToViewDef = YamlViewDefLoader.nameToViewDef
  def tableDef(typeDef: XsdTypeDef): TableDef =
    // TODO get line, file info from xsd type def
    if (typeDef.xtnds == null)
      md.get(typeDef.table) getOrElse
        sys.error("table not found: " + typeDef.table +
          ", type def: " + typeDef.name)
    else tableDef(nameToViewDef(typeDef.xtnds))

  def getCol(typeDef: XsdTypeDef, f: XsdFieldDef) = {
    val tableMd = tableDef(typeDef)
    val cols = tableMd.cols.map(c => (c.name, c)).toMap // TODO cache col map for all tables!
    val colName = DbConventions.xsdNameToDbName(f.name)
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
  private val xtd = YamlViewDefLoader.nameToExtendedViewDef
  def getViewDef(viewClass: Class[_ <: AnyRef]): XsdTypeDef =
    xtd.get(ElementName.get(viewClass)) getOrElse
      (xtd.get(ElementName.get(viewClass).replace("-", "_")) getOrElse
        (viewClass.getSuperclass match {
          case c: Class[_] =>
            try getViewDef(c.asInstanceOf[Class[_ <: AnyRef]]) catch {
              case e: Exception => throw new RuntimeException(
                "Failed to get view definition for " + viewClass.getName, e)
            }
          case x => throw new RuntimeException(
            "Failed to get view definition for " + viewClass.getName)
        }))
}

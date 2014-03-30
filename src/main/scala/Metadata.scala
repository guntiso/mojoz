package mojoz.metadata

case class XsdType(name: String, length: Option[Int],
  totalDigits: Option[Int], fractionDigits: Option[Int], isComplexType: Boolean) {
  def this(name: String) = this(name, None, None, None, false)
  def this(name: String, isComplexType: Boolean) =
    this(name, None, None, None, isComplexType)
  def this(name: String, length: Int) =
    this(name, Some(length), None, None, false)
  def this(name: String, totalDigits: Int, fractionDigits: Int) =
    this(name, None, Some(totalDigits), Some(fractionDigits), false)

  def intDigits = totalDigits.map(n => n - fractionDigits.getOrElse(0))
}
case class DbIndex(
  name: String,
  cols: Seq[String])
case class Ref(
  name: String,
  cols: Seq[String],
  refTable: String,
  refCols: Seq[String],
  defaultTableAlias: String,
  defaultRefTableAlias: String,
  onDeleteAction: String,
  onUpdateAction: String)
case class TableDef(
  name: String,
  comment: String,
  cols: Seq[ColumnDef],
  pk: Option[DbIndex],
  uk: Seq[DbIndex],
  idx: Seq[DbIndex],
  refs: Seq[Ref])
case class ColumnDef(
  name: String,
  xsdType: XsdType,
  nullable: Boolean,
  dbDefault: String,
  enum: Seq[String],
  comment: String)

trait Metadata { this: TableDefSource with ViewDefSource =>
  private lazy val md = entities.map(e => (e.name, e)).toMap

  def tableDef(tableName: String): TableDef =
    md.get(tableName) getOrElse
      sys.error("table not found: " + tableName)

  def tableDefOption(typeDef: XsdTypeDef): Option[TableDef] =
    md.get(typeDef.table)

  def tableDef(typeDef: XsdTypeDef): TableDef =
    // TODO get line, file info from xsd type def
    md.get(typeDef.table) getOrElse
      sys.error("table not found: " + typeDef.table +
        ", type def: " + typeDef.name)

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
            else " (table alias " + f.tableAlias + ")") + ")"
            + ", joins: " + typeDef.joins, ex)
    }
  }
}

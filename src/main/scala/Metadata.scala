package mojoz.metadata

import scala.annotation.tailrec

import mojoz.metadata.in.I18nRules
import mojoz.metadata.in.YamlTableDefLoader
import TableDef._
import ColumnDef._
import ViewDef._
import FieldDef._

case class Type(name: String, length: Option[Int],
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
object TableDef {
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
  case class CheckConstraint(
    name: String,
    // TODO check constraint deferrability?
    // TODO table check constraint or column check constraint?
    expression: String)
  trait TableDefBase[+C <: ColumnDefBase[_]] { // TODO bound too tight?
    val name: String
    val comments: String
    val cols: Seq[C]
    val pk: Option[DbIndex]
    val uk: Seq[DbIndex]
    val ck: Seq[CheckConstraint]
    val idx: Seq[DbIndex]
    val refs: Seq[Ref]
  }
}

case class TableDef[+C <: ColumnDefBase[_]]( // TODO bound too tight?
  name: String,
  comments: String,
  cols: Seq[C],
  pk: Option[DbIndex],
  uk: Seq[DbIndex],
  ck: Seq[CheckConstraint],
  idx: Seq[DbIndex],
  refs: Seq[Ref],
  extras: Map[String, Any]) extends TableDefBase[C] {
  def toLowerCase: this.type = mapTableNames(_.toLowerCase)
    .mapColumnNames(_.toLowerCase)
    .mapConstraintNames(_.toLowerCase)
  def toSimpleNames: this.type = mapTableNames((s: String) =>
    if (s.indexOf(".") < 0) s else s.substring(s.lastIndexOf(".") + 1))
  def unprefixTableNames(prefix: String): this.type = mapTableNames((s: String) =>
    if (s startsWith prefix) s.substring(prefix.length) else s)
  def mapTableNames(transform: (String) => String): this.type = copy(
    name = transform(name),
    refs = refs.map(r => r.copy(refTable = transform(r.refTable)))).asInstanceOf[this.type]
  def mapColumnNames(transform: (String) => String): this.type = copy(
    cols = cols.map(c => c.rename(transform(c.name))),
    pk = pk.map(x => x.copy(cols = x.cols.map(transform))),
    uk = uk.map(x => x.copy(cols = x.cols.map(transform))),
    idx = idx.map(x => x.copy(cols = x.cols.map(transform))),
    refs = refs.map(r => r.copy(
      cols = r.cols.map(transform),
      refCols = r.refCols.map(transform)))).asInstanceOf[this.type]
  def mapConstraintNames(transform: (String) => String): this.type = {
    def safeTransform(s: String) = if (s == null) s else transform(s)
    def idxTransform(idx: TableDef.DbIndex) =
      idx.copy(name = safeTransform(idx.name))
    copy(
      pk = pk.map(idxTransform),
      uk = uk.map(idxTransform),
      ck = ck.map(c => c.copy(name = safeTransform(c.name))),
      idx = idx.map(idxTransform),
      refs = refs.map(r => r.copy(name = safeTransform(r.name)))).asInstanceOf[this.type]
  }
}
object ColumnDef {
  trait ColumnDefBase[+T] {
    val name: String
    val type_ : T
    val nullable: Boolean
    val dbDefault: String
    val enum: Seq[String]
    val comments: String
    def rename(name: String): this.type
  }
}
case class ColumnDef[+T](
  name: String,
  type_ : T,
  nullable: Boolean,
  dbDefault: String,
  enum: Seq[String],
  comments: String,
  extras: Map[String, Any]) extends ColumnDefBase[T] {
  override def rename(name: String) = copy(name = name).asInstanceOf[this.type]
}

class TableMetadata[+T <: TableDefBase[ColumnDefBase[Type]]](
  val tableDefs: Seq[T] = (new YamlTableDefLoader()).tableDefs,
  val dbName: String => String = Naming.dbName) {
  private val md = tableDefs.map(e => (e.name, e)).toMap
  private val refTableAliasToRef = tableDefs.map(t => t.refs
    .filter(_.defaultRefTableAlias != null)
    .map(r => ((t.name, r.defaultRefTableAlias), r)))
    .flatMap(x => x)
    .toMap
  private val colNameToCol =
    tableDefs.map(t => t.cols.map(c => ((t.name, c.name), c))).flatten.toMap
  def tableDef(tableName: String) =
    md.get(tableName) getOrElse
      sys.error("table not found: " + tableName)

  def tableDefOption(typeDef: ViewDefBase[_]) =
    md.get(typeDef.table)

  def tableDef(typeDef: ViewDefBase[_]) =
    // TODO get line, file info from xsd type def
    md.get(typeDef.table) getOrElse
      sys.error("table not found: " + typeDef.table +
        ", type def: " + typeDef.name)

  def columnDef(viewDef: ViewDefBase[_], fieldDef: FieldDefBase[_]) = {
    val typeDef = viewDef
    val f = fieldDef
    val colName = dbName(f.name)
    try colNameToCol((f.table, colName)) catch {
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

  def col(table: String, column: String) =
    colNameToCol.get((table, column))
  def ref(table: String, refTableAlias: String): Option[Ref] =
    refTableAliasToRef.get((table, refTableAlias))
}

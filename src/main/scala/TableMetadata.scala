package org.mojoz.metadata

import scala.collection.immutable.{Map, Seq}

import org.mojoz.metadata.in.YamlTableDefLoader
import TableMetadata._

object TableMetadata {
  private def validCols(cols: Seq[String]) =
    cols != null && cols.size > 0 && !cols.exists(col => col == null || col.trim == "")
  private def validPartitionKeySize(cols: Seq[String], part: Int) =
    part == 0 || part <= cols.size
  sealed trait DbIndex {
    def name: String
    def cols: Seq[String]
    def mapName(f: String => String): this.type
    def mapCols(f: String => String): this.type
  }
  object DbIndex {
    def apply(name: String, cols: Seq[String]) = Index(name, cols)
  }
  case class Index(
    name: String,
    cols: Seq[String],
  ) extends DbIndex {
    require(validCols(cols),
      "Invalid columns for index: " + cols)
    override def mapName(f: String => String) =
      copy(name = f(name)).asInstanceOf[this.type]
    override def mapCols(f: String => String) =
      copy(cols = cols.map(f)).asInstanceOf[this.type]
  }
  case class ComplexKey(
    name: String,
    cols: Seq[String],
    part: Int = 0, // for cassandra - partition key column count
  ) extends DbIndex {
    require(validCols(cols),
      "Invalid columns for index: " + cols)
    require(validPartitionKeySize(cols, part),
      s"Invalid partition key size $part for index columns: $cols")
    override def mapName(f: String => String) =
      copy(name = f(name)).asInstanceOf[this.type]
    override def mapCols(f: String => String) =
      copy(cols = cols.map(f)).asInstanceOf[this.type]
  }
  case class Ref(
    name: String,
    cols: Seq[String],
    refTable: String,
    refCols: Seq[String],
    defaultTableAlias: String,
    defaultRefTableAlias: String,
    onDeleteAction: String,
    onUpdateAction: String) {
    require(validCols(cols),
      "Invalid columns for ref: " + cols)
    require(validCols(refCols),
      "Invalid ref columns for ref: " + refCols)
    require(refTable != null && refTable.trim != "",
      "Invalid ref table for ref: " + refTable)
  }
  case class CheckConstraint(
    name: String,
    // TODO check constraint deferrability?
    // TODO table check constraint or column check constraint?
    expression: String) {
    require(expression != null && expression.trim != "",
      "Invalid expression for check constraint: " + expression)
  }
}

case class TableDef_[+C <: ColumnDef_[_]](
  db: String,
  name: String,
  comments: String,
  cols: Seq[C],
  pk: Option[DbIndex],
  uk: Seq[DbIndex],
  ck: Seq[CheckConstraint],
  idx: Seq[DbIndex],
  refs: Seq[Ref],
  extras: Map[String, Any]) {
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
    cols = cols.map(c => c.withName(transform(c.name))),
    pk = pk.map(_ mapCols transform),
    uk = uk.map(_ mapCols transform),
    idx = idx.map(_ mapCols transform),
    refs = refs.map(r => r.copy(
      cols = r.cols.map(transform),
      refCols = r.refCols.map(transform)))).asInstanceOf[this.type]
  def mapConstraintNames(transform: (String) => String): this.type = {
    def safeTransform(s: String) = if (s == null) s else transform(s)
    def idxTransform(idx: TableMetadata.DbIndex) =
      if (idx.name == null) idx else idx.mapName(transform)
    copy(
      pk = pk.map(idxTransform),
      uk = uk.map(idxTransform),
      ck = ck.map(c => c.copy(name = safeTransform(c.name))),
      idx = idx.map(idxTransform),
      refs = refs.map(r => r.copy(name = safeTransform(r.name)))).asInstanceOf[this.type]
  }
}
case class ColumnDef_[+T](
  name: String,
  type_ : T,
  nullable: Boolean,
  dbDefault: String,
  enum_ : Seq[String],
  comments: String,
  extras: Map[String, Any]) {
  def withName(name: String) = copy(name = name).asInstanceOf[this.type]
}

class TableMetadata(
  val tableDefs: Seq[TableDef] = (new YamlTableDefLoader()).tableDefs,
  val dbName: String => String = identity,
  val aliasToDb: Map[String, String] = Map.empty,
) {
  def this(tableDefs: Seq[TableDef], dbName: String => String) = this(tableDefs, dbName, Map.empty)
  val dbToTableDefs: Map[String, Seq[TableDef]] = tableDefs.groupBy(_.db)
  private val dbToAlias: Map[String, Set[String]] =
    (dbToTableDefs.keySet.map(db => (db, db)) ++ aliasToDb)
      .groupBy(_._2)
      .transform { case (_, set) => set.map(_._1) }
  private def withDbAlias[T](dbAndValue: (String, T)): Set[(String, T)] = dbAndValue match {
    case (db, value) => dbToAlias(db).map(alias => (alias, value))
  }
  private val md = dbToTableDefs.map { case (db, tableDefs) =>
    db -> tableDefs.map(e => (e.name, e)).toMap
  }.flatMap(withDbAlias)
  private val dbToRefTableAliasToRef = dbToTableDefs.map { case (db, tableDefs) =>
    db -> tableDefs.map(t => t.refs
            .filter(_.defaultRefTableAlias != null)
            .map(r => ((t.name, r.defaultRefTableAlias), r)))
            .flatMap(x => x)
            .toMap
  }.flatMap(withDbAlias)
  private val dbToRefTableOrAliasToRef = dbToTableDefs.map { case (db, tableDefs) =>
    db -> tableDefs.map(t => t.refs
            .map(r => ((t.name, Option(r.defaultRefTableAlias).getOrElse(r.refTable)), r)))
            .flatMap(x => x)
            .toMap
  }.flatMap(withDbAlias)
  private val dbToColNameToCol = dbToTableDefs.map { case (db, tableDefs) =>
    db -> tableDefs.map(t => t.cols.map(c => ((t.name, c.name), c))).flatten.toMap
  }.flatMap(withDbAlias)
  private def throwDbNotFound(db: String) =
    sys.error(s"Table metadata for database or alias '$db' not found")
  private def dbAndTable(db: String, table: String) =
    Option(db).map(db => s"$db:$table") getOrElse table

  def tableDefOption(tableName: String, db: String): Option[TableDef] =
    md.getOrElse(db, throwDbNotFound(db))
      .get(tableName)
  def tableDef(tableName: String, db: String): TableDef =
    md.getOrElse(db, throwDbNotFound(db))
      .getOrElse(tableName,
        sys.error(s"Table not found: ${dbAndTable(db, tableName)}"))
  def tableDefOption(viewDef: ViewDef_[_]): Option[TableDef] =
    md.getOrElse(viewDef.db, throwDbNotFound(viewDef.db))
      .get(viewDef.table)
  def tableDef(viewDef: ViewDef_[_]): TableDef =
    md.getOrElse(viewDef.db, throwDbNotFound(viewDef.db))
      .getOrElse(viewDef.table,
        sys.error(s"Table not found: ${dbAndTable(viewDef.db, viewDef.table)} (view: ${viewDef.name})"))
  def columnDef(viewDef: ViewDef_[_], fieldDef: FieldDef_[_]): ColumnDef = {
    val f = fieldDef
    val colName = dbName(f.name)
    import viewDef.db
    dbToColNameToCol
      .getOrElse(db, throwDbNotFound(db))
      .getOrElse((f.table, colName), {
        tableDefOption(f.table, db) match {
          case None =>
            sys.error(s"Table not found: ${dbAndTable(db, f.table)}" +
              s" (view: ${viewDef.name}${
                Option(f.tableAlias).map(alias => s" (table alias $alias") getOrElse ""})${
                Option(viewDef.joins).map(joins => s", joins: $joins") getOrElse ""}")
          case Some(tableDef) =>
            sys.error(s"Column not found: $colName" +
              s" (table: ${dbAndTable(db, f.table)}, view: ${viewDef.name}${
                Option(f.tableAlias).map(alias => s" (table alias $alias") getOrElse ""})${
                Option(viewDef.joins).map(joins => s", joins: $joins") getOrElse ""}")
        }
      })
  }
  def columnDefOption(viewDef: ViewDef_[_], fieldDef: FieldDef_[_]): Option[ColumnDef] =
    dbToColNameToCol
      .getOrElse(viewDef.db, throwDbNotFound(viewDef.db))
      .get((fieldDef.table, dbName(fieldDef.name)))

  def col(table: String, column: String, db: String): Option[ColumnDef] =
    dbToColNameToCol
      .getOrElse(db, throwDbNotFound(db))
      .get((table, column))
  def aliasedRef(table: String, refTableAlias: String, db: String): Option[Ref] =
    dbToRefTableAliasToRef
      .getOrElse(db, throwDbNotFound(db))
      .get((table, refTableAlias))
  def ref(table: String, refTableOrAlias: String, db: String): Option[Ref] =
    dbToRefTableOrAliasToRef
      .getOrElse(db, throwDbNotFound(db))
      .get((table, refTableOrAlias))
}

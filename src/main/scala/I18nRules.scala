package mojoz.metadata.in

import mojoz.metadata.TableMetadata
import mojoz.metadata.Type
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.TableDef.{ TableDefBase => TableDef }
import mojoz.metadata.ColumnDef.{ ColumnDefBase => ColumnDef }
import scala.language.higherKinds

object I18nRules {

class SuffixI18nRules(
  val tableMetadata: TableMetadata[TableDef[ColumnDef[Type]]],
  val i18nSuffixes: Set[String],
  val noI18nFields: Set[String],
  val iI8nForcedViews: Set[String]) {
  def i18nFields[F[T] <: FieldDef[T], T](tableMetadata: TableMetadata[TableDef[ColumnDef[Type]]], view: ViewDef[F[T]]): Set[F[T]] = {
    val fMap = view.fields.filter(_.table != null)
      .map(f => (f.table + "." + f.name, f)).toMap
    val i18n = view.fields.filter(_.table != null)
      .filter(f => !i18nSuffixes.exists(f.name.endsWith))
      .filter(f => !i18nSuffixes.exists(sx =>
        fMap.contains(f.table + "." + f.name + sx)))
      .filter(f => i18nSuffixes.forall(sx =>
        tableMetadata.tableDef(f.table).cols.exists(_.name == f.name + sx)))
      .toSet
      .filter(f =>
        !noI18nFields(f.table + "." + f.name) || iI8nForcedViews(view.name))
    i18n
  }
  def setI18n[T](view: mojoz.metadata.ViewDef[mojoz.metadata.FieldDef[T]]) = {
    val i18n = i18nFields(tableMetadata, view)
    if (i18n.size == 0) view
    else view.copy(fields = view.fields.map(f =>
      if (i18n contains f) f.copy(isI18n = true) else f))
  }
}

  def suffixI18n(
    tableMetadata: TableMetadata[TableDef[ColumnDef[Type]]],
    i18nSuffixes: Set[String],
    noI18nFields: Set[String] = Set(),
    iI8nForcedViews: Set[String] = Set()) =
    new SuffixI18nRules(
      tableMetadata, i18nSuffixes, noI18nFields, iI8nForcedViews)
}

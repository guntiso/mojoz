package mojoz.metadata.in

import mojoz.metadata.Metadata
import mojoz.metadata.ViewDef

trait I18nRules {
  def setI18n[T](tableMetadata: Metadata[_], view: ViewDef[T]): ViewDef[T]
}

private[in] class NoI18n extends I18nRules {
  override def setI18n[T](md: Metadata[_], t: ViewDef[T]) = t
}

private[in] class SuffixI18nRules(
  val i18nSuffixes: Set[String],
  val noI18nFields: Set[String],
  val iI8nForcedViews: Set[String]) extends I18nRules {
  override def setI18n[T](tableMetadata: Metadata[_], t: ViewDef[T]) = {
    val fMap = t.fields.filter(!_.isExpression).filter(!_.isCollection)
      .map(f => (f.table + "." + f.name, f)).toMap
    val i18n = t.fields.filter(!_.isExpression).filter(!_.isCollection)
      .filter(f => !i18nSuffixes.exists(f.name.endsWith))
      .filter(f => !i18nSuffixes.exists(sx =>
        fMap.contains(f.table + "." + f.name + sx)))
      .filter(f => i18nSuffixes.forall(sx =>
        tableMetadata.tableDef(f.table).cols.exists(_.name == f.name + sx)))
      .toSet
      .filter(f =>
        !noI18nFields(f.table + "." + f.name) || iI8nForcedViews(t.name))
    if (i18n.size == 0) t
    else t.copy(fields = t.fields.map(f =>
      if (i18n contains f) f.copy(isI18n = true) else f))
  }
}

object I18nRules {
  def noI18n: I18nRules = new NoI18n
  def suffixI18n(
    i18nSuffixes: Set[String],
    noI18nFields: Set[String] = Set(),
    iI8nForcedViews: Set[String] = Set()): I18nRules =
    new SuffixI18nRules(i18nSuffixes, noI18nFields, iI8nForcedViews)
}

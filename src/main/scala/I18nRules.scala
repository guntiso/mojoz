package mojoz.metadata.in

import mojoz.metadata.TableMetadata
import mojoz.metadata.ViewDef

trait I18nRules[T] {
  def setI18n(t: ViewDef[T]): ViewDef[T]
}

package rules {

trait NoI18n[T] extends I18nRules[T] {
  override def setI18n(t: ViewDef[T]) = t
}

trait SuffixI18nRules[T] extends I18nRules[T] { this: TableMetadata[T] =>
  val i18nSuffixes: Set[String]
  val noI18nFields: Set[String] = Set()
  val iI8nForcedViews: Set[String] = Set()
  override def setI18n(t: ViewDef[T]) = {
    val fMap = t.fields.filter(!_.isExpression).filter(!_.isCollection)
      .map(f => (f.table + "." + f.name, f)).toMap
    val i18n = t.fields.filter(!_.isExpression).filter(!_.isCollection)
      .filter(f => !i18nSuffixes.exists(f.name.endsWith))
      .filter(f => !i18nSuffixes.exists(sx =>
        fMap.contains(f.table + "." + f.name + sx)))
      .filter(f => i18nSuffixes.forall(sx =>
        tableDef(f.table).cols.exists(_.name == f.name + sx)))
      .toSet
      .filter(f =>
        !noI18nFields(f.table + "." + f.name) || iI8nForcedViews(t.name))
    if (i18n.size == 0) t
    else t.copy(fields = t.fields.map(f =>
      if (i18n contains f) f.copy(isI18n = true) else f))
  }
}
}

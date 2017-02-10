package mojoz.metadata

package object in {
  case class Join(alias: String, table: String, columns: Seq[ColumnDef[Type]])
  /** basetable and joins to parsed joins */
  type JoinsParser = (String, Seq[String]) => Seq[Join]
}

package object io {
  type SimplePatternMdConventions = MdConventions.SimplePatternMdConventions
}

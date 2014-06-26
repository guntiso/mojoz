package mojoz.metadata

package object in {
  case class Join(alias: String, table: String, nullable: Either[String, Boolean])
  /** basetable and joins to parsed joins */
  type JoinsParser = (String, String) => Seq[Join]
}

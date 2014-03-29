package mojoz.metadata.in

case class Join(alias: String, table: String, nullable: Either[String, Boolean])

trait JoinsParser {
  def parseJoins(baseTable: String, joins: String): Seq[Join]
}

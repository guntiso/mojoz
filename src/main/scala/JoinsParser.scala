package mojoz.metadata.in

object JoinsParser {
  case class Join(alias: String, table: String, nullable: Either[String, Boolean])
}
trait JoinsParser {
  def parseJoins(baseTable: String, joins: String): Seq[JoinsParser.Join]
}

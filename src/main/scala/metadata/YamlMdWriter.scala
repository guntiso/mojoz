package metadata

import scala.Option.option2Iterable
import scala.annotation.tailrec

trait YamlMdWriter { this: Metadata =>
  val MaxLineLength = 100
  def escapeYamlValue(value: String) = { // TODO escapeYamlValue properly
    if (value contains ":") "\"" + value + "\"" else value
  }
  def toYamlColDef(colDef: ExFieldDef) = {
    import colDef._
    val t = colDef.xsdType getOrElse new XsdType(null, None, None, None, false)
    val typeString = List(
      Option(t.name),
      t.length,
      t.totalDigits,
      t.fractionDigits).flatMap(x => x) mkString " "

    val defString = List(
      (name, 20),
      (colDef.nullable map (b => if (b) "?" else "!") getOrElse " ", 1),
      (typeString, 8)).foldLeft(("", 0))((r, t) => (
        List(r._1, t._1).mkString(" ").trim.padTo(r._2 + t._2, " ").mkString,
        r._2 + t._2 + 1))._1

    val hasComment = (comment != null && comment.trim != "")
    val slComment =
      if (hasComment) " : " + escapeYamlValue(comment.trim) else ""
    if (!hasComment) defString.trim
    else if (MaxLineLength >= defString.length + slComment.length)
      (defString + slComment).trim
    else {
      val indent = " " * (2 + defString.length + 3)
      wrapped(escapeYamlValue(comment.trim), defString + " :", indent)
    }
  }
  def toYaml(entity: TableDef): String =
    toYaml(MdConventions.toExternal(entity))
  def toYaml(entity: ExTypeDef): String =
    List(Some(entity.name).map("table:   " + _),
      Option(entity.comment).filter(_ != "").map(c =>
        wrapped(c.trim, "comment:", " " * 9)),
      Some("columns:"),
      Option(entity.cols.map(f => "- " + toYamlColDef(f)).mkString("\n")))
      .flatMap(x => x).mkString("\n")
  def toYaml: String = (entities map toYaml).mkString("\n---\n\n")
  private def wrapped(words: String, prefix: String, indent: String) = {
    @tailrec
    def _wrapped(
      words: List[String], lines: List[String], line: String): List[String] =
      words match {
        case word :: tail if line.length + word.length + 1 < MaxLineLength =>
          _wrapped(tail, lines, line + " " + word)
        case word :: tail =>
          _wrapped(tail, line :: lines, indent + word)
        case Nil => (line :: lines).reverse
      }
    _wrapped(words.trim.split("[\\s]+").toList, Nil, prefix).mkString("\n")
  }
}

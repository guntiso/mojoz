package mojoz.metadata.out

import scala.Option.option2Iterable
import scala.annotation.tailrec
import mojoz.metadata._
import mojoz.metadata.io._

trait YamlMdWriter { this: TableDefSource =>
  val MaxLineLength = 100
  private val yamlChA = ":#"
    .toCharArray.map(_.toString).toSet
  private val yamlChB = ",[]{}&*!|>'%@`\""
    .toCharArray.map(_.toString).toSet
  def escapeYamlValue(s: String) =
    if (s != null &&
      (yamlChA.exists(s contains _) || yamlChB.exists(s startsWith _)))
      "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    else s
  def toYamlColDef(colDef: ExFieldDef) = {
    import colDef._
    val t = colDef.xsdType getOrElse new XsdType(null, None, None, None, false)
    val typeString = List(
      Option(t.name),
      t.length,
      t.totalDigits,
      t.fractionDigits).flatMap(x => x) mkString " "
    val enumString = Option(colDef.enum)
      .filter(_ != null)
      .filter(_ != Nil)
      .map(_.mkString("(", ", ", ")"))
      .getOrElse("")

    val defString = List(
      (name, 20),
      (colDef.nullable map (b => if (b) "?" else "!") getOrElse " ", 1),
      (typeString, 10),
      (enumString, 2))
      .foldLeft(("", 0))((r, t) => (
        List(r._1, t._1).mkString(" ").trim.padTo(r._2 + t._2, " ").mkString,
        r._2 + t._2 + 1))._1

    val hasComment = (comment != null && comment.trim != "")
    val slComment =
      if (hasComment) " : " + escapeYamlValue(comment.trim) else ""
    if (!hasComment) defString.trim
    else if (MaxLineLength >= defString.length + slComment.length)
      (defString + slComment).trim
    else {
      val indent =
        if (MaxLineLength < defString.length + 20) " " * 41
        else " " * (2 + defString.length + 3)
      wrapped(escapeYamlValue(comment.trim), defString + " :", indent)
    }
  }
  def toYaml(entity: TableDef): String =
    toYaml(MdConventions.toExternal(entity))
  def toYaml(entity: ExTypeDef): String =
    List(Some(entity.name).map("table:   " + _),
      Option(entity.comment).filter(_ != "").map(c =>
        wrapped(escapeYamlValue(c.trim), "comment:", " " * 9)),
      Some("columns:"),
      Option(entity.cols.map(f => "- " + toYamlColDef(f)).mkString("\n")))
      .flatMap(x => x).mkString("\n")
  def toYamlTableDefs: String = (entities map toYaml).mkString("\n---\n\n")
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

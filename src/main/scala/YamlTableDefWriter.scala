package org.mojoz.metadata.out

import scala.annotation.tailrec
import scala.collection.immutable.Seq

import org.mojoz.metadata._
import org.mojoz.metadata.io._

class YamlTableDefWriter {
  val MaxLineLength = 100
  private val yamlChA = ":#\t\r\n" // TODO more escapes
    .toCharArray.map(_.toString).toSet
  private val yamlChB = ",[]{}&*!|>'%@`\" "
    .toCharArray.map(_.toString).toSet
  private val yamlChC = " "
    .toCharArray.map(_.toString).toSet
  private def escapeYamlValue(s: String) =
    if (s != null &&
        (yamlChA.exists(s contains   _) ||
         yamlChB.exists(s startsWith _) ||
         yamlChC.exists(s endsWith   _) ||
         s == ""                        ))
      "\"" +
       s.replace("\\", "\\\\")
        .replace("\t", "\\t")
        .replace("\r", "\\r")
        .replace("\n", "\\n")
        .replace("\"", "\\\"") +
      "\""
    else s
  def toYaml(colDef: IoColumnDef) = {
    import colDef._
    val t = colDef.type_.type_ getOrElse new Type(null, None, None, None, false)
    val typeString = List(
      Option(t.name),
      t.length,
      t.totalDigits,
      t.fractionDigits).flatMap(x => x) mkString " "
    val enumString = Option(colDef.enum_)
      .filter(_ != null)
      .filter(_ != Nil)
      .map(_.map(e => if (e contains ' ') "'" + e + "'" else e))
      .map(_.mkString("(", ", ", ")"))
      .getOrElse("")

    val hasDefault = (dbDefault != null && dbDefault != "")
    val default = if (hasDefault) " = " + dbDefault else ""
    val defString = List(
      (name, 22),
      (colDef.type_.nullable map (b => if (b) "?" else "!") getOrElse " ", 2),
      (typeString, 12),
      (enumString, 2))
      .foldLeft(("", 0))((r, t) => (
        List(r._1, t._1).mkString(" ").trim.padTo(r._2 + t._2 - 1, " ").mkString,
        r._2 + t._2))._1 +
        default // TODO formatting?

    val hasComment = comments != null
    val hasExtras = extras != null && !extras.isEmpty
    val slComment =
      if (hasComment) s" : ${escapeYamlValue(comments)}" else ""
    if (!hasComment && !hasExtras) defString.trim
    else if (hasExtras) {
      // TODO handle various types of extras
      val prefix = "  -"
      val indent = "    "
      val w_comments =
        if (hasComment) wrapped(escapeYamlValue(comments), prefix, indent)
        else ""
      val lines = (defString + " :") :: w_comments :: extras.map(e =>
        s"$prefix ${escapeYamlValue(e._1)}" + (
          if (e._1 == e._2) ""
          else s": ${escapeYamlValue("" + e._2)}")).toList
      lines
        .filter(_ != "")
        .mkString("\n")
    } else if (MaxLineLength >= defString.length + slComment.length)
      (defString + slComment).trim
    else {
      val indent =
        if (MaxLineLength < defString.length + 20) " " * 42
        else " " * (2 + defString.length + 3)
      wrapped(escapeYamlValue(comments), defString + " :", indent)
    }
  }
  private def toYaml(name: String, cols: Seq[String]): String =
    Option(name).filter(_ != "")
      .map(_ + cols.mkString("(", ", ", ")"))
      .getOrElse(cols.mkString(", "))
  private def toYaml(ck: TableDef.CheckConstraint): String =
    Option(ck.name).map(n => s"$n = ${ck.expression}") getOrElse ck.expression
  private def toYaml(index: TableDef.DbIndex): String =
    toYaml(index.name, index.cols)
  private def toYaml(ref: TableDef.Ref): String =
    List(
      Some(toYaml(ref.name, ref.cols)),
      Some("->"),
      Some(toYaml(ref.refTable, ref.refCols)),
      Option(ref.onDeleteAction).map("on delete " + _),
      Option(ref.onUpdateAction).map("on update " + _))
      .flatMap(x => x).mkString(" ")
  def toYaml(tableDef: IoTableDef): String =
    List(Some(tableDef.name).map("table:    " + _),
      Option(tableDef.comments).map(c =>
        wrapped(escapeYamlValue(c), "comments:", " " * 10)),
      Some("columns:"),
      Option(tableDef.cols.map(f => "- " + toYaml(f)).mkString("\n")),
      tableDef.pk.map { pk => if (pk == null) "pk:" else s"pk: ${toYaml(pk)}" },
      Option(tableDef.uk).filter(_.size > 0).map(x => "uk:"),
      Option(tableDef.uk).filter(_.size > 0)
        .map(_.map(i => "- " + toYaml(i)).mkString("\n")),
      Option(tableDef.ck).filter(_.size > 0).map(x => "ck:"),
      Option(tableDef.ck).filter(_.size > 0)
        .map(_.map(i => "- " + toYaml(i)).mkString("\n")),
      Option(tableDef.idx).filter(_.size > 0).map(x => "idx:"),
      Option(tableDef.idx).filter(_.size > 0)
        .map(_.map(i => "- " + toYaml(i)).mkString("\n")),
      Option(tableDef.refs).filter(_.size > 0).map(x => "refs:"),
      Option(tableDef.refs).filter(_.size > 0)
        .map(_.map(i => "- " + toYaml(i)).mkString("\n")),
      Option(tableDef.extras).filter(!_.isEmpty) // TODO map extras - all types
        .map(_.map(e => e._1 + ": " + e._2.toString).mkString("\n")))
      .flatMap(x => x).mkString("\n")
  def toYaml(tableDefs: Seq[IoTableDef]): String =
    tableDefs.map(toYaml).mkString("\n\n") +
      (if (tableDefs.size > 0) "\n" else "")
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

object YamlTableDefWriter {
  def toYaml(tableDefs: Seq[MojozTableDef],
    conventions: (MojozTableDef) => IoTableDef = MdConventions.toExternal) =
    (new YamlTableDefWriter).toYaml(tableDefs.map(conventions))
}

package org.mojoz.metadata
package out

import scala.annotation.tailrec
import scala.collection.immutable.Seq

import org.mojoz.metadata.io._

class YamlViewDefWriter {
  // TODO duplicate code (copied from YamlTableDefWriter)
  // TODO extract yaml processing
  // TODO inline views
  val MaxLineLength = 100
  private val yamlChA = ":#\t\r\n" // FIXME more escapes for yaml key / value
    .toCharArray.map(_.toString).toSet
  private val yamlChB = ",[]{}&*!|>'%@`\" "
    .toCharArray.map(_.toString).toSet
  private val yamlChC = " "
    .toCharArray.map(_.toString).toSet
  private def escapeYamlKey(s: String) =
    // FIXME more escapes for yaml key!
    if (s != null && s.contains(" #"))
      "\"" +
       s +
      "\""
    else s
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

  def toYaml(field: IoFieldDef) = {
    import field._
    val t = type_.type_ getOrElse new Type(null, None, None, None, false)
    val joinString = Option(joinToParent) getOrElse ""
    val typeString = List(
      Option(t.name),
      t.length,
      t.totalDigits,
      t.fractionDigits).flatMap(x => x) mkString " "
    val orderByString = Option(orderBy) getOrElse ""
    val enumString = Option(enum_)
      .filter(_ != null)
      .filter(_ != Nil)
      .map { enums =>
        if  (enums.exists(e => e.contains(' ') || e.contains('\'')))
             enums.map("'" + _ + "'")
        else enums
      }
      .map(_.mkString("(", ", ", ")"))
      .getOrElse("")

    val hasExpr = isExpression || expression != null
    val hasAlias = alias != null && alias != name.replace('.', '_')
    val expr =
      if (hasExpr)
        Option(expression).map(" = " + _).getOrElse(" =")
      else if (hasAlias)
        Option(name).map(" = " + _) getOrElse " ="
      else ""
    val explicitSaveTo =
      // TODO & != unique refs.defaultRefTableAlias (use code from loader)
      Option(saveTo).filter(_ != fieldName).orNull
    val hasResolver = explicitSaveTo != null || saveTo != null && isExpression || resolver != null
    val saveExpr =
      if (hasResolver)
        " ->" +
          Option(explicitSaveTo).map(" " + _).getOrElse("") +
          Option(resolver).map(" = " + _).getOrElse("")
      else ""
    val optionsString = Option(options) getOrElse ""
    val defString = escapeYamlKey(List(
      (if (hasAlias) alias else name, 22),
      (optionsString,  0),
      ((type_.nullable, isCollection) match {
        case (Some(false), false) => "!"
        case (Some(false), true)  => "+"
        case (Some(true),  false) => "?"
        case (_,           true)  => "*"
        case (None,        false) => ""
      },  2),
      (joinString,  0),
      (typeString, 12),
      (orderByString, 0),
      (enumString, 2))
      .foldLeft(("", 0))((r, t) => (
        List(r._1, t._1).mkString(" ").trim.padTo(r._2 + t._2 - 1, " ").mkString,
        r._2 + t._2))._1 +
      expr + saveExpr
    )

    val hasComment = comments != null
    val hasExtras = extras != null && !extras.isEmpty
    val slComment =
      if (hasComment) s" : ${escapeYamlValue(comments)}" else ""
    if (!hasComment && !hasExtras) defString.trim
    else if (hasExtras && !extras.contains("fields")) { // XXX not is view TODO inline child views!
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
  def toYaml(view: IoViewDef): String = {
    import view._
    List(
      Some(name).map("name:     " + _),
      Option(db).map(escapeYamlValue).map("db:       " + _),
      Option(table)
        .map("table:    " + _ + Option(tableAlias).map(" " + _).getOrElse(""))
        .map(_.trim),
      Option(comments).map(c =>
        wrapped(escapeYamlValue(c), "comments:", " " * 10)),
      Option(extends_).map(escapeYamlValue).map("extends:  " + _),
      Option(joins).filter(_.size > 0).map(x => "joins:"),
      Option(joins).filter(_.size > 0)
        .map(_.map("- " + escapeYamlValue(_)).mkString("\n")),
      Option(saveTo).map(x => "save-to:"),
      Option(saveTo).filter(_.size > 0)
        .map(_.map("- " + escapeYamlValue(_)).mkString("\n")),
      Some("fields:"),
      Option(fields.map(f => "- " + toYaml(f)).mkString("\n")),
      Option(groupBy).filter(_.size > 0).map(x => "group:"),
      Option(groupBy).filter(_.size > 0)
        .map(_.map("- " + escapeYamlValue(_)).mkString("\n")),
      Option(having).filter(_.size > 0).map(x => "having:"),
      Option(having).filter(_.size > 0)
        .map(_.map("- " + escapeYamlValue(_)).mkString("\n")),
      Option(orderBy).filter(_.size > 0).map(x => "order:"),
      Option(orderBy).filter(_.size > 0)
        .map(_.map("- " + escapeYamlValue(_)).mkString("\n")),
      Option(filter).filter(_.size > 0).map(x => "filter:"),
      Option(filter).filter(_.size > 0)
        .map(_.map("- " + escapeYamlValue(_)).mkString("\n")),
      Option(extras).filter(!_.isEmpty) // TODO map extras - all types
        .map(_.map(e => e._1 + ": " + e._2.toString).mkString("\n")),
    ).flatMap(x => x).mkString("\n")
  }
  def toYaml(views: Seq[IoViewDef]): String =
    views.map(toYaml).mkString("\n\n") +
      (if (!views.isEmpty) "\n" else "")
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

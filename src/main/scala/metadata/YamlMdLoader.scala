package metadata

import java.io.File
import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import scala.io.Source
import scala.reflect.BeanProperty
import scala.xml.PrettyPrinter
import org.yaml.snakeyaml.Yaml

case class YamlTypeDef(
  table: String,
  comment: String,
  columns: Seq[YamlFieldDef])

case class YamlFieldDef(
  name: String,
  cardinality: String,
  maxOccurs: Option[Int],
  typeName: String,
  length: Option[Int],
  fraction: Option[Int],
  isExpression: Boolean,
  expression: String,
  joinToParent: String,
  orderBy: String,
  comment: String)

object YamlMdLoader {
  /*
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  // FIXME use common resources
  def typedefFiles = recursiveListFiles(new File("../db")).toSeq
    .filter(_.getName endsWith ".yaml")
  val typedefResources = classOf[YamlTypeDef].getClassLoader.getResources("")
    .asScala.toSeq.flatMap(u =>
      Source.fromURL(u, "UTF-8").mkString.trim.split("\\s+"))
    .filter(_.endsWith(".yaml")).map("/" + _)
  def filesToStrings = typedefFiles.map(f =>
    try Source.fromFile(f)("UTF-8").mkString catch {
      case e: Exception =>
        throw new RuntimeException("Error reading file:" + f, e)
    })
  def resourcesToStrings = typedefResources.map(r =>
    Source.fromInputStream(getClass.getResourceAsStream(r)).mkString)
  val typedefStrings = filesToStrings.map(s =>
    s.split("\\-\\-\\-").toSeq).flatMap(x =>
    x).map(_.trim).filter(_.length > 0) toSeq
  val yamlTypeDefs = loadYamlTypeDefs
  private def loadYamlTypeDefs = {
    typedefStrings map loadYamlTypeDef
  }
  */
  def loadYamlTypeDef(typeDef: String) = {
    val tdMap = mapAsScalaMap(
      (new Yaml).load(typeDef).asInstanceOf[java.util.Map[String, _]]).toMap
    val table = tdMap.get("table").map(_.toString) getOrElse null
    val comment = tdMap.get("comment").map(_.toString) getOrElse null
    val colSrc = tdMap.get("columns")
      .map(m => m.asInstanceOf[java.util.ArrayList[_]].toList)
      .getOrElse(Nil)
    val colDefs = colSrc map loadYamlFieldDef
    YamlTypeDef(table, comment, colDefs)
  }

  val FieldDef = {
    val ident = "[_a-zA-z][_a-zA-Z0-9]*"
    val qualifiedIdent = <a>{ident}(\.{ident})?</a>.text
    val int = "[0-9]+"
    val s = "\\s*"

    val name = qualifiedIdent
    val quant = "([\\?\\!]|([\\*\\+](\\.\\.(\\d*[1-9]\\d*))?))"
    val join = "\\[.*?\\]"
    val order = "\\~?#(\\s*\\(.*?\\))?"
    val typ = qualifiedIdent
    val len = int
    val frac = int
    val expr = ".*"
    val pattern =
      <a>
        ({name})({s}{quant})?({s}{join})?({s}{typ})?({s}{len})?({s}{frac})?({s}{order})?({s}=({expr})?)?
      </a>.text.trim

    ("^" + pattern + "$").r
  }

  def loadYamlFieldDef(src: Any) = {
    val ThisFail = "Failed to load column definition"
    def colDef(nameEtc: String, comment: String) = nameEtc match {
      case FieldDef(name, _, quant, _, _, _, maxOcc, joinToParent, typ, _,
        len, frac, order, _, isExpr, expr) =>
        def t(s: String) = Option(s).map(_.trim).filter(_ != "").orNull
        def i(s: String) = Option(s).map(_.trim.toInt)
        def cardinality = Option(t(quant)).map(_.take(1)).orNull
        YamlFieldDef(name, cardinality, i(maxOcc), t(typ), i(len), i(frac),
          isExpr != null, t(expr), t(joinToParent), t(order), comment)
      case _ => throw new RuntimeException(ThisFail +
        " - unexpected format: " + nameEtc.trim)
    }
    src match {
      case nameEtc: java.lang.String =>
        colDef(nameEtc.toString, null)
      case x: java.util.Map[_, _] =>
        val m = x.asInstanceOf[java.util.Map[_, _]]
        if (m.size == 1) {
          val entry = m.entrySet.toList(0)
          val nameEtc = entry.getKey
          val comment = entry.getValue
          colDef(nameEtc.toString, comment.toString)
        } else throw new RuntimeException(ThisFail +
          " - more than one entry for column: " + m.toMap.toString())
      case x => throw new RuntimeException(ThisFail +
        " - unexpected field definition class: " + x.getClass
        + "\nentry: " + x.toString)
    }
  }

  def xsdType(f: YamlFieldDef) = (f.typeName, f.length, f.fraction) match {
    // FIXME do properly (check unsupported patterns, ...)
    // FIXME TODO complex types
    case (null, None, None) => null
    case (null, Some(len), None) => new XsdType(null, len)
    case (null, Some(len), Some(frac)) => new XsdType("decimal", len, frac)
    case ("anySimpleType", _, _) => new XsdType("anySimpleType")
    case ("date", _, _) => new XsdType("date")
    case ("dateTime", _, _) => new XsdType("dateTime")
    case ("string", None, _) => new XsdType("string")
    case ("string", Some(len), _) => new XsdType("string", len)
    case ("boolean", _, _) => new XsdType("boolean")
    case ("int", None, _) => new XsdType("int")
    case ("int", Some(len), _) => new XsdType("int", len)
    case ("long", None, _) => new XsdType("long")
    case ("long", Some(len), _) => new XsdType("long", len)
    case ("decimal", None, None) => new XsdType("decimal")
    case ("decimal", Some(len), None) => new XsdType("decimal", len, 0)
    case ("decimal", Some(len), Some(frac)) => new XsdType("decimal", len, frac)
    case ("base64binary", None, _) => new XsdType("base64binary")
    case ("base64binary", Some(len), _) => new XsdType("base64binary", len)
    // if no known xsd type name found - let it be complex type!
    case (x, _, _) => new XsdType(x, true)
  }
}

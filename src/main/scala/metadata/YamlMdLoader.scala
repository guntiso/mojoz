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
  typeName: String,
  length: Option[Int],
  fraction: Option[Int],
  isExpression: Boolean,
  expression: String,
  comment: String)

object YamlMdLoader {
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
  def loadYamlFieldDef(src: Any) = {
    val ThisFail = "Failed to load column definition"
    def colDef(nameAndType: String, comment: String) = {
      val defAndValue = nameAndType.split("=", 2).toList
      val isExpression = defAndValue.length > 1
      val value = Option(if (isExpression) defAndValue(1) else null)
        .map(_.trim).filter(_ != "") getOrElse null
      val parts = defAndValue(0).split("[\\s]+").toList
      val name = parts(0)
      val cardinalities = parts.tail.filter(Set("?", "!", "*", "+").contains(_))
      val cardinality = cardinalities match {
        case Nil => null
        case c :: Nil => c
        case _ => throw new RuntimeException(ThisFail +
          " - unexpected format: " + nameAndType.trim)
      }
      val sizes = parts.tail
        .map(s => try Some(s.toInt) catch { case x => None })
        .flatMap(x => x)
      val (length, fraction) = sizes match {
        case Nil => (None, None)
        case length :: Nil => (Some(length), None)
        case total :: fraction :: Nil => (Some(total), Some(fraction))
        case x => throw new RuntimeException(ThisFail +
          " - unexpected format: " + nameAndType.trim)
      }
      val types = parts.tail
        .filterNot(cardinalities contains)
        .filterNot(sizes.map(_.toString) contains)
      val typeName = types match {
        case Nil => null
        case t :: Nil => t
        case _ => throw new RuntimeException(ThisFail +
          " - unexpected format: " + nameAndType.trim)
      }
      YamlFieldDef(name, cardinality, typeName, length, fraction,
        isExpression, value, comment)
    }
    src match {
      case nameAndType: java.lang.String =>
        colDef(nameAndType.toString, null)
      case x: java.util.Map[_, _] =>
        val m = x.asInstanceOf[java.util.Map[_, _]]
        if (m.size == 1) {
          val entry = m.entrySet.toList(0)
          val nameAndType = entry.getKey
          val comment = entry.getValue
          colDef(nameAndType.toString, comment.toString)
        } else throw new RuntimeException(ThisFail +
          " - more than one entry for column: " + m.toMap.toString())
      case x => throw new RuntimeException(ThisFail +
        " - unexpected field definition class: " + x.getClass
        + "\nentry: " + x.toString)
    }
  }
  def xsdType(col: YamlFieldDef) = (col.name, col.typeName) match {
    // FIXME do properly (support lengths, check unsupported patterns, ...)
    // FIXME TODO complex types
    case (_, null) => null
    case (_, "date") => new XsdType("date")
    case (_, "dateTime") => new XsdType("dateTime")
    case (_, "string") =>
      if (col.length isDefined) new XsdType("string", col.length.get)
      else new XsdType("string")
    case (_, "boolean") => new XsdType("boolean")
    case (_, "int") => new XsdType("int")
    case (_, "long") => new XsdType("long")
    case (_, "decimal") =>
      new XsdType("decimal", col.length.get, col.fraction.get)
    case (_, "base64binary") => new XsdType("base64binary")
    // if no known xsd type name found - let it be complex type!
    case (_, x) => new XsdType(x, true)
  }
}

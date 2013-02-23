package metadata

import java.io.File
import java.util.ArrayList

import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import scala.io.Source
import scala.reflect.BeanProperty

import org.yaml.snakeyaml.Yaml

import metadata.DbConventions.{ xsdNameToDbName => dbName }

case class XsdTypeDef(
  name: String,
  table: String,
  joins: String, // tresql from clause
  xtnds: String,
  comment: String,
  fields: Seq[XsdFieldDef])

case class XsdFieldDef(
  table: String,
  tableAlias: String,
  name: String,
  alias: String,
  isCollection: Boolean,
  isExpression: Boolean,
  comment: String)

object YamlViewDefLoader {
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def typedefFiles = recursiveListFiles(new File("../views")).toSeq
  val typedefResources = classOf[XsdTypeDef].getClassLoader.getResources("")
    .asScala.toSeq.flatMap(u =>
      Source.fromURL(u, "UTF-8").mkString.trim.split("\\s+"))
    .filter(_.endsWith(".yaml")).map("/" + _)
  def filesToStrings =
    typedefFiles.map(f => Source.fromFile(f).mkString)
  def resourcesToStrings = typedefResources.map(r =>
    Source.fromInputStream(getClass.getResourceAsStream(r)).mkString)
  val typedefStrings = resourcesToStrings.map(s =>
    s.split("\\-\\-\\-").toSeq).flatMap(x =>
    x).map(_.trim).filter(_.length > 0) toSeq
  val typedefs = loadTypeDefs
  def loadTypeDef(typeDef: String) = {
    case class YamlXsdTypeDef(
      @BeanProperty var name: String,
      @BeanProperty var table: String,
      @BeanProperty var joins: String,
      @BeanProperty var `extends`: String,
      @BeanProperty var comment: String,
      @BeanProperty var fields: ArrayList[YamlXsdTypeDef]) {
      def this() = this(null, null, null, null, null, null)
      def this(name: String) = this(name, null, null, null, null, null)
    }
    def mapF(fields: ArrayList[YamlXsdTypeDef]) =
      if (fields == null) null else fields.map(xsdTypeDef).map(f =>
        XsdFieldDef(f.table, null, f.name, null, false, false, f.comment)).toList
    // TODO defer this analysis to later phase, it will clean up the code! 
    def xsdTypeDef(yamlTypeDef: YamlXsdTypeDef): XsdTypeDef = yamlTypeDef match {
      case YamlXsdTypeDef(name, null, j, null, c, f) =>
        XsdTypeDef(name, dbName(name), j, null, c, mapF(f))
      case YamlXsdTypeDef(name, null, j, x, c, f) =>
        XsdTypeDef(name, null, j, x, c, mapF(f))
      case YamlXsdTypeDef(null, table, j, null, c, f) =>
        XsdTypeDef(table, dbName(table), j, null, c, mapF(f))
      case YamlXsdTypeDef(name, table, j, x, c, f) =>
        XsdTypeDef(name, dbName(table), j, x, c, mapF(f))
    }
    xsdTypeDef((new Yaml).loadAs(typeDef, classOf[YamlXsdTypeDef]))
  }
  private def checkTypedefs(td: Seq[XsdTypeDef]) = {
    // TODO diagnostics: m(t.xtnds) and all other!!!
    // check names not repeating
    // check extends only existing
    // check field names not repeating
    // check field names not repeating on extends? or overwrite instead?
  }
  private def loadTypeDefs = {
    val td = typedefStrings map loadTypeDef
    checkTypedefs(td)
    val m = td.map(t => (t.name, t)).toMap
    // TODO extends is also typedef, join!
    def mapExtends(t: XsdTypeDef) =
      if (t.table != null) t else {
        @tailrec
        def baseTable(t: XsdTypeDef, visited: List[String]): String =
          if (visited contains t.name)
            sys.error("Cyclic extends: " +
              (t.name :: visited).reverse.mkString(" -> "))
          else if (t.table != null) t.table
          else baseTable(m(t.xtnds), t.name :: visited)
        t.copy(table = baseTable(t, Nil))
      }
    def mapFields(t: XsdTypeDef) = {
      val aliasToTable = JoinsParser.aliases(t.joins)
      def mapCollection(f: XsdFieldDef) =
        if (f.name.indexOf("*") < 0) f
        else f.copy(name = f.name.replace("*", "").trim, isCollection = true)
      def mapExpression(f: XsdFieldDef) =
        if (f.name.indexOf("=") < 0) f
        else f.copy(name = f.name.replace("=", "").trim, isExpression = true)
      def mapField(f: XsdFieldDef) =
        if (f.name.indexOf(".") < 0)
          f.copy(table = dbName(t.table), name = dbName(f.name))
        else {
          val parts = f.name.split("\\.")
          val tableOrAlias = dbName(parts(0))
          val table = dbName(aliasToTable.getOrElse(tableOrAlias, tableOrAlias))
          val tableAlias = if (table == tableOrAlias) null else tableOrAlias
          val name = dbName(parts(1))
          val alias = dbName(f.name.replace(".", "_"))
          f.copy(table = table, tableAlias = tableAlias,
            name = name, alias = alias)
        }
      t.copy(fields =
        t.fields.map(mapCollection).map(mapExpression).map(mapField))
    }
    td.map(mapExtends).map(mapFields)
  }
  // typedef name to typedef
  val nameToViewDef = typedefs.map(t => (t.name, t)).toMap
  // typedef name to typedef with extended field list
  val nameToExtendedViewDef = typedefs.map(t =>
    if (t.xtnds == null) t else {
      @tailrec
      def baseFields(t: XsdTypeDef, visited: List[String]): Seq[XsdFieldDef] =
        if (visited contains t.name)
          sys.error("Cyclic extends: " +
            (t.name :: visited).reverse.mkString(" -> "))
        else if (t.xtnds == null) t.fields
        else baseFields(nameToViewDef(t.xtnds), t.name :: visited) ++ t.fields
      t.copy(fields = baseFields(t, Nil))
    }).map(t => (t.name, t)).toMap
}

package metadata

import java.io.File
import java.util.ArrayList

import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.JavaConversions._
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
  expression: String,
  nullable: Boolean,
  xsdType: XsdType,
  isI18n: Boolean,
  comment: String)

object YamlViewDefLoader {
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def typedefFiles = recursiveListFiles(new File("../views")).toSeq
  // getClass.getClassLoader.getResources("") does not work from jar :(
  val typedefResources = ((getClass.getClassLoader.getResources("")
    .asScala.toSeq.flatMap(u =>
      Source.fromURL(u, "UTF-8").mkString.trim.split("\\s+"))) ++
    Option(getClass.getResourceAsStream("/.view-files.txt"))
    .map(Source.fromInputStream(_)(io.Codec("UTF-8"))
      .getLines.toList).getOrElse(Nil))
    .filter(_.endsWith(".yaml")).map("/" + _).toSet.toSeq
  def filesToStrings =
    typedefFiles.map(f => Source.fromFile(f).mkString)
  def resourcesToStrings = typedefResources.map(r =>
    Source.fromInputStream(getClass.getResourceAsStream(r)).mkString)
  val typedefStrings = resourcesToStrings.map(s =>
    s.split("\\-\\-\\-").toSeq).flatMap(x =>
    x).map(_.trim).filter(_.length > 0) toSeq
  private val rawTypeDefs = typedefStrings map loadTypeDef
  private val nameToRawTypeDef = rawTypeDefs.map(t => (t.name, t)).toMap
  val nameToTableName = rawTypeDefs.map(t =>
    // TODO repeating code xtnds visiting
    if (t.table != null) (t.name, t.table) else {
      @tailrec
      def baseTable(t: XsdTypeDef, visited: List[String]): String =
        if (visited contains t.name)
          sys.error("Cyclic extends: " +
            (t.name :: visited).reverse.mkString(" -> "))
        else if (t.table != null) t.table
        else baseTable(nameToRawTypeDef(t.xtnds), t.name :: visited)
      (t.name, baseTable(t, Nil))
    }).toMap
  lazy val typedefs = loadTypeDefs
  def loadTypeDef(typeDef: String) = {
    val tdMap = mapAsScalaMap(
      (new Yaml).load(typeDef).asInstanceOf[java.util.Map[String, _]]).toMap
    def get(name: String) = tdMap.get(name).map(_.toString) getOrElse null
    val rawName = get("name")
    val rawTable = get("table")
    val joins = get("joins")
    val xtnds = get("extends")
    val comment = get("comment")
    val fieldsSrc = tdMap.get("fields")
      .map(m => m.asInstanceOf[java.util.ArrayList[_]].toList)
      .getOrElse(Nil)
    val (name, table) = (rawName, rawTable, xtnds) match {
      case (name, null, null) => (name, dbName(name))
      case (name, null, _) => (name, null)
      case (null, table, null) => (table, dbName(table))
      case (name, table, _) => (name, dbName(table))
    }
    val yamlFieldDefs = fieldsSrc map YamlMdLoader.loadYamlFieldDef
    def toXsdFieldDef(yfd: YamlFieldDef) = {
      val table = null
      val tableAlias = null
      // XXX add cardinality marker to support nullable override
      val name = yfd.name + Option(yfd.cardinality).map(_ => "|").getOrElse("")
      val alias = null
      val isCollection = Set("*", "+").contains(yfd.cardinality)
      val isExpression = yfd.isExpression
      val expression = yfd.expression
      val nullable = Option(yfd.cardinality)
        .map(c => Set("?", "*").contains(c)) getOrElse true
      val comment = yfd.comment
      // FIXME COMPLEX TYPES!!!
      val rawXsdType =
        if (isExpression) Option(YamlMdLoader.xsdType(yfd)) else None
      val xsdTypeFe =
        if (isExpression)
          MdConventions.fromExternal(
            // XXX unnecessary complex structure used
            ExFieldDef(name, rawXsdType, None, null, comment)).xsdType
        else null
      // XXX undo convention
      val xsdType =
        if (xsdTypeFe != null && xsdTypeFe.name == "string" && rawXsdType == None)
          new XsdType("string")
        else xsdTypeFe

      XsdFieldDef(table, tableAlias, name, alias, isCollection,
        isExpression, expression, nullable, xsdType, false, comment)
    }
    XsdTypeDef(name, table, joins, xtnds, comment,
      yamlFieldDefs map toXsdFieldDef)
  }
  private def checkTypedefs(td: Seq[XsdTypeDef]) = {
    // TODO diagnostics: m(t.xtnds) and all other!!!
    // check names not repeating
    // check extends only existing
    // check field names not repeating
    // check field names not repeating on extends? or overwrite instead?
  }
  private def loadTypeDefs = {
    val td = rawTypeDefs
    checkTypedefs(td)
    val m = td.map(t => (t.name, t)).toMap
    // TODO extends is also typedef, join!
    def mapExtends(t: XsdTypeDef) =
      // TODO repeating code xtnds visiting
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
      val joins = JoinsParser(t.table, t.joins)
      val aliasToTable =
        joins.filter(_.alias != null).map(j => j.alias -> j.table).toMap
      val tableOrAliasToJoin =
        joins.map(j => Option(j.alias).getOrElse(j.table) -> j).toMap
      def hasCardinalityOverride(f: XsdFieldDef) = f.name.endsWith("|")
      def cleanName(name: String) = name.replace("|", "")
      val nameToCardinalityOverride = t.fields.map(f =>
        (cleanName(f.name), hasCardinalityOverride(f))).toMap
      def mapCleanName(f: XsdFieldDef) = f.copy(name = cleanName(f.name))
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
      def mapColumnDef(f: XsdFieldDef, cardinalityOverride: Boolean) = {
        if (f.isExpression) f
        else {
          val col = Metadata.getCol(t, f)
          val tableOrAlias = Option(f.tableAlias) getOrElse f.table
          // FIXME autojoins nullable?
          val nullable =
            if (cardinalityOverride) f.nullable
            else tableOrAliasToJoin.get(tableOrAlias).map(_.nullable)
              .getOrElse(Right(col.nullable)) match {
                case Right(b) => b || col.nullable
                case Left(s) => true // FIXME Left(nullableTableDependency)!
              }
          f.copy(nullable = nullable, xsdType = col.xsdType,
            comment = Option(f.comment) getOrElse col.comment)
        }
      }
      t.copy(fields = t.fields.map(mapCleanName)
        .map(f => (mapField(f), nameToCardinalityOverride(f.name)))
        .map(fo => mapColumnDef(fo._1, fo._2)))
    }
    td.map(mapExtends).map(mapFields)
  }

  private val noI18n = Set("company.name", "customer.name")
  private def setI18n(t: XsdTypeDef) = {
    val fMap = t.fields.filter(!_.isExpression).filter(!_.isCollection)
      .map(f => (f.table + "." + f.name, f)).toMap // todo or use table alias?
    val i18n = t.fields.filter(!_.isExpression).filter(!_.isCollection)
      .filter(f => !f.name.endsWith("_eng") && !f.name.endsWith("_rus"))
      .filter(f =>
        !fMap.containsKey(f.table + "." + f.name + "_eng") &&
          !fMap.containsKey(f.table + "." + f.name + "_rus"))
      .filter(f =>
        Metadata.tableDef(f.table).cols.exists(_.name == f.name + "_eng") &&
          Metadata.tableDef(f.table).cols.exists(_.name == f.name + "_rus"))
      .toSet
      // XXX do not translate company name :( TODO plugin rules
      .filter(f => !noI18n(f.table + "." + f.name))
    if (i18n.size == 0) t
    else t.copy(fields = t.fields.map(f =>
      if (i18n contains f) f.copy(isI18n = true) else f))
  }
  // typedef name to typedef
  lazy val nameToViewDef = typedefs.map(t => (t.name, t)).toMap
  // typedef name to typedef with extended field list
  lazy val nameToExtendedViewDef = typedefs.map(t =>
    // TODO repeating code xtnds visiting
    if (t.xtnds == null) t else {
      @tailrec
      def baseFields(t: XsdTypeDef, visited: List[String]): Seq[XsdFieldDef] =
        if (visited contains t.name)
          sys.error("Cyclic extends: " +
            (t.name :: visited).reverse.mkString(" -> "))
        else if (t.xtnds == null) t.fields
        else baseFields(nameToViewDef(t.xtnds), t.name :: visited) ++ t.fields
      t.copy(fields = baseFields(t, Nil))
    })
    .map(setI18n)
    .map(t => (t.name, t)).toMap
}

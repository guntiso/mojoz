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
  draftOf: String,
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
    Option(getClass.getResourceAsStream("/-view-files.txt"))
    .map(Source.fromInputStream(_)(io.Codec("UTF-8"))
      .getLines.toList).getOrElse(Nil))
    .filter(_.endsWith(".yaml")).map("/" + _).toSet.toSeq
  def filesToStrings =
    typedefFiles.map(f => Source.fromFile(f).mkString)
  def resourcesToStrings = typedefResources.map(r =>
    Source.fromInputStream(getClass.getResourceAsStream(r)).mkString)
  val typedefStrings = resourcesToStrings.map(s =>
    s.split("\\-\\-\\-").toSeq).flatMap(x =>
    x).map(_.trim).filter(_.length > 0).toSeq
  private val rawTypeDefs = typedefStrings map loadTypeDef
  private val nameToRawTypeDef = rawTypeDefs.map(t => (t.name, t)).toMap
  @tailrec
  private def baseTable(t: XsdTypeDef, nameToTypeDef: Map[String, XsdTypeDef],
    visited: List[String]): String =
    if (visited contains t.name)
      sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
    else if (t.table != null) t.table
    else baseTable(nameToTypeDef.get(Option(t.xtnds) getOrElse t.draftOf)
      .getOrElse(sys.error("base table not found, type: " + t.name)),
      nameToTypeDef, t.name :: visited)
  lazy val typedefs = loadTypeDefs
  def loadTypeDef(typeDef: String) = {
    val tdMap = mapAsScalaMap(
      (new Yaml).load(typeDef).asInstanceOf[java.util.Map[String, _]]).toMap
    def get(name: String) = tdMap.get(name).map(_.toString) getOrElse null
    val rawName = get("name")
    val rawTable = get("table")
    val joins = get("joins")
    val xtnds = get("extends")
    val draftOf = get("draft-of")
    val comment = get("comment")
    val fieldsSrc = tdMap.get("fields")
      .map(m => m.asInstanceOf[java.util.ArrayList[_]].toList)
      .getOrElse(Nil)
    val xtndsOrDraft = Option(xtnds) getOrElse draftOf
    val (name, table) = (rawName, rawTable, xtndsOrDraft) match {
      case (name, null, null) => (name, dbName(name))
      case (name, null, _) => (name, null)
      case (null, table, null) => (table, dbName(table))
      case (name, table, _) => (name, dbName(table))
    }
    if (xtnds != null && draftOf != null) sys.error(
      "extends and draft-of is not supported simultaneously, type: " + name)
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
      val rawXsdType = Option(YamlMdLoader.xsdType(yfd))
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
        else if (xsdTypeFe != null) xsdTypeFe
        else rawXsdType.getOrElse(null)

      XsdFieldDef(table, tableAlias, name, alias, isCollection,
        isExpression, expression, nullable, xsdType, false, comment)
    }
    XsdTypeDef(name, table, joins, xtnds, draftOf, comment,
      yamlFieldDefs map toXsdFieldDef)
  }
  private def checkTypedefs(td: Seq[XsdTypeDef]) = {
    val m = td.map(t => (t.name, t)).toMap
    if (m.size < td.size) sys.error("repeating definition of " +
      td.groupBy(_.name).filter(_._2.size > 1).map(_._1).mkString(", "))
    @tailrec
    def checkExtends(t: XsdTypeDef, nameToTypeDef: Map[String, XsdTypeDef],
      visited: List[String]): Boolean =
      if (visited contains t.name) sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
      else if (t.xtnds == null && t.draftOf == null) true
      else checkExtends(nameToTypeDef.get(Option(t.xtnds) getOrElse t.draftOf)
        .getOrElse(sys.error("Type " + t.name + " extends non-existing type " +
          Option(t.xtnds).getOrElse(t.draftOf))),
        nameToTypeDef, t.name :: visited)
    td.foreach(t => checkExtends(t, m, Nil))
    def propName(f: XsdFieldDef) = Option(f.alias) getOrElse f.name
    def checkRepeatingFieldNames(t: XsdTypeDef) =
      if (t.fields.map(propName).toSet.size < t.fields.size) sys.error(
        "Type " + t.name + " defines multiple fields named " + t.fields
          .groupBy(propName).filter(_._2.size > 1).map(_._1).mkString(", "))
    td foreach checkRepeatingFieldNames
    // check field names not repeating on extends? or overwrite instead?
  }
  private def checkTypedefMapping(td: Seq[XsdTypeDef]) = {
    val m = td.map(t => (t.name, t)).toMap
    td foreach { t =>
      t.fields.foreach { f =>
        if (f.xsdType.isComplexType)
          m.get(f.xsdType.name) getOrElse sys.error("Type " + f.xsdType.name +
            " referenced from " + t.name + " is not found")
        else if (!f.isExpression) Metadata.getCol(t, f)
      }
    }
  }
  private def loadTypeDefs = {
    val td = rawTypeDefs
    checkTypedefs(td)
    val m = td.map(t => (t.name, t)).toMap
    def draftName(n: String) = n + "_draft"
    // TODO missing details drafts
    val missingDrafts = td
      .filter(_.draftOf != null)
      .map(t => m(t.draftOf))
      .map(t => Option(t.xtnds)) // TODO missing parent draft hierarchy
      .flatMap(x => x)
      .map(n => XsdTypeDef(draftName(n), m(n).table, null, null, n, null, Nil))
      .filter(t => !m.containsKey(t.name))
      .toSet

    val td1 = (td ++ missingDrafts)
    val m1 = td1.map(t => (t.name, t)).toMap
    def mapDraftExtends(t: XsdTypeDef) =
      // TODO missing parent draft hierarchy
      if (t.draftOf == null || m1(t.draftOf).xtnds == null) t
      else t.copy(xtnds = draftName(m1(t.draftOf).xtnds))
    def mapTableExtends(t: XsdTypeDef) =
      if (t.table != null) t else t.copy(table = baseTable(t, m1, Nil))
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
          def maybeNoPrefix(fName: String) = fName.indexOf("_.") match {
            case -1 => fName
            case rmIdx => fName.substring(rmIdx + 2)
          }
          val alias = dbName(maybeNoPrefix(f.name).replace(".", "_"))
          f.copy(table = table, tableAlias = tableAlias,
            name = name, alias = alias)
        }
      def mapColumnDef(f: XsdFieldDef, cardinalityOverride: Boolean) = {
        if (f.isExpression || f.isCollection) f
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
    def inheritJoins(t: XsdTypeDef) = {
      @tailrec
      def inheritedJoins(t: XsdTypeDef): String =
        if (t.joins != null || t.xtnds == null && t.draftOf == null) t.joins
        else inheritedJoins(m1(Option(t.xtnds) getOrElse t.draftOf))
      if (t.xtnds == null && t.draftOf == null) t
      else t.copy(joins = inheritedJoins(t))
    }
    val td2 = td1
      .map(mapDraftExtends)
      .map(mapTableExtends)
      .map(mapFields)
      .map(inheritJoins)
    val m2 = td2.map(t => (t.name, t)).toMap
    def resolveDraftFields(t: XsdTypeDef) =
      if (t.draftOf == null) t else {
        @tailrec
        def baseFields(t: XsdTypeDef, fields: Seq[XsdFieldDef]): Seq[XsdFieldDef] =
          if (t.draftOf == null) t.fields.map(_.copy(nullable = true)) ++ fields
          else baseFields(m2(t.draftOf), t.fields ++ fields)
        t.copy(fields = baseFields(t, Nil))
      }
    val td3 = td2.map(resolveDraftFields)
    checkTypedefs(td3)
    checkTypedefMapping(td3)
    td3
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
    if (t.xtnds == null) t else {
      @tailrec
      def baseFields(t: XsdTypeDef, fields: Seq[XsdFieldDef]): Seq[XsdFieldDef] =
        if (t.xtnds == null) t.fields ++ fields
        else baseFields(nameToViewDef(t.xtnds), t.fields ++ fields)
      t.copy(fields = baseFields(t, Nil))
    })
    .map(setI18n)
    .map(t => (t.name, t)).toMap
}

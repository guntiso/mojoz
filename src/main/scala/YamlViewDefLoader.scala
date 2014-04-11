package mojoz.metadata

import java.io.File
import java.util.ArrayList

import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import scala.collection.mutable.Queue
import scala.io.Source
import scala.reflect.BeanProperty

import org.yaml.snakeyaml.Yaml

import mojoz.metadata.in._
import mojoz.metadata.io._
import mojoz.metadata.DbConventions.{ xsdNameToDbName => dbName }

case class ViewDef[T](
  name: String,
  table: String,
  tableAlias: String,
  joins: String, // from clause
  filter: String, // where clause
  group: String, // group by clause
  order: String, // order by clause
  xtnds: String,
  draftOf: String,
  detailsOf: String,
  comment: String,
  fields: Seq[FieldDef[T]])

case class FieldDef[T](
  table: String,
  tableAlias: String,
  name: String,
  alias: String,
  isCollection: Boolean,
  maxOccurs: String,
  isExpression: Boolean,
  isFilterable: Boolean,
  expression: String,
  nullable: Boolean,
  isForcedCardinality: Boolean,
  type_ : T,
  enum: Seq[String],
  joinToParent: String,
  orderBy: String,
  isI18n: Boolean,
  comment: String) {
}

package in {

class YamlViewDefLoader(val tableMetadata: TableMetadata[XsdType], val rawViewDefs: Seq[MdDef]) {
  this: JoinsParser with ExpressionRules =>

  private val typedefStrings = rawViewDefs
  private val rawTypeDefs = typedefStrings map { md =>
    try loadRawTypeDef(md.body) catch {
      case e: Exception => throw new RuntimeException(
        "Failed to load typedef from " + md.filename, e) // TODO line number
    }
  }
  private val nameToRawTypeDef = rawTypeDefs.map(t => (t.name, t)).toMap
  private def isSimpleType(f: FieldDef[XsdType]) =
    f.type_ == null || !f.type_.isComplexType
  private def isComplexType(f: FieldDef[XsdType]) =
    f.type_ != null && f.type_.isComplexType
  @tailrec
  private def baseTable(t: ViewDef[_],
    nameToTypeDef: collection.Map[String, ViewDef[_]],
    visited: List[String]): String =
    if (visited contains t.name)
      sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
    else if (t.table != null) t.table
    else baseTable(nameToTypeDef.get(t.xtnds)
      .getOrElse(sys.error("base table not found, type: " + t.name)),
      nameToTypeDef, t.name :: visited)
  val viewDefs = buildTypeDefs(rawTypeDefs).sortBy(_.name)
  def loadRawTypeDef(typeDef: String) = {
    val tdMap = mapAsScalaMap(
      (new Yaml).load(typeDef).asInstanceOf[java.util.Map[String, _]]).toMap
    def get(name: String) = tdMap.get(name).map(_.toString) getOrElse null
    val rawName = get("name")
    val rawTable = get("table")
    val joins = get("joins")
    val filter = get("filter")
    val group = get("group")
    val order = get("order")
    val xtnds = get("extends")
    val draftOf = get("draft-of")
    val detailsOf = get("details-of")
    val comment = get("comment")
    val fieldsSrc = tdMap.get("fields")
      .map(m => m.asInstanceOf[java.util.ArrayList[_]].toList)
      .getOrElse(Nil)
    val extendsOrModifies =
      Option(xtnds).orElse(Option(detailsOf)).getOrElse(draftOf)
    val (name, table) = (rawName, rawTable, extendsOrModifies) match {
      case (name, null, null) => (name, dbName(name))
      case (name, null, _) => (name, null)
      case (null, table, null) => (table, dbName(table))
      case (name, table, _) => (name, dbName(table))
    }
    if (List(xtnds, draftOf, detailsOf).filter(_ != null).size > 1) sys.error(
      "extends, draft-of, details-of are not supported simultaneously, type: " + name)
    val yamlFieldDefs = fieldsSrc map YamlMdLoader.loadYamlFieldDef
    def toXsdFieldDef(yfd: YamlFieldDef) = {
      val table = null
      val tableAlias = null
      val name = yfd.name
      val alias = null
      val isCollection = Set("*", "+").contains(yfd.cardinality)
      val maxOccurs = yfd.maxOccurs.map(_.toString).orNull
      val isExpression = yfd.isExpression
      val expression = yfd.expression
      val isFilterable = if (!isExpression) true
      else if (expression == null) false
      else isExpressionFilterable(expression)
      /*
      // if expression consists of a call to function attached to Env,
      // then we consider is not filterable, otherwise consider it db function and filterable
      else QueryParser.parseExp(expression) match {
	    case QueryParser.Fun(f, p, _) 
	    if Env.isDefined(f) && Env.functions.flatMap(_.getClass.getMethods.filter(
	      m => m.getName == f && m.getParameterTypes.length == p.size
	    ).headOption) != None 
	      => false
	    case _ => true
	  }
      */
      val nullable = Option(yfd.cardinality)
        .map(c => Set("?", "*").contains(c)) getOrElse true
      val isForcedCardinality = yfd.cardinality != null
      val joinToParent = yfd.joinToParent
      val enum = yfd.enum
      val orderBy = yfd.orderBy
      val comment = yfd.comment
      val rawXsdType = Option(YamlMdLoader.xsdType(yfd))
      val xsdTypeFe =
        if (isExpression)
          MdConventions.fromExternal(
            // XXX unnecessary complex structure used
            ExColumnDef(name, rawXsdType, None, null, null, comment)).type_
        else null
      val xsdType =
        if (xsdTypeFe != null) xsdTypeFe else rawXsdType getOrElse null

      FieldDef(table, tableAlias, name, alias, isCollection, maxOccurs,
        isExpression, isFilterable, expression, nullable, isForcedCardinality,
        xsdType, enum, joinToParent, orderBy, false, comment)
    }
    ViewDef(name, table, null, joins, filter, group, order,
      xtnds, draftOf, detailsOf, comment,
      yamlFieldDefs map toXsdFieldDef)
  }
  private def checkTypedefs(td: Seq[ViewDef[_]]) = {
    val m: Map[String, ViewDef[_]] = td.map(t => (t.name, t)).toMap
    if (m.size < td.size) sys.error("repeating definition of " +
      td.groupBy(_.name).filter(_._2.size > 1).map(_._1).mkString(", "))
    @tailrec
    def checkExtends(t: ViewDef[_], nameToTypeDef: Map[String, ViewDef[_]],
      visited: List[String]): Boolean = {
      val extendsOrModifies =
        Option(t.xtnds).orElse(Option(t.detailsOf)).getOrElse(t.draftOf)
      if (visited contains t.name) sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
      else if (extendsOrModifies == null) true
      else checkExtends(nameToTypeDef.get(extendsOrModifies)
        .getOrElse(sys.error("Type " + t.name +
          " extends or modifies non-existing type " + extendsOrModifies)),
        nameToTypeDef, t.name :: visited)
    }
    td.foreach(t => checkExtends(t, m, Nil))
    def propName(f: FieldDef[_]) = Option(f.alias) getOrElse f.name
    def checkRepeatingFieldNames(t: ViewDef[_]) =
      if (t.fields.map(propName).toSet.size < t.fields.size) sys.error(
        "Type " + t.name + " defines multiple fields named " + t.fields
          .groupBy(propName).filter(_._2.size > 1).map(_._1).mkString(", "))
    td foreach checkRepeatingFieldNames
    // check field names not repeating on extends? or overwrite instead?
  }
  private def checkTypedefMapping(td: Seq[ViewDef[XsdType]]) = {
    val m = td.map(t => (t.name, t)).toMap
    td foreach { t =>
      t.fields.foreach { f =>
        if (f.type_.isComplexType)
          m.get(f.type_.name) getOrElse sys.error("Type " + f.type_.name +
            " referenced from " + t.name + " is not found")
        else if (!f.isExpression) tableMetadata.columnDef(t, f)
      }
    }
  }
  def draftName(n: String) = // XXX
    if (n endsWith "_details") n.replace("_details", "_draft_details")
    else n + "_draft"
  def detailsName(n: String) = n + "_details"
  private def buildTypeDefs(rawTypeDefs: Seq[ViewDef[XsdType]]) = {
    //checkTypedefs(rawTypeDefs) FIXME does not allow draft names in type hierarchy
    val rawTypesMap: Map[String, ViewDef[XsdType]] = rawTypeDefs.map(t => (t.name, t)).toMap
    val resolvedTypes = new collection.mutable.ArrayBuffer[ViewDef[XsdType]]()
    val resolvedTypesMap = collection.mutable.Map[String, ViewDef[XsdType]]()

    def inheritTable[T](t: ViewDef[T]) =
      if (t.table != null) t
      else t.copy(table = baseTable(t, resolvedTypesMap, Nil))

    def inheritJoins(t: ViewDef[XsdType]) = {
      @tailrec
      def inheritedJoins(t: ViewDef[XsdType]): String =
        if (t.joins != null || t.xtnds == null) t.joins
        else inheritedJoins(m(t.xtnds))
      if (t.xtnds == null) t
      else t.copy(joins = inheritedJoins(t))
    }

    def inheritFilter(t: ViewDef[XsdType]) = {
      @tailrec
      def inheritedFilter(t: ViewDef[XsdType]): String =
        if (t.filter != null || t.xtnds == null) t.filter
        else inheritedFilter(m(t.xtnds))
      if (t.xtnds == null) t
      else t.copy(filter = inheritedFilter(t))
    }

    def resolveBaseTableAlias[T](t: ViewDef[T]) = t.copy(tableAlias =
      parseJoins(t.table, t.joins).filter(_.table == t.table).toList match {
        case Join(a, _, _) :: Nil => // if only one base table encountered return alias
          Option(a) getOrElse t.table
        case _ => "b" // default base table alias 
      })

    def resolveFieldNamesAndTypes(t: ViewDef[XsdType]) = {
      val joins = parseJoins(t.table, t.joins)
      val aliasToTable =
        joins.filter(_.alias != null).map(j => j.alias -> j.table).toMap
      val tableOrAliasToJoin =
        joins.map(j => Option(j.alias).getOrElse(j.table) -> j).toMap
      def resolveNameAndTable[T](f: FieldDef[T]) =
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
      def resolveTypeFromDbMetadata(f: FieldDef[XsdType]) = {
        if (f.isExpression || f.isCollection) f
        else {
          val col = tableMetadata.columnDef(t, f)
          val tableOrAlias = Option(f.tableAlias) getOrElse f.table
          // FIXME autojoins nullable?
          val nullable =
            if (f.isForcedCardinality) f.nullable
            else tableOrAliasToJoin.get(tableOrAlias).map(_.nullable)
              .getOrElse(Right(col.nullable)) match {
                case Right(b) => b || col.nullable
                case Left(s) => true // FIXME Left(nullableTableDependency)!
              }
          f.copy(nullable = nullable, type_ = col.type_,
            enum = Option(f.enum) getOrElse col.enum,
            comment = Option(f.comment) getOrElse col.comment)
        }
      }
      t.copy(fields = t.fields
        .map(resolveNameAndTable)
        .map(resolveTypeFromDbMetadata))
    }

    // drafts logic
    def resolveDraft(draft: ViewDef[XsdType], addMissing: (ViewDef[XsdType]) => Any) = {
      def canReuseSimpleFieldsForDraft(t: ViewDef[XsdType]) = {
        // FIXME this is not exactly correct, or should be pluggable
        !t.fields
          .filter(isSimpleType)
          .filter(!_.isCollection)
          .filter(!_.nullable)
          .exists(_.isForcedCardinality)
      }
      def canReuseComplexFieldsForDraft(t: ViewDef[XsdType]) = {
        !t.fields
          .filter(isComplexType)
          .exists(f => !canReuseAsDraft(m(f.type_.name)))
      }
      def canReuseFieldsForDraft(t: ViewDef[XsdType]) =
        canReuseSimpleFieldsForDraft(t) && canReuseComplexFieldsForDraft(t)
      def canReuseAsDraft(t: ViewDef[XsdType]): Boolean =
        canReuseFieldsForDraft(t) &&
          (t.xtnds == null || canReuseAsDraft(m(t.xtnds)))
      def addMissingDraftOf(draftOf: ViewDef[XsdType]) = addMissing(
        draftOf.copy(name = draftName(draftOf.name), table = null, joins = null,
          xtnds = null, draftOf = draftOf.name, fields = Nil))
      def draftField(f: FieldDef[XsdType]) =
        if (isComplexType(f)) {
          val t = m(f.type_.name)
          def fCopy = f.copy(type_ = f.type_.copy(name = draftName(t.name)))
          if (isDefined(draftName(t.name))) fCopy
          else if (canReuseAsDraft(t)) f
          else { addMissingDraftOf(t); fCopy }
        } else if (f.isForcedCardinality && !f.nullable && !f.isCollection)
          f.copy(nullable = true)
        else f
      val draftOf = m(draft.draftOf)
      if (canReuseAsDraft(draftOf))
        draft.copy(xtnds = draftOf.name, draftOf = null)
      else {
        val xDraftName = Option(draftOf.xtnds).map(draftName).orNull
        val xtnds =
          if (draftOf.xtnds == null) null
          else if (isDefined(xDraftName)) xDraftName
          else if (canReuseAsDraft(m(draftOf.xtnds))) draftOf.xtnds
          else { addMissingDraftOf(m(draftOf.xtnds)); xDraftName }
        val table = Option(draft.table) getOrElse draftOf.table
        val joins = Option(draft.joins) getOrElse draftOf.joins
        draft.copy(
          table = table, joins = joins, xtnds = xtnds, draftOf = null,
          fields = draftOf.fields.map(draftField) ++ draft.fields)
      }
    }

    // details logic
    def resolveDetails(details: ViewDef[XsdType], addMissing: (ViewDef[XsdType]) => Any) = {
      def hasChildrenWithDetails(t: ViewDef[XsdType]): Boolean =
        t.fields
          .filter(isComplexType)
          .map(_.type_)
          .filter(t =>
            isDefined(detailsName(t.name)) || hasChildrenWithDetails(m(t.name)))
          .size > 0
      def canReuseFieldsForDetails(t: ViewDef[XsdType]) = !hasChildrenWithDetails(t)
      def canReuseAsDetailsSuper(t: ViewDef[XsdType]): Boolean =
        canReuseFieldsForDetails(t) &&
          (t.xtnds == null || canReuseAsDetails(m(t.xtnds)))
      def canReuseAsDetails(t: ViewDef[XsdType]): Boolean =
        !isDefined(detailsName(t.name)) && canReuseAsDetailsSuper(t)
      def addMissingDetailsOf(dtOf: ViewDef[_]) = addMissing(
        dtOf.copy(name = detailsName(dtOf.name), table = null, joins = null,
          xtnds = null, detailsOf = dtOf.name, fields = Nil))
      def detailsField(f: FieldDef[XsdType]) = if (isSimpleType(f)) f else {
        val t = m(f.type_.name)
        def fCopy = f.copy(type_ = f.type_.copy(name = detailsName(t.name)))
        if (isDefined(detailsName(t.name))) fCopy
        else if (canReuseAsDetails(t)) f
        else { addMissingDetailsOf(t); fCopy }
      }
      val detailsOf = m(details.detailsOf)
      if (canReuseAsDetailsSuper(detailsOf))
        details.copy(xtnds = detailsOf.name, detailsOf = null)
      else {
        val xDetailsName = Option(detailsOf.xtnds).map(detailsName).orNull
        val xtnds =
          if (detailsOf.xtnds == null) null
          else if (isDefined(xDetailsName)) xDetailsName
          else if (canReuseAsDetails(m(detailsOf.xtnds))) detailsOf.xtnds
          else { addMissingDetailsOf(m(detailsOf.xtnds)); xDetailsName }
        val table = Option(details.table) getOrElse (
          if (xtnds == null) detailsOf.table else null)
        val joins = Option(details.joins) getOrElse (
          if (xtnds == null) detailsOf.joins else null)
        details.copy(
          table = table, joins = joins, xtnds = xtnds, detailsOf = null,
          fields = detailsOf.fields.map(detailsField) ++ details.fields)
      }
    }

    def isDefined(tName: String) = rawTypesMap.contains(tName)
    def typeResolved(t: ViewDef[XsdType]) = {
      resolvedTypes += t
      resolvedTypesMap(t.name) = t
      t
    }
    def addMissing(t: ViewDef[XsdType]) = {
      // println("Adding missing type: " + t.name) // TODO not distinct
      resolveType(t)
    }
    // TODO add stack overflow protection
    def m(tName: String) = resolveTypeByName(tName)
    def resolveTypeByName(tName: String): ViewDef[XsdType] =
      resolvedTypesMap.getOrElse(tName,
        resolveUnresolvedType(rawTypesMap(tName)))
    def resolveType(t: ViewDef[XsdType]): ViewDef[XsdType] =
      resolvedTypesMap.getOrElse(t.name, resolveUnresolvedType(t))
    def resolveUnresolvedType(t: ViewDef[XsdType]): ViewDef[XsdType] = typeResolved {
      (t.draftOf, t.detailsOf) match {
        case (null, null) => t
        case (draftOf, null) => resolveDraft(t, addMissing)
        case (null, detailsOf) => resolveDetails(t, addMissing)
      }
    }

    rawTypeDefs foreach resolveType
    val result = resolvedTypes.toList
      .map(inheritTable)
      .map(inheritJoins)
      .map(inheritFilter)
      .map(resolveBaseTableAlias)
      .map(resolveFieldNamesAndTypes)
    checkTypedefs(result)
    checkTypedefMapping(result)
    result
  }
}
}

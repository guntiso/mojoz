package mojoz.metadata

import java.io.File
import java.util.ArrayList

import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import scala.collection.mutable.Queue
import scala.io.Source

import org.yaml.snakeyaml.Yaml

import mojoz.metadata.in._
import mojoz.metadata.in.JoinsParser.Join
import mojoz.metadata.io._
import mojoz.metadata.DbConventions.{ xsdNameToDbName => dbName }

case class ViewDef[T](
  name: String,
  table: String,
  tableAlias: String,
  joins: String, // from clause
  filter: String, // where clause
  groupBy: String,
  having: String,
  orderBy: String,
  extends_ : String,
  draftOf: String,
  detailsOf: String,
  comments: String,
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
  comments: String) {
}

package in {

class YamlViewDefLoader(
    tableMetadata: Metadata[Type],
    yamlMd: Seq[YamlMd],
    conventions: MdConventions = new MdConventions,
    isExpressionFilterable: (String) => Boolean = (_) => true) {
  this: JoinsParser =>

  val sources = yamlMd.filter(YamlMd.isViewDef)
  private val rawTypeDefs = sources map { md =>
    try loadRawTypeDef(md.body) catch {
      case e: Exception => throw new RuntimeException(
        "Failed to load viewdef from " + md.filename, e) // TODO line number
    }
  }
  private val nameToRawTypeDef = rawTypeDefs.map(t => (t.name, t)).toMap
  private def isSimpleType(f: FieldDef[Type]) =
    f.type_ == null || !f.type_.isComplexType
  private def isComplexType(f: FieldDef[Type]) =
    f.type_ != null && f.type_.isComplexType
  @tailrec
  private def baseTable(t: ViewDef[_],
    nameToTypeDef: collection.Map[String, ViewDef[_]],
    visited: List[String]): String =
    if (visited contains t.name)
      sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
    else if (t.table != null) t.table
    else baseTable(nameToTypeDef.get(t.extends_)
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
    val having = get("having")
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
      // then we consider is not filterable,
      // otherwise consider it db function and filterable
      QueryParser.parseExp(expression) match {
        case QueryParser.Fun(f, p, _) if Env.isDefined(f) && Env.functions.flatMap {
          _.getClass.getMethods.filter(
            m => m.getName == f && m.getParameterTypes.length == p.size).headOption
        } != None => false
        case _ => true
      }
      */
      val nullable = Option(yfd.cardinality)
        .map(c => Set("?", "*").contains(c)) getOrElse true
      val isForcedCardinality = yfd.cardinality != null
      val joinToParent = yfd.joinToParent
      val enum = yfd.enum
      val orderBy = yfd.orderBy
      val comment = yfd.comments
      val rawXsdType = Option(YamlMdLoader.xsdType(yfd, conventions))
      val xsdTypeFe =
        if (isExpression)
          conventions.fromExternal(name, rawXsdType, None)._1
        else null
      val xsdType =
        if (xsdTypeFe != null) xsdTypeFe else rawXsdType getOrElse null

      FieldDef(table, tableAlias, name, alias, isCollection, maxOccurs,
        isExpression, isFilterable, expression, nullable, isForcedCardinality,
        xsdType, enum, joinToParent, orderBy, false, comment)
    }
    ViewDef(name, table, null, joins, filter, group, having, order,
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
        Option(t.extends_).orElse(Option(t.detailsOf)).getOrElse(t.draftOf)
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
  private def checkTypedefMapping(td: Seq[ViewDef[Type]]) = {
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
  private def buildTypeDefs(rawTypeDefs: Seq[ViewDef[Type]]) = {
    //checkTypedefs(rawTypeDefs) FIXME does not allow draft names in type hierarchy
    val rawTypesMap: Map[String, ViewDef[Type]] = rawTypeDefs.map(t => (t.name, t)).toMap
    val resolvedTypes = new collection.mutable.ArrayBuffer[ViewDef[Type]]()
    val resolvedTypesMap = collection.mutable.Map[String, ViewDef[Type]]()

    def inheritTable[T](t: ViewDef[T]) =
      if (t.table != null) t
      else t.copy(table = baseTable(t, resolvedTypesMap, Nil))

    def inheritJoins(t: ViewDef[Type]) = {
      @tailrec
      def inheritedJoins(t: ViewDef[Type]): String =
        if (t.joins != null || t.extends_ == null) t.joins
        else inheritedJoins(m(t.extends_))
      if (t.extends_ == null) t
      else t.copy(joins = inheritedJoins(t))
    }

    def inheritFilter(t: ViewDef[Type]) = {
      @tailrec
      def inheritedFilter(t: ViewDef[Type]): String =
        if (t.filter != null || t.extends_ == null) t.filter
        else inheritedFilter(m(t.extends_))
      if (t.extends_ == null) t
      else t.copy(filter = inheritedFilter(t))
    }

    def resolveBaseTableAlias[T](t: ViewDef[T]) = t.copy(tableAlias =
      parseJoins(t.table, t.joins).filter(_.table == t.table).toList match {
        case Join(a, _, _) :: Nil => // if only one base table encountered return alias
          Option(a) getOrElse t.table
        case _ => "b" // default base table alias 
      })

    def resolveFieldNamesAndTypes(t: ViewDef[Type]) = {
      val joins = parseJoins(t.table, t.joins)
      val aliasToTable =
        joins.filter(_.alias != null).map(j => j.alias -> j.table).toMap
      val tableOrAliasToJoin =
        joins.map(j => Option(j.alias).getOrElse(j.table) -> j).toMap
      def reduceExpression[T](f: FieldDef[T]) =
        if (f.isExpression && f.name.indexOf(".") < 0 && f.expression != null &&
          YamlTableDefLoader.QualifiedIdentDef.pattern.matcher(f.expression).matches)
          f.copy(isExpression = false, expression = null,
            name = f.expression, alias = f.name)
        else if (f.isExpression && f.expression != null &&
          // escape syntax for no-arg functions and pseudo-columns
          f.expression.startsWith("()"))
          f.copy(expression = f.expression.substring(2).trim)
        else f
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
          val alias = Option(f.alias).map(dbName) getOrElse
            dbName(maybeNoPrefix(f.name).replace(".", "_"))
          f.copy(table = table, tableAlias = tableAlias,
            name = name, alias = alias)
        }
      def resolveTypeFromDbMetadata(f: FieldDef[Type]) = {
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
            comments = Option(f.comments) getOrElse col.comments)
        }
      }
      t.copy(fields = t.fields
        .map(reduceExpression)
        .map(resolveNameAndTable)
        .map(resolveTypeFromDbMetadata))
    }

    // drafts logic
    def resolveDraft(draft: ViewDef[Type], addMissing: (ViewDef[Type]) => Any) = {
      def canReuseSimpleFieldsForDraft(t: ViewDef[Type]) = {
        // FIXME this is not exactly correct, or should be pluggable
        !t.fields
          .filter(isSimpleType)
          .filter(!_.isCollection)
          .filter(!_.nullable)
          .exists(_.isForcedCardinality)
      }
      def canReuseComplexFieldsForDraft(t: ViewDef[Type]) = {
        !t.fields
          .filter(isComplexType)
          .exists(f => !canReuseAsDraft(m(f.type_.name)))
      }
      def canReuseFieldsForDraft(t: ViewDef[Type]) =
        canReuseSimpleFieldsForDraft(t) && canReuseComplexFieldsForDraft(t)
      def canReuseAsDraft(t: ViewDef[Type]): Boolean =
        canReuseFieldsForDraft(t) &&
          (t.extends_ == null || canReuseAsDraft(m(t.extends_)))
      def addMissingDraftOf(draftOf: ViewDef[Type]) = addMissing(
        draftOf.copy(name = draftName(draftOf.name), table = null, joins = null,
          extends_ = null, draftOf = draftOf.name, fields = Nil))
      def draftField(f: FieldDef[Type]) =
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
        draft.copy(extends_ = draftOf.name, draftOf = null)
      else {
        val xDraftName = Option(draftOf.extends_).map(draftName).orNull
        val xtnds =
          if (draftOf.extends_ == null) null
          else if (isDefined(xDraftName)) xDraftName
          else if (canReuseAsDraft(m(draftOf.extends_))) draftOf.extends_
          else { addMissingDraftOf(m(draftOf.extends_)); xDraftName }
        val table = Option(draft.table) getOrElse draftOf.table
        val joins = Option(draft.joins) getOrElse draftOf.joins
        draft.copy(
          table = table, joins = joins, extends_ = xtnds, draftOf = null,
          fields = draftOf.fields.map(draftField) ++ draft.fields)
      }
    }

    // details logic
    def resolveDetails(details: ViewDef[Type], addMissing: (ViewDef[Type]) => Any) = {
      def hasChildrenWithDetails(t: ViewDef[Type]): Boolean =
        t.fields
          .filter(isComplexType)
          .map(_.type_)
          .filter(t =>
            isDefined(detailsName(t.name)) || hasChildrenWithDetails(m(t.name)))
          .size > 0
      def canReuseFieldsForDetails(t: ViewDef[Type]) = !hasChildrenWithDetails(t)
      def canReuseAsDetailsSuper(t: ViewDef[Type]): Boolean =
        canReuseFieldsForDetails(t) &&
          (t.extends_ == null || canReuseAsDetails(m(t.extends_)))
      def canReuseAsDetails(t: ViewDef[Type]): Boolean =
        !isDefined(detailsName(t.name)) && canReuseAsDetailsSuper(t)
      def addMissingDetailsOf(dtOf: ViewDef[_]) = addMissing(
        dtOf.copy(name = detailsName(dtOf.name), table = null, joins = null,
          extends_ = null, detailsOf = dtOf.name, fields = Nil))
      def detailsField(f: FieldDef[Type]) = if (isSimpleType(f)) f else {
        val t = m(f.type_.name)
        def fCopy = f.copy(type_ = f.type_.copy(name = detailsName(t.name)))
        if (isDefined(detailsName(t.name))) fCopy
        else if (canReuseAsDetails(t)) f
        else { addMissingDetailsOf(t); fCopy }
      }
      val detailsOf = m(details.detailsOf)
      if (canReuseAsDetailsSuper(detailsOf))
        details.copy(extends_ = detailsOf.name, detailsOf = null)
      else {
        val xDetailsName = Option(detailsOf.extends_).map(detailsName).orNull
        val xtnds =
          if (detailsOf.extends_ == null) null
          else if (isDefined(xDetailsName)) xDetailsName
          else if (canReuseAsDetails(m(detailsOf.extends_))) detailsOf.extends_
          else { addMissingDetailsOf(m(detailsOf.extends_)); xDetailsName }
        val table = Option(details.table) getOrElse (
          if (xtnds == null) detailsOf.table else null)
        val joins = Option(details.joins) getOrElse (
          if (xtnds == null) detailsOf.joins else null)
        details.copy(
          table = table, joins = joins, extends_ = xtnds, detailsOf = null,
          fields = detailsOf.fields.map(detailsField) ++ details.fields)
      }
    }

    def isDefined(tName: String) = rawTypesMap.contains(tName)
    def typeResolved(t: ViewDef[Type]) = {
      resolvedTypes += t
      resolvedTypesMap(t.name) = t
      t
    }
    def addMissing(t: ViewDef[Type]) = {
      // println("Adding missing type: " + t.name) // TODO not distinct
      resolveType(t)
    }
    // TODO add stack overflow protection
    def m(tName: String) = resolveTypeByName(tName)
    def resolveTypeByName(tName: String): ViewDef[Type] =
      resolvedTypesMap.getOrElse(tName,
        resolveUnresolvedType(rawTypesMap(tName)))
    def resolveType(t: ViewDef[Type]): ViewDef[Type] =
      resolvedTypesMap.getOrElse(t.name, resolveUnresolvedType(t))
    def resolveUnresolvedType(t: ViewDef[Type]): ViewDef[Type] = typeResolved {
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

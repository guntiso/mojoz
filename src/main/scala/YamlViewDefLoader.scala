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
import mojoz.metadata.io._
import mojoz.metadata.DbConventions.{ xsdNameToDbName => dbName }
import ViewDef._
import FieldDef._

object ViewDef {
  trait ViewDefBase[+F] {
    val name: String
    val table: String
    val tableAlias: String
    val joins: Seq[String] // from clause
    val filter: Seq[String] // where clause
    val groupBy: Seq[String]
    val having: Seq[String]
    val orderBy: Seq[String]
    val extends_ : String
    val draftOf: String
    val detailsOf: String
    val comments: String
    val fields: Seq[F]
  }
}
case class ViewDef[+F](
  name: String,
  table: String,
  tableAlias: String,
  joins: Seq[String], // from clause
  filter: Seq[String], // where clause
  groupBy: Seq[String],
  having: Seq[String],
  orderBy: Seq[String],
  extends_ : String,
  draftOf: String,
  detailsOf: String,
  comments: String,
  fields: Seq[F],
  extras: Map[String, Any]) extends ViewDefBase[F]

object FieldDef {
  trait FieldDefBase[+T] {
    val table: String
    val tableAlias: String
    val name: String
    val alias: String
    val isCollection: Boolean
    val maxOccurs: String
    val isExpression: Boolean
    val expression: String
    val nullable: Boolean
    val type_ : T
    val enum: Seq[String]
    val joinToParent: String
    val orderBy: String
    val comments: String
  }
}
case class FieldDef[+T](
  table: String,
  tableAlias: String,
  name: String,
  alias: String,
  isCollection: Boolean,
  maxOccurs: String,
  isExpression: Boolean,
  expression: String,
  nullable: Boolean,
  isForcedCardinality: Boolean,
  type_ : T,
  enum: Seq[String],
  joinToParent: String,
  orderBy: String,
  isI18n: Boolean,
  comments: String,
  extras: Map[String, Any]) extends FieldDefBase[T]

package in {

class YamlViewDefLoader(
    tableMetadata: TableMetadata[TableDef.TableDefBase[ColumnDef.ColumnDefBase[Type]]],
    yamlMd: Seq[YamlMd],
    joinsParser: JoinsParser = (_, _) => Nil, 
    conventions: MdConventions = MdConventions,
    extendedViewDefTransformer: ViewDef[FieldDef[Type]] => ViewDef[FieldDef[Type]] = v => v) {
  import YamlViewDefLoader._

  val parseJoins = joinsParser
  val sources = yamlMd.filter(YamlMd.isViewDef)
  private val rawTypeDefs = sources.map { md: YamlMd =>
    try loadRawTypeDefs(md.body) catch {
      case e: Exception => throw new RuntimeException(
        "Failed to load viewdef from " + md.filename, e) // TODO line number
    }
  }.flatten
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
  private[in] val nameToViewDef = viewDefs.map(t => (t.name, t)).toMap
  val extendedViewDefs = viewDefs.map(t =>
    if (t.extends_ == null) t else {
      @tailrec
      def baseFields(v: ViewDef[FieldDef[Type]], fields: t.fields.type): t.fields.type =
        if (v.extends_ == null) (v.fields ++ fields).asInstanceOf[t.fields.type]
        else baseFields(nameToViewDef(v.extends_),
          (v.fields ++ fields).asInstanceOf[t.fields.type])
      t.copy(fields = baseFields(t, Nil.asInstanceOf[t.fields.type]))
    })
    .map(extendedViewDefTransformer)
    .map(t => (t.name, t)).toMap
  def loadRawTypeDefs(typeDef: String): List[ViewDef[FieldDef[Type]]] = {
    val tdMap = mapAsScalaMap(
      (new Yaml).load(typeDef).asInstanceOf[java.util.Map[String, _]]).toMap
    loadRawTypeDefs(tdMap)
  }
  def loadRawTypeDefs(tdMap: Map[String, Any]): List[ViewDef[FieldDef[Type]]] = {
    def get(name: ViewDefKeys.ViewDefKeys) = getStringSeq(name, true) match {
      case Nil => null
      case x => x mkString ""
    }
    def getStringSeq(name: ViewDefKeys.ViewDefKeys,
        nullToString: Boolean = false): Seq[String] = {
      getSeq(name, nullToString) map {
        case s: java.lang.String => s
        case m: java.util.Map[_, _] =>
          if (m.size == 1) m.entrySet.toList(0).getKey.toString
          else m.toString // TODO error?
        case x => x.toString
      }
    }
    def getSeq(name: ViewDefKeys.ViewDefKeys,
        nullToString: Boolean = false): Seq[_] = tdMap.get(name.toString) match {
      case Some(s: java.lang.String) => Seq(s)
      case Some(a: java.util.ArrayList[_]) => a.toList
      case None => Nil
      case Some(null) if nullToString => Seq("")
      case Some(null) => Nil
      case x => Seq(x)
    }
    val k = ViewDefKeys
    val rawName = get(k.name)
    val rawTable = get(k.table)
    val joins = getStringSeq(k.joins)
    val filter = getStringSeq(k.filter)
    val group = getStringSeq(k.group)
    val having = getStringSeq(k.having)
    val order = getStringSeq(k.order)
    val xtnds = get(k.extends_)
    val draftOf = get(k.draftOf)
    val detailsOf = get(k.detailsOf)
    val comment = get(k.comment)
    val fieldsSrc = getSeq(k.fields).toList
    val extras = tdMap -- ViewDefKeyStrings
    val extendsOrModifies =
      Option(xtnds).orElse(Option(detailsOf)).getOrElse(draftOf)
    val (name, table) = (rawName, rawTable, extendsOrModifies) match {
      case (name, null, null) => (name, dbName(name)) // FIXME matches null null null => nullpointer
      case (name, null, _) => (name, null)
      case (null, table, null) => (table, dbName(table))
      case (name, table, _) => (name, dbName(table))
    }
    if (List(xtnds, draftOf, detailsOf).filter(_ != null).size > 1) sys.error(
      "extends, draft-of, details-of are not supported simultaneously, type: " + name)
    val yamlFieldDefs = fieldsSrc map YamlMdLoader.loadYamlFieldDef
    def typeName(v: Map[String, Any], defaultSuffix: String) =
      if (v.containsKey("name")) "" + v("name")
      else name + "_" + defaultSuffix
    def toXsdFieldDef(yfd: YamlFieldDef) = {
      val table = null
      val tableAlias = null
      val name = yfd.name
      val alias = null
      val maxOccurs = yfd.maxOccurs.map(_.toString).orNull
      val isCollection = Set("*", "+").contains(yfd.cardinality) && (maxOccurs == null || maxOccurs.toInt > 1)
      val isExpression = yfd.isExpression
      val expression = yfd.expression
      val nullable = Option(yfd.cardinality)
        .map(c => Set("?", "*").contains(c)) getOrElse true
      val isForcedCardinality = yfd.cardinality != null
      val joinToParent = yfd.joinToParent
      val enum = yfd.enum
      val orderBy = yfd.orderBy
      val comment = yfd.comments
      val extras = yfd.extras
      val rawXsdType = Option(YamlMdLoader.xsdType(yfd, conventions))
      if (isViewDef(yfd.extras))
        (yfd.typeName, yfd.extras.get("name").orNull) match {
          case (null, null) | (null, _) | (_, null) =>
          case (fType, cType) => if (fType != cType)
            sys.error(s"Name conflict for inline view: $fType != $cType")
        }
      val xsdTypeFe =
        if (yfd.typeName == null && isViewDef(yfd.extras))
          new Type(typeName(yfd.extras, name), true)
        else if (isExpression || rawTable == "")
          conventions.fromExternal(name, rawXsdType, None)._1
        else null
      val xsdType =
        if (xsdTypeFe != null) xsdTypeFe else rawXsdType getOrElse null

      FieldDef(table, tableAlias, name, alias, isCollection, maxOccurs,
        isExpression, expression, nullable, isForcedCardinality,
        xsdType, enum, joinToParent, orderBy, false, comment, extras)
    }
    def isViewDef(m: Map[String, Any]) =
      m != null && m.containsKey("fields")
    val fieldDefs = yamlFieldDefs map toXsdFieldDef
    ViewDef(name, table, null, joins, filter, group, having, order,
      xtnds, draftOf, detailsOf, comment, fieldDefs, extras) ::
      yamlFieldDefs
      .map(_.extras)
      .zip(fieldDefs)
      .filter(f => isViewDef(f._1))
      .map(f => f._1 + (("name", f._2.type_.name)))
      .map(loadRawTypeDefs)
      .flatten
  }
  private def checkTypedefs(td: Seq[ViewDef[FieldDef[_]]]) = {
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
    def checkRepeatingFieldNames(t: ViewDef[FieldDef[_]]) =
      if (t.fields.map(propName).toSet.size < t.fields.size) sys.error(
        "Type " + t.name + " defines multiple fields named " + t.fields
          .groupBy(propName).filter(_._2.size > 1).map(_._1).mkString(", "))
    td foreach checkRepeatingFieldNames
    // check field names not repeating on extends? or overwrite instead?
  }
  private def checkTypedefMapping(td: Seq[ViewDef[FieldDef[Type]]]) = {
    val m = td.map(t => (t.name, t)).toMap
    td foreach { t =>
      t.fields.foreach { f =>
        if (f.type_.isComplexType)
          m.get(f.type_.name) getOrElse sys.error("Type " + f.type_.name +
            " referenced from " + t.name + " is not found")
        else if (f.table != null)
          tableMetadata.columnDef(t, f)
      }
    }
  }
  // TODO draftName to inputs!
  def draftName(n: String) = // XXX
    if (n endsWith "_details") n.replace("_details", "_draft_details")
    else n + "_draft"
  // TODO detailsName to inputs!
  def detailsName(n: String) = n + "_details"
  private def buildTypeDefs(rawTypeDefs: Seq[ViewDef[FieldDef[Type]]]) = {
    //checkTypedefs(rawTypeDefs) FIXME does not allow draft names in type hierarchy
    val rawTypesMap: Map[String, ViewDef[FieldDef[Type]]] = rawTypeDefs.map(t => (t.name, t)).toMap
    val resolvedTypes = new collection.mutable.ArrayBuffer[ViewDef[FieldDef[Type]]]()
    val resolvedTypesMap = collection.mutable.Map[String, ViewDef[FieldDef[Type]]]()

    def inheritTable[T](t: ViewDef[T]) =
      if (t.table != null) t
      else t.copy(table = baseTable(t, resolvedTypesMap, Nil))

    def inheritTableComments[T](t: ViewDef[T]) =
      if (t.table == null || t.comments != null) t
      else tableMetadata.tableDef(t.table).comments match {
        case null => t
        case tableComments => t.copy(comments = tableComments)
      }

    def mergeSeqs(t: ViewDef[FieldDef[Type]], base: ViewDef[FieldDef[Type]]) =
      t.copy(
        joins = base.joins ++ t.joins,
        filter = base.filter ++ t.filter,
        groupBy = base.groupBy ++ t.groupBy,
        having = base.having ++ t.having,
        orderBy = base.orderBy ++ t.orderBy,
        extras = base.extras ++ t.extras)

    def inheritSeqs(t: ViewDef[FieldDef[Type]]): ViewDef[FieldDef[Type]] =
      if (t.extends_ == null) t
      else mergeSeqs(t, inheritSeqs(m(t.extends_)))

    def resolveBaseTableAlias[T](t: ViewDef[T]) = {
      val partsList =
        Option(t.table).filter(_ != "").map(_.split("\\s+").toList).orNull
      val (table, tableAlias) = partsList match {
        case Nil | null => (null, null)
        case List(table) => (table, null)
        case List(table, tableAlias) => (table, tableAlias)
        case _ => sys.error("Unexpected format for base table: " + t.table)
      }
      t.copy(table = table, tableAlias = tableAlias)
    }

    def resolveFieldNamesAndTypes(t: ViewDef[FieldDef[Type]]) = {
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
          if (f.isExpression || t.table == null) f
          else f.copy(name = dbName(f.name), table = dbName(t.table))
        else {
          val parts = f.name.split("\\.")
          val tableOrAlias = dbName(parts(0))
          var table = Option(
            aliasToTable.get(tableOrAlias)
              .getOrElse(tableMetadata.ref(t.table, tableOrAlias).map(_.refTable)
                .getOrElse(tableOrAlias))).map(dbName).orNull
          val tableAlias =
            if (table == tableOrAlias || parts.size > 2) null else tableOrAlias
          val partsReverseList = parts.toList.reverse
          val name = dbName(partsReverseList.head)
          val path = partsReverseList.tail.reverse
          path.tail foreach { step =>
            table = tableMetadata.ref(table, step) match {
              case Some(ref) => ref.refTable
              case None => step
            }
          }
          def maybeNoPrefix(fName: String) = fName.indexOf("_.") match {
            case -1 => fName
            case rmIdx => fName.substring(rmIdx + 2)
          }
          val alias = Option(f.alias).map(dbName) getOrElse
            dbName(maybeNoPrefix(f.name).replace(".", "_"))
          val expression =
            if (f.expression != null || parts.size < 3) f.expression
            else parts.map(dbName).mkString(".")
          f.copy(table = table, tableAlias = tableAlias,
            name = name, alias = alias, expression = expression)
        }
      def resolveTypeFromDbMetadata(f: FieldDef[Type]) = {
        if (f.isExpression || f.isCollection || (f.type_ != null && f.type_.isComplexType)) f
        else if (f.table == null && Option(f.type_).map(_.name).orNull == null)
          f.copy(type_ = conventions.fromExternal(f.name, Option(f.type_), None)._1)
        else if (t.table == null) f
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
    def resolveDraft(draft: ViewDef[FieldDef[Type]], addMissing: (ViewDef[FieldDef[Type]]) => Any) = {
      def canReuseSimpleFieldsForDraft(t: ViewDef[FieldDef[Type]]) = {
        // FIXME this is not exactly correct, or should be pluggable
        !t.fields
          .filter(isSimpleType)
          .filter(!_.isCollection)
          .filter(!_.nullable)
          .exists(_.isForcedCardinality)
      }
      def canReuseComplexFieldsForDraft(t: ViewDef[FieldDef[Type]]) = {
        !t.fields
          .filter(isComplexType)
          .exists(f => !canReuseAsDraft(m(f.type_.name)))
      }
      def canReuseFieldsForDraft(t: ViewDef[FieldDef[Type]]) =
        canReuseSimpleFieldsForDraft(t) && canReuseComplexFieldsForDraft(t)
      def canReuseAsDraft(t: ViewDef[FieldDef[Type]]): Boolean =
        canReuseFieldsForDraft(t) &&
          (t.extends_ == null || canReuseAsDraft(m(t.extends_)))
      def addMissingDraftOf(draftOf: ViewDef[FieldDef[Type]]) = addMissing(
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
    def resolveDetails(details: ViewDef[FieldDef[Type]], addMissing: (ViewDef[FieldDef[Type]]) => Any) = {
      def hasChildrenWithDetails(t: ViewDef[FieldDef[Type]]): Boolean =
        t.fields
          .filter(isComplexType)
          .map(_.type_)
          .filter(t =>
            isDefined(detailsName(t.name)) || hasChildrenWithDetails(m(t.name)))
          .size > 0
      def canReuseFieldsForDetails(t: ViewDef[FieldDef[Type]]) = !hasChildrenWithDetails(t)
      def canReuseAsDetailsSuper(t: ViewDef[FieldDef[Type]]): Boolean =
        canReuseFieldsForDetails(t) &&
          (t.extends_ == null || canReuseAsDetails(m(t.extends_)))
      def canReuseAsDetails(t: ViewDef[FieldDef[Type]]): Boolean =
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
    def typeResolved(t: ViewDef[FieldDef[Type]]) = {
      resolvedTypes += t
      resolvedTypesMap(t.name) = t
      t
    }
    def addMissing(t: ViewDef[FieldDef[Type]]) = {
      // println("Adding missing type: " + t.name) // TODO not distinct
      resolveType(t)
    }
    // TODO add stack overflow protection
    def m(tName: String) = resolveTypeByName(tName)
    def resolveTypeByName(tName: String): ViewDef[FieldDef[Type]] =
      resolvedTypesMap.getOrElse(tName,
        resolveUnresolvedType(rawTypesMap(tName)))
    def resolveType(t: ViewDef[FieldDef[Type]]): ViewDef[FieldDef[Type]] =
      resolvedTypesMap.getOrElse(t.name, resolveUnresolvedType(t))
    def resolveUnresolvedType(t: ViewDef[FieldDef[Type]]): ViewDef[FieldDef[Type]] = typeResolved {
      (t.draftOf, t.detailsOf) match {
        case (null, null) => t
        case (draftOf, null) => resolveDraft(t, addMissing)
        case (null, detailsOf) => resolveDetails(t, addMissing)
      }
    }

    rawTypeDefs foreach resolveType
    val result = resolvedTypes.toList
      .map(inheritTable)
      .map(inheritSeqs)
      .map(resolveBaseTableAlias)
      .map(resolveFieldNamesAndTypes)
      .map(inheritTableComments)
    checkTypedefs(result)
    checkTypedefMapping(result)
    result
  }
}

object YamlViewDefLoader {
  private object ViewDefKeys extends Enumeration {
    type ViewDefKeys = Value
    val name, table, joins, filter, group, having, order = Value
    val extends_ = Value("extends")
    val draftOf = Value("draft-of")
    val detailsOf = Value("details-of")
    val comment, fields = Value
  }
  private val ViewDefKeyStrings = ViewDefKeys.values.map(_.toString)
  def apply(
    tableMetadata: TableMetadata[TableDef.TableDefBase[ColumnDef.ColumnDefBase[Type]]],
    yamlMd: Seq[YamlMd],
    joinsParser: JoinsParser = (_, _) => Nil,
    conventions: MdConventions = MdConventions,
    extendedViewDefTransformer: ViewDef[FieldDef[Type]] => ViewDef[FieldDef[Type]] = v => v) =
    new YamlViewDefLoader(
      tableMetadata, yamlMd, joinsParser, conventions, extendedViewDefTransformer)
}
}

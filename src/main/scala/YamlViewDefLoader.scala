package mojoz.metadata

import java.io.File
import java.util.ArrayList

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.Map
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.control.NonFatal

import org.yaml.snakeyaml.Yaml

import mojoz.metadata.in._
import mojoz.metadata.io._
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
    val comments: String
    val fields: Seq[F]
    val saveTo: Seq[String]
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
  comments: String,
  fields: Seq[F],
  saveTo: Seq[String],
  extras: Map[String, Any]) extends ViewDefBase[F]

object FieldDef {
  trait FieldDefBase[+T] {
    val table: String
    val tableAlias: String
    val name: String
    val alias: String
    val options: String // persistence options
    val isCollection: Boolean
    val maxOccurs: String
    val isExpression: Boolean
    val expression: String
    val saveTo: String
    val resolver: String // expression, calculates value to be saved
    val nullable: Boolean
    val initial: String
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
  options: String, // persistence options
  isCollection: Boolean,
  maxOccurs: String,
  isExpression: Boolean,
  expression: String,
  saveTo: String,
  resolver: String, // expression, calculates value to be saved
  nullable: Boolean,
  initial: String,
  isForcedCardinality: Boolean,
  type_ : T,
  enum: Seq[String],
  joinToParent: String,
  orderBy: String,
  comments: String,
  extras: Map[String, Any]) extends FieldDefBase[T] {
  def this(name: String, type_ : T = null) = this(
    table = null,
    tableAlias = null,
    name = name,
    alias = null,
    options = null, // persistence options
    isCollection = false,
    maxOccurs = null,
    isExpression = false,
    expression = null,
    saveTo = null,
    resolver = null, // expression, calculates value to be saved
    nullable = true,
    initial = null,
    isForcedCardinality = false,
    type_  = type_,
    enum = null,
    joinToParent = null,
    orderBy = null,
    comments = null,
    extras = null
  )
  def this(that: FieldDefBase[T]) = this(
    table = that.table,
    tableAlias = that.tableAlias,
    name = that.name,
    alias = that.alias,
    options = that.options, // persistence options
    isCollection = that.isCollection,
    maxOccurs = that.maxOccurs,
    isExpression = that.isExpression,
    expression = that.expression,
    saveTo = that.saveTo,
    resolver = that.resolver, // expression, calculates value to be saved
    nullable = that.nullable,
    initial = that.initial,
    isForcedCardinality = if (that.isInstanceOf[FieldDef[_]]) that.asInstanceOf[FieldDef[_]].isForcedCardinality else false,
    type_ = that.type_,
    enum = that.enum,
    joinToParent = that.joinToParent,
    orderBy = that.orderBy,
    comments = that.comments,
    extras = if (that.isInstanceOf[FieldDef[_]]) that.asInstanceOf[FieldDef[_]].extras else null
  )
}

package in {

class YamlViewDefLoader(
    tableMetadata: TableMetadata[TableDef.TableDefBase[ColumnDef.ColumnDefBase[Type]]] = new TableMetadata,
    yamlMd: Seq[YamlMd] = YamlMd.fromResources(),
    joinsParser: JoinsParser = (_, _) => Nil, 
    conventions: MdConventions = new SimplePatternMdConventions,
    uninheritableExtras: Seq[String] = Seq(),
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {
  import YamlViewDefLoader._
  import tableMetadata.dbName

  val parseJoins = joinsParser
  val sources = yamlMd.filter(YamlMd.isViewDef)
  private val rawTypeDefs = sources.map { md: YamlMd =>
    try loadRawTypeDefs(md.body) catch {
      case e: Exception => throw new RuntimeException(
        "Failed to load viewdef from " + md.filename, e) // TODO line number
    }
  }.flatten
  private val nameToRawTypeDef = {
    val duplicateNames =
      rawTypeDefs.map(_.name).groupBy(n => n).filter(_._2.size > 1).map(_._1)
    if (duplicateNames.size > 0)
      throw new RuntimeException(
        "Duplicate view definitions: " + duplicateNames.mkString(", "))
    rawTypeDefs.map(t => (t.name, t)).toMap
  }
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
      .getOrElse(sys.error("Base table not found, type: " + t.name)),
      nameToTypeDef, t.name :: visited)
  val viewDefs = buildTypeDefs(rawTypeDefs).sortBy(_.name)
  private[in] val nameToViewDef = viewDefs.map(t => (t.name, t)).toMap
  val extendedViewDefs = viewDefs.map(t =>
    if (t.extends_ == null) t else {
      def maybeAssignTable(tableName: String)(f: FieldDef[Type]) = {
        if (tableName == null ||
            f.table != null || f.saveTo != null || f.isExpression || f.isCollection ||
            f.type_ != null && f.type_.isComplexType)
          f
        else
          tableMetadata.col(tableName, f.name).map(c => f.copy(table = dbName(tableName))) getOrElse f
      }
      @tailrec
      def baseFields(
          v: ViewDef[FieldDef[Type]],
          fields: Seq[FieldDef[Type]],
          tableName: String): Seq[FieldDef[Type]] =
        if (v.extends_ == null) (v.fields.map(maybeAssignTable(tableName)) ++ fields)
        else
          baseFields(
            nameToViewDef(v.extends_),
            (v.fields.map(maybeAssignTable(tableName)) ++ fields),
            v.table
          )
      t.copy(fields = baseFields(t, Nil, null))
    })
    .map(t => (t.name, t)).toMap
  def loadRawTypeDefs(typeDef: String): List[ViewDef[FieldDef[Type]]] = {
    Option((new Yaml).load(typeDef))
      .map(v => v.asInstanceOf[java.util.Map[String, _]].asScala)
      .map(_.toMap)
      .map(loadRawTypeDefs)
      .getOrElse(Nil)
  }
  def loadRawTypeDefs(tdMap: Map[String, Any]): List[ViewDef[FieldDef[Type]]] = {
    def get(name: ViewDefKeys.ViewDefKeys) = getStringSeq(name) match {
      case null => null
      case Nil => ""
      case x => x mkString ""
    }
    def getStringSeq(name: ViewDefKeys.ViewDefKeys): Seq[String] = {
      Option(getSeq(name)).map(_ map {
        case s: java.lang.String => s
        case m: java.util.Map[_, _] =>
          if (m.size == 1) m.entrySet.asScala.toList(0).getKey.toString
          else m.toString // TODO error?
        case x => x.toString
      }).orNull
    }
    def getSeq(name: ViewDefKeys.ViewDefKeys): Seq[_] = tdMap.get(name.toString) match {
      case Some(s: java.lang.String) => Seq(s)
      case Some(a: java.util.ArrayList[_]) => a.asScala.toList.filter(_ != null)
      case Some(null) => Nil
      case Some(x) => Seq(x)
      case None => null
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
    val comment = get(k.comment)
    val fieldsSrc = Option(getSeq(k.fields)).getOrElse(Nil).toList
    val saveTo = getStringSeq(k.saveTo)
    val extras = tdMap -- ViewDefKeyStrings
    val extendsOrModifies =
      xtnds
    val (name, table) = (rawName, rawTable, extendsOrModifies) match {
      case (name, null, null) => (name, dbName(name)) // FIXME matches null null null => nullpointer
      case (name, null, _) => (name, null)
      case (null, table, null) => (table, dbName(table))
      case (name, table, _) => (name, dbName(table))
    }
    val YamlMdLoader = new YamlMdLoader(typeDefs)
    val yamlFieldDefs = fieldsSrc map YamlMdLoader.loadYamlFieldDef
    def typeName(v: Map[String, Any], defaultSuffix: String) =
      if (v.contains("name")) "" + v("name")
      else name + "_" + defaultSuffix
    def toXsdFieldDef(yfd: YamlFieldDef, viewName: String, viewTable: String, viewSaveTo: Seq[String]) = {
      val table = null
      val tableAlias = null
      val name = yfd.name
      val alias = null
      val options = yfd.options
      val maxOccurs = yfd.maxOccurs.map(_.toString).orNull
      val isCollection = Set("*", "+").contains(yfd.cardinality) && (maxOccurs == null || maxOccurs.toInt > 1)
      val isExpression = yfd.isExpression
      val expression = yfd.expression
      val isResolvable = yfd.isResolvable
      val saveTo = Option(yfd.saveTo) getOrElse {
        if (isResolvable) {
          val simpleName = if (name.indexOf('.') > 0) name.substring(name.indexOf('.') + 1) else name
          def errorMessage =
            s"Failed to resolve save target for $viewName.$simpleName" +
              ", please provide target column name"
          Option(viewSaveTo).filter(_.size > 0)
              .orElse(Option(viewTable).map(_.split("\\s+", 2)(0)).map(Seq(_))).map { tNames =>
            val tables = tNames.flatMap(tName => tableMetadata.tableDefOption(tName))
            if (tables.exists(t => t.cols.exists(_.name == simpleName)))
              simpleName
            else {
              val matches: Set[String] = tables
                .flatMap(_.refs.filter { ref =>
                  ref.cols.size == 1 &&
                    Option(ref.defaultRefTableAlias).getOrElse(ref.refTable) == simpleName
                })
                .map(_.cols(0))
                .foldLeft(Set[String]())(_ + _).toSet
              if (matches.size == 1) matches.head
              else throw new RuntimeException(errorMessage)
            }
          }.getOrElse(throw new RuntimeException(errorMessage))
        } else null
      }
      val resolver = yfd.resolver
      val nullable = Option(yfd.cardinality)
        .map(c => Set("?", "*").contains(c)) getOrElse true
      val initial = null // TODO fieldDef initial?
      val isForcedCardinality = yfd.cardinality != null
      val joinToParent = yfd.joinToParent
      val enum = yfd.enum
      val orderBy = yfd.orderBy
      val comment = yfd.comments
      val extras = yfd.extras
      val rawXsdType = Option(YamlMdLoader.yamlTypeToMojozType(yfd, conventions))
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
          conventions.typeFromExternal(name, rawXsdType)
        else null
      val xsdType =
        if (xsdTypeFe != null) xsdTypeFe else rawXsdType getOrElse null

      FieldDef(table, tableAlias, name, alias, options, isCollection, maxOccurs,
        isExpression, expression, saveTo, resolver, nullable, initial, isForcedCardinality,
        xsdType, enum, joinToParent, orderBy, comment, extras)
    }
    def isViewDef(m: Map[String, Any]) =
      m != null && m.contains("fields")
    val fieldDefs = yamlFieldDefs
      .map(toXsdFieldDef(_, name, table, saveTo))
      .map { f =>
        if (f.extras == null) f
        else f.copy(
          extras = Option(f.extras).map(_ -- ViewDefKeyStrings)
            .filterNot(_.isEmpty).orNull)
      }
    ViewDef(name, table, null, joins, filter, group, having, order,
      xtnds, comment, fieldDefs, saveTo, extras) ::
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
        t.extends_
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
  private def buildTypeDefs(rawTypeDefs: Seq[ViewDef[FieldDef[Type]]]) = {
    val rawTypesMap: Map[String, ViewDef[FieldDef[Type]]] = rawTypeDefs.map(t => (t.name, t)).toMap

    def inheritTable[T](t: ViewDef[T]) =
      if (t.table != null) t
      else t.copy(table = try baseTable(t, rawTypesMap, Nil) catch {
        case ex: Exception => throw new RuntimeException("Failed to find base table for " + t.name, ex)
      })

    def inheritTableComments[T](t: ViewDef[T]) =
      if (t.table == null || t.comments != null) t
      else tableMetadata.tableDef(t.table).comments match {
        case null => t
        case tableComments => t.copy(comments = tableComments)
      }

    def mergeStringSeqs(s1: Seq[String], s2: Seq[String]) = (s1, s2) match {
      case (null, null) => null
      case (null, list) => list
      case (list, null) => list
      case (seq1, seq2) => seq1 ++ seq2
    }

    def mergeSeqs(t: ViewDef[FieldDef[Type]], base: ViewDef[FieldDef[Type]]) =
      t.copy(
        joins = mergeStringSeqs(base.joins, t.joins),
        filter = mergeStringSeqs(base.filter, t.filter),
        groupBy = mergeStringSeqs(base.groupBy, t.groupBy),
        having = mergeStringSeqs(base.having, t.having),
        orderBy = mergeStringSeqs(base.orderBy, t.orderBy),
        extras = base.extras -- uninheritableExtras ++ t.extras)

    def inheritSeqs(t: ViewDef[FieldDef[Type]]): ViewDef[FieldDef[Type]] =
      if (t.extends_ == null) t
      else mergeSeqs(t, inheritSeqs(rawTypesMap(t.extends_)))

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

    def resolveFieldNamesAndTypes(t: ViewDef[FieldDef[Type]]) =
      try resolveFieldNamesAndTypes_(t) catch {
        case NonFatal(ex) =>
          throw new RuntimeException("Failed to resolve field names and types for " + t.name, ex)
      }

    def resolveFieldNamesAndTypes_(t: ViewDef[FieldDef[Type]]) = {
      lazy val joins = parseJoins(
        Option(t.table)
          .map(_ + Option(t.tableAlias).map(" " + _).getOrElse(""))
          .orNull,
        t.joins)
      lazy val aliasToTable =
        joins.filter(_.alias != null).map(j => j.alias -> j.table).toMap ++
          Seq(t.tableAlias).filter(_ != null).map(_ -> t.table).toMap
      lazy val tableOrAliasToJoin =
        joins.map(j => Option(j.alias).getOrElse(j.table) -> j).toMap
      def reduceExpression[T](f: FieldDef[T]) =
        if (f.isExpression && f.name.indexOf(".") < 0 && f.expression != null &&
          YamlTableDefLoader.QualifiedIdentDef.pattern.matcher(f.expression).matches &&
          !Set("true", "false").contains(f.expression))
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
            if (table == tableOrAlias ||
                t.tableAlias == tableOrAlias ||
                parts.size > 2)
              null
            else tableOrAlias
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
            Some(dbName(maybeNoPrefix(f.name).replace(".", "_"))).filter(_ != name).orNull
          val expression =
            if (f.expression != null || parts.size < 3) f.expression
            else parts.map(dbName).mkString(".")
          f.copy(table = table, tableAlias = tableAlias,
            name = name, alias = alias, expression = expression)
        }
      def overwriteSimpleType(base: Type, overwrite: Type) = Type(
        Option(overwrite.name) getOrElse base.name,
        overwrite.length orElse base.length,
        overwrite.totalDigits orElse base.totalDigits,
        overwrite.fractionDigits orElse base.fractionDigits,
        false)
      def resolveTypeFromDbMetadata(f: FieldDef[Type]) = {
        if (f.isExpression || f.isCollection || (f.type_ != null && f.type_.isComplexType)) f
        else if (f.table == null && Option(f.type_).map(_.name).orNull == null)
          f.copy(type_ = conventions.typeFromExternal(f.name, Option(f.type_)))
        else if (f.table == null) f
        else if (t.table == null) f
        else {
          val col = tableMetadata.columnDef(t, f)
          val tableOrAlias = Option(f.tableAlias) getOrElse f.table
          // FIXME autojoins nullable?
          val joinOpt = tableOrAliasToJoin.get(tableOrAlias)
          val joinColOpt =
            // TODO make use of join col type info?
            joinOpt.map(_.columns).getOrElse(Nil).filter(_.name == f.name).headOption
          val nullable =
            if (f.isForcedCardinality) f.nullable
            else joinColOpt.map(_.nullable).getOrElse(col.nullable)
          f.copy(nullable = nullable,
            type_ =
              if (f.type_ != null && f.alias == null) overwriteSimpleType(col.type_, f.type_)
              else col.type_,
            enum = Option(f.enum) getOrElse col.enum,
            comments = Option(f.comments) getOrElse col.comments)
        }
      }
      t.copy(fields = t.fields
        .map(reduceExpression)
        .map(resolveNameAndTable)
        .map(resolveTypeFromDbMetadata))
    }
    val result = rawTypeDefs.toList
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
    val saveTo = Value("save-to")
    val comment, fields = Value
  }
  private val ViewDefKeyStrings = ViewDefKeys.values.map(_.toString)
  def apply(
    tableMetadata: TableMetadata[TableDef.TableDefBase[ColumnDef.ColumnDefBase[Type]]],
    yamlMd: Seq[YamlMd],
    joinsParser: JoinsParser = (_, _) => Nil,
    conventions: MdConventions = new SimplePatternMdConventions,
    uninheritableExtras: Seq[String] = Seq(),
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) =
    new YamlViewDefLoader(
      tableMetadata, yamlMd, joinsParser, conventions, uninheritableExtras, typeDefs)
}
}

package org.mojoz.metadata
package in

import java.io.File
import java.util.ArrayList

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.collection.immutable.Map
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.control.NonFatal

import org.yaml.snakeyaml.LoaderOptions
import org.yaml.snakeyaml.Yaml

import org.mojoz.metadata.in._
import org.mojoz.metadata.io._
import ViewDef._
import FieldDef._


class YamlViewDefLoader(
    tableMetadata: TableMetadata[TableDef.TableDefBase[ColumnDef.ColumnDefBase[Type]]] = new TableMetadata,
    yamlMd: Seq[YamlMd] = YamlMd.fromResources(),
    joinsParser: JoinsParser = (_, _) => Nil, 
    conventions: MdConventions = new SimplePatternMdConventions,
    uninheritableExtras: Seq[String] = Seq(),
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {
  import YamlViewDefLoader._
  import tableMetadata.dbName
  private val MojozExplicitComments    = "mojoz.explicit.comments"
  private val MojozExplicitEnum        = "mojoz.explicit.enum"
  private val MojozExplicitNullability = "mojoz.explicit.nullability"
  private val MojozExplicitType        = "mojoz.explicit.type"
  private val parseJoins = joinsParser
  val sources = yamlMd.filter(YamlMd.isViewDef)
  private val rawViewDefs = transformRawViewDefs(sources.map { md: YamlMd =>
    try loadRawViewDefs(md.body) catch {
      case e: Exception => throw new RuntimeException(
        "Failed to load viewdef from " + md.filename, e) // TODO line number
    }
  }.flatten)
  private val nameToRawViewDef = {
    val duplicateNames =
      rawViewDefs.map(_.name).groupBy(n => n).filter(_._2.size > 1).map(_._1)
    if (duplicateNames.size > 0)
      throw new RuntimeException(
        "Duplicate view definitions: " + duplicateNames.mkString(", "))
    rawViewDefs.map(t => (t.name, t)).toMap
  }
  private def isSimpleType(f: MojozFieldDef) =
    f.type_ == null || !f.type_.isComplexType
  private def isComplexType(f: MojozFieldDef) =
    f.type_ != null && f.type_.isComplexType
  @tailrec
  private def baseTable(t: ViewDef[_],
    nameToViewDef: collection.Map[String, ViewDef[_]],
    visited: List[String]): String =
    if (visited contains t.name)
      sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
    else if (t.table != null) t.table
    else baseTable(nameToViewDef.get(t.extends_)
      .getOrElse(sys.error("Base table not found, view: " + t.name)),
      nameToViewDef, t.name :: visited)
  val plainViewDefs: List[MojozViewDef] = buildViewDefs(rawViewDefs).sortBy(_.name)
  private[in] val nameToPlainViewDef = plainViewDefs.map(t => (t.name, t)).toMap
  protected def overrideField(baseView: MojozViewDef, baseField: MojozFieldDef,
                              view: MojozViewDef, field: MojozFieldDef,
                              viewNamesVisited: List[String]
                             ): MojozFieldDef = {
    def fieldOverrideFailed(msg: String): Nothing = throw new RuntimeException(
      s"Bad override of ${baseView.name}.${propName(baseField)} with ${view.name}.${propName(field)}: $msg")
    fieldOverrideIncompatibilityMessage(baseField, field, viewNamesVisited) match {
      case null =>
        if  (field.enum     == baseField.enum     &&
             field.comments == baseField.comments &&
             field.nullable == baseField.nullable)
             field
        else field.copy(
          enum     = baseField.enum,
          comments = baseField.comments,
          nullable = baseField.nullable)
      case errorMessage =>
        fieldOverrideFailed(errorMessage)
    }
  }
  protected def fieldOverrideIncompatibilityMessage(
      baseField: MojozFieldDef, field: MojozFieldDef, viewNamesVisited: List[String]): String = {
    def typeInfo(t: Type) =
      (if (t.isComplexType) "complex type" else "simple type") + " \"" +
      List(Option(t.name), t.length, t.totalDigits, t.fractionDigits).flatMap(x => x).mkString(" ") +
      "\""
    if (baseField.type_ != field.type_)
      if (baseField.type_.isComplexType && field.type_.isComplexType) {
        val view     = plainViewDefToViewDef(nameToPlainViewDef(field    .type_.name), viewNamesVisited)
        val baseView = plainViewDefToViewDef(nameToPlainViewDef(baseField.type_.name), viewNamesVisited)
        val nameToField =
          view.fields.map(f => propName(f) -> f).toMap
        baseView.fields.find(baseF => nameToField.get(propName(baseF))
          .map(f => fieldOverrideIncompatibilityMessage(baseF, f, viewNamesVisited) != null)
          .getOrElse(true)
        ).map { problematicBaseF =>
          s"${typeInfo(baseField.type_).capitalize} is not compatible with ${typeInfo(field.type_)} because "
          nameToField.get(propName(problematicBaseF))
          .map(problematicF =>
            fieldOverrideIncompatibilityMessage(problematicBaseF, problematicF, viewNamesVisited)
          ).getOrElse(s"field ${field.type_.name}.${propName(problematicBaseF)} is missing")
        }.orNull
      } else {
        s"${typeInfo(baseField.type_).capitalize} is not equal to ${typeInfo(field.type_)}"
      }
    else if (field.enum != null && baseField.enum != field.enum &&
             field.extras != null && field.extras.get(MojozExplicitEnum) == Some(true))
      s"Enum ${baseField.enum} is not equal to ${field.enum}"
    else if (field.comments != null && baseField.comments != field.comments &&
             field.extras != null && field.extras.get(MojozExplicitComments) == Some(true))
      s"Comments '${baseField.comments}' are not equal to '${field.comments}'"
    else if (baseField.isCollection != field.isCollection)
      s"Is collection '${baseField.isCollection}' is not equal to '${field.isCollection}'"
    else if (baseField.nullable != field.nullable &&
             field.extras != null && field.extras.get(MojozExplicitNullability) == Some(true))
      s"Nullable '${baseField.nullable}' is not equal to '${field.nullable}'"
    else
      null
  }
  private def plainViewDefToViewDef(t: MojozViewDef, viewNamesVisited: List[String]): MojozViewDef = {
    if (t.extends_ == null) t else {
      def maybeAssignTable(tableName: String)(f: MojozFieldDef) = {
        if (tableName == null ||
            f.table != null || f.saveTo != null || f.isExpression || f.isCollection ||
            f.type_ != null && f.type_.isComplexType)
          f
        else
          tableMetadata.col(tableName, f.name).map(c => f.copy(table = dbName(tableName))) getOrElse f
      }
      @tailrec
      def baseFields(
          v: MojozViewDef,
          extV: MojozViewDef,
          fields: Seq[MojozFieldDef],
          tableName: String,
          visited: List[String]): Seq[MojozFieldDef] =
        if (v.extends_ == null && fields.isEmpty)
          v.fields.map(maybeAssignTable(tableName)) ++ fields
        else {
          if (visited contains v.name) sys.error("Cyclic extends/overrides: " +
            (v.name :: visited).reverse.mkString(" -> "))
          val vFieldsTransformed = v.fields.map(maybeAssignTable(tableName))
          val vFieldNames = vFieldsTransformed.map(propName).toSet
          val overridingFields = fields.collect {
            case f if vFieldNames.contains(propName(f)) => f
          }
          val mergedFields =
            if (overridingFields.isEmpty)
              vFieldsTransformed ++ fields
            else {
              val nameToOverrideField =
                overridingFields.map(f => propName(f) -> f).toMap
              vFieldsTransformed.map { f =>
                nameToOverrideField.get(propName(f))
                  .map(of => overrideField(v, f, extV, of, v.name :: visited))
                  .getOrElse(f)
              } ++ fields.filterNot(overridingFields.contains)
            }
          if (v.extends_ == null)
            mergedFields
          else
            baseFields(
              nameToPlainViewDef(v.extends_),
              v,
              mergedFields,
              v.table,
              v.name :: visited
            )
        }
      t.copy(fields = baseFields(t, null, Nil, null, viewNamesVisited))
    }
  }
  val nameToViewDef: Map[String, MojozViewDef] =
    plainViewDefs.map(plainViewDefToViewDef(_, Nil))
      .map(t => (t.name, t)).toMap
  protected def loadRawViewDefs(defs: String): List[MojozViewDef] = {
    val loadingConfig = new LoaderOptions // new instance because it is not thread-safe otherwise
    loadingConfig.setAllowDuplicateKeys(false)
    Option((new Yaml(loadingConfig)).loadAs(defs, classOf[java.util.Map[String, _]]))
      .map {
        case m: java.util.Map[String @unchecked, _] => m.asScala.toMap
        case x => throw new RuntimeException(
          "Unexpected class: " + Option(x).map(_.getClass).orNull)
      }
      .map(loadRawViewDefs)
      .getOrElse(Nil)
  }
  protected def loadRawViewDefs(tdMap: Map[String, Any]): List[MojozViewDef] = {
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
    val comments = get(k.comments)
    val fieldsSrc = Option(getSeq(k.fields)).getOrElse(Nil).toList
    val saveTo = getStringSeq(k.saveTo)
    val extras = tdMap -- ViewDefKeyStrings
    val (name, table) = (rawName, rawTable) match {
      case (null, table) => throw new RuntimeException("Missing view name" +
        List(table, xtnds).filter(_ != null).filter(_ != "").mkString(" (view is based on ", ", ", ")"))
      case (name, null) => (name, null)
      case (name, table) => (name, dbName(table))
    }
    val YamlMdLoader = new YamlMdLoader(typeDefs)
    val yamlFieldDefs = fieldsSrc map YamlMdLoader.loadYamlFieldDef
    def typeName(v: Map[String, Any], defaultSuffix: String) =
      if (v.contains("name")) "" + v("name")
      else name + "_" + defaultSuffix
    def toMojozFieldDef(yfd: YamlFieldDef, viewName: String, viewTable: String, viewSaveTo: Seq[String]) = {
      val table = null
      val tableAlias = null
      val name = yfd.name
      val alias = null
      val options = yfd.options
      val cardinality = Option(yfd.cardinality).map(_.take(1)).orNull
      val isOverride = false
      val isCollection = Set("*", "+").contains(cardinality)
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
      val nullable = Option(cardinality)
        .map(c => Set("?", "*").contains(c)) getOrElse true
      val isForcedCardinality = cardinality != null
      val isForcedEnum = yfd.enum != null
      val isForcedType = yfd.typeName != null || yfd.length.isDefined || yfd.fraction.isDefined
      val joinToParent = yfd.joinToParent
      val enum = yfd.enum
      val orderBy = yfd.orderBy
      val comments = yfd.comments
      val extras =
        if  (isForcedCardinality || isForcedEnum || isForcedType)
             Option(yfd.extras).getOrElse(Map.empty) ++
              Map(
                MojozExplicitEnum        -> isForcedEnum,
                MojozExplicitType        -> isForcedType,
                MojozExplicitNullability -> isForcedCardinality
              ).filter(_._2)
        else yfd.extras
      val rawMojozType = Option(YamlMdLoader.yamlTypeToMojozType(yfd, conventions))
      if (isViewDef(yfd.extras))
        (yfd.typeName, yfd.extras.get("name").orNull) match {
          case (null, null) | (null, _) | (_, null) =>
          case (fType, cType) => if (fType != cType)
            sys.error(s"Name conflict for inline view: $fType != $cType")
        }
      val mojozTypeFe =
        if (yfd.typeName == null && isViewDef(yfd.extras))
          new Type(typeName(yfd.extras, name), true)
        else if (isExpression || rawTable == "")
          conventions.typeFromExternal(name, rawMojozType)
        else null
      val mojozType =
        if (mojozTypeFe != null) mojozTypeFe else rawMojozType getOrElse null

      FieldDef(table, tableAlias, name, alias, options, isOverride, isCollection,
        isExpression, expression, saveTo, resolver, nullable,
        mojozType, enum, joinToParent, orderBy, comments, extras)
    }
    def isViewDef(m: Map[String, Any]) =
      m != null && !m.contains("columns") && (m.contains("fields") || m.contains("extends"))
    val fieldDefs = yamlFieldDefs
      .map(yfd => yfd -> toMojozFieldDef(yfd, name, table, saveTo))
      .map { case (yfd, f) => yfd -> (
        if (f.extras == null) f
        else f.copy(
          extras = Option(f.extras).map(_ -- ViewDefKeyStrings)
            .filterNot(_.isEmpty).orNull)
      )}
      .map{ case (yfd, f) => transformRawFieldDef(yfd, f) }
    ViewDef(name, table, null, joins, filter, group, having, order,
      xtnds, comments, fieldDefs, saveTo, extras) ::
      yamlFieldDefs
      .map(_.extras)
      .zip(fieldDefs)
      .filter(f => isViewDef(f._1))
      .map(f => f._1 + (("name", f._2.type_.name)))
      .map(loadRawViewDefs)
      .flatten
  }
  /** can be overriden to send cardinality to extras - for maxOccurs custom processing */
  protected def transformRawFieldDef(
    yfd: YamlFieldDef, mojozFieldDef: MojozFieldDef): MojozFieldDef = mojozFieldDef
  /** called once, can be overriden to transform raw viewdefs */
  protected def transformRawViewDefs(raw: Seq[MojozViewDef]): Seq[MojozViewDef] = raw
  private def propName(f: FieldDef[_]) = Option(f.alias) getOrElse f.name
  private def checkViewDefs(td: Seq[ViewDef[FieldDef[_]]]) = {
    val m: Map[String, ViewDef[_]] = td.map(t => (t.name, t)).toMap
    if (m.size < td.size) sys.error("repeating definition of " +
      td.groupBy(_.name).filter(_._2.size > 1).map(_._1).mkString(", "))
    @tailrec
    def checkExtends(t: ViewDef[_], nameToViewDef: Map[String, ViewDef[_]],
      visited: List[String]): Boolean = {
      val extendsOrModifies =
        t.extends_
      if (visited contains t.name) sys.error("Cyclic extends: " +
        (t.name :: visited).reverse.mkString(" -> "))
      else if (extendsOrModifies == null) true
      else checkExtends(nameToViewDef.get(extendsOrModifies)
        .getOrElse(sys.error("Type " + t.name +
          " extends or modifies non-existing type " + extendsOrModifies)),
        nameToViewDef, t.name :: visited)
    }
    td.foreach(t => checkExtends(t, m, Nil))
    def checkRepeatingFieldNames(t: ViewDef[FieldDef[_]]) =
      if (t.fields.map(propName).toSet.size < t.fields.size) sys.error(
        "Type " + t.name + " defines multiple fields named " + t.fields
          .groupBy(propName).filter(_._2.size > 1).map(_._1).mkString(", "))
    td foreach checkRepeatingFieldNames
  }
  private def checkTypedefMapping(td: Seq[MojozViewDef]) = {
    val m = td.map(t => (t.name, t)).toMap
    td foreach { t =>
      t.fields.foreach { f =>
        if (f.type_ == null)
          sys.error(
            "Unexpected null type for field " + t.name + "." + f.name)
        else if (f.type_.isComplexType)
          m.get(f.type_.name) getOrElse sys.error("Type " + f.type_.name +
            " referenced from " + t.name + "." + f.name + " is not found")
        else if (f.table != null)
          tableMetadata.columnDef(t, f)
      }
    }
  }
  private def markOverrides(views: List[MojozViewDef]): List[MojozViewDef] = {
    val nameToView = views.map(t => (t.name, t)).toMap
    views.map { t =>
      if (t.extends_ == null || t.fields.isEmpty) t else {
        @tailrec
        def allFieldNames(
            v: MojozViewDef,
            names: Set[String]): Set[String] = {
          val allNames = names ++ v.fields.map(propName).toSet
          if (v.extends_ == null) allNames
          else allFieldNames(nameToView(v.extends_), allNames)
        }
        val superNames = allFieldNames(nameToView(t.extends_), Set.empty)
        def isOverride(f: MojozFieldDef) =
          superNames.contains(propName(f))
        if (t.fields.exists(isOverride))
          t.copy(fields = t.fields.map { f =>
            if (isOverride(f)) f.copy(isOverride = true) else f
          })
        else t
      }
    }
  }
  private def buildViewDefs(rawViewDefs: Seq[MojozViewDef]) = {
    val rawTypesMap: Map[String, MojozViewDef] = rawViewDefs.map(t => (t.name, t)).toMap

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

    def mergeSeqs(t: MojozViewDef, base: MojozViewDef) =
      t.copy(
        joins = mergeStringSeqs(base.joins, t.joins),
        filter = mergeStringSeqs(base.filter, t.filter),
        groupBy = mergeStringSeqs(base.groupBy, t.groupBy),
        having = mergeStringSeqs(base.having, t.having),
        orderBy = mergeStringSeqs(base.orderBy, t.orderBy),
        extras = base.extras -- uninheritableExtras ++ t.extras)

    def inheritSeqs(t: MojozViewDef): MojozViewDef =
      if (t.extends_ == null || t.extends_ == t.name) t
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

    def resolveFieldNamesAndTypes(t: MojozViewDef) =
      try resolveFieldNamesAndTypes_(t) catch {
        case NonFatal(ex) =>
          throw new RuntimeException("Failed to resolve field names and types for " + t.name, ex)
      }

    def resolveFieldNamesAndTypes_(t: MojozViewDef) = {
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
      def overrideSimpleType(base: Type, override_ : Type) = Type(
        Option(override_.name) getOrElse base.name,
        override_.length orElse base.length,
        override_.totalDigits orElse base.totalDigits,
        override_.fractionDigits orElse base.fractionDigits,
        false)
      def resolveTypeFromDbMetadata(f: MojozFieldDef) = {
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
            joinOpt.map(_.columns).getOrElse(Nil).find(_.name == f.name)
          val nullable =
            if (f.extras != null && f.extras.get(MojozExplicitNullability) == Some(true)) f.nullable
            else joinColOpt.map(_.nullable).getOrElse(col.nullable)
          f.copy(nullable = nullable,
            type_ =
              if (f.type_ != null &&
                  (f.alias == null || f.extras != null && f.extras.get(MojozExplicitType) == Some(true)))
                   overrideSimpleType(col.type_, f.type_)
              else col.type_,
            enum = Option(f.enum) getOrElse col.enum,
            comments = Option(f.comments) getOrElse col.comments,
            extras =
              if  (f.comments != null)
                   Option(f.extras).getOrElse(Map.empty) ++ Map(MojozExplicitComments -> true)
              else f.extras
          )
        }
      }
      def cleanExtras(f: MojozFieldDef) =
        if  (f.extras != null && (
                f.extras.contains(MojozExplicitComments)    ||
                f.extras.contains(MojozExplicitEnum)        ||
                f.extras.contains(MojozExplicitNullability) ||
                f.extras.contains(MojozExplicitType)))
             f.copy(extras =
               Option(f.extras)
                .map(_ - MojozExplicitComments
                       - MojozExplicitEnum
                       - MojozExplicitNullability
                       - MojozExplicitType)
                .filterNot(_.isEmpty).orNull)
        else f
      t.copy(fields = t.fields
        .map(reduceExpression)
        .map(resolveNameAndTable)
        .map(resolveTypeFromDbMetadata)
        .map(cleanExtras))
    }
    val result = rawViewDefs.toList
      .map(inheritTable)
      .map(inheritSeqs)
      .map(resolveBaseTableAlias)
      .map(resolveFieldNamesAndTypes)
      .map(inheritTableComments)
    checkViewDefs(result)
    checkTypedefMapping(result)
    markOverrides(result)
  }
}

object YamlViewDefLoader {
  private object ViewDefKeys extends Enumeration {
    type ViewDefKeys = Value
    val name, table, joins, filter, group, having, order = Value
    val extends_ = Value("extends")
    val saveTo = Value("save-to")
    val comments, fields = Value
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

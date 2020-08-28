package mojoz.metadata.in

import java.io.File
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.JavaConverters._
import scala.io.Source
import mojoz.metadata.TypeDef
import mojoz.metadata.TypeMetadata
import mojoz.metadata.io._
import mojoz.metadata.ColumnDef
import mojoz.metadata.TableDef
import mojoz.metadata.TableDef._
import mojoz.metadata.Type
import org.yaml.snakeyaml.Yaml

// TODO remove yaml table def
private[in] case class YamlTableDef(
  table: String,
  comments: String,
  columns: Seq[YamlFieldDef],
  pk: Option[DbIndex],
  uk: Seq[DbIndex],
  idx: Seq[DbIndex],
  refs: Seq[Ref],
  extras: Map[String, Any])

private[in] case class YamlFieldDef(
  name: String,
  options: String, // persistence options
  cardinality: String,
  typeName: String,
  length: Option[Int],
  fraction: Option[Int],
  isExpression: Boolean,
  expression: String,
  isResolvable: Boolean,
  saveTo: String,
  resolver: String,
  enum: Seq[String],
  joinToParent: String,
  orderBy: String,
  comments: String,
  extras: Map[String, Any])

private[in] object YamlTableDefLoader {
  val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  val qualifiedIdent = s"$ident(\\.$ident)*"
  val s = "\\s*"
  val idxName = qualifiedIdent
  val col = s"$ident(\\s+[aA][sS][cC]|\\s+[dD][eE][sS][cC])?"
  val cols = s"$col($s\\,$s$col)*"
  val colsIdxDef = s"$s($s$cols)$s"
  val namedIdxDef = s"$s($idxName)?$s\\($s($cols)$s\\)$s"
  val onDelete = "on delete (restrict|set null|cascade|no action)".replace(" ", "\\s+")
  val onUpdate = "on update (restrict|set null|cascade|no action)".replace(" ", "\\s+")

  private def regex(pattern: String) = ("^" + pattern + "$").r

  val ColsIdxDef = regex(colsIdxDef)
  val NamedIdxDef = regex(namedIdxDef)
  val QualifiedIdentDef = regex(qualifiedIdent)
  val FkDef = regex( // FIXME no asc, desc for ref cols!
    s"($colsIdxDef|$namedIdxDef)->$namedIdxDef(?i)" +
    s"($onDelete|($onDelete\\s+)?$onUpdate)?$s")
  val OnDeleteDef = regex(s"$s$onDelete$s")
  val OnDeleteOnUpdateDef = regex(s"$s($onDelete\\s+)?$onUpdate$s")
  object TableDefKeys extends Enumeration {
    type TableDefKeys = Value
    val table, comments, columns, pk, uk, idx, refs = Value
  }
  private val TableDefKeyStrings = TableDefKeys.values.map(_.toString)
}
class YamlTableDefLoader(yamlMd: Seq[YamlMd] = YamlMd.fromResources(),
    conventions: MdConventions = new SimplePatternMdConventions,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {

  // TODO load check constraints!
  import YamlTableDefLoader._
  val sources = yamlMd.filter(YamlMd.isTableDef)
  private def checkRawTableDefs(td: Seq[TableDef[ColumnDef[_]]]) = {
    val m: Map[String, _] = td.map(t => (t.name, t)).toMap
    if (m.size < td.size) sys.error("repeating definition of " +
      td.groupBy(_.name).filter(_._2.size > 1).map(_._1).mkString(", "))
    def checkName(t: TableDef[_]) =
      if (t.name == null || t.name.trim == "") sys.error("Table without name")
    def checkHasColumns(t: TableDef[_]) =
      if (t.cols == null || t.cols.size == 0) sys.error(
        "Table " + t.name + " has no columns")
    def checkRepeatingColumnNames(t: TableDef[ColumnDef[_]]) =
      if (t.cols.map(_.name).toSet.size < t.cols.size) sys.error(
        "Table " + t.name + " defines multiple columns named " + t.cols
          .groupBy(_.name).filter(_._2.size > 1).map(_._1).mkString(", "))
    td foreach checkName
    td foreach checkHasColumns
    td foreach checkRepeatingColumnNames
  }
  private def checkTableDefs(td: Seq[TableDef[ColumnDef[_]]]) = {
    def checkIndices(t: TableDef[ColumnDef[_]]): Unit = {
      val colNames = t.cols.map(_.name).toSet
      def checkIdx(indexType: String, idx: DbIndex) = {
        if (idx.cols == null || idx.cols.size == 0) sys.error(
          "Table " + t.name + " defines " + indexType + " with no columns " + idx)
        idx.cols foreach { c =>
          if (c == null || c == "") sys.error(
            "Table " + t.name + " defines " + indexType + " with columns without names - " + idx)
          val colName =
            if (c contains "(") null // skip checks for functions TODO parse and check
            else if (c.endsWith(" asc") || c.endsWith(" desc"))
              c.substring(0, c.lastIndexOf(" ")).trim
            else c
          if (colName != null && !colNames.contains(colName)) sys.error(
            "Table " + t.name + " defines " + indexType + " referencing unknown column name '" + colName + "' - " + idx)
        }
      }
      t.pk foreach { pk => checkIdx("primary key", pk) }
      t.uk foreach { uk => checkIdx("unique key", uk) }
      t.idx foreach { idx => checkIdx("index", idx) }
    }
    td foreach checkIndices
  }
  val tableDefs = {
    val rawTableDefs = sources map { md =>
      try yamlTypeDefToTableDef(loadYamlTableDef(md.body)) catch {
        case e: Exception => throw new RuntimeException(
          "Failed to load typedef from " + md.filename, e) // TODO line number
      }
    }
    checkRawTableDefs(rawTableDefs)

    def resolvedName(n: String) = n.replace('.', '_')
    val nameToTableDef = {
      val duplicateNames =
        rawTableDefs.map(_.name).groupBy(n => n).filter(_._2.size > 1).map(_._1)
      if (duplicateNames.size > 0)
        throw new RuntimeException(
          "Duplicate table definitions: " + duplicateNames.mkString(", "))
      rawTableDefs.map(t => (t.name, t)).toMap
    }
    def refToCol(ref: String) =
      (ref.split("\\.", 2).toList match {
        case t :: c :: Nil => Some((t, c)) case x => None
      })
        .flatMap(tc => nameToTableDef.get(tc._1).map((_, tc._2)))
        .flatMap(tc => tc._1.cols.find(c => resolvedName(c.name) == tc._2)
          .map(c => (tc._1, c)))
        .getOrElse(sys.error("Bad ref: " + ref))
    def overwriteMojozType(base: Type, overwrite: Type) = Type(
      Option(base.name) getOrElse overwrite.name,
      overwrite.length orElse base.length,
      overwrite.totalDigits orElse base.totalDigits,
      overwrite.fractionDigits orElse base.fractionDigits,
      false)
    def baseRefChain(col: ColumnDef[Type], visited: List[String]): List[String] = {
      def chain(ref: String) =
        if (visited contains ref)
          sys.error("Cyclic column refs: " +
            (ref :: visited).reverse.mkString(" -> "))
        else baseRefChain(refToCol(ref)._2, ref :: visited)
      // TODO ??? if type explicitly defined, why ref? throw?
      if (Option(col.type_.name).map(_ contains ".") getOrElse false)
        chain(col.type_.name)
      else if (col.name contains ".") chain(col.name)
      else visited
    }
    val tableDefs = rawTableDefs map { r =>
      val resolvedColsAndRefs: Seq[(ColumnDef[Type], Seq[Ref])] = r.cols.map { c =>
        val refChain = baseRefChain(c, Nil)
        if (refChain.size == 0) (c, Nil)
        else {
          val defaultRefTableAlias =
            if (c.type_.name == null || c.name.indexOf('.') < 0) null
            else c.name.substring(0, c.name.indexOf('.'))
          val colName = c.name.replace('.', '_')
          val (refTable, refCol) =
            refToCol(Option(c.type_.name) getOrElse c.name)
          val mojozType = overwriteMojozType(
            refChain.foldLeft(new Type(null))((t, ref) =>
              overwriteMojozType(t, refToCol(ref)._2.type_)),
            c.type_)
          val ref = Ref(null, List(colName), refTable.name, List(refCol.name),
            null, defaultRefTableAlias, null, null)
          (c.copy(name = colName, type_ = mojozType), List(ref))
        }
      }
      def mergeRefs(implicitRefs: Seq[Ref], explicitRefs: Seq[Ref]) = {
        def refKey(r: Ref) = (r.cols, r.refTable, r.refCols)
        val explicitsMap = explicitRefs.map(r => (refKey(r), r)).toMap
        val implicitsMap = implicitRefs.map(r => (refKey(r), r)).toMap
        List(
          implicitRefs.filterNot(r => explicitsMap.contains(refKey(r))),
          implicitRefs.filter(r => explicitsMap.contains(refKey(r))).map { r =>
            val x = explicitsMap(refKey(r))
            r.copy(
              name = x.name,
              onDeleteAction = x.onDeleteAction,
              onUpdateAction = x.onUpdateAction)
          },
          explicitRefs.filterNot(r => implicitsMap.contains(refKey(r)))).flatten
      }
      r.copy(
        cols = resolvedColsAndRefs.map(_._1),
        refs = mergeRefs(resolvedColsAndRefs.flatMap(_._2), r.refs))
    }
    checkTableDefs(tableDefs)
    tableDefs
  }
  private def loadYamlIndexDef(src: Any) = {
    val ThisFail = "Failed to load index definition"
    def dbIndex(name: String, cols: String) = DbIndex(name,
      Option(cols).map(_.split("\\,").toList.map(_.trim)) getOrElse Nil)
    def extractIndex(src: Any): DbIndex = src match {
      case idx: java.lang.String => idx match {
        case NamedIdxDef(name, _, cols, _, _, _) => dbIndex(name, cols)
        case ColsIdxDef(cols, _, _, _) => dbIndex(null, cols)
        case _ => throw new RuntimeException(ThisFail +
          " - unexpected format: " + idx.trim)
      }
      case arr: java.util.ArrayList[_] =>
        extractIndex(arr.asScala.mkString(", "))
      case x => throw new RuntimeException(ThisFail +
        " - unexpected index definition class: " + x.getClass
        + "\nentry: " + x.toString)
    }
    extractIndex(src)
  }
  private def loadYamlRefDef(src: Any) = {
    val ThisFail = "Failed to load ref definition"
    src match {
      case r: java.lang.String =>
        if (FkDef.unapplySeq(r).isDefined) {
          val parts = r.split("->")
          val nameAndCols = loadYamlIndexDef(parts(0))
          val refAndActions = parts(1).split("\\)")
          val refTableRefCols = loadYamlIndexDef(refAndActions(0) + ")")
          val (onDelete, onUpdate) =
            if (refAndActions.size < 2) (null, null)
            else refAndActions(1) match {
              case OnDeleteDef(d) => (d, null)
              case OnDeleteOnUpdateDef(_, d, u) => (d, u)
              case x => throw new RuntimeException(ThisFail +
                " - unexpected format: " + x.trim)
            }
          Ref(nameAndCols.name, nameAndCols.cols,
            refTableRefCols.name, refTableRefCols.cols,
            null, null,
            onDelete, onUpdate)
        } else throw new RuntimeException(ThisFail +
          " - unexpected format: " + r.trim)
      case x => throw new RuntimeException(ThisFail +
        " - unexpected ref definition class: " + x.getClass
        + "\nentry: " + x.toString)
    }
  }
  private def toList(src: Option[Any]): List[Any] =
    src
      .map(_ match {
        case null => Nil
        case s: String =>
          List(s)
        case a: java.util.ArrayList[_] =>
          a.asScala.toList
        case x => List(x)
      })
      .getOrElse(Nil)
  lazy val YamlMdLoader = new YamlMdLoader(typeDefs)
  private def loadYamlTableDef(tableDefString: String) = {
    val tdMap =
      (new Yaml).load(tableDefString) match {
        case m: java.util.Map[String @unchecked, _] => m.asScala.toMap
        case x => throw new RuntimeException(
          "Unexpected class: " + Option(x).map(_.getClass).orNull)
      }
    val table = tdMap.get("table").map(_.toString)
      .getOrElse(sys.error("Missing table name"))
    val comments = toList(tdMap.get("comments")) match {
      case Nil => null
      case x => x.map(_.toString).mkString("\n")
    }
    val colSrc = tdMap.get("columns")
      .map {
        case null => Nil
        case a: java.util.ArrayList[_] => a.asScala.toList
        case x => throw new RuntimeException("Unexpected class: " + x.getClass)
      }
      .getOrElse(Nil)
    val colDefs = colSrc map YamlMdLoader.loadYamlFieldDef
    val pk_list = toList(tdMap.get("pk"))
      .map(loadYamlIndexDef)
    if (pk_list.size > 1)
      throw new RuntimeException(
        "Multiple primary keys are not allowed, composite key columns should be comma-separated")
    val pk = pk_list.headOption
    val uk = toList(tdMap.get("uk"))
      .map(loadYamlIndexDef)
    val idx = toList(tdMap.get("idx"))
      .map(loadYamlIndexDef)
    val refs = toList(tdMap.get("refs"))
      .map(loadYamlRefDef)
    val extras = tdMap -- TableDefKeyStrings
    YamlTableDef(table, comments, colDefs, pk, uk, idx, refs, extras)
  }
  private def yamlTypeDefToTableDef(y: YamlTableDef) = {
    // TODO cleanup?
    val name = y.table
    val comments = y.comments
    val cols = y.columns.map(yamlFieldDefToExFieldDef)
    val pk = y.pk
    val uk = y.uk
    val ck = Nil // FIXME ck!
    val idx = y.idx
    val refs = y.refs
    val extras = y.extras
    val exTypeDef = TableDef(name, comments, cols, pk, uk, ck, idx, refs, extras)
    conventions.fromExternal(exTypeDef)
  }
  private def yamlFieldDefToExFieldDef(yfd: YamlFieldDef) = {
    val name = yfd.name
    val dbDefault = yfd.expression
    val nullable = yfd.cardinality match {
      case null => None
      case "?" => Some(true)
      case "!" => Some(false)
      case x =>
        sys.error("Unexpected cardinality for table column: " + x)
    }
    val enum = yfd.enum
    if (yfd.joinToParent != null)
      sys.error("joinToParent not supported for table columns")
    if (yfd.orderBy != null)
      sys.error("orderBy not supported for table columns")
    val comments = yfd.comments
    val rawMojozType = Option(YamlMdLoader.yamlTypeToMojozType(yfd, conventions))
    val extras = yfd.extras
    ColumnDef(name, IoColumnType(nullable, rawMojozType),
      nullable getOrElse true, dbDefault, enum, comments, extras)
  }
}

private[in] class YamlMdLoader(typeDefs: Seq[TypeDef]) {
  val FieldPattern = {
    val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
    val qualifiedIdent = s"$ident(\\.$ident)*"
    val int = "[0-9]+"
    val s = "\\s*"

    val name = qualifiedIdent
    val quant = "([\\?\\!]|([\\*\\+](\\.\\.(\\d*[1-9]\\d*))?))"
    val options = "\\[[\\+\\-\\=]+\\]"
    val join = "\\[.*?\\]"
    val order = "\\~?#(\\s*\\(.*?\\))?"
    val enum = "\\(.*?\\)"
    val typ = qualifiedIdent
    val len = int
    val frac = int
    val midArrow = "\\s+->\\s+"
    val endArrow = "\\s+->\\s*"
    val expr = ".*"
    val pattern =
        (s"($name)( $options)?( $quant)?( $join)?( $typ)?" + 
          s"( $len)?( $frac)?" +
          s"( $order)?( $enum)?($endArrow|(( =|$midArrow)($expr)?))?").replace(" ", s)

    ("^" + pattern + "$").r
  }

  def loadYamlFieldDef(src: Any) = {
    val ThisFail = "Failed to load column definition"
    def colDef(nameEtc: String, comments: String,
        child: Map[String, Any]) = nameEtc match {
      case FieldPattern(name, _, options, quant, _, _, _, _, joinToParent, typ, _,
        len, frac, order, _, enum,
        exprOrResolverWithDelimiter, _, exprOrResolverDelimiter, exprOrResolver) =>
        def t(s: String) = Option(s).map(_.trim).filter(_ != "").orNull
        def i(s: String) = Option(s).map(_.trim.toInt)
        def e(enum: String) = Option(enum)
          .map { e =>
            // TODO parse enums properly? Allow whitespace etc. when (single/double) quoted, allow escapes
            if (e contains '\'')
              e.replace("(", "").replace(")", "").trim.split("',\\s*").map(_.replace("'", ""))
            else
              e.split("[\\(\\)\\s,]+")
          }
          .map(_.toList.filter(_ != ""))
          .filter(_.size > 0).orNull
        def cardinality = t(quant)
        val isExpr = exprOrResolverDelimiter != null && exprOrResolverDelimiter.indexOf('=') >= 0
        val isResolvable =
          exprOrResolverWithDelimiter != null &&
          s"$exprOrResolverWithDelimiter ".indexOf(" -> ") >= 0
        val exprOrResolverParts = Option(t(exprOrResolver)).map(_.split("\\s*->\\s*", 2)) getOrElse Array[String]()
        val expr = if (isExpr && exprOrResolverParts.size > 0) t(exprOrResolverParts(0)) else null
        val saveAndResolverString =
          if (isExpr && exprOrResolverParts.size > 1) exprOrResolverParts(1)
          else if (isResolvable) exprOrResolver
          else null
        val saveAndResolverParts =
          Option(saveAndResolverString).map(_.split("\\s*=\\s*", 2)) getOrElse Array[String]()
        val saveTo = if (saveAndResolverParts.size > 0) t(saveAndResolverParts(0)) else null
        val resolver = if (saveAndResolverParts.size > 1) t(saveAndResolverParts(1)) else null
        YamlFieldDef(name, t(options), cardinality, t(typ), i(len), i(frac),
          isExpr, expr, isResolvable, saveTo, resolver, e(enum), t(joinToParent), t(order), comments,
          child)
      case _ => throw new RuntimeException(ThisFail +
        " - unexpected format: " + nameEtc.trim)
    }
    src match {
      case nameEtc: java.lang.String =>
        colDef(nameEtc.toString, null, null)
      case m: java.util.Map[_, _] =>
        if (m.size == 1) {
          val entry = m.entrySet.asScala.toList(0)
          val nameEtc = entry.getKey
          val (comments, child) = entry.getValue match {
            case s: String => (s, null)
            case m: java.util.Map[String @unchecked, _] =>
              (null, m.asScala.toMap)
            case a: java.util.ArrayList[_] =>
              val l = a.asScala.toList
              val strings =
                l.collect { case s: java.lang.String => s }
              val child =
                l.collect { case m: java.util.Map[String @unchecked, _] => m.asScala }
                  .foldLeft(Map[String, Any]())(_ ++ _)
              val comments = strings.headOption.getOrElse(child.get("comments").orNull)
              // TODO handle (raise error for?) other cases
              (comments, child ++ strings.drop(1).map(s => s -> s).toMap)
            case null => (null, null)
            case x => sys.error(ThisFail +
              " - unexpected child definition class: " + x.getClass
              + "\nvalue: " + x.toString)
          }
          colDef(nameEtc.toString, Option(comments).map(_.toString).orNull, child)
        } else throw new RuntimeException(ThisFail +
          // TODO do not throw, allow decomposed or with custom extras instead
          " - more than one entry for column: " + m.asScala.toMap.toString())
      case x => throw new RuntimeException(ThisFail +
        " - unexpected field definition class: " + x.getClass
        + "\nentry: " + x.toString)
    }
  }

  lazy val yamlLoadInfoToTypeDef =
    typeDefs.flatMap(td => td.yamlLoad.map(_ -> td))
  def yamlTypeToMojozType(f: YamlFieldDef, conventions: MdConventions): Type =
     yamlTypeToMojozType(f.typeName, f.length, f.fraction, conventions)
  def yamlTypeToMojozType(yamlTypeName: String, size: Option[Int], frac: Option[Int],
      conventions: MdConventions): Type = (yamlTypeName, size, frac) match {
    // FIXME do properly (check unsupported patterns, ...)
    case (null, None, None) => null
    case (null, Some(len), None) => new Type(null, len)
    case _ =>
      yamlLoadInfoToTypeDef.find { case (yl, td) =>
        yl.typeName.orNull == yamlTypeName &&
        sizeOptionMatch(yl.minSize, yl.maxSize, size) &&
        sizeOptionMatch(yl.minFractionDigits, yl.maxFractionDigits, frac)
      }.map { case (yl, td) =>
        val length = yl.targetLength match {
          case Some(null) => size // xxx Some(null) means copy from source
          case x => x.map(_.intValue)
        }
        val totalDigits = yl.targetTotalDigits match {
          case Some(null) => size // xxx Some(null) means copy from source
          case x => x.map(_.intValue)
        }
        val fractionDigits = yl.targetFractionDigits match {
          case Some(null) => frac // xxx Some(null) means copy from source
          case x => x.map(_.intValue)
        }
        val isComplexType = false
        Type(td.name, length, totalDigits, fractionDigits, isComplexType)
      }.getOrElse {
        if (conventions.isRefName(yamlTypeName))
          Type(yamlTypeName, size, None, frac, false) // FIXME len <> totalDigits, resolve!
        else
          // if no known mojoz type name found - let it be complex type!
          new Type(yamlTypeName, true)
      }
  }
  private def sizeOptionMatch(min: Option[Int], max: Option[Int], value: Option[Int]) =
    min.isEmpty && value.isEmpty ||
      min.isDefined && value.isDefined && min.get <= value.get &&
      max.map(_ >= value.get).getOrElse(true)
}

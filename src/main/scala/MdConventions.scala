package org.mojoz.metadata.io

import org.mojoz.metadata._
import org.mojoz.metadata.TableDef._
import org.mojoz.metadata.out.SqlGenerator
import scala.collection.immutable.Seq

case class IoColumnType(nullable: Option[Boolean], type_ : Option[Type])

class MdConventions(naming: SqlGenerator.ConstraintNamingRules = new SqlGenerator.SimpleConstraintNamingRules) {
  def idTypeName = "long"
  def isIdName(name: String) = name.toLowerCase == "id"
  def isCodeName(name: String) = name.toLowerCase == "code"
  def isRefName(name: String) = name != null && name.contains(".")
  def isBooleanName(name: String) =
    name.toLowerCase.startsWith("is_") || name.toLowerCase.startsWith("has_")
  def isDateName(name: String) = name.toLowerCase.endsWith("_date")
  def isDateTimeName(name: String) = name.toLowerCase.endsWith("_time")
  def isIdRefName(name: String) = name.toLowerCase.endsWith("_id")
  def isTypedName(name: String) =
    isBooleanName(name) || isDateName(name) || isDateTimeName(name) ||
    isIdName(name) || isIdRefName(name)
  def fromExternal(table: IoTableDef): MojozTableDef = {
    val pk = fromExternalPk(table)
    table.copy(
      cols = table.cols.map(col => fromExternal(col, pk)),
      pk = pk
    )
  }
  def fromExternalPk(tableDef: TableDef[ColumnDef[_]]) = {
    import scala.language.existentials
    val cols = tableDef.cols.map(_.name)
    if (tableDef.pk.isDefined) tableDef.pk.filter(_ != null)
    else if (cols.filter(isIdName).size == 1)
      Some(DbIndex(null, cols.filter(isIdName)))
    else if (cols.filter(isCodeName).size == 1)
      Some(DbIndex(null, cols.filter(isCodeName)))
    else if (cols.size == 2 && cols.filter(isIdRefName).size == 2)
      Some(DbIndex(null, cols))
    else None
  }
  def fromExternal(col: IoColumnDef, pk: Option[DbIndex]): MojozColumnDef = {
    val typ = typeFromExternal(col.name, col.type_.type_)
    val nullable = nullableFromExternal(col.name, col.type_.nullable, pk)
    val dbDefault = (typ.name, col.dbDefault) match {
      case (_, null) => null
      case ("string", d) =>
        if (d startsWith "'") d
        else if (d startsWith "()") (d substring 2).trim
        else if (d contains "(") d
        else s"'$d'"
      case (_, d) => d
    }
    col.copy(
      type_ = typ,
      nullable = nullable,
      dbDefault = dbDefault)
  }
  def typeFromExternal(name: String, type_ : Option[Type]): Type = {
    def defaultType(defaultName: String) =
      type_ map { x =>
        if (x.name == null) x.copy(name = defaultName)
        else x
      } getOrElse new Type(defaultName)
    (name, type_) match {
      case (name, Some(typ)) if isRefName (typ.name) => typ
      case (name, _)         if isIdName      (name) => defaultType(idTypeName)
      case (name, typ)       if isRefName     (name) => typ getOrElse new Type(null)
      case (name, _)         if isBooleanName (name) => defaultType("boolean")
      case (name, _)         if isDateName    (name) => defaultType("date")
      case (name, _)         if isDateTimeName(name) => defaultType("dateTime")
      case (name, _)         if isIdRefName   (name) => defaultType(idTypeName)
      case _                                         => defaultType("string")
    }
  }
  def nullableFromExternal(name: String, nullable: Option[Boolean], pk: Option[DbIndex]): Boolean =
    nullable getOrElse !pk.exists(_.cols contains name)
  def toExternal(table: MojozTableDef): IoTableDef =
    table.copy(
      cols = table.cols.map(toExternal(table, _)),
      pk = toExternalPk(table),
      uk = toExternalUk(table),
      idx = toExternalIdx(table),
      refs = toExternalRefs(table))

  private def toExternalIdx(defaultName: String)(idx: DbIndex) = {
    if (!idx.cols.exists(_.toLowerCase endsWith " asc")) idx
    else idx.copy(cols = idx.cols.map(c =>
      if (c.toLowerCase endsWith " asc") c.substring(0, c.length - 4) else c))
  } match {
    case idx if idx.name == defaultName => idx.copy(name = null)
    case idx => idx
  }
  def toExternalPk(tableDef: MojozTableDef) = {
    val cols = tableDef.cols.map(_.name)
    val defaultPkName = naming.pkName(tableDef.name)
    def pkNormalize(pk: Option[DbIndex]) = pk.map { pk =>
      if (pk.name == defaultPkName) pk.copy(name = null) else pk
    }
    val defaultPk = fromExternalPk(tableDef.copy(pk = None))
    (if (pkNormalize(tableDef.pk) != pkNormalize(defaultPk))
      (if (tableDef.pk.isDefined) tableDef.pk.map(toExternalIdx(defaultPkName)) else Some(null)) else None)
  }

  def toExternalUk(table: MojozTableDef) = {
    if (table.pk.isDefined) {
      val pkCols = toExternalIdx("")(table.pk.get).cols
      table.uk.filter(toExternalIdx("")(_).cols != pkCols)
    } else table.uk
  }.map(uk => toExternalIdx(naming.ukName(table.name, uk))(uk))

  def toExternalIdx(table: MojozTableDef): Seq[DbIndex] =
    table.idx.map(idx => toExternalIdx(naming.idxName(table.name, idx))(idx))

  def toExternalRefs(table: MojozTableDef) = table.refs
    .map(r => if (r.onDeleteAction == "no action") r.copy(onDeleteAction = null) else r)
    .map(r => if (r.onUpdateAction == "no action") r.copy(onUpdateAction = null) else r)
    .map(r => if (r.name == naming.fkName(table.name, r)) r.copy(name = null) else r)
    .filter(r => r.cols.size > 1 ||
      r.onDeleteAction != null || r.onUpdateAction != null || r.name != null)

  def nullableOpt(name: String, nullable: Boolean, table: MojozTableDef) = (name, nullable) match {
      case (name, false) if table.pk.exists(_.cols contains name) => None
      case (_, true) => None
      case (_, nullable) => Some(nullable)
  }
  def toExternal(table: MojozTableDef, col: MojozColumnDef): IoColumnDef = {
    val nullOpt = nullableOpt(col.name, col.nullable, table)
    val dbDefault = (col.type_.name, col.dbDefault) match {
      case (_, null) => null
      case ("string", d) =>
        if (d.startsWith("'") && d.endsWith("'"))
          if (d.contains("(")) d else d.substring(1, d.length - 1)
        else if (!(d contains "(")) s"() $d"
        else d
      case (_, d) => d
    }
    val ref = table.refs
      .filter(_.cols.size == 1)
      .find(_.cols(0) == col.name)
    if (ref.isDefined) {
      // TODO ref checks (type match, type override)
      // TODO multicol refs, ...?
      // TODO drop enum, if differs?
      def prefix(s: String) =
        if (s.indexOf(".") < 0) "" else s.substring(0, s lastIndexOf ".") + "."
      def suffix(s: String) =
        if (s.indexOf(".") < 0) s else s.substring(s.lastIndexOf(".") + 1)
      def toRefColName(col: String, refCol: String) =
        if (col.endsWith("_" + refCol))
          col.substring(0, col.length - refCol.length - 1) + "." + refCol
        else col
      val refTable = ref.get.refTable
      val refCol = ref.get.refCols(0)
      val refTypeName =
        /* De-optimized. TODO re-optimize ref representation in column type?
        if (prefix(table.name) == prefix(refTable))
          suffix(refTable) + "." + refCol
        else */
          refTable + "." + refCol
      val (typeOpt, refColName) =
        if (col.name == refTypeName.replace(".", "_"))
          (None, refTypeName)
        else
          (Some(new Type(refTypeName)), toRefColName(col.name, refCol))
      col.copy(
        name = refColName,
        type_ = IoColumnType(nullOpt, typeOpt),
        dbDefault = dbDefault)
    } else {
      col.copy(
        type_ = IoColumnType(nullOpt, typeOpt(col.name, col.type_)),
        dbDefault = dbDefault)
    }
  }
  def typeOpt(name: String, type_ : Type) = {
      (name, type_.name, type_.length) match {
        case (name, type_, _) if isIdName(name) && type_ == idTypeName => None
        case (name, type_, _) if isIdRefName(name) && type_ == idTypeName => None
        case (name, "boolean", _) if isBooleanName(name) => None
        case (name, "date", _) if isDateName(name) => None
        case (name, "dateTime", _) if isDateTimeName(name) => None
        case (name, "string", None) if !isTypedName(name) => None
        case (name, "string", Some(len)) if !isTypedName(name) => Some(new Type(null: String, len))
        case _ => Option(type_)
      }
  }
  def toExternal(view: MojozViewDef, tableMetadata: TableMetadata[MojozTableDef], allViews: Map[String, MojozViewDef]) = {
    def mapField(f: MojozFieldDef) = {
      import f._
      if (table == null || type_.isComplexType)
        f.copy(type_ = new IoColumnType(
          Option(nullable).filter(!_),
          typeOpt(name, type_)))
      else {
        val col = tableMetadata.columnDef(view, f)
        val nullableOpt = Option(nullable).filter(_ != col.nullable)
        val type_ = Option(f.type_).filter(_ != col.type_)
        val (newName, newExpr) =
          if (isExpression || expression != null) {
            if (expression != null && expression.replace(".", "_") == fieldName)
              (expression, null)
            else (fieldName, expression)
          } else if (tableAlias != null && tableAlias != view.tableAlias)
            (tableAlias + "." + name, expression)
          else if (table != null && table != view.table)
            (table + "." + name, expression)
          else (name, expression)
        f.copy(
          name = newName,
          expression = newExpr,
          comments = Option(comments).filter(_ != col.comments).orNull,
          type_ = new IoColumnType(nullableOpt, type_))
      }
    }
    import view._
    val tableDef = if (table == null) null else tableMetadata.tableDef(view)
    view.copy(
      table =
        if (table == null) ""
        else if (extends_ != null && allViews.get(extends_).map(_.table == table).getOrElse(false)) null
        else table,
      comments = Option(comments)
        .filter(tableDef == null || _ != tableDef.comments).orNull,
      fields = fields.map(mapField))
  }
}

object MdConventions extends MdConventions(new SqlGenerator.SimpleConstraintNamingRules) {

  private def splitNamePatternString(patternString: String): Seq[String] =
    patternString.trim.split("[,\\s]+").toList.map(_.trim)

  /** loads and returns name patterns from resource, returns defaultPatterns if resource is not found */
  def namePatternsFromResource(resourceName: String, defaultPatterns: Seq[String]): Seq[String] = {
    Option(getClass.getResourceAsStream(resourceName)).map(scala.io.Source.fromInputStream)
      .map {s => val patterns = splitNamePatternString(s.mkString); s.close; patterns }
      .getOrElse(defaultPatterns)
  }
  def namePatternsFromFile(resourceFileOpt: Option[java.io.File], defaultPatterns: Seq[String]): Seq[String] = {
    resourceFileOpt.map(scala.io.Source.fromFile)
      .map {s => val patterns = splitNamePatternString(s.mkString); s.close; patterns }
      .getOrElse(defaultPatterns)
  }
  def namePatternsFromResource(patternSource: PatternSource): Seq[String] =
    namePatternsFromResource(patternSource.filename, patternSource.defaultPatterns)

  sealed trait Pattern
  case class Equals(pattern: String) extends Pattern
  case class Starts(pattern: String) extends Pattern
  case class Ends(pattern: String) extends Pattern

  case class PatternSource(filename: String, defaultPatterns: Seq[String])
  val defaultBooleanNamePatternSource = PatternSource(
    "/md-conventions/boolean-name-patterns.txt",
    Seq("is_*", "has_*"))
  val defaultDateNamePatternSource = PatternSource(
    "/md-conventions/date-name-patterns.txt",
    Seq("date_*", "*_date", "*_date_from", "*_date_to"))
  val defaultDateTimeNamePatternSource = PatternSource(
    "/md-conventions/datetime-name-patterns.txt",
    Seq("time_*", "*_time", "*_time_from", "*_time_to"))

  class SimplePatternMdConventions(
      booleanNamePatternStrings: Seq[String] =
        namePatternsFromResource(defaultBooleanNamePatternSource),
      dateNamePatternStrings: Seq[String] =
        namePatternsFromResource(defaultDateNamePatternSource),
      dateTimeNamePatternStrings: Seq[String] =
        namePatternsFromResource(defaultDateTimeNamePatternSource)
    ) extends MdConventions {

    val booleanNamePatterns = booleanNamePatternStrings.map(pattern).toSeq
    val dateNamePatterns = dateNamePatternStrings.map(pattern).toSeq
    val dateTimeNamePatterns = dateTimeNamePatternStrings.map(pattern).toSeq

    /** Supported patterns: "name", "prefix*", "*suffix" */
    def pattern(patternString: String): Pattern = {
      if (patternString startsWith "*") Ends(patternString.substring(1))
      else if (patternString endsWith "*") Starts(patternString.substring(0, patternString.length - 1))
      else Equals(patternString)
    }

    def matches(name: String)(pattern: Pattern): Boolean = pattern match {
      case Equals(s) => name == s
      case Starts(s) => name startsWith s
      case Ends(s) => name endsWith s
    }

    override def isBooleanName(name: String) =
      booleanNamePatterns exists matches(name)
    override def isDateName(name: String) =
      dateNamePatterns exists matches(name)
    override def isDateTimeName(name: String) =
      dateTimeNamePatterns exists matches(name)
  }
}

package mojoz.metadata.io

import mojoz.metadata._
import mojoz.metadata.TableDef._
import mojoz.metadata.out.SqlWriter

case class IoColumnType(nullable: Option[Boolean], type_ : Option[Type])

class MdConventions(naming: SqlWriter.ConstraintNamingRules = new SqlWriter.SimpleConstraintNamingRules) {
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
    isIdRefName(name)
  def fromExternal(table: TableDef[ColumnDef[IoColumnType]]): TableDef[ColumnDef[Type]] =
    table.copy(
      cols = table.cols map fromExternal,
      pk = fromExternalPk(table))
  def fromExternalPk(typeDef: TableDef[ColumnDef[_]]) = {
    import scala.language.existentials
    val cols = typeDef.cols.map(_.name)
    if (typeDef.pk.isDefined) typeDef.pk
    else if (cols.filter(isIdName).size == 1)
      Some(DbIndex(null, cols.filter(isIdName)))
    else if (cols.filter(isCodeName).size == 1)
      Some(DbIndex(null, cols.filter(isCodeName)))
    else if (cols.size == 2 && cols.filter(isIdRefName).size == 2)
      Some(DbIndex(null, cols))
    else None
  }
  def fromExternal(col: ColumnDef[IoColumnType]): ColumnDef[Type] = {
    val (typ, nullable) =
      fromExternal(col.name, col.type_.type_, col.type_.nullable)
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
  def fromExternal(name: String, type_ : Option[Type], nullable: Option[Boolean]) = {
    def defaultType(defaultName: String) =
      type_ map { x =>
        if (x.name == null) x.copy(name = defaultName)
        else x
      } getOrElse new Type(defaultName)
    def nullableOrTrue = nullable getOrElse true
    def nullableOrFalse = nullable getOrElse false
    (name, type_) match {
      case (name, Some(typ)) if isRefName(typ.name) =>
        (typ, nullableOrTrue)
      case (name, _) if isIdName(name) =>
        (defaultType(idTypeName), nullableOrFalse)
      case (name, typ) if isRefName(name) =>
        (typ getOrElse new Type(null), nullableOrTrue)
      case (name, _) if isBooleanName(name) =>
        (defaultType("boolean"), nullableOrTrue)
      case (name, _) if isDateName(name) =>
        (defaultType("date"), nullableOrTrue)
      case (name, _) if isDateTimeName(name) =>
        (defaultType("dateTime"), nullableOrTrue)
      case (name, _) if isIdRefName(name) =>
        (defaultType(idTypeName), nullableOrTrue)
      case _ =>
        (defaultType("string"), nullableOrTrue)
    }
  }
  def toExternal(table: TableDef[ColumnDef[Type]]): TableDef[ColumnDef[IoColumnType]] =
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
  def toExternalPk(typeDef: TableDef[ColumnDef[Type]]) = {
    val cols = typeDef.cols.map(_.name)
    // TODO pk: <missing>!
    // TODO overridable isDefaultPkName(name), use ConstraintNamingRules!
    def isDefaultPkName(name: String) =
      name == null || name.equalsIgnoreCase("pk_" + typeDef.name)
    if (!typeDef.pk.isDefined) None
    else if (cols.filter(isIdName).size == 1)
      typeDef.pk match {
        case Some(DbIndex(name, List(id))) if isIdName(id) &&
          isDefaultPkName(name) => None
        case pk => pk
      }
    else if (cols.size == 2 && cols.filter(isIdRefName).size == 2)
      typeDef.pk match {
        case Some(DbIndex(name,
          List(col1, col2))) if (col1.toLowerCase, col2.toLowerCase) ==
          (cols(0).toLowerCase, cols(1).toLowerCase) &&
          isDefaultPkName(name) => None
        case pk => pk
      }
    else None
  }.map(toExternalIdx(naming.pkName(typeDef.name)))

  def toExternalUk(table: TableDef[ColumnDef[Type]]) = {
    if (table.pk.isDefined) {
      val pkCols = toExternalIdx("")(table.pk.get).cols
      table.uk.filter(toExternalIdx("")(_).cols != pkCols)
    } else table.uk
  }.map(uk => toExternalIdx(naming.ukName(table.name, uk))(uk))

  def toExternalIdx(table: TableDef[ColumnDef[Type]]): Seq[DbIndex] =
    table.idx.map(idx => toExternalIdx(naming.idxName(table.name, idx))(idx))

  def toExternalRefs(table: TableDef[ColumnDef[Type]]) = table.refs
    .map(r => if (r.onDeleteAction == "no action") r.copy(onDeleteAction = null) else r)
    .map(r => if (r.onUpdateAction == "no action") r.copy(onUpdateAction = null) else r)
    .map(r => if (r.name == naming.fkName(table.name, r)) r.copy(name = null) else r)
    .filter(r => r.cols.size > 1 ||
      r.onDeleteAction != null || r.onUpdateAction != null || r.name != null)

  def toExternal(table: TableDef[ColumnDef[Type]], col: ColumnDef[Type]): ColumnDef[IoColumnType] = {
    val nullOpt = (col.name, col.nullable) match {
      case (name, false) if isIdName(name) => None
      case (_, true) => None
      case (_, nullable) => Some(nullable)
    }
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
        if (prefix(table.name) == prefix(refTable))
          suffix(refTable) + "." + refCol
        else refTable + "." + refCol
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
      val typeOpt = (col.name, col.type_.name, col.type_.length) match {
        case (name, type_, _) if isIdName(name) && type_ == idTypeName => None
        case (name, type_, _) if isIdRefName(name) && type_ == idTypeName => None
        case (name, "boolean", _) if isBooleanName(name) => None
        case (name, "date", _) if isDateName(name) => None
        case (name, "dateTime", _) if isDateTimeName(name) => None
        case (name, "string", None) if !isTypedName(name) => None
        case (name, "string", Some(len)) if !isTypedName(name) =>
          Some(new Type(null.asInstanceOf[String], len))
        case _ => Option(col.type_)
      }
      col.copy(
        type_ = IoColumnType(nullOpt, typeOpt),
        dbDefault = dbDefault)
    }
  }
}

object MdConventions extends MdConventions(new SqlWriter.SimpleConstraintNamingRules)

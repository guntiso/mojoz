package mojoz.metadata.io

import mojoz.metadata._
import mojoz.metadata.TableDef._

case class IoColumnType(nullable: Option[Boolean], type_ : Option[Type])

class MdConventions {
  def isIdName(name: String) = name.toLowerCase == "id"
  def isCodeName(name: String) = name.toLowerCase == "code"
  def isRefName(name: String) = name != null && name.contains(".")
  def isBooleanName(name: String) =
    name.toLowerCase.startsWith("is_") || name.toLowerCase.startsWith("has_")
  def isDateName(name: String) = name.toLowerCase.endsWith("_date")
  def isIdRefName(name: String) = name.toLowerCase.endsWith("_id")
  def isTypedName(name: String) =
    isBooleanName(name) || isDateName(name) || isIdRefName(name)
  def fromExternal(typeDef: TableDef[IoColumnType]): TableDef[Type] = {
    val cols = typeDef.cols map fromExternal
    val primaryKey = fromExternalPk(typeDef)
    TableDef(typeDef.name, typeDef.comments, cols, primaryKey, Nil, Nil, Nil)
  }
  def fromExternalPk(typeDef: TableDef[_]) = {
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
    col.copy(type_ = typ, nullable = nullable)
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
        (defaultType("long"), nullableOrFalse)
      case (name, typ) if isRefName(name) =>
        (typ getOrElse new Type(null), nullableOrTrue)
      case (name, _) if isBooleanName(name) =>
        (defaultType("boolean"), nullableOrTrue)
      case (name, _) if isDateName(name) =>
        (defaultType("date"), nullableOrTrue)
      case (name, _) if isIdRefName(name) =>
        (defaultType("long"), nullableOrTrue)
      case _ =>
        (defaultType("string"), nullableOrTrue)
    }
  }
  def toExternal(table: TableDef[Type]): TableDef[IoColumnType] =
    table.copy(
      cols = table.cols.map(toExternal(table, _)),
      pk = toExternalPk(table))
  def toExternalPk(typeDef: TableDef[Type]) = {
    val cols = typeDef.cols.map(_.name)
    val DefaultPkName = "pk_" + typeDef.name
    if (!typeDef.pk.isDefined) None
    else if (cols.filter(isIdName).size == 1)
      typeDef.pk match {
        case Some(DbIndex(DefaultPkName, List(id))) if isIdName(id) => None
        case pk => pk
      }
    else if (cols.size == 2 && cols.filter(isIdRefName).size == 2)
      typeDef.pk match {
        case Some(DbIndex(DefaultPkName,
          List(col1, col2))) if (col1.toLowerCase, col2.toLowerCase) ==
          (cols(0).toLowerCase, cols(1).toLowerCase) => None
        case pk => pk
      }
    else None
  }

  def toExternal(table: TableDef[Type], col: ColumnDef[Type]): ColumnDef[IoColumnType] = {
    val nullOpt = (col.name, col.nullable) match {
      case (name, false) if isIdName(name) => None
      case (_, true) => None
      case (_, nullable) => Some(nullable)
    }
    val ref = table.refs
      .filter(_.cols.size == 1)
      .find(_.cols(0) == col.name)
    if (ref.isDefined) {
      // TODO ref checks (type match, type override)
      // TODO multicol refs, ...?
      // TODO drop enum, if differs?
      val refTypeName = ref.map(r => r.refTable + "." + r.refCols(0)).get
      val typeOpt =
        if (col.name == ref.map(r => r.refTable + "_" + r.refCols(0)).get) None
        else Some(new Type(refTypeName))
      val name =
        if (typeOpt.isDefined) col.name // FIXME "." for alias? moth.id/pers.id
        else refTypeName
      col.copy(
        name = name,
        type_ = IoColumnType(nullOpt, typeOpt))
    } else {
      val typeOpt = (col.name, col.type_.name, col.type_.length) match {
        case (name, "long", _) if isIdName(name) => None
        case (name, "long", _) if isIdRefName(name) => None
        case (name, "boolean", _) if isBooleanName(name) => None
        case (name, "date", _) if isDateName(name) => None
        case (name, "string", None) if !isTypedName(name) => None
        case (name, "string", Some(len)) if !isTypedName(name) =>
          Some(new Type(null.asInstanceOf[String], len))
        case _ => Option(col.type_)
      }
      col.copy(type_ = IoColumnType(nullOpt, typeOpt))
    }
  }
}

object MdConventions extends MdConventions

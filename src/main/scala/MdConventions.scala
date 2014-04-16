package mojoz.metadata.io

import mojoz.metadata._
import mojoz.metadata.TableDef._

case class ExColumnType(nullable: Option[Boolean], type_ : Option[Type])

class MdConventions {
  def isRefName(name: String) = name != null && name.contains(".")
  def isBooleanName(name: String) =
    name.startsWith("is_") || name.startsWith("has_")
  def isDateName(name: String) = name.endsWith("_date")
  def isIdRefName(name: String) = name.endsWith("_id")
  def isTypedName(name: String) =
    isBooleanName(name) || isDateName(name) || isIdRefName(name)
  def fromExternal(typeDef: TableDef[ExColumnType]): TableDef[Type] = {
    val cols = typeDef.cols map fromExternal
    val primaryKey = fromExternalPk(typeDef)
    TableDef(typeDef.name, typeDef.comments, cols, primaryKey, Nil, Nil, Nil)
  }
  def fromExternalPk(typeDef: TableDef[_]) = {
    import scala.language.existentials
    val cols = typeDef.cols
    if (typeDef.pk.isDefined) typeDef.pk
    else if (cols.filter(_.name == "id").size == 1)
      Some(DbIndex(null, List("id")))
    else if (cols.filter(_.name == "code").size == 1)
      Some(DbIndex(null, List("code")))
    else if (cols.size == 2 && cols.filter(_.name endsWith "_id").size == 2)
      Some(DbIndex(null, cols.map(_.name)))
    else None
  }
  def fromExternal(col: ColumnDef[ExColumnType]): ColumnDef[Type] = {
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
      case ("id", _) =>
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
  def toExternal(table: TableDef[Type]): TableDef[ExColumnType] =
    table.copy(
      cols = table.cols.map(toExternal(table, _)),
      pk = toExternalPk(table))
  def toExternalPk(typeDef: TableDef[Type]) = {
    val cols = typeDef.cols
    val DefaultPkName = "pk_" + typeDef.name
    if (!typeDef.pk.isDefined) None
    else if (cols.filter(_.name == "id").size == 1)
      typeDef.pk match {
        case Some(DbIndex(DefaultPkName, List("id"))) => None
        case pk => pk
      }
    else if (cols.size == 2 && cols.filter(_.name endsWith "_id").size == 2)
      typeDef.pk match {
        case Some(DbIndex(DefaultPkName,
          List(col1, col2))) if (col1, col2) == (cols(0).name, cols(1).name) =>
          None
        case pk => pk
      }
    else None
  }

  def toExternal(table: TableDef[Type], col: ColumnDef[Type]): ColumnDef[ExColumnType] = {
    val nullOpt = (col.name, col.nullable) match {
      case ("id", false) => None
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
        name = refTypeName,
        type_ = ExColumnType(nullOpt, typeOpt))
    } else {
      val typeOpt = (col.name, col.type_.name, col.type_.length) match {
        case ("id", "long", _) => None
        case (name, "long", _) if isIdRefName(name) => None
        case (name, "boolean", _) if isBooleanName(name) => None
        case (name, "date", _) if isDateName(name) => None
        case (name, "string", None) if !isTypedName(name) => None
        case (name, "string", Some(len)) if !isTypedName(name) =>
          Some(new Type(null.asInstanceOf[String], len))
        case _ => Option(col.type_)
      }
      col.copy(type_ = ExColumnType(nullOpt, typeOpt))
    }
  }
}

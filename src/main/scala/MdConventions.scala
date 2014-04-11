package mojoz.metadata.io

import mojoz.metadata._

case class ExTableDef(
  name: String,
  comments: String,
  cols: Seq[ExColumnDef],
  pk: Option[DbIndex])
case class ExColumnDef(
  name: String,
  xsdType: Option[XsdType],
  nullable: Option[Boolean],
  dbDefault: String,
  enum: Seq[String],
  comments: String)

object MdConventions {
  // TODO default comment for id col?
  def isRefName(name: String) = name != null && name.contains(".")
  def isBooleanName(name: String) =
    name.startsWith("is_") || name.startsWith("has_")
  def isDateName(name: String) = name.endsWith("_date")
  def isIdRefName(name: String) = name.endsWith("_id")
  def isTypedName(name: String) =
    isBooleanName(name) || isDateName(name) || isIdRefName(name)
  def fromExternal(typeDef: ExTableDef): TableDef[XsdType] = {
    val cols = typeDef.cols map fromExternal
    val primaryKey = fromExternalPk(typeDef)
    TableDef(typeDef.name, typeDef.comments, cols, primaryKey, Nil, Nil, Nil)
  }
  def fromExternalPk(typeDef: ExTableDef) = {
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
  def fromExternal(col: ExColumnDef): ColumnDef[XsdType] = {
    def defaultType(defaultName: String) =
      col.xsdType map { x =>
        if (x.name == null) x.copy(name = defaultName)
        else x
      } getOrElse new XsdType(defaultName)
    col match {
      case ExColumnDef(name, Some(xsdType), nullable, dbDefault, enum, comment) if isRefName(xsdType.name) =>
        ColumnDef(name, xsdType,
          nullable getOrElse true, dbDefault, enum, comment)
      case ExColumnDef("id", xsdType, nullable, dbDefault, enum, comment) =>
        ColumnDef("id", defaultType("long"),
          nullable getOrElse false, dbDefault, enum, comment)
      case ExColumnDef(name, xsdType, nullable, dbDefault, enum, comment) if isRefName(name) =>
        ColumnDef(name, xsdType getOrElse new XsdType(null),
          nullable getOrElse true, dbDefault, enum, comment)
      case ExColumnDef(name, xsdType, nullable, dbDefault, enum, comment) if isBooleanName(name) =>
        ColumnDef(name, defaultType("boolean"),
          nullable getOrElse true, dbDefault, enum, comment)
      case ExColumnDef(name, xsdType, nullable, dbDefault, enum, comment) if isDateName(name) =>
        ColumnDef(name, defaultType("date"),
          nullable getOrElse true, dbDefault, enum, comment)
      case ExColumnDef(name, xsdType, nullable, dbDefault, enum, comment) if isIdRefName(name) =>
        ColumnDef(name, defaultType("long"),
          nullable getOrElse true, dbDefault, enum, comment)
      case x =>
        ColumnDef(x.name, defaultType("string"),
          x.nullable getOrElse true, x.dbDefault, x.enum, x.comments)
    }
  }
  def toExternal(typeDef: TableDef[XsdType]): ExTableDef =
    ExTableDef(typeDef.name, typeDef.comments, typeDef.cols map toExternal,
      toExternalPk(typeDef))
  def toExternalPk(typeDef: TableDef[XsdType]) = {
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

  def toExternal(col: ColumnDef[XsdType]): ExColumnDef = {
    val nullOpt = (col.name, col.nullable) match {
      case ("id", false) => None
      case (_, true) => None
      case (_, nullable) => Some(nullable)
    }
    val typeOpt = (col.name, col.type_.name, col.type_.length) match {
      case ("id", "long", _) => None
      case (name, "long", _) if isIdRefName(name) => None
      case (name, "boolean", _) if isBooleanName(name) => None
      case (name, "date", _) if isDateName(name) => None
      case (name, "string", None) if !isTypedName(name) => None
      case (name, "string", Some(len)) if !isTypedName(name) =>
        Some(new XsdType(null.asInstanceOf[String], len))
      case _ => Option(col.type_)
    }
    ExColumnDef(col.name, typeOpt, nullOpt, col.dbDefault, col.enum, col.comments)
  }
}

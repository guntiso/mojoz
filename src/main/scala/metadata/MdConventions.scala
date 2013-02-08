package metadata

case class ExTypeDef(name: String, comment: String, cols: Seq[ExFieldDef])
case class ExFieldDef(
  name: String,
  xsdType: Option[XsdType],
  nullable: Option[Boolean],
  comment: String)

object MdConventions {
  // TODO default comment for id col?
  def isBooleanName(name: String) =
    name.startsWith("is_") || name.startsWith("has_")
  def isDateName(name: String) = name.endsWith("_date")
  def isIdRefName(name: String) = name.endsWith("_id")
  def isTypedName(name: String) =
    isBooleanName(name) || isDateName(name) || isIdRefName(name)
  def fromExternal(typeDef: ExTypeDef): TableDef = {
    TableDef(typeDef.name, typeDef.comment, typeDef.cols.map(fromExternal(_)))
  }
  def fromExternal(col: ExFieldDef): ColumnDef = {
    col match {
      case ExFieldDef("id", xsdType, nullable, comment) =>
        ColumnDef("id", xsdType getOrElse new XsdType("long"),
          nullable getOrElse false, comment)
      case ExFieldDef(name, xsdType, nullable, comment) if isBooleanName(name) =>
        ColumnDef(name, xsdType getOrElse new XsdType("boolean"),
          nullable getOrElse true, comment)
      case ExFieldDef(name, xsdType, nullable, comment) if isDateName(name) =>
        ColumnDef(name, xsdType getOrElse new XsdType("date"),
          nullable getOrElse true, comment)
      // FIXME typename not defined, len defined
      case x =>
        ColumnDef(x.name, x.xsdType getOrElse new XsdType("string", 256),
          x.nullable getOrElse true, x.comment)
    }
  }
  def toExternal(typeDef: TableDef): ExTypeDef =
    ExTypeDef(typeDef.name, typeDef.comment, typeDef.cols map toExternal)

  def toExternal(col: ColumnDef): ExFieldDef = {
    val nullOpt = (col.name, col.nullable) match {
      case ("id", false) => None
      case (_, true) => None
      case (_, nullable) => Some(nullable)
    }
    val typeOpt = (col.name, col.xsdType.name, col.xsdType.length) match {
      case ("id", "long", _) => None
      case (name, "long", _) if isIdRefName(name) => None
      case (name, "boolean", _) if isBooleanName(name) => None
      case (name, "date", _) if isDateName(name) => None
      case (name, "string", None) if !isTypedName(name) => None
      case (name, "string", Some(len)) if !isTypedName(name) =>
        Some(new XsdType(null.asInstanceOf[String], len))
      case _ => Option(col.xsdType)
    }
    ExFieldDef(col.name, typeOpt, nullOpt, col.comment)
  }
}

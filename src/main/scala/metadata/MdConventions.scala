package metadata

case class ExTypeDef(
  name: String,
  comment: String,
  cols: Seq[ExFieldDef],
  pk: Option[DbIndex])
case class ExFieldDef(
  name: String,
  xsdType: Option[XsdType],
  nullable: Option[Boolean],
  dbDefault: String,
  enum: Seq[String],
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
    val cols = typeDef.cols.map(fromExternal(_))
    val primaryKey = fromExternalPk(typeDef)
    TableDef(typeDef.name, typeDef.comment, cols, primaryKey)
  }
  def fromExternalPk(typeDef: ExTypeDef) = {
    val cols = typeDef.cols
    if (typeDef.pk.isDefined) typeDef.pk
    else if (cols.filter(_.name == "id").size == 1)
      Some(DbIndex("pk_" + typeDef.name, List("id")))
    else if (cols.size == 2 && cols.filter(_.name endsWith "_id").size == 2)
      Some(DbIndex("pk_" + typeDef.name, cols.map(_.name)))
    else None
  }
  def fromExternal(col: ExFieldDef): ColumnDef = {
    def defaultType(defaultName: String) =
      col.xsdType map { x =>
        if (x.name == null) x.copy(name = defaultName)
        else x
      } getOrElse new XsdType(defaultName)
    col match {
      case ExFieldDef("id", xsdType, nullable, dbDefault, enum, comment) =>
        ColumnDef("id", defaultType("long"),
          nullable getOrElse false, dbDefault, enum, comment)
      case ExFieldDef(name, xsdType, nullable, dbDefault, enum, comment) if isBooleanName(name) =>
        ColumnDef(name, defaultType("boolean"),
          nullable getOrElse true, dbDefault, enum, comment)
      case ExFieldDef(name, xsdType, nullable, dbDefault, enum, comment) if isDateName(name) =>
        ColumnDef(name, defaultType("date"),
          nullable getOrElse true, dbDefault, enum, comment)
      case ExFieldDef(name, xsdType, nullable, dbDefault, enum, comment) if isIdRefName(name) =>
        ColumnDef(name, defaultType("long"),
          nullable getOrElse true, dbDefault, enum, comment)
      case x =>
        ColumnDef(x.name, defaultType("string"),
          x.nullable getOrElse true, x.dbDefault, x.enum, x.comment)
    }
  }
  def toExternal(typeDef: TableDef): ExTypeDef =
    ExTypeDef(typeDef.name, typeDef.comment, typeDef.cols map toExternal,
      toExternalPk(typeDef))
  def toExternalPk(typeDef: TableDef) = {
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
    ExFieldDef(col.name, typeOpt, nullOpt, col.dbDefault, col.enum, col.comment)
  }
}

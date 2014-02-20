package metadata

import metadata.DbConventions.{ dbNameToXsdName => xsdName }

trait ScalaClassWriter {
  def nl = System.getProperty("line.separator")
  def scalaClassName(name: String) = xsdName(name)
  def scalaFieldName(name: String) = xsdName(name) match {
    case x if x.length == 1 || (x.length > 1 && (x(1).isLower || x(1).isDigit)) =>
      x(0).toLower + x.substring(1)
    case x => x
  }
  def scalaFieldTypeName(field: XsdFieldDef) = {
    val itemTypeName =
      if (field.isComplexType) scalaComplexTypeName(field.xsdType)
      else scalaSimpleTypeName(field.xsdType)
    if (field.isCollection) scalaCollectionTypeName(itemTypeName)
    else itemTypeName
  }
  def scalaCollectionTypeName(itemTypeName: String) = s"List[$itemTypeName]"
  def scalaSimpleTypeName(t: XsdType) = t.name match {
    case "integer" => "BigInt"
    case "long" => "java.lang.Long"
    case "int" => "java.lang.Integer"
    case "decimal" => "BigDecimal"
    case "date" => "java.sql.Date"
    case "dateTime" => "java.sql.Timestamp"
    case "string" => "String"
    case "boolean" => "java.lang.Boolean"
    case "base64Binary" => "Array[Byte]"
    case "anyType" => "Any"
    case x =>
      throw new RuntimeException("Unexpected type: " + t)
  }
  def scalaComplexTypeName(t: XsdType) = scalaClassName(t.name)
  def initialValueString(col: XsdFieldDef) =
    if (col.isCollection) "Nil" else "null"
  private def scalaFieldString(fieldName: String, col: XsdFieldDef) =
    s"var $fieldName: ${scalaFieldTypeName(col)} = ${initialValueString(col)}"
  def scalaClassExtends(typeDef: XsdTypeDef) =
    Option(typeDef.xtnds).filter(_ != "").map(scalaClassName)
  def scalaClassTraits(typeDef: XsdTypeDef): Seq[String] = Seq()
  def scalaFieldsIndent = "  "
  def scalaFieldsStrings(typeDef: XsdTypeDef) =
    typeDef.fields.map(f => scalaFieldString(
      scalaFieldName(Option(f.alias) getOrElse f.name), f))
  def createScalaClassString(typeDef: XsdTypeDef) = {
    val fieldsString = scalaFieldsStrings(typeDef)
      .map(scalaFieldsIndent + _ + nl).mkString
    val extendsString = Option(scalaClassTraits(typeDef))
      .map(scalaClassExtends(typeDef).toList ::: _.toList)
      .map(t => t.filter(_ != null).filter(_.trim != ""))
      .filter(_.size > 0)
      .map(_.mkString(" extends ", " with ", ""))
      .getOrElse("")
    s"class ${scalaClassName(typeDef.name)}$extendsString {$nl$fieldsString}"
  }
  def createScalaClassesString(
    headers: Seq[String], typedefs: Seq[XsdTypeDef], footers: Seq[String]) =
    List(headers, typedefs map createScalaClassString, footers)
      .flatMap(x => x)
      .mkString("", nl, nl)
}

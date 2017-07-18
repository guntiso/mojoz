package mojoz.metadata.out

import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.Type
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }

trait ScalaClassWriter {
  def nl = System.getProperty("line.separator")
  def scalaClassName(name: String) = name
  def scalaFieldName(name: String) = name
  def scalaFieldTypeName(field: FieldDef[Type]) = {
    val itemTypeName =
      if (field.type_.isComplexType) scalaComplexTypeName(field.type_)
      else scalaSimpleTypeName(field.type_)
    if (field.isCollection) scalaCollectionTypeName(itemTypeName)
    else itemTypeName
  }
  def scalaCollectionTypeName(itemTypeName: String) = s"List[$itemTypeName]"
  // TODO generic, extract
  def scalaSimpleTypeName(t: Type) = t.name match {
    case "integer" => "BigInt"
    case "long" => "java.lang.Long"
    case "int" => "java.lang.Integer"
    case "double" => "java.lang.Double"
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
  def scalaComplexTypeName(t: Type) = scalaClassName(t.name)
  def initialValueString(col: FieldDef[Type]) =
    if (col.isCollection) "Nil" else "null"
  private def scalaFieldString(fieldName: String, col: FieldDef[Type]) =
    try
      s"var $fieldName: ${scalaFieldTypeName(col)} = ${initialValueString(col)}"
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process field: $fieldName", ex)
    }
  def scalaClassExtends(typeDef: ViewDef[FieldDef[Type]]) =
    Option(typeDef.extends_).filter(_ != "").map(scalaClassName)
  def scalaClassTraits(typeDef: ViewDef[FieldDef[Type]]): Seq[String] = Seq()
  def scalaFieldsIndent = "  "
  def scalaFieldsStrings(typeDef: ViewDef[FieldDef[Type]]) =
    try
      typeDef.fields.map(f => scalaFieldString(
        scalaFieldName(Option(f.alias) getOrElse f.name), f))
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process view: ${typeDef.name}", ex)
    }
  def createScalaClassString(typeDef: ViewDef[FieldDef[Type]]) = {
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
    headers: Seq[String], typedefs: Seq[ViewDef[FieldDef[Type]]], footers: Seq[String]) =
    List(headers, typedefs map createScalaClassString, footers)
      .flatMap(x => x)
      .mkString("", nl, nl)
}

object ScalaClassWriter extends ScalaClassWriter

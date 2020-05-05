package mojoz.metadata.out

import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.Type
import mojoz.metadata.TypeDef
import mojoz.metadata.TypeMetadata
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

// TODO ScalaWriter, ScalaTraitWriter, Scala[Companion]ObjectWriter
class ScalaClassWriter(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {
  val scalaKeywords: Set[String] = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends",
    "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy",
    "match", "new", "null", "object", "override", "package", "private", "protected",
    "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield")
  def nl = System.getProperty("line.separator")
  def scalaNameString(name: String) =
    if (scalaKeywords contains name) s"`$name`" else name
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
  lazy val typeNameToScalaTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("scala").orNull)
      .filter(_._2 != null)
      .toMap
  def scalaSimpleTypeName(t: Type) =
    typeNameToScalaTypeName.get(t.name).getOrElse(sys.error("Unexpected type: " + t))
  def scalaComplexTypeName(t: Type) = scalaClassName(t.name)
  def initialValueString(col: FieldDef[Type]) =
    if (col.isCollection) "Nil" else "null"
  def scalaFieldString(fieldName: String, col: FieldDef[Type]) =
    s"var ${scalaNameString(scalaFieldName(fieldName))}: ${scalaFieldTypeName(col)} = ${initialValueString(col)}"
  def scalaFieldStringWithHandler(fieldName: String, col: FieldDef[Type]) =
    try
      scalaFieldString(fieldName, col)
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process field: $fieldName", ex)
    }
  def scalaClassExtends(viewDef: ViewDef[FieldDef[Type]]) =
    Option(viewDef.extends_).filter(_ != "").map(scalaClassName)
  def scalaClassTraits(viewDef: ViewDef[FieldDef[Type]]): Seq[String] = Seq()
  def scalaFieldsIndent = "  "
  def scalaFieldsStrings(viewDef: ViewDef[FieldDef[Type]]) =
    viewDef.fields.map(f => scalaFieldStringWithHandler(Option(f.alias) getOrElse f.name, f))
  def scalaFieldsStringsWithHandler(viewDef: ViewDef[FieldDef[Type]]) =
    try
      scalaFieldsStrings(viewDef)
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process view: ${viewDef.name}", ex)
    }
  def scalaFieldsString(viewDef: ViewDef[FieldDef[Type]]) =
    scalaFieldsStringsWithHandler(viewDef)
      .map(scalaFieldsIndent + _ + nl).mkString
  def scalaBody(viewDef: ViewDef[FieldDef[Type]]) =
    scalaFieldsString(viewDef) + Option(scalaBodyExtra(viewDef)).getOrElse("")
  def scalaBodyExtra(viewDef: ViewDef[FieldDef[Type]]) = ""
  def scalaExtendsString(viewDef: ViewDef[FieldDef[Type]]) =
    Option(scalaClassTraits(viewDef))
      .map(scalaClassExtends(viewDef).toList ::: _.toList)
      .map(t => t.filter(_ != null).filter(_.trim != ""))
      .filter(_.size > 0)
      .map(_ map scalaNameString)
      .map(_.mkString(" extends ", " with ", ""))
      .getOrElse("")
  def scalaPrefix(viewDef: ViewDef[FieldDef[Type]]) = "class"
  def createScalaClassString(viewDef: ViewDef[FieldDef[Type]]) = {
    s"${scalaPrefix(viewDef)} ${scalaNameString(scalaClassName(viewDef.name))}${scalaExtendsString(viewDef)} {$nl${scalaBody(viewDef)}}"
  }
  def createScalaClassesString(
    headers: Seq[String], typedefs: Seq[ViewDef[FieldDef[Type]]], footers: Seq[String]) =
    List(headers, typedefs map createScalaClassString, footers)
      .flatMap(x => x)
      .mkString("", nl, nl)
}

class ScalaCaseClassWriter(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends ScalaClassWriter(typeDefs) {
  override def scalaFieldString(fieldName: String, col: FieldDef[Type]) =
    s"${scalaNameString(scalaFieldName(fieldName))}: ${scalaFieldTypeName(col)} = ${initialValueString(col)}"
  override def scalaFieldsStrings(viewDef: ViewDef[FieldDef[Type]]) = {
    val fieldsStrings = super.scalaFieldsStrings(viewDef)
    if (fieldsStrings.size < 2) fieldsStrings
    else (fieldsStrings.reverse.head :: fieldsStrings.reverse.tail.map(_ + ",").toList).reverse
  }
  // FIXME extends for case classes?
  override def scalaPrefix(viewDef: ViewDef[FieldDef[Type]]) = "case class"
  override def scalaClassExtends(viewDef: ViewDef[FieldDef[Type]]) = None
  override def createScalaClassString(viewDef: ViewDef[FieldDef[Type]]) = {
    s"${scalaPrefix(viewDef)} ${scalaNameString(scalaClassName(viewDef.name))}${scalaExtendsString(viewDef)} ($nl${scalaFieldsString(viewDef)})" +
      Option(scalaBodyExtra(viewDef)).filter(_.trim != "").map(txt => " {" + nl + txt + "}").getOrElse("")
  }
}

object ScalaClassWriter extends ScalaClassWriter(TypeMetadata.customizedTypeDefs)
object ScalaCaseClassWriter extends ScalaCaseClassWriter(TypeMetadata.customizedTypeDefs)

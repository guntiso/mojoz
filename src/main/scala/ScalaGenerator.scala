package org.mojoz.metadata
package out

import org.mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import org.mojoz.metadata.Type
import org.mojoz.metadata.TypeDef
import org.mojoz.metadata.TypeMetadata
import org.mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

// TODO ScalaTraitGenerator
class ScalaGenerator(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val SimpleIdentR = ("^" + ident + "$").r
  val scalaKeywords: Set[String] = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends",
    "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy",
    "match", "new", "null", "object", "override", "package", "private", "protected",
    "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield")
  def nl = System.getProperty("line.separator")
  def nonStickName(name: String) = if (name endsWith "_") s"$name " else name
  def scalaNameString(name: String) =
    if (SimpleIdentR.pattern.matcher(name).matches && !(scalaKeywords contains name)) name
    else s"`$name`"
  def scalaClassName(name: String) = name
  def scalaFieldName(name: String) = name
  def scalaFieldTypeName(field: MojozFieldDef) = {
    val itemTypeName = scalaTypeName(field.type_)
    if (field.isCollection) scalaCollectionTypeName(itemTypeName)
    else itemTypeName
  }
  def scalaTypeName(type_ : Type): String =
    if  (type_.isComplexType)
         scalaComplexTypeName(type_)
    else scalaSimpleTypeName(type_)
  def scalaCollectionTypeName(itemTypeName: String) = s"List[$itemTypeName]"
  lazy val typeNameToScalaTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("scala").orNull)
      .filter(_._2 != null)
      .toMap
  def scalaSimpleTypeName(t: Type) =
    typeNameToScalaTypeName.get(t.name).getOrElse(sys.error("Unexpected type: " + t))
  def scalaComplexTypeName(t: Type) = scalaClassName(t.name)
  def initialValueString(col: MojozFieldDef) =
    if (col.isCollection) "Nil" else "null"
  def scalaFieldString(fieldName: String, col: MojozFieldDef) =
    s"var ${nonStickName(scalaNameString(scalaFieldName(fieldName)))}: ${scalaFieldTypeName(col)} = ${initialValueString(col)}"
  def scalaFieldStringWithHandler(fieldName: String, col: MojozFieldDef) =
    try
      scalaFieldString(fieldName, col)
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process field: $fieldName", ex)
    }
  def scalaClassExtends(viewDef: MojozViewDef) =
    Option(viewDef.extends_).filter(_ != "").map(scalaClassName)
  def scalaClassTraits(viewDef: MojozViewDef): Seq[String] = Seq()
  def scalaFieldsIndent = "  "
  def scalaFieldsStrings(viewDef: MojozViewDef) =
    viewDef.fields.map(f => scalaFieldStringWithHandler(Option(f.alias) getOrElse f.name, f))
  def scalaFieldsStringsWithHandler(viewDef: MojozViewDef) =
    try
      scalaFieldsStrings(viewDef)
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process view: ${viewDef.name}", ex)
    }
  def scalaFieldsString(viewDef: MojozViewDef) =
    scalaFieldsStringsWithHandler(viewDef)
      .map(scalaFieldsIndent + _ + nl).mkString
  def scalaBody(viewDef: MojozViewDef) =
    scalaFieldsString(viewDef) + Option(scalaBodyExtra(viewDef)).getOrElse("")
  def scalaBodyExtra(viewDef: MojozViewDef) = ""
  def scalaExtendsString(viewDef: MojozViewDef) =
    Option(scalaClassTraits(viewDef))
      .map(scalaClassExtends(viewDef).toList ::: _.toList)
      .map(t => t.filter(_ != null).filter(_ != ""))
      .filter(_.size > 0)
      .map(_ map scalaNameString)
      .map(_.mkString(" extends ", " with ", ""))
      .getOrElse("")
  def scalaPrefix(viewDef: MojozViewDef) = "class"
  def scalaClassString(viewDef: MojozViewDef) = {
    s"${scalaPrefix(viewDef)} ${scalaNameString(scalaClassName(viewDef.name))}${scalaExtendsString(viewDef)} {$nl${scalaBody(viewDef)}}"
  }
  def scalaObjectString(viewDef: MojozViewDef): String = ""
  def scalaClassAndObjectString(viewDef: MojozViewDef): String =
    Seq(scalaClassString(viewDef), scalaObjectString(viewDef))
      .filter(_ != null).filter(_ != "").mkString(nl)
  def generateScalaSource(
    headers: Seq[String], viewDefs: Seq[MojozViewDef], footers: Seq[String]) =
    List(headers, viewDefs map scalaClassAndObjectString, footers)
      .flatMap(x => x)
      .mkString("", nl, nl)
}

class ScalaCaseClassGenerator(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends ScalaGenerator(typeDefs) {
  override def scalaFieldString(fieldName: String, col: MojozFieldDef) =
    s"${nonStickName(scalaNameString(scalaFieldName(fieldName)))}: ${scalaFieldTypeName(col)} = ${initialValueString(col)}"
  override def scalaFieldsStrings(viewDef: MojozViewDef) = {
    val fieldsStrings = super.scalaFieldsStrings(viewDef)
    if (fieldsStrings.size < 2) fieldsStrings
    else (fieldsStrings.reverse.head :: fieldsStrings.reverse.tail.map(_ + ",").toList).reverse
  }
  // FIXME extends for case classes?
  override def scalaPrefix(viewDef: MojozViewDef) = "case class"
  override def scalaClassExtends(viewDef: MojozViewDef) = None
  override def scalaClassString(viewDef: MojozViewDef) = {
    s"${scalaPrefix(viewDef)} ${scalaNameString(scalaClassName(viewDef.name))}${scalaExtendsString(viewDef)} ($nl${scalaFieldsString(viewDef)})" +
      Option(scalaBodyExtra(viewDef)).filter(_ != "").map(txt => " {" + nl + txt + "}").getOrElse("")
  }
}

object ScalaGenerator extends ScalaGenerator(TypeMetadata.customizedTypeDefs)
object ScalaCaseClassGenerator extends ScalaCaseClassGenerator(TypeMetadata.customizedTypeDefs)

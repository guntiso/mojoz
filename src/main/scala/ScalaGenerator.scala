package org.mojoz.metadata
package out

import scala.collection.immutable.Seq
import scala.collection.immutable.Set

// TODO ScalaTraitGenerator
class ScalaGenerator(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {
  private val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  private val qualifiedIdent = s"$ident(\\.$ident)*"
  private val SimpleIdentR = ("^" + ident + "$").r
  private val QualifiedIdentR = ("^" + qualifiedIdent + "$").r
  val scalaKeywords: Set[String] = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends",
    "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy",
    "match", "new", "null", "object", "override", "package", "private", "protected",
    "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield",
    // Scala 3 regular keywords
    "abstract", "case", "catch", "class", "def", "do", "else",
    "enum", "export", "extends", "false", "final", "finally", "for",
    "given", "if", "implicit", "import", "lazy", "match", "new",
    "null", "object", "override", "package", "private", "protected", "return",
    "sealed", "super", "then", "throw", "trait", "true", "try",
    "type", "val", "var", "while", "with", "yield",
    // Scala 3 soft keywords
    "as", "derives", "end", "extension", "infix", "inline", "opaque", "open", "transparent", "using",
  )
  def nl = System.getProperty("line.separator")
  def nonStickName(name: String) = if (name endsWith "_") s"$name " else name
  def scalaNameString(name: String) =
    if (name.startsWith("`") || SimpleIdentR.pattern.matcher(name).matches && !(scalaKeywords contains name)) name
    else s"`$name`"
  def scalaQualifiedNameString(name: String) =
    if (name.startsWith("`") || QualifiedIdentR.pattern.matcher(name).matches && !(scalaKeywords contains name)) name
    else s"`$name`"
  def scalaClassName(name: String) = name
  def scalaFieldName(name: String) = name
  def scalaFieldTypeName(field: FieldDef) = {
    val typeNameNoOpt = scalaFieldTypeNameNoOpt(field)
    if (field.options != null && field.options.contains('?')) s"Option[$typeNameNoOpt]" else typeNameNoOpt
  }
  def scalaFieldTypeNameNoOpt(field: FieldDef) = {
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
      .map {
        case (typeName, "java.lang.String") => (typeName, "String")
        case x => x
      }
      .toMap
  def scalaSimpleTypeName(t: Type) =
    typeNameToScalaTypeName.get(t.name).getOrElse(sys.error("Unexpected type: " + t))
  def scalaComplexTypeName(t: Type) = scalaClassName(t.name)
  def initialValueString(field: FieldDef) =
    if (field.options != null && field.options.contains('?')) "None" else initialValueStringNoOpt(field)
  def initialValueStringNoOpt(field: FieldDef) =
    if (field.isCollection) "Nil" else "null"
  def scalaFieldString(fieldName: String, field: FieldDef) =
    s"var ${nonStickName(scalaNameString(scalaFieldName(fieldName)))}: ${scalaFieldTypeName(field)} = ${initialValueString(field)}"
  def scalaFieldStringWithHandler(fieldName: String, field: FieldDef) =
    try
      scalaFieldString(fieldName, field)
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process field: $fieldName", ex)
    }
  def scalaClassExtends(viewDef: ViewDef) =
    Option(viewDef.extends_).filter(_ != "").map(scalaClassName)
  def isExtendsDisabled(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]): Boolean = {
    def fieldType(viewName: String, fieldName: String): Type = {
      allViewDefs.get(viewName).map { v =>
        v.fieldOpt(fieldName)
          .map(_.type_)
          .getOrElse(fieldType(v.extends_, fieldName))
      }.orNull
    }
    viewDef.fields.exists(f =>
      f.type_.isComplexType && f.isOverride && f.type_ != fieldType(viewDef.extends_, f.fieldName))
  }
  def scalaClassTraits(viewDef: ViewDef): Seq[String] = Seq()
  def scalaFieldsIndent = "  "
  def scalaFieldsStrings(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]): Seq[String] = {
    val xtndsOpt = scalaClassExtends(viewDef)
    val xtndsDisabled = xtndsOpt.isDefined && isExtendsDisabled(viewDef, allViewDefs)
    def fieldsStrings(viewDef: ViewDef, overrided: Set[String]) =
      viewDef.fields
        .map(f =>
          (if (overrided.nonEmpty && overrided.contains(f.fieldName)) "// "
           else if (f.isOverride) (if (xtndsDisabled) "/* override */ " else "// override ") else "") +
          scalaFieldStringWithHandler(f.fieldName, f))
    def fieldsStringsWithBase(viewDef: ViewDef, overrided: Set[String]): Seq[String] = {
      val baseFields =
        if (viewDef.extends_ == null) Nil
        else {
          val baseView = allViewDefs.get(viewDef.extends_).getOrElse {
            sys.error(
              s"Failed to include scala fields into ${viewDef.name} from ${viewDef.extends_} because it is not found")
          }
          fieldsStringsWithBase(baseView, overrided ++ viewDef.fields.map(f => f.fieldName))
        }
      baseFields ++ (
        s"// --- ${scalaNameString(scalaClassName(viewDef.name))}" +:
        fieldsStrings(viewDef, overrided))
    }
    if (xtndsDisabled)
         fieldsStringsWithBase(viewDef, Set.empty)
    else fieldsStrings(viewDef, Set.empty)
  }
  def scalaFieldsStringsWithHandler(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) =
    try
      scalaFieldsStrings(viewDef, allViewDefs)
    catch {
      case ex: Exception =>
        throw new RuntimeException(s"Failed to process view: ${viewDef.name}", ex)
    }
  def scalaFieldsString(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) =
    scalaFieldsStringsWithHandler(viewDef, allViewDefs)
      .map(scalaFieldsIndent + _ + nl).mkString
  def scalaBody(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) =
    scalaFieldsString(viewDef, allViewDefs) + Option(scalaBodyExtra(viewDef, allViewDefs)).getOrElse("")
  def scalaBodyExtra(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) = ""
  def scalaExtendsString(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) = {
    val xtndsOpt = scalaClassExtends(viewDef)
    val xtndsDisabled = xtndsOpt.isDefined && isExtendsDisabled(viewDef, allViewDefs)
    Option(scalaClassTraits(viewDef))
      .map(xtndsOpt.toList ::: _.toList)
      .map(_.filter(_ != null).filter(_ != ""))
      .filter(_.size > 0)
      .map(_ map scalaQualifiedNameString)
      .map(c =>
        if (xtndsDisabled)
          if (c.size == 1)
             s" /* extends ${c.head} */"
          else
             s" extends /* ${c.head} */ " + c.tail.mkString(" with ")
        else c.mkString(" extends ", " with ", "")
      )
      .getOrElse("")
  }
  def scalaPrefix(viewDef: ViewDef) = "class"
  def scalaClassString(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) = {
    s"${scalaPrefix(viewDef)} ${scalaNameString(scalaClassName(viewDef.name))}${scalaExtendsString(viewDef, allViewDefs)} {$nl${scalaBody(viewDef, allViewDefs)}}"
  }
  def scalaObjectString(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]): String = ""
  def scalaClassAndObjectString(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]): String =
    Seq(scalaClassString(viewDef, allViewDefs), scalaObjectString(viewDef, allViewDefs))
      .filter(_ != null).filter(_ != "").mkString(nl)
  def generateScalaSource(
    headers: Seq[String], viewDefs: Seq[ViewDef], footers: Seq[String], allViewDefs: Map[String, ViewDef] = null) = {
    val allViewDefsWithFallback = Option(allViewDefs).getOrElse(viewDefs.map(v => v.name -> v).toMap)
    List(headers, viewDefs.map(scalaClassAndObjectString(_, allViewDefsWithFallback)), footers)
      .flatMap(x => x)
      .mkString("", nl, nl)
  }
}

trait ScalaCaseClassGenerator extends ScalaGenerator {
  override def scalaFieldString(fieldName: String, field: FieldDef) =
    s"${nonStickName(scalaNameString(scalaFieldName(fieldName)))}: ${scalaFieldTypeName(field)} = ${initialValueString(field)}"
  override def scalaFieldsStrings(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) = {
    val fieldsStrings = super.scalaFieldsStrings(viewDef, allViewDefs)
    if (fieldsStrings.size < 2) fieldsStrings
    else {
      var isLastFound = false
      fieldsStrings.reverse.map { f =>
        if (f.startsWith("//")) f
        else if (isLastFound) f + ","
        else { isLastFound = true; f }
      }.reverse
    }
  }
  override def isExtendsDisabled(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]): Boolean = true
  override def scalaPrefix(viewDef: ViewDef) = "case class"
  override def scalaClassString(viewDef: ViewDef, allViewDefs: Map[String, ViewDef]) = {
    s"${scalaPrefix(viewDef)} ${scalaNameString(scalaClassName(viewDef.name))}${scalaExtendsString(viewDef, allViewDefs)} ($nl${scalaFieldsString(viewDef, allViewDefs)})" +
      Option(scalaBodyExtra(viewDef, allViewDefs)).filter(_ != "").map(txt => " {" + nl + txt + "}").getOrElse("")
  }
}

object ScalaGenerator extends ScalaGenerator(TypeMetadata.customizedTypeDefs)
object ScalaCaseClassGenerator extends ScalaGenerator(TypeMetadata.customizedTypeDefs) with ScalaCaseClassGenerator

package org.mojoz.metadata
package out

import scala.collection.immutable.Seq

class XsdGenerator(viewDefs: Seq[ViewDef],
    xsdName: String => String = identity,
    xsdComplexTypeName: String => String = identity,
    createListWrapper: ViewDef => Boolean = _.name endsWith "_list_row",
    listWrapperBaseName: String = "list_wrapper",
    listWrapperName: String => String =
      Option(_).map(_.replace("_list_row", "_list_wrapper")).orNull,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) {
  private val typedefs = viewDefs
  private val indentString: String = "  "
  private def indent(level: Int, s: String) = {
    def i(s: String) = {
      val oldI = s.takeWhile(_ == ' ')
      val newI = indentString * level
      ("\n" + s).replace("\n" + oldI, "\n" + newI).substring(1)
        .replace("\n" + newI + "\r\n", "\n\r\n")
        .replace("\n" + newI + "\n", "\n\n")
    }
    if (s == null || s == "") s
    else if (s startsWith "\r\n") "\r\n" + i(s.substring(2))
    else if (s startsWith "\n") "\n" + i(s.substring(1))
    else i(s)
  }
  private def esc(s: String) =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
  private def escAttr(s: String) = esc(s)
    .replace("\"", "&quot;").replace("'", "&apos;")
    .replace("\t", "&#x9;").replace("\r", "&#xD;").replace("\n", "&#xA;")
  private def attribs(a: String, v: String*) =
    a.split("\\s+").toList.zip(v)
      .filter(_._2 != null)
      .map(e => s"""${e._1}="${escAttr(e._2)}"""")
      .mkString(" ")
  private def annotation(comments: String, level: Int) =
    if (comments != null)
      indent(level, s"""
      <xs:annotation>
        <xs:documentation>${ esc(comments) }</xs:documentation>
      </xs:annotation>
      """)
    else ""
  lazy val simpleTypeNameToXsdSimpleTypeName =
    typeDefs
      .map(td => td.name -> td.targetNames.get("xsd").orNull)
      .filter(_._2 != null)
      .toMap
  def xsdSimpleTypeName(t: Type) =
    simpleTypeNameToXsdSimpleTypeName.get(t.name).getOrElse(sys.error("Unexpected type for xsd writer: " + t))
  protected def getMaxOccurs(col: FieldDef): String = {
    if (col.isCollection) "unbounded" else null
    /*
    val maxOccurs = Option(col.maxOccurs) getOrElse {
      if (col.isCollection) "unbounded" else null
    } match {
      case "1" => null
      case maxOccurs => maxOccurs
    }
    */
  }
  private def createElement(elName: String, col: FieldDef, level: Int = 0) = {
    val required = !col.nullable
    val maxOccurs = getMaxOccurs(col)
    val minOccurs = if (required) null else "0" // minOccurs and maxOccurs default to 1
    val nillable = if (required || col.isCollection) null else "true"
    val typeName =
      if (col.type_.isComplexType) "tns:" + xsdComplexTypeName(col.type_.name)
      else "xs:" + xsdSimpleTypeName(col.type_)
    val noBlankStr = required && typeName == "xs:string" &&
      (col.type_.length getOrElse 1) > 0
    val minLength = if (noBlankStr) Some(1) else None
    val t = col.type_
    (minLength, t.length, t.totalDigits, t.fractionDigits, t.intDigits, Option(col.enum_)) match {
      case (None, None, None, None, None, None) =>
        indent(level, s"""
        <xs:element ${attribs("name nillable minOccurs maxOccurs type",
            elName, nillable, minOccurs, maxOccurs, typeName)}>${
          Some(annotation(col.comments, 5))
            .filter(_ != "").map("\n" + indentString * 5 + _.trim) getOrElse ""}
        </xs:element>
        """)
      case (minL, maxL, totD, frcD, intD, enm) =>
        indent(level, s"""
        <xs:element ${attribs("name nillable minOccurs maxOccurs",
            elName, nillable, minOccurs, maxOccurs)}>${
          Some(annotation(col.comments, 5))
            .filter(_ != "").map("\n" + indentString * 5 + _.trim) getOrElse ""}
          <xs:simpleType>
            <xs:restriction base="${ typeName }">
              ${indent(7, List(
              minL.map(n => s"""<xs:minLength value="${ n.toString }"/>""").orNull,
              if (noBlankStr) raw"""<xs:pattern value="[\s\S]*[\S][\s\S]*"/>"""  else null,
              maxL.map(n => s"""<xs:maxLength value="${ n.toString }"/>""").orNull,
              totD.map(n => s"""<xs:totalDigits value="${ n.toString }"/>""").orNull,
              intD.map(n => s"""<xs:maxExclusive value="${("1" :: List.fill(n)("0")).mkString}"/>""").orNull,
              frcD.map(n => s"""<xs:fractionDigits value="${ n.toString }"/>""").orNull,
              enm.getOrElse(Nil).map(op => s"""<xs:enumeration value="${ escAttr(op) }"/>""").mkString("\n"))
              .filter(_ != null)
              .mkString("\n")
              ).trim}
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
        """)
    }
  }
  def complexType(viewDef: ViewDef, indentLevel: Int = 1) = {
    def createFields(level: Int) = {
      // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
      // TODO when no restriction:  type="xs:string"
      indent(level, s"""
      <xs:sequence>
        ${viewDef.fields.filterNot(_.isOverride).map(f =>
          createElement(xsdName(f.fieldName), f, 4))
          .map(_.trim).mkString("\n" + indentString * 4)
        }
      </xs:sequence>
      """)
    }
    indent(indentLevel, s"""
    <xs:complexType name="${ xsdComplexTypeName(viewDef.name) }">
      ${List(
      annotation(viewDef.comments, 3),
      if (viewDef.extends_ == null) createFields(3)
      else indent(3, s"""
        <xs:complexContent>
          <xs:extension base="${ "tns:" + xsdComplexTypeName(viewDef.extends_) }">
            ${ createFields(6).trim }
          </xs:extension>
        </xs:complexContent>
      """)).map(_.trim).filter(_ != "")
      .mkString("\n" + indentString * 3)
      }
    </xs:complexType>
    """)
  }
  private def listWrapperXsdTypeName = xsdComplexTypeName(listWrapperBaseName)
  def listWrapperBase(indentLevel: Int = 1) =
    indent(indentLevel, s"""
    <xs:complexType name="${ listWrapperXsdTypeName }">
      <xs:sequence>
        <xs:element type="xs:int" name="Count"/>
        <xs:element type="xs:int" minOccurs="0" name="Limit"/>
        <xs:element type="xs:int" minOccurs="0" name="Offset"/>
      </xs:sequence>
    </xs:complexType>
    """)
  def listWrapper(viewDef: ViewDef, indentLevel: Int = 1) =
    indent(indentLevel, s"""
    <xs:complexType name="${ xsdComplexTypeName(listWrapperName(viewDef.name)) }">
      <xs:complexContent>
        <xs:extension base="${ "tns:" + listWrapperXsdTypeName }">
          <xs:sequence>
            <xs:element ${attribs("maxOccurs minOccurs nillable type name",
              "unbounded", "0", "true", "tns:" + xsdComplexTypeName(viewDef.name),
              xsdName(viewDef.table))}/>
          </xs:sequence>
        </xs:extension>
      </xs:complexContent>
    </xs:complexType>
    """)
  def schema(targetNamespace: String = "my.tns.com") = {
    // TODO elementFormDefault="qualified">
    (indent(0, s"""
    <xs:schema ${attribs("version targetNamespace xmlns:xs xmlns:tns",
        "1.0", targetNamespace, "http://www.w3.org/2001/XMLSchema", targetNamespace)}>
      ${
          (typedefs.map(complexType(_, 3)) ++
          (if (typedefs.exists(createListWrapper)) Seq(listWrapperBase(3)) else Nil) ++
          typedefs.filter(createListWrapper).map(listWrapper(_, 3)))
          .map(_.trim).mkString("\n" + indentString)
      }
    </xs:schema>
    """).trim)
    .replace("\r\n", "\n")
  }
  def jaxbBindings(schemaLocation: String = "my-schema.xsd") = indent(0, s"""
    <jaxb:bindings version="2.1" xmlns:jaxb="http://java.sun.com/xml/ns/jaxb"
        xmlns:xjc="http://java.sun.com/xml/ns/jaxb/xjc"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <jaxb:globalBindings generateElementProperty="false">
        <xjc:simple/>
      </jaxb:globalBindings>
      <jaxb:bindings schemaLocation="$schemaLocation" node="/xs:schema">
        ${
          val names = typedefs.map(_.name) ++
          (if (typedefs.exists(createListWrapper)) List(listWrapperBaseName) else Nil) ++
            typedefs.filter(createListWrapper).map(t => listWrapperName(t.name))
          names
            .filter(name => xsdName(name) != xsdComplexTypeName(name))
            .map { name =>
              val path = "//xs:complexType[@name='" + xsdComplexTypeName(name) + "']"
              val className = xsdName(name)
              indent(4  , s"""
              <jaxb:bindings node="${ path }">
                <jaxb:class name="${ className }"/>
              </jaxb:bindings>
              """)
            }.map(_.trim).mkString("\n" + indentString * 4)
        }
      </jaxb:bindings>
    </jaxb:bindings>
    """).trim
    .replace("\r\n", "\n") + "\n"
}

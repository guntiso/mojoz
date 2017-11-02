package mojoz.metadata.out

import mojoz.metadata._
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import scala.collection.immutable.Seq

class XsdWriter(viewDefs: Seq[ViewDef[FieldDef[Type]]],
    xsdName: String => String = identity,
    xsdTypeName: String => String = identity,
    createListWrapper: ViewDef[FieldDef[Type]] => Boolean = _.name endsWith "_list_row",
    listWrapperBaseName: String = "list_wrapper",
    listWrapperName: String => String =
      Option(_).map(_.replace("_list_row", "_list_wrapper")).orNull) {
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
  private def annotation(comment: String, level: Int) =
    if (comment != null && comment.trim.length > 0)
      indent(level, s"""
      <xs:annotation>
        <xs:documentation>${ esc(comment) }</xs:documentation>
      </xs:annotation>
      """)
    else ""
  private def createElement(elName: String, col: FieldDef[Type], level: Int = 0) = {
    val required = !col.nullable
    val maxOccurs = Option(col.maxOccurs) getOrElse {
      if (col.isCollection) "unbounded" else null
    } match {
      case "1" => null
      case maxOccurs => maxOccurs
    }
    val minOccurs = if (required) null else "0"
    val nillable = if (required || col.isCollection) null else "true"
    val typeName =
      if (col.type_.isComplexType) "tns:" + xsdTypeName(col.type_.name)
      else "xs:" + col.type_.name
    val noBlankStr = required && typeName == "xs:string" &&
      (col.type_.length getOrElse 1) > 0
    val minLength = if (noBlankStr) Some(1) else None
    val t = col.type_
    (minLength, t.length, t.totalDigits, t.fractionDigits, t.intDigits, Option(col.enum)) match {
      case (None, None, None, None, None, None) =>
        indent(level, s"""
        <xs:element ${attribs("name nillable minOccurs maxOccurs type",
            elName, nillable, minOccurs, maxOccurs, typeName)}>${
          Some(annotation(col.comments, 5))
            .filter(_ != "").map("\n" + indentString * 5 + _.trim) getOrElse ""}
        </xs:element>
        """)
      case (minL, maxL, totD, frcD, intD, enum) =>
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
              enum.getOrElse(Nil).map(op => s"""<xs:enumeration value="${ escAttr(op) }"/>""").mkString("\n"))
              .filter(_ != null)
              .mkString("\n")
              ).trim}
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
        """)
    }
  }
  def complexType(typeDef: ViewDef[FieldDef[Type]], indentLevel: Int = 1) = {
    def createFields(level: Int) = {
      // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
      // TODO when no restriction:  type="xs:string"
      indent(level, s"""
      <xs:sequence>
        ${typeDef.fields.map(f =>
          createElement(xsdName(Option(f.alias) getOrElse f.name), f, 4))
          .map(_.trim).mkString("\n" + indentString * 4)
        }
      </xs:sequence>
      """)
    }
    indent(indentLevel, s"""
    <xs:complexType name="${ xsdTypeName(typeDef.name) }">
      ${List(
      annotation(typeDef.comments, 3),
      if (typeDef.extends_ == null) createFields(3)
      else indent(3, s"""
        <xs:complexContent>
          <xs:extension base="${ "tns:" + xsdTypeName(typeDef.extends_) }">
            ${ createFields(6).trim }
          </xs:extension>
        </xs:complexContent>
      """)).map(_.trim).filter(_ != "")
      .mkString("\n" + indentString * 3)
      }
    </xs:complexType>
    """)
  }
  private def listWrapperXsdTypeName = xsdTypeName(listWrapperBaseName)
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
  def listWrapper(typeDef: ViewDef[FieldDef[Type]], indentLevel: Int = 1) =
    indent(indentLevel, s"""
    <xs:complexType name="${ xsdTypeName(listWrapperName(typeDef.name)) }">
      <xs:complexContent>
        <xs:extension base="${ "tns:" + listWrapperXsdTypeName }">
          <xs:sequence>
            <xs:element ${attribs("maxOccurs minOccurs nillable type name",
              "unbounded", "0", "true", "tns:" + xsdTypeName(typeDef.name),
              xsdName(typeDef.table))}/>
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
            .filter(name => xsdName(name) != xsdTypeName(name))
            .map { name =>
              val path = "//xs:complexType[@name='" + xsdTypeName(name) + "']"
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

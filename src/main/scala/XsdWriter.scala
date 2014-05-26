package mojoz.metadata.out

import mojoz.metadata._
import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }

class XsdWriter(metadata: Metadata[Type]) {
  // FIXME ensure valid xml - escape special chars!
  private val typedefs = metadata.viewDefs
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
  private def attribs(a: String, v: String*) =
    a.split("\\s+").toList.zip(v)
      .filter(_._2 != null)
      .map(e => s"""${e._1}="${e._2}"""")
      .mkString(" ")
  private def annotation(comment: String, level: Int) =
    if (comment != null && comment.trim.length > 0)
      indent(level, s"""
      <xs:annotation>
        <xs:documentation>${ comment }</xs:documentation>
      </xs:annotation>
      """)
    else ""
  private def xsdTypeName(name: String) = xsdName(name) + "Type"
  private def createElement(elName: String, col: FieldDef[Type], level: Int = 0) = {
    val required = !col.nullable
    val maxOccurs = Option(col.maxOccurs) getOrElse {
      if (col.isCollection) "unbounded" else null
    }
    // FIXME for refed values, depends on ref-chain nullable!
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
            elName, nillable, minOccurs, maxOccurs, typeName)}>
          ${ annotation(col.comments, 5).trim }
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
              enum.getOrElse(Nil).map(op => s"""<xs:enumeration value="{ op }"/>""").mkString("\n"))
              .filter(_ != null)
              .mkString("\n")
              ).trim}
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
        """)
    }
  }
  def createComplexType(typeDef: ViewDef[Type], level: Int = 1) = {
    val tableComment = metadata.tableDefOption(typeDef).map(_.comments)
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
    indent(level, s"""
    <xs:complexType name="${ xsdTypeName(typeDef.name) }">
      ${List(
      annotation(Option(typeDef.comments).orElse(tableComment).orNull, 3),
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
  val listWrapperXsdTypeName = xsdTypeName("list_wrapper")
  def listWrapper(level: Int = 1) = // XXX
    indent(level, s"""
    <xs:complexType name="${ listWrapperXsdTypeName }">
      <xs:sequence>
        <xs:element type="xs:int" name="Count"/>
        <xs:element type="xs:int" minOccurs="0" name="Limit"/>
        <xs:element type="xs:int" minOccurs="0" name="Offset"/>
      </xs:sequence>
    </xs:complexType>
    """)
  def listWrapperName(typeDef: ViewDef[Type]) =
    typeDef.name.replace("_list_row", "_list_wrapper")
  def createListWrapper(typeDef: ViewDef[Type], level: Int = 1) = // XXX
    indent(level, s"""
    <xs:complexType name="${ xsdTypeName(listWrapperName(typeDef)) }">
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
  def createSchema = {
    // FIXME namespaces etc.
    // TODO elementFormDefault="qualified">
    (indent(0, s"""
    <xs:schema ${attribs("version targetNamespace xmlns:xs xmlns:tns",
        "1.0", "kps.ldz.lv", "http://www.w3.org/2001/XMLSchema", "kps.ldz.lv")}>
      ${
          (typedefs.map(createComplexType(_, 3)) ++ Seq(listWrapper(3)) ++
          typedefs.filter(_.name.endsWith("_list_row")).map(createListWrapper(_, 3)))
          .map(_.trim).mkString("\n" + indentString)
      }
    </xs:schema>
    """).trim)
    .replace("\r\n", "\n")
  }
  def createSchemaString = createSchema
  def createBindings = indent(0, s"""
    <jaxb:bindings version="2.1" xmlns:jaxb="http://java.sun.com/xml/ns/jaxb"
        xmlns:xjc="http://java.sun.com/xml/ns/jaxb/xjc"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <jaxb:globalBindings generateElementProperty="false">
        <xjc:simple/>
      </jaxb:globalBindings>
      <jaxb:bindings schemaLocation="kpsws.xsd" node="/xs:schema">
        ${
          val names = typedefs.map(_.name) ++ List("list_wrapper") ++
            typedefs.filter(_.name.endsWith("_list_row")).map(listWrapperName)
          names
            .filter(name => xsdName(name) != xsdTypeName(name))
            .map { name =>
              val path = "//xs:complexType[@name='" + xsdTypeName(name) + "']"
              val className = xsdName(name)
              s"""
              <jaxb:bindings node=${ path }>
                <jaxb:class name=${ className }/>
              </jaxb:bindings>
              """
            }
        }
      </jaxb:bindings>
    </jaxb:bindings>
    """)
  def createBindingsString = createBindings
}

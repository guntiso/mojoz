package mojoz.metadata.out

import scala.xml.PrettyPrinter

import mojoz.metadata._
import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }

class XsdWriter(metadata: Metadata[XsdType]) {
  private val typedefs = metadata.viewDefs
  private def annotation(comment: String) =
    if (comment != null && comment.trim.length > 0)
      <xs:annotation>
        <xs:documentation>{ comment }</xs:documentation>
      </xs:annotation>
  private def xsdTypeName(name: String) = xsdName(name) + "Type"
  private def createElement(elName: String, col: FieldDef[XsdType]) = {
    val colcomment = annotation(col.comment)
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
        <xs:element name={ elName } nillable={ nillable }
            minOccurs={ minOccurs } maxOccurs={ maxOccurs } type={ typeName }>{
          colcomment
        }</xs:element>
      case (minL, maxL, totD, frcD, intD, enum) =>
        <xs:element name={ elName } nillable={ nillable }
            minOccurs={ minOccurs } maxOccurs={ maxOccurs }>
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ typeName }>
              { minL.map(n => <xs:minLength value={ n.toString }/>).orNull }
              { if (noBlankStr) <xs:pattern value="[\s\S]*[\S][\s\S]*"/> }
              { maxL.map(n => <xs:maxLength value={ n.toString }/>).orNull }
              { totD.map(n => <xs:totalDigits value={ n.toString }/>).orNull }
              { intD.map(n => <xs:maxExclusive value={("1" :: List.fill(n)("0")).mkString} />).orNull }
              { frcD.map(n => <xs:fractionDigits value={ n.toString }/>).orNull }
              { enum.getOrElse(Nil).map(op => <xs:enumeration value={ op }/>) }
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
    }
  }
  def createComplexType(typeDef: ViewDef[XsdType]) = {
    val tableComment = metadata.tableDefOption(typeDef).map(_.comment)
    def createFields = {
      // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
      // TODO when no restriction:  type="xs:string"
      <xs:sequence>{
        typeDef.fields.map(f =>
          createElement(xsdName(Option(f.alias) getOrElse f.name), f))
      }</xs:sequence>
    }
    <xs:complexType name={ xsdTypeName(typeDef.name) }>{
      annotation(Option(typeDef.comment).orElse(tableComment).orNull)
    }{
      if (typeDef.extends_ == null) createFields
      else <xs:complexContent>
             <xs:extension base={ "tns:" + xsdTypeName(typeDef.extends_) }>
               { createFields }
             </xs:extension>
           </xs:complexContent>
    }</xs:complexType>
  }
  val listWrapperXsdTypeName = xsdTypeName("list_wrapper")
  val listWrapper = // XXX
    <xs:complexType name={ listWrapperXsdTypeName }>
      <xs:sequence>
        <xs:element type="xs:int" name="Count"/>
        <xs:element type="xs:int" minOccurs="0" name="Limit"/>
        <xs:element type="xs:int" minOccurs="0" name="Offset"/>
      </xs:sequence>
    </xs:complexType>
  def listWrapperName(typeDef: ViewDef[XsdType]) =
    typeDef.name.replace("_list_row", "_list_wrapper")
  def createListWrapper(typeDef: ViewDef[XsdType]) = // XXX
    <xs:complexType name={ xsdTypeName(listWrapperName(typeDef)) }>
      <xs:complexContent>
        <xs:extension base={ "tns:" + listWrapperXsdTypeName }>
          <xs:sequence>
            <xs:element maxOccurs="unbounded" minOccurs="0" nillable="true"
                type={ "tns:" + xsdTypeName(typeDef.name) }
                name={ xsdName(typeDef.table) }/>
          </xs:sequence>
        </xs:extension>
      </xs:complexContent>
    </xs:complexType>
  def createSchema = {
    // FIXME namespaces etc.
    // TODO elementFormDefault="qualified">
    <xs:schema xmlns:tns="kps.ldz.lv" xmlns:xs="http://www.w3.org/2001/XMLSchema"
      version="1.0" targetNamespace="kps.ldz.lv">
      {
        typedefs.map(createComplexType) ++ listWrapper ++
          typedefs.filter(_.name.endsWith("_list_row")).map(createListWrapper)
      }
    </xs:schema>
  }
  def createSchemaString = new PrettyPrinter(200, 2).format(createSchema)
    .replace("\"   >", "\">") // XXX remove strange artifact from schema
  def createBindings =
    <jaxb:bindings version="2.1" xmlns:jaxb="http://java.sun.com/xml/ns/jaxb"
        xmlns:xjc="http://java.sun.com/xml/ns/jaxb/xjc"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <jaxb:globalBindings generateElementProperty="false">
        <xjc:simple/>
      </jaxb:globalBindings>
      <jaxb:bindings schemaLocation="kpsws.xsd" node="/xs:schema">
        {
          val names = typedefs.map(_.name) ++ List("list_wrapper") ++
            typedefs.filter(_.name.endsWith("_list_row")).map(listWrapperName)
          names
            .filter(name => xsdName(name) != xsdTypeName(name))
            .map { name =>
              val path = "//xs:complexType[@name='" + xsdTypeName(name) + "']"
              val className = xsdName(name)
              <jaxb:bindings node={ path }>
                <jaxb:class name={ className }/>
              </jaxb:bindings>
            }
        }
      </jaxb:bindings>
    </jaxb:bindings>
  def createBindingsString = new PrettyPrinter(200, 2).format(createBindings)
}

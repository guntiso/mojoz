package metadata

import scala.xml.PrettyPrinter

import YamlViewDefLoader.typedefs
import metadata.DbConventions.{ dbNameToXsdName => xsdName }

object XsdWriter {
  private def annotation(comment: String) =
    if (comment != null && comment.trim.length > 0)
      <xs:annotation>
        <xs:documentation>{ comment }</xs:documentation>
      </xs:annotation>
  private def createElement(elName: String, col: XsdFieldDef) = {
    val colcomment = annotation(col.comment)
    val required = !col.nullable
    val maxOccurs = if (col.isCollection) "unbounded" else null
    // FIXME for refed values, depends on ref-chain nullable!
    val minOccurs = if (required) null else "0"
    val nillable = if (required) null else "true"
    val typeName =
      if (col.xsdType.isComplexType) "tns:" + xsdName(col.xsdType.name)
      else "xs:" + col.xsdType.name
    col.xsdType match {
      case XsdType(_, None, None, None, _) =>
        <xs:element name={ elName } type={ typeName } nillable={ nillable }
            minOccurs={ minOccurs } maxOccurs={ maxOccurs }>{
          colcomment
        }</xs:element>
      case XsdType(_, Some(length), None, None, _) =>
        <xs:element name={ elName } nillable={ nillable }
            minOccurs={ minOccurs } maxOccurs={ maxOccurs }>
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ typeName }>
              <xs:maxLength value={ length.toString }/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      case XsdType(_, _, Some(totalDigits), fractionDigitsOption, _) =>
        <xs:element name={ elName } nillable={ nillable }
            minOccurs={ minOccurs } maxOccurs={ maxOccurs }>
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ typeName }>
              <xs:totalDigits value={ totalDigits.toString }/>
              {
                if (fractionDigitsOption != None)
                  <xs:fractionDigits value={ fractionDigitsOption.get.toString }/>
              }
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
    }
  }
  def createComplexType(typeDef: XsdTypeDef) = {
    val tableMd = Metadata.tableDef(typeDef)
    def createFields = {
      // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
      // TODO when no restriction:  type="xs:string"
      <xs:sequence>{
        typeDef.fields.map(f =>
          createElement(xsdName(Option(f.alias) getOrElse f.name), f))
      }</xs:sequence>
    }
    <xs:complexType name={ xsdName(typeDef.name) }>{
      annotation(Option(typeDef.comment) getOrElse tableMd.comment)
    }{
      if (typeDef.xtnds == null) createFields
      else <xs:complexContent>
             <xs:extension base={ "tns:" + xsdName(typeDef.xtnds) }>
               { createFields }
             </xs:extension>
           </xs:complexContent>
    }</xs:complexType>
  }
  val listWrapper = // XXX
    <xs:complexType name="ListWrapper">
      <xs:sequence>
        <xs:element type="xs:int" name="Count"/>
        <xs:element type="xs:int" minOccurs="0" name="Limit"/>
        <xs:element type="xs:int" minOccurs="0" name="Offset"/>
      </xs:sequence>
    </xs:complexType>
  def createListWrapper(typeDef: XsdTypeDef) = // XXX
    <xs:complexType name={ xsdName(typeDef.name.replace("_list_row", "_list_wrapper")) }>
      <xs:complexContent>
        <xs:extension base="tns:ListWrapper">
          <xs:sequence>
            <xs:element maxOccurs="unbounded" minOccurs="0" nillable="true"
                type={ "tns:" + xsdName(typeDef.name) }
                name={ xsdName(typeDef.table) }/>
          </xs:sequence>
        </xs:extension>
      </xs:complexContent>
    </xs:complexType>
  def createSchema = {
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
  def createBindings =
    <jaxb:bindings version="2.1" xmlns:jaxb="http://java.sun.com/xml/ns/jaxb"
        xmlns:xjc="http://java.sun.com/xml/ns/jaxb/xjc"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <jaxb:globalBindings generateElementProperty="false">
        <xjc:simple/>
      </jaxb:globalBindings>
    </jaxb:bindings>
  def createBindingsString = new PrettyPrinter(200, 2).format(createBindings)
}

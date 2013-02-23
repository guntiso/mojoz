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
  private def createElement(elName: String, col: ColumnDef) = {
    val colcomment = annotation(col.comment)
    val required = (col.nullable, col.name) match {
      // FIXME do not handle ids here, add ? in views instead!
      case (_, "id") => false // XXX for inserts id is not required
      case (nullable, _) => !nullable
    }
    val maxOccurs = if (col.isCollection) "unbounded" else null
    // FIXME for refed values, depends on ref-chain nullable!
    val minOccurs = if (required) null else "0"
    col.xsdType match {
      case XsdType(typeName, None, None, None) =>
        <xs:element name={ elName } type={ "xs:" + typeName } minOccurs={ minOccurs } maxOccurs={ maxOccurs }>{
          colcomment
        }</xs:element>
      case XsdType(typeName, Some(length), None, None) =>
        <xs:element name={ elName } minOccurs={ minOccurs } maxOccurs={ maxOccurs }>
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ "xs:" + typeName }>
              <xs:length value={ length.toString }/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      case XsdType(typeName, _, Some(totalDigits), fractionDigitsOption) =>
        <xs:element name={ elName } minOccurs={ minOccurs } maxOccurs={ maxOccurs }>
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ "xs:" + typeName }>
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
          createElement(xsdName(Option(f.alias) getOrElse f.name),
            Metadata.getCol(typeDef, f)))
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
            <xs:element maxOccurs="unbounded" minOccurs="0" nillable="true" type={ "tns:" + xsdName(typeDef.name) } name={ xsdName(typeDef.table) }/>
          </xs:sequence>
        </xs:extension>
      </xs:complexContent>
    </xs:complexType>
  def createSchema = {
    <xs:schema xmlns:tns="kps.ldz.lv" xmlns:xs="http://www.w3.org/2001/XMLSchema" version="1.0" targetNamespace="kps.ldz.lv">
      {
        typedefs.map(createComplexType) ++ listWrapper ++
          typedefs.filter(_.name.endsWith("_list_row")).map(createListWrapper)
      }
    </xs:schema>
  }
  def createSchemaString = new PrettyPrinter(200, 2).format(createSchema)
}

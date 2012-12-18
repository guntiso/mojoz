package xsdgen

import java.io.InputStream
import java.util.ArrayList

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.asScalaBuffer
import scala.reflect.BeanProperty
import scala.xml.PrettyPrinter

import org.yaml.snakeyaml.Yaml

case class XsdTypeDef(
  name: String,
  fields: Seq[XsdTypeDef])

object XsdGen {
  def resource = XsdGen.getClass.getResourceAsStream("/views/company.yaml")
  def loadTypeDef(io: InputStream) = {
    case class YamlXsdTypeDef(
      @BeanProperty var name: String,
      @BeanProperty var fields: ArrayList[YamlXsdTypeDef]) {
      def this() = this(null, null)
      def this(name: String) = this(name, null)
    }
    def xsdTypeDef(yamlTypeDef: YamlXsdTypeDef): XsdTypeDef = yamlTypeDef match {
      case YamlXsdTypeDef(name, null) => XsdTypeDef(name, null)
      case YamlXsdTypeDef(name, fields) =>
        XsdTypeDef(name, fields.map(f => xsdTypeDef(f)).toList)
    }
    xsdTypeDef((new Yaml).loadAs(io, classOf[YamlXsdTypeDef]))
  }
  def xsdName(name: String) = dbNameToXsdName(name)
  def dbNameToXsdName(dbName: String) = {
    // TODO special cases
    dbName.split("_") map (_.capitalize) mkString
  }
  def xsdNameToDbName(dbName: String) = {
    // TODO special cases
    ElementName.get(dbName).replace('-', '_')
  }
  def createSchema = {
    // FIXME BOOLEAN
    val typeDef = loadTypeDef(resource)
    val md = EntityMetadata.getMetadata
    val tableMd = md.table(typeDef.name)
    def annotation(comments: String) = {
      if (comments != null)
        <xs:annotation>
          <xs:documentation>{ comments }</xs:documentation>
        </xs:annotation>
    }
    <xs:schema xmlns:tns="kps.ldz.lv" xmlns:xs="http://www.w3.org/2001/XMLSchema" version="1.0" targetNamespace="kps.ldz.lv">
      <xs:complexType name={ xsdName(typeDef.name) }>
        { annotation(tableMd.comments) }
        <xs:sequence>
          { // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
            // TODO when no restriction:  type="xs:string"
            val cols = tableMd.cols
            typeDef.fields.map(f => {
              val dbFieldName = xsdNameToDbName(f.name)
              val col = cols(dbFieldName)
              val xsdType =
                JdbcToXsdTypeMapper.map(col.sqlType, col.size, col.decimalDigits)
              val colComments = {
                annotation(
                  if (col.comments != null) col.comments
                  else "Sorry, comments missing")
              }
              {
                (xsdType.xsdTypeName, xsdType.length,
                  xsdType.totalDigits, xsdType.fractionDigits) match {
                    case (typeName, null, null, null) =>
                      <xs:element name={ xsdName(f.name) } type={ "xs:" + typeName }>{
                        colComments
                      }</xs:element>
                    case (typeName, len, null, null) =>
                      <xs:element name={ xsdName(f.name) }>
                        { colComments }
                        <xs:simpleType>
                          <xs:restriction base={ "xs:" + typeName }>
                            <xs:length value={ len.toString }/>
                          </xs:restriction>
                        </xs:simpleType>
                      </xs:element>
                    case (typeName, length, totalDigits, fractionDigits) =>
                      <xs:element name={ xsdName(f.name) }>
                        { colComments }
                        <xs:simpleType>
                          <xs:restriction base={ "xs:" + typeName }>
                            <xs:totalDigits value={ totalDigits.toString }/>
                            <xs:fractionDigits value={ fractionDigits.toString }/>
                          </xs:restriction>
                        </xs:simpleType>
                      </xs:element>
                  }
              }
            })
          }
        </xs:sequence>
      </xs:complexType>
    </xs:schema>
  }
  def createSchemaString = new PrettyPrinter(200, 2).format(createSchema)
}

package xsdgen

import java.io.InputStream
import java.util.ArrayList
import scala.Array.canBuildFrom
import scala.collection.JavaConversions.asScalaBuffer
import scala.reflect.BeanProperty
import scala.xml.PrettyPrinter
import org.yaml.snakeyaml.Yaml
import java.io.File
import scala.io.Source
import scala.collection.JavaConverters._

case class XsdTypeDef(
  name: String,
  table: String,
  xtnds: String,
  comment: String,
  fields: Seq[XsdTypeDef])

object XsdGen {
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def typedefFiles = recursiveListFiles(new File("../views")).toSeq
  val typedefResources = classOf[XsdTypeDef].getClassLoader.getResources("")
    .asScala.toSeq.flatMap(u =>
      Source.fromURL(u, "UTF-8").mkString.trim.split("\\s+"))
    .filter(_.endsWith(".yaml")).map("/" + _)
  def filesToStrings =
    typedefFiles.map(f => Source.fromFile(f).mkString)
  def resourcesToStrings = typedefResources.map(r =>
    Source.fromInputStream(getClass.getResourceAsStream(r)).mkString)
  val typedefStrings = resourcesToStrings.map(s =>
    s.split("\\-\\-\\-").toSeq).flatMap(x =>
    x).map(_.trim).filter(_.length > 0) toSeq
  val typedefs = loadTypeDefs
  def loadTypeDef(typeDef: String) = {
    case class YamlXsdTypeDef(
      @BeanProperty var name: String,
      @BeanProperty var table: String,
      @BeanProperty var `extends`: String,
      @BeanProperty var comment: String,
      @BeanProperty var fields: ArrayList[YamlXsdTypeDef]) {
      def this() = this(null, null, null, null, null)
      def this(name: String) = this(name, null, null, null, null)
    }
    def mapF(fields: ArrayList[YamlXsdTypeDef]) =
      if (fields == null) null else fields.map(f => xsdTypeDef(f)).toList
    def xsdTypeDef(yamlTypeDef: YamlXsdTypeDef): XsdTypeDef = yamlTypeDef match {
      case YamlXsdTypeDef(name, null, null, c, f) =>
        XsdTypeDef(name, name, null, c, mapF(f))
      case YamlXsdTypeDef(name, null, x, c, f) =>
        XsdTypeDef(name, null, x, c, mapF(f))
      case YamlXsdTypeDef(null, table, null, c, f) =>
        XsdTypeDef(table, table, null, c, mapF(f))
      case YamlXsdTypeDef(name, table, x, c, f) =>
        XsdTypeDef(name, table, x, c, mapF(f))
    }
    xsdTypeDef((new Yaml).loadAs(typeDef, classOf[YamlXsdTypeDef]))
  }
  private def checkTypedefs = {
    // TODO diagnostics: m(t.xtnds) and all other!!!
  }
  private def loadTypeDefs = {
    val td = typedefStrings map loadTypeDef
    checkTypedefs
    val m = td.map(t => (t.name, t)).toMap
    // FIXME support recursive extends!
    // TODO extends is also typedef, join!
    td.map(t =>
      if (t.table != null) t
      else XsdTypeDef(t.name, m(t.xtnds).table, t.xtnds, t.comment, t.fields))
  }
  private def xsdName(name: String) = Schema.dbNameToXsdName(name)
  private def xsdNameToDbName(xsdName: String) = Schema.xsdNameToDbName(xsdName)
  private def annotation(comment: String) =
    if (comment != null && comment.trim.length > 0)
      <xs:annotation>
        <xs:documentation>{ comment }</xs:documentation>
      </xs:annotation>
  private def createElement(elName: String, col: XsdCol) = {
    val colcomment = annotation(col.comment)
    col.xsdType match {
      case XsdType(typeName, None, None, None) =>
        <xs:element name={ elName } type={ "xs:" + typeName } minOccurs="0">{
          colcomment
        }</xs:element>
      case XsdType(typeName, Some(length), None, None) =>
        <xs:element name={ elName } minOccurs="0">
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ "xs:" + typeName }>
              <xs:length value={ length.toString }/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      case XsdType(typeName, _, Some(totalDigits), fractionDigitsOption) =>
        <xs:element name={ elName } minOccurs="0">
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
  private val md = Schema.entities.map(e => (e.name, e)).toMap
  // typedef name to typedef
  private val td = typedefs.map(t => (t.name, t)).toMap
  // typedef name to typedef with extended field list
  val xtd = typedefs.map(t => // TODO rename, move
    // FIXME support recursive extends!
    if (t.xtnds == null) t
    else XsdTypeDef(t.name, t.table, t.xtnds, t.comment,
      td(t.xtnds).fields ++ t.fields)).map(t => (t.name, t)).toMap
  def createComplexType(typeDef: XsdTypeDef) = {
    val tableMd =
      if (typeDef.xtnds == null) md(typeDef.table)
      else md(td(typeDef.xtnds).table)
    val cols = tableMd.cols.map(c => (c.name, c)).toMap
    def getCol(colName: String) = try {
      colName.split("\\.").toList.reverse match {
        case name :: table :: path =>
          val cols = md(table).cols.map(c => (c.name, c)).toMap // TODO optimize
          cols(name)
        case name :: Nil => cols(name)
        case Nil => throw new RuntimeException("Impossible!")
      }
    } catch {
      case ex: Exception =>
        // TODO print filename, lineNr, colNr, too!
        throw new RuntimeException(
          "Problem finding column (typeDef: " + typeDef.name
            + ", column: " + colName + ")", ex)
    }
    def createFields = {
      // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
      // TODO when no restriction:  type="xs:string"
      <xs:sequence>{
        typeDef.fields.map(f => {
          val dbFieldName = xsdNameToDbName(f.name) // TODO by db name?
          val col = getCol(dbFieldName)
          createElement(xsdName(f.name), col)
        })
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
  def createSchema = {
    <xs:schema xmlns:tns="kps.ldz.lv" xmlns:xs="http://www.w3.org/2001/XMLSchema" version="1.0" targetNamespace="kps.ldz.lv">
      { typedefs map createComplexType }
    </xs:schema>
  }
  def createSchemaString = new PrettyPrinter(200, 2).format(createSchema)
}

package xsdgen

import java.io.File
import java.util.ArrayList

import scala.Array.canBuildFrom
import scala.annotation.tailrec
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
import scala.io.Source
import scala.reflect.BeanProperty
import scala.xml.PrettyPrinter

import org.yaml.snakeyaml.Yaml

import Schema.{ dbNameToXsdName => xsdName }
import Schema.{ xsdNameToDbName => dbName }

case class XsdFieldDef(
  table: String,
  tableAlias: String,
  name: String,
  alias: String,
  comment: String)

case class XsdTypeDef(
  name: String,
  table: String,
  joins: String, // tresql from clause
  xtnds: String,
  comment: String,
  fields: Seq[XsdFieldDef])

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
      @BeanProperty var joins: String,
      @BeanProperty var `extends`: String,
      @BeanProperty var comment: String,
      @BeanProperty var fields: ArrayList[YamlXsdTypeDef]) {
      def this() = this(null, null, null, null, null, null)
      def this(name: String) = this(name, null, null, null, null, null)
    }
    def mapF(fields: ArrayList[YamlXsdTypeDef]) =
      if (fields == null) null else fields.map(xsdTypeDef).map(f =>
        XsdFieldDef(f.table, null, f.name, null, f.comment)).toList
    // TODO defer this analysis to later phase, it will clean up the code! 
    def xsdTypeDef(yamlTypeDef: YamlXsdTypeDef): XsdTypeDef = yamlTypeDef match {
      case YamlXsdTypeDef(name, null, j, null, c, f) =>
        XsdTypeDef(name, dbName(name), j, null, c, mapF(f))
      case YamlXsdTypeDef(name, null, j, x, c, f) =>
        XsdTypeDef(name, null, j, x, c, mapF(f))
      case YamlXsdTypeDef(null, table, j, null, c, f) =>
        XsdTypeDef(table, dbName(table), j, null, c, mapF(f))
      case YamlXsdTypeDef(name, table, j, x, c, f) =>
        XsdTypeDef(name, dbName(table), j, x, c, mapF(f))
    }
    xsdTypeDef((new Yaml).loadAs(typeDef, classOf[YamlXsdTypeDef]))
  }
  private def checkTypedefs(td: Seq[XsdTypeDef]) = {
    // TODO diagnostics: m(t.xtnds) and all other!!!
    // check names not repeating
    // check extends only existing
    // check field names not repeating
    // check field names not repeating on extends? or overwrite instead?
  }
  private def loadTypeDefs = {
    val td = typedefStrings map loadTypeDef
    checkTypedefs(td)
    val m = td.map(t => (t.name, t)).toMap
    // TODO extends is also typedef, join!
    def mapExtends(t: XsdTypeDef) =
      if (t.table != null) t else {
        @tailrec
        def baseTable(t: XsdTypeDef, visited: List[String]): String =
          if (visited contains t.name)
            sys.error("Cyclic extends: " +
              (t.name :: visited).reverse.mkString(" -> "))
          else if (t.table != null) t.table
          else baseTable(m(t.xtnds), t.name :: visited)
        t.copy(table = baseTable(t, Nil))
      }
    def mapFields(t: XsdTypeDef) = {
      val aliasToTable = JoinsParser.aliases(t.joins)
      def mapField(f: XsdFieldDef) =
        if (f.name.indexOf(".") < 0)
          XsdFieldDef(dbName(t.table), null, dbName(f.name), null, f.comment)
        else {
          val parts = f.name.split("\\.")
          val tableOrAlias = dbName(parts(0))
          val table = dbName(aliasToTable.getOrElse(tableOrAlias, tableOrAlias))
          val tableAlias = if (table == tableOrAlias) null else tableOrAlias
          val name = dbName(parts(1))
          val alias = dbName(f.name.replace(".", "_"))
          XsdFieldDef(table, tableAlias, name, alias, f.comment)
        }
      t.copy(fields = t.fields.map(mapField))
    }
    td.map(mapExtends).map(mapFields)
  }
  private def annotation(comment: String) =
    if (comment != null && comment.trim.length > 0)
      <xs:annotation>
        <xs:documentation>{ comment }</xs:documentation>
      </xs:annotation>
  private def createElement(elName: String, col: XsdCol) = {
    val colcomment = annotation(col.comment)
    val required = (col.nullable, col.name) match {
      // FIXME do not handle ids here, add ? in views instead!
      case (_, "id") => false // XXX for inserts id is not required
      case (nullable, _) => !nullable
    }
    // FIXME for refed values, depends on ref-chain nullable!
    val minOccurs = if (required) null else "0"
    col.xsdType match {
      case XsdType(typeName, None, None, None) =>
        <xs:element name={ elName } type={ "xs:" + typeName } minOccurs={ minOccurs }>{
          colcomment
        }</xs:element>
      case XsdType(typeName, Some(length), None, None) =>
        <xs:element name={ elName } minOccurs={ minOccurs }>
          { colcomment }
          <xs:simpleType>
            <xs:restriction base={ "xs:" + typeName }>
              <xs:length value={ length.toString }/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      case XsdType(typeName, _, Some(totalDigits), fractionDigitsOption) =>
        <xs:element name={ elName } minOccurs={ minOccurs }>
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
  // typedef name to typedef
  val td = typedefs.map(t => (t.name, t)).toMap // TODO rename, move
  // typedef name to typedef with extended field list
  val xtd = typedefs.map(t => // TODO rename, move
    if (t.xtnds == null) t else {
      @tailrec
      def baseFields(t: XsdTypeDef, visited: List[String]): Seq[XsdFieldDef] =
        if (visited contains t.name)
          sys.error("Cyclic extends: " +
            (t.name :: visited).reverse.mkString(" -> "))
        else if (t.xtnds == null) t.fields
        else baseFields(td(t.xtnds), t.name :: visited) ++ t.fields
      t.copy(fields = baseFields(t, Nil))
    }).map(t => (t.name, t)).toMap
  def createComplexType(typeDef: XsdTypeDef) = {
    val tableMd = Schema.tableDef(typeDef)
    def createFields = {
      // TODO nillable="true" minOccurs="0" maxOccurs="unbounded">
      // TODO when no restriction:  type="xs:string"
      <xs:sequence>{
        typeDef.fields.map(f =>
          createElement(xsdName(Option(f.alias) getOrElse f.name), Schema.getCol(typeDef, f)))
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

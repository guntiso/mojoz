import scala.io.Source
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.Seq
import org.mojoz.metadata._
import org.mojoz.metadata.in._
import org.mojoz.metadata.io._
import org.mojoz.metadata.out._
import java.io.PrintWriter

class ViewDefTests extends FlatSpec with Matchers {
  object Naming {
    def camelize(name: String) =
      name.split("[_\\-\\.]+").toList
        .map(_.toLowerCase.capitalize)
        .mkString + (if (name endsWith "_") "_" else "")
    def camelizeLower(name: String) = camelize(name) match {
      case x if x.length == 1 || (x.length > 1 && (x(1).isLower || x(1).isDigit)) =>
        s"${x(0).toLower}${x.substring(1)}"
      case x => x
    }
  }
  trait CamelizedNames extends ScalaGenerator {
    override def scalaClassName(name: String) = Naming.camelize(name)
    override def scalaFieldName(name: String) = Naming.camelizeLower(name)
  }
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs)
  val viewDefs = YamlViewDefLoader(tableMd, mdDefs).plainViewDefs
  val nameToViewDef = viewDefs.map(v => v.name -> v).toMap
  val xsdWriter = new XsdGenerator(
    viewDefs,
    Naming.camelize _,
    Naming.camelize(_) + "Type")
  val nl = System.getProperty("line.separator")
  "known keys view" should "have no extras" in {
    viewDefs.find(_.name == "with_many_known_keys").head.extras should be(Map.empty)
  }
  "field nullability" should "be set correctly" in {
    def checkNullability(viewName: String, fieldName: String, expectedNullable: Boolean) = {
      def infoString(viewName: String, fieldName: String, nullable: Boolean): String = {
        val prefix = if (nullable) "1" else "0" // to not trim viewName.fieldName in error message
        s"$prefix: $viewName.$fieldName.nullable = $nullable"
      }
      val actualNullable =
        nameToViewDef(viewName).field(fieldName).nullable
      val actual   = infoString(viewName, fieldName, actualNullable)
      val expected = infoString(viewName, fieldName, expectedNullable)
      actual shouldBe expected
    }
    checkNullability("person",  "name",                           false)
    checkNullability("person",  "surname",                        true)
    checkNullability("person",  "mother_name",                    true)
    checkNullability("person",  "maternal_grandmother_name",      true)
    checkNullability("resolver_test_2",  "account",               false)
    checkNullability("resolver_test_2",  "account_bank_code",     false)
    checkNullability("resolver_test_2",  "account_bank_name_eng", true)
    checkNullability("resolver_test_4",  "name",                  false)
    checkNullability("bank_list_row",    "country_name",          true)
    checkNullability("resolver_test_7",  "country",               true)
    checkNullability("resolver_test_7b", "country",               true)
  }
  "generated yaml file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "views-out.yaml")
    val ioViews = viewDefs.map(MdConventions.toExternal(_, tableMd, nameToViewDef))
    val produced = (new YamlViewDefWriter).toYaml(ioViews)
    if (expected != produced)
      toFile(path + "/" + "views-out-produced.yaml", produced)
    expected should be(produced)
  }
  "generated yaml roundtrip file" should "equal sample file" in {
    val mdDefsFromOut = YamlMd.fromFiles(
      path = path, filter = _.getName == "views-out.yaml")
    val viewDefsFromOut = YamlViewDefLoader(tableMd, mdDefsFromOut).plainViewDefs
    val expected = fileToString(path + "/" + "views-out.yaml")
    val ioViews = viewDefsFromOut.map(MdConventions.toExternal(_, tableMd, nameToViewDef))
    val produced = (new YamlViewDefWriter).toYaml(ioViews)
    if (expected != produced)
      toFile(path + "/" + "views-out-roundtrip-produced.yaml", produced)
    expected should be(produced)
    viewDefs.zip(viewDefsFromOut).filter{ case (a, b) => a != b }.map(_._1.name) should be(List("resolver_test_7")) // TODO Nil
  }
  "generated xsd file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "xsd-out.xsd")
    val produced = xsdWriter.schema("kps.ldz.lv")
    if (expected != produced)
      toFile(path + "/" + "xsd-out-produced.xsd", produced)
    expected should be(produced)
  }
  "generated bindings file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "xsd-bindings-out.xjb")
    val produced = xsdWriter.jaxbBindings("my-ws-schema.xsd")
    if (expected != produced)
      toFile(path + "/" + "xsd-bindings-out-produced.xjb", produced)
    expected should be(produced)
  }
  // TODO visualizer tests
  "generated scala class file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "classes-out.scala")
    object ScalaBuilder extends ScalaGenerator with CamelizedNames {
      override def scalaClassTraits(viewDef: ViewDef) =
        if (viewDef.fields.exists(f => f.name == "id" && f.type_.name == "long"))
          List("DtoWithId")
        else List("Dto")
    }
    val produced = ScalaBuilder.generateScalaSource(
      List("package some.pack", ""), viewDefs, Seq("// end"))
      .replace(nl, "\n") // normalize newlines here? TODO
    if (expected != produced)
      toFile(path + "/" + "classes-out-produced.scala", produced)
    expected should be(produced)
  }
  "generated scala case class file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "case-classes-out.scala")
    object ScalaBuilder extends ScalaCaseClassGenerator with CamelizedNames
    val produced = ScalaBuilder.generateScalaSource(
      List("package some.caseclass.pack", ""), viewDefs, Seq("// end"))
      .replace(nl, "\n") // normalize newlines here? TODO
    if (expected != produced)
      toFile(path + "/" + "case-classes-out-produced.scala", produced)
    expected should be(produced)
  }
  def fileToString(filename: String) = {
    val source = Source.fromFile(filename)
    val body = source.mkString
    source.close()
    body.replace(nl, "\n") // normalize newlines
  }
  def toFile(filename: String, message: String): Unit = {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}

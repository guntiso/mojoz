import scala.io.Source
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.immutable.Seq
import mojoz.metadata._
import mojoz.metadata.in._
import mojoz.metadata.io._
import mojoz.metadata.out._
import java.io.PrintWriter
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }

class ViewDefTests extends FlatSpec with Matchers {
  object Naming {
    def camelize(name: String) = {
      val parts = name.split("[_\\-\\.]+")
      parts.toList
        .map(_.toLowerCase)
        .map(_.capitalize)
        .mkString
    }
    def camelizeLower(name: String) = camelize(name) match {
      case x if x.length == 1 || (x.length > 1 && (x(1).isLower || x(1).isDigit)) =>
        s"${x(0).toLower}${x.substring(1)}"
      case x => x
    }
  }
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs)
  val viewDefs = YamlViewDefLoader(tableMd, mdDefs).viewDefs
  val xsdWriter = new XsdWriter(
    viewDefs,
    Naming.camelize _,
    Naming.camelize(_) + "Type")
  val nl = System.getProperty("line.separator")
  "generated yaml file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "views-out.yaml")
    val ioViews = viewDefs.map(MdConventions.toExternal(_, tableMd))
    val produced = (new YamlViewDefWriter).toYaml(ioViews)
    if (expected != produced)
      toFile(path + "/" + "views-out-produced.yaml", produced)
    expected should be(produced)
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
    // TODO api sucks
    object ScalaBuilder extends ScalaClassWriter {
      override def scalaClassName(name: String) = Naming.camelize(name)
      override def scalaFieldName(name: String) = Naming.camelizeLower(name)
      override def scalaClassTraits(typeDef: ViewDef[FieldDef[Type]]) =
        if (typeDef.fields.exists(f => f.name == "id" && f.type_.name == "long"))
          List("DtoWithId")
        else List("Dto")
    }
    // TODO api sucks
    val produced = ScalaBuilder.createScalaClassesString(
      List("package some.pack", ""), viewDefs, Seq("// end"))
      .replace(nl, "\n") // normalize newlines here? TODO
    if (expected != produced)
      toFile(path + "/" + "classes-out-produced.scala", produced)
    expected should be(produced)
  }
  "generated scala case class file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "case-classes-out.scala")
    // TODO api sucks
    object ScalaBuilder extends ScalaCaseClassWriter {
      override def scalaClassName(name: String) = Naming.camelize(name)
      override def scalaFieldName(name: String) = Naming.camelizeLower(name)
    }
    // TODO api sucks
    val produced = ScalaBuilder.createScalaClassesString(
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

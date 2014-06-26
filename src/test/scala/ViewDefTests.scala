import scala.io.Source
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import mojoz.metadata._
import mojoz.metadata.in._
import mojoz.metadata.out._
import java.io.PrintWriter
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }

class ViewDefTests extends FlatSpec with Matchers {
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val tableMd = new TableMetadata(tableDefs)
  val viewDefs = YamlViewDefLoader(tableMd, mdDefs).viewDefs
    // TODO I18nRules.suffixI18n(i18nSuffixes = Set("_eng", "_rus")))
  val nl = System.getProperty("line.separator")
  "generated xsd file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "xsd-out.xsd")
    val produced = (new XsdWriter(viewDefs)).schema("kps.ldz.lv")
    if (expected != produced)
      toFile(path + "/" + "xsd-out-produced.xsd", produced)
    expected should be(produced)
  }
  "generated bindings file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "xsd-bindings-out.xjb")
    val produced = (new XsdWriter(viewDefs)).jaxbBindings("my-ws-schema.xsd")
    if (expected != produced)
      toFile(path + "/" + "xsd-bindings-out-produced.xjb", produced)
    expected should be(produced)
  }
  // TODO visualizer tests
  "generated scala class file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "classes-out.scala")
    // TODO api sucks
    object ScalaBuilder extends ScalaClassWriter {
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
  def fileToString(filename: String) = {
    val source = Source.fromFile(filename)
    val body = source.mkString
    source.close()
    body.replace(nl, "\n") // normalize newlines
  }
  def toFile(filename: String, message: String) {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}

import scala.io.Source
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import mojoz.metadata.in.YamlMd
import mojoz.metadata.in.YamlTableDefLoader
import mojoz.metadata.io.MdConventions
import mojoz.metadata.out.SqlWriter
import mojoz.metadata.out.YamlTableDefWriter
import java.io.PrintWriter

class TableDefTests extends FlatSpec with Matchers {
  val path = "src/test/resources/table-def"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName == "tables-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val nl = System.getProperty("line.separator")
  "generated yaml file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = YamlTableDefWriter.toYaml(tableDefs)
    toFile(path + "/" + "tables-out-produced.yaml", produced)
    expected should be(produced)
  }
  "generated oracle sql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-oracle.sql")
    val produced = SqlWriter.oracle().createStatements(tableDefs)
    toFile(path + "/" + "tables-out-oracle-produced.sql", produced)
    expected should be(produced)
  }
  def fileToString(filename: String) = {
    val source = Source.fromFile(filename)
    val body = source.mkString
    source.close()
    body
  }
  def toFile(filename: String, message: String) {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}

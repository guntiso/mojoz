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
  val loader = new YamlTableDefLoader(mdDefs)
  val nl = System.getProperty("line.separator")
  "generated yaml file" should "equal sample file" in {
    val source = Source.fromFile(path + "/" + "tables-out.yaml")
    val expected = source.mkString
    source.close()
    val exTableDefs = loader.tableDefs.map((new MdConventions).toExternal) // TODO ???
    val produced = (new YamlTableDefWriter).toYamlTableDefs(exTableDefs)
    // toFile(path + "/" + "tables-out-produced.yaml", produced)
    expected should be(produced)
  }
  "generated oracle sql file" should "equal sample file" in {
    val source = Source.fromFile(path + "/" + "tables-out-oracle.sql")
    val expected = source.mkString
    source.close()
    val produced = SqlWriter.oracle().createStatements(loader.tableDefs)
    // toFile(path + "/" + "tables-out-oracle-produced.sql", produced)
    expected should be(produced)
  }
  /*
  def toFile(filename: String, message: String) {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
  */
}

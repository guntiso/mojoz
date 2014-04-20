import scala.io.Source
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import mojoz.metadata.in._
import mojoz.metadata.io._
import mojoz.metadata.out._
import java.io.PrintWriter
import java.sql.DriverManager
import com.typesafe.config.ConfigFactory

class TableDefIntegrationTests extends FlatSpec with Matchers {
  case class Cfg(url: String, dbaUser: String, dbaPassword: String,
    testUser: String, testPassword: String, debug: Boolean)
  val conf = ConfigFactory.load()
  val path = "src/test/resources/table-def"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName == "tables-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val nl = System.getProperty("line.separator")
  "generated oracle roundtrip file" should "equal sample file" in {
    val cfg = getCfg("mojoz.oracle.")
    clearDbSchema(cfg)
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = SqlWriter.oracle().createStatements(tableDefs)
      .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(cfg, statements)
    val conn = DriverManager.getConnection(
      cfg.url, cfg.testUser, cfg.testPassword)
    val Schema = "MOJOZ"
    val Prefx = Schema + "."
    val jdbcTableDefs = JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      .map(_.mapTableNames { s: String =>
        (if (s startsWith Prefx) s.substring(Prefx.length) else s).toLowerCase
      })
      .map(_.mapColumnNames((t: String, c: String) => c.toLowerCase))
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    toFile(path + "/" + "tables-out-oracle-jdbc-produced.yaml", produced)
    expected should be(produced)
  }
  def getCfg(prefix: String) = Cfg(
    url = conf.getString(prefix + "jdbc.url"),
    dbaUser = conf.getString(prefix + "dba.user"),
    dbaPassword = conf.getString(prefix + "dba.password"),
    testUser = conf.getString(prefix + "test.user"),
    testPassword = conf.getString(prefix + "test.password"),
    debug = conf.getBoolean(prefix + "debug"))
  def clearDbSchema(cfg: Cfg) = {
    val conn = DriverManager.getConnection(
      cfg.url, cfg.dbaUser, cfg.dbaPassword)
    val statement = conn.createStatement
    try statement.execute("drop user mojoz cascade") catch {
      case ex: Exception => println("failed to drop test user: " + ex.toString)
    }
    statement.execute("create user mojoz identified by mojoz")
    statement.execute("grant connect, resource to mojoz")
    statement.close()
    conn.close()
  }
  def executeStatements(cfg: Cfg, statements: Seq[String]) = {
    val conn = DriverManager.getConnection(
      cfg.url, cfg.testUser, cfg.testPassword)
    val statement = conn.createStatement
    statements.foreach { s =>
      if (cfg.debug) println(s)
      statement execute s
    }
    statement.close()
    conn.close()
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

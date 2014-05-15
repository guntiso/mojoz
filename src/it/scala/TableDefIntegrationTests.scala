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
  case class Cfg(url: String, user: String, password: String, debug: Boolean)
  val conf = ConfigFactory.load()
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName == "tables-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val nl = System.getProperty("line.separator")
  "generated oracle roundtrip file" should "equal sample file" in {
    Class.forName("oracle.jdbc.OracleDriver") //fix random No suitable driver found
    clearOracleDbSchema(getCfg("mojoz.oracle.dba."))
    def skipSome(s: String) = {
      // oracle fails to return column name for desc index
      s.split("\\r?\\n")
        .filterNot(_.contains("uk_test_table1_code_col2"))
        .filterNot(_.contains("idx_tt1_spec_col3_col5d"))
        .mkString(nl)
    }
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = SqlWriter.oracle().schema(tableDefs)
      .split(";").toList.map(_.trim).filter(_ != "")
    val cfg = getCfg("mojoz.oracle.")
    executeStatements(cfg, statements: _*)
    val conn = DriverManager.getConnection(cfg.url, cfg.user, cfg.password)
    val Schema = "MOJOZ"
    val jdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      finally conn.close()
    }.map(_.toSimpleNames).map(_.toLowerCase)
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-oracle-jdbc-produced.yaml", produced)
    skipSome(expected) should be(skipSome(produced))
  }
  "generated postgresql roundtrip file" should "equal sample file" in {
    Class.forName("org.postgresql.Driver") //fix random No suitable driver found
    val cfg = getCfg("mojoz.postgresql.")
    clearPostgresqlDbSchema(cfg)
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = SqlWriter.postgresql().schema(tableDefs)
      .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(cfg, statements: _*)
    val conn = DriverManager.getConnection(cfg.url, cfg.user, cfg.password)
    val Schema = "mojoz"
    val jdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null, "TABLE")
      finally conn.close
    }.map(_.toSimpleNames).map(_.toLowerCase)
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-postgresql-jdbc-produced.yaml", produced)
    expected should be(produced)
  }
  def getCfg(prefix: String) = Cfg(
    url = conf.getString(prefix + "jdbc.url"),
    user = conf.getString(prefix + "user"),
    password = conf.getString(prefix + "password"),
    debug = conf.getBoolean(prefix + "debug"))
  def clearOracleDbSchema(cfg: Cfg) {
    try executeStatements(cfg, "drop user mojoz cascade") catch {
      case ex: Exception => println("failed to drop test user: " + ex.toString)
    }
    executeStatements(cfg,
      "create user mojoz identified by mojoz",
      "grant connect, resource to mojoz")
  }
  def clearPostgresqlDbSchema(cfg: Cfg) {
    try executeStatements(cfg, "drop schema mojoz cascade") catch {
      case ex: Exception => println("failed to drop schema: " + ex.toString)
    }
    executeStatements(cfg, "create schema mojoz authorization mojoz")
  }
  def executeStatements(cfg: Cfg, statements: String*) {
    val conn = DriverManager.getConnection(cfg.url, cfg.user, cfg.password)
    try {
      val statement = conn.createStatement
      try statements foreach { s =>
        if (cfg.debug) println(s)
        statement execute s
      } finally statement.close()
    } finally conn.close()
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

import scala.io.Source
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.mojoz.metadata.in._
import org.mojoz.metadata.io._
import org.mojoz.metadata.out._
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
  val hasOra = conf.getBoolean("mojoz.oracle.available")
  if (hasOra) "generated oracle roundtrip file" should "almost equal sample file" in {
    Class.forName("oracle.jdbc.OracleDriver") //fix random No suitable driver found
    clearOracleDbSchema(getCfg("mojoz.oracle.dba."))
    def skipSome(s: String) = {
      s.split("\\r?\\n")
        // oracle fails to return column name for desc index
        .filterNot(_ startsWith "- uk_test_table1_code_col2")
        .filterNot(_ startsWith "- idx_tt1_spec_col3_col5d")
        .filterNot(_ startsWith "- code, col2 desc")
        // do not compare extras-dependent lines
        .filterNot(_ == "- code                  ! 16            :")
        .filterNot(_ contains "SWIFT")
        .filterNot(_ contains "extra-for-bank")
        // jdbc roundtrip fails on oracle:
        .filterNot(_ startsWith "- int_col ") // inexact length mapping
        .filterNot(_ startsWith "- long_col ") // inexact length mapping
        .filterNot(_ startsWith "- date_col ") // mapped to dateTime
        .filterNot(_ startsWith "- string6k_col ") // clob length lost & incorrect
        .mkString(nl)
    }
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = DdlGenerator.oracle().schema(tableDefs)
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

    // accept short constraint names as default
    val oraConventions =
      new MdConventions(new DdlGenerator.OracleConstraintNamingRules)
    // ignore on update: no sql to set it & default is cascade unlike other dbs
    val producedXXX = YamlTableDefWriter.toYaml(jdbcTableDefs.map(t =>
      t.copy(refs = t.refs.map(_.copy(onUpdateAction = null)))),
      oraConventions.toExternal)
    skipSome(expected) should be(skipSome(producedXXX))
  }
  val hasPostgres = conf.getBoolean("mojoz.postgresql.available")
  if (hasPostgres) "generated postgresql roundtrip file" should "equal sample file" in {
    Class.forName("org.postgresql.Driver") //fix random No suitable driver found
    val dbAndCfgList = List(
      (null,       getCfg("mojoz.postgresql.")),
      ("other_db", getCfg("mojoz.postgresql.", "2")),
    )
    def skipSome(s: String) = {
      s.split("\\r?\\n")
        .filterNot(_ startsWith "- col2                    1") // postgres empty comments roundtrip fails
        .filterNot(_ ==   "comments: \"\"")                    // postgres empty comments roundtrip fails
        // do not compare extras-dependent lines
        .filterNot(_ == "- code                  ! 16            :")
        .filterNot(_ contains "SWIFT")
        .filterNot(_ contains "extra-for-bank")
        .mkString(nl)
    }
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = dbAndCfgList.map { case (db, cfg) =>
      clearPostgresqlDbSchema(cfg)
      val statements = DdlGenerator.postgresql().schema(tableDefs.filter(_.db == db))
        .split(";").toList.map(_.trim).filter(_ != "")
      executeStatements(cfg, statements: _*)
      val conn = DriverManager.getConnection(cfg.url, cfg.user, cfg.password)
      val Schema0 = "mojoz"
      val Schema1 = "test_schema_1"
      val Schemas = List(Schema0, Schema1)
      val jdbcTableDefs = {
        try
          Schemas.flatMap(schema => JdbcTableDefLoader.tableDefs(conn, null, schema, null, "TABLE"))
        finally conn.close
      }
      .map(td =>
        if (td.name contains Schema1)
          td
        else
          td.toSimpleNames
      )
      .map(_.toLowerCase)
      .map(_.copy(db = db))
      YamlTableDefWriter.toYaml(jdbcTableDefs)
    }.mkString("\n")
    if (expected != produced)
      toFile(path + "/" + "tables-out-postgresql-jdbc-produced.yaml", produced)
    skipSome(expected) should be(skipSome(produced))
  }
  def getCfg(prefix: String, suffix: String = "") = Cfg(
    url = conf.getString(prefix + "jdbc.url" + suffix),
    user = conf.getString(prefix + "user"),
    password = conf.getString(prefix + "password"),
    debug = conf.getBoolean(prefix + "debug"))
  def clearOracleDbSchema(cfg: Cfg): Unit = {
    try executeStatements(cfg, "drop user mojoz cascade") catch {
      case ex: Exception => println("failed to drop test user: " + ex.toString)
    }
    executeStatements(cfg,
      "create user mojoz identified by mojoz",
      "grant connect, resource to mojoz")
  }
  def clearPostgresqlDbSchema(cfg: Cfg): Unit = {
    try executeStatements(cfg,
        "drop schema mojoz cascade",
        "drop schema test_schema_1 cascade",
    ) catch {
      case ex: Exception => println("failed to drop schema: " + ex.toString)
    }
    executeStatements(cfg,
      "create schema mojoz authorization mojoz",
      "create schema test_schema_1 authorization mojoz",
    )
  }
  def executeStatements(cfg: Cfg, statements: String*): Unit = {
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
  def toFile(filename: String, message: String): Unit = {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}

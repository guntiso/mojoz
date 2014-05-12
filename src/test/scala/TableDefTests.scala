import java.io.PrintWriter
import java.sql.DriverManager

import scala.io.Source

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import mojoz.metadata.in.JdbcTableDefLoader
import mojoz.metadata.in.YamlMd
import mojoz.metadata.in.YamlTableDefLoader
import mojoz.metadata.out.SqlWriter
import mojoz.metadata.out.YamlTableDefWriter

class TableDefTests extends FlatSpec with Matchers {
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName == "tables-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  val nl = System.getProperty("line.separator")
  "generated yaml file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = YamlTableDefWriter.toYaml(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-produced.yaml", produced)
    expected should be(produced)
  }
  "generated oracle sql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-oracle.sql")
    val produced = SqlWriter.oracle().createStatements(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-oracle-produced.sql", produced)
    expected should be(produced)
  }
  "generated postgresql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-postgresql.sql")
    val produced = SqlWriter.postgresql().createStatements(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-postgresql-produced.sql", produced)
    expected should be(produced)
  }
  "generated hsqldb file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-hsqldb.sql")
    val produced = SqlWriter.hsqldb().createStatements(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-hsqldb-produced.sql", produced)
    expected should be(produced)
  }
  "generated hsqldb roundtrip file" should "equal sample file" in {
    val (url, user, password) = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
    def executeStatements(statements: String*) {
      val conn = DriverManager.getConnection(url, user, password)
      try {
        val statement = conn.createStatement
        try statements foreach { statement.execute } finally statement.close()
      } finally conn.close()
    }
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = SqlWriter.hsqldb().createStatements(tableDefs)
      .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(statements: _*)
    val conn = DriverManager.getConnection(url, user, password)
    val Catalog = "PUBLIC"
    val Schema = "PUBLIC"
    val Prefx = Catalog + "." + Schema + "."
    val rawJdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      finally conn.close
    }.map(t => t.copy( // hsqldb-specific index cleanup
      uk = t.uk.filterNot(_.name startsWith "SYS_IDX_"),
      idx = t.idx.filterNot(_.name startsWith "SYS_IDX_")))

    val jdbcTableDefs = rawJdbcTableDefs.map(_.toSimpleNames).map(_.toLowerCase)
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-hsqldb-jdbc-produced.yaml", produced)
    expected should be(produced)
    val jdbcTableDefs2 =
      rawJdbcTableDefs.map(_.unprefixTableNames(Prefx)).map(_.toLowerCase)
    val produced2 = YamlTableDefWriter.toYaml(jdbcTableDefs2)
    if (expected != produced2)
      toFile(path + "/" + "tables-out-hsqldb-jdbc-produced.yaml", produced2)
    expected should be(produced2)
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

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
  import TableDefTests._
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName == "tables-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  "generated yaml file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = YamlTableDefWriter.toYaml(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-produced.yaml", produced)
    expected should be(produced)
  }
  "generated oracle sql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-oracle.sql")
    val produced = SqlWriter.oracle().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-oracle-produced.sql", produced)
    expected should be(produced)
  }
  "generated postgresql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-postgresql.sql")
    val produced = SqlWriter.postgresql().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-postgresql-produced.sql", produced)
    expected should be(produced)
  }
  "generated h2 file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-hsqldb.sql")
    val produced = SqlWriter.h2().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-h2-produced.sql", produced)
    expected should be(produced)
  }
  "generated hsqldb file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-hsqldb.sql")
    val produced = SqlWriter.hsqldb().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-hsqldb-produced.sql", produced)
    expected should be(produced)
  }
  "generated h2 roundtrip file" should "almost equal sample file" in {
    implicit val ci = ("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "SA", "")
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements =
      SqlWriter.h2().schema(tableDefs)
        .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(statements: _*)
    val conn = getConn
    val Schema = "PUBLIC"
    val rawJdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      finally conn.close
    }.map(t => t.copy( // h2-specific synthetic index cleanup
      idx = t.idx.filterNot(_.name endsWith "_INDEX_E")
        .filterNot(_.name endsWith "_INDEX_1")))

    val jdbcTableDefs = rawJdbcTableDefs.map(_.toSimpleNames).map(_.toLowerCase)
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-h2-jdbc-produced.yaml", produced)
    skipSome(expected) should be(skipSome(produced))
  }
  "generated hsqldb roundtrip file" should "almost equal sample file" in {
    implicit val ci = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = SqlWriter.hsqldb().schema(tableDefs)
      .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(statements: _*)
    val conn = getConn
    val Catalog = "PUBLIC"
    val Schema = "PUBLIC"
    val Prefx = Catalog + "." + Schema + "."
    val rawJdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      finally conn.close
    }.map(t => t.copy( // hsqldb-specific synthetic index cleanup
      uk = t.uk.filterNot(_.name startsWith "SYS_IDX_"),
      idx = t.idx.filterNot(_.name startsWith "SYS_IDX_")))

    val jdbcTableDefs = rawJdbcTableDefs.map(_.toSimpleNames).map(_.toLowerCase)
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-hsqldb-jdbc-produced.yaml", produced)
    skipSome(expected) should be(skipSome(produced))
    val jdbcTableDefs2 =
      rawJdbcTableDefs.map(_.unprefixTableNames(Prefx)).map(_.toLowerCase)
    val produced2 = YamlTableDefWriter.toYaml(jdbcTableDefs2)
    skipSome(expected) should be(skipSome(produced2))
  }
}

object TableDefTests {
  val nl = System.getProperty("line.separator")
  def skipSome(s: String) = {
    // h2 and hsqldb ignores 'desc' on index cols, do not compare these lines
    s.split("\\r?\\n")
      .filterNot(_.contains("- code, col2"))
      .filterNot(_.contains("idx_tt1_spec_col3_col5d"))
      .mkString(nl)
  }
  def getConn(implicit ci: (String, String, String)) =
    DriverManager.getConnection(ci._1, ci._2, ci._3)
  def executeStatements(statements: String*)(implicit ci: (String, String, String)) {
    val conn = getConn(ci)
    try {
      val statement = conn.createStatement
      try statements foreach { statement.execute } finally statement.close()
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

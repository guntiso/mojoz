import java.io.PrintWriter
import java.sql.DriverManager

import scala.io.Source

import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import org.mojoz.metadata.in.JdbcTableDefLoader
import org.mojoz.metadata.in.YamlMd
import org.mojoz.metadata.in.YamlTableDefLoader
import org.mojoz.metadata.out.SqlWriter
import org.mojoz.metadata.out.YamlTableDefWriter

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
    val expected = fileToString(path + "/" + "tables-out-h2.sql")
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
    Class.forName("org.h2.Driver") // fix "sbt +test" - No suitable driver found
    implicit val ci = h2Ci
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements =
      SqlWriter.h2().schema(tableDefs)
        .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(statements: _*)
    val conn = getConn
    val Schema = "PUBLIC"
    val idxNames = indexNames(statements).map(_.toUpperCase)
    val rawJdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      finally conn.close
    }.map(t => t.copy(
      // h2-specific synthetic index cleanup
      idx = t.idx.filter(i => idxNames.contains(i.name)),
      // h2 empty comments cleanup
      comments = Option(t.comments).filter(_ != "").orNull,
      cols = t.cols.map(c => c.copy(comments = Option(c.comments).filter(_ != "").orNull))
    ))

    val jdbcTableDefs = rawJdbcTableDefs.map(_.toSimpleNames).map(_.toLowerCase)
    val produced = YamlTableDefWriter.toYaml(jdbcTableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-h2-jdbc-produced.yaml", produced)
    def skipSomeH2(s: String) = {
      // ignoring h2 STRINGDECODE garbage in check constraints (diacritics enum roundtrip fails for h2)
      skipSome(s).split("\\r?\\n")
        .filterNot(_ == "ck:")
        .filterNot(_.contains("diacritics"))
        .filterNot(_.contains("DIACRITICS"))
        .filterNot(_ startsWith "- col2                    1") // h2 empty comments roundtrip fails
        .filterNot(_ ==   "comments: \"\"")                    // h2 empty comments roundtrip fails
        .mkString(nl)
    }
    skipSomeH2(expected) should be(skipSomeH2(produced))
  }
  "generated hsqldb roundtrip file" should "almost equal sample file" in {
    implicit val ci = hsqldbCi
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val statements = SqlWriter.hsqldb().schema(tableDefs)
      .split(";").toList.map(_.trim).filter(_ != "")
    executeStatements(statements: _*)
    val conn = getConn
    val Catalog = "PUBLIC"
    val Schema = "PUBLIC"
    val Prefx = Catalog + "." + Schema + "."
    val idxNames = indexNames(statements).map(_.toUpperCase)
    val rawJdbcTableDefs = {
      try JdbcTableDefLoader.tableDefs(conn, null, Schema, null)
      finally conn.close
    }.map(t => t.copy( // hsqldb-specific synthetic index cleanup
      uk = t.uk.map { uk =>
        if (!(uk.name startsWith "SYS_IDX_")) uk
        else {
          val idx = uk.name.lastIndexOf('_')
          val newName =
            if (idx < "SYS_IDX_".length) null
            else uk.name.substring("SYS_IDX_".length, idx)
          uk.copy(name = newName)
        }
      },
      idx = t.idx.filter(i => idxNames.contains(i.name))))

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
  val h2Ci = ("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "SA", "")
  val hsqldbCi = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
  def skipSome(s: String) = {
    // h2 and hsqldb ignores 'desc' on index cols, do not compare these lines
    s.split("\\r?\\n")
      .filterNot(_.contains("- code, col2"))
      .filterNot(_.contains("idx_tt1_spec_col3_col5d"))
      // do not compara extras-dependent lines
      .filterNot(_ == "- code                  ! 16            :")
      .filterNot(_ contains "SWIFT")
      .filterNot(_ contains "extra-for-bank")
      .mkString(nl)
  }
  def indexNames(statements: Seq[String]) =
    statements.map(_.split("\\s+").toList match {
      case ("create" :: "index" :: index :: tail) => List(index)
      case ("create" :: "unique" :: "index" :: index :: tail) => List(index)
      case _ => Nil
    }).flatten.toSet
  def getConn(implicit ci: (String, String, String)) =
    DriverManager.getConnection(ci._1, ci._2, ci._3)
  def executeStatements(statements: String*)(implicit ci: (String, String, String)): Unit = {
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
  def toFile(filename: String, message: String): Unit = {
    val out = new PrintWriter(filename, "UTF-8")
    try out.print(message) finally out.close
  }
}

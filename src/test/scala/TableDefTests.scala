import java.io.PrintWriter
import java.sql.DriverManager

import scala.io.Source

import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import org.mojoz.metadata.in.JdbcTableDefLoader
import org.mojoz.metadata.in.YamlMd
import org.mojoz.metadata.in.YamlTableDefLoader
import org.mojoz.metadata.out.SqlGenerator
import org.mojoz.metadata.out.YamlTableDefWriter

class TableDefTests extends FlatSpec with Matchers {
  import TableDefTests._
  "generated yaml file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = YamlTableDefWriter.toYaml(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-produced.yaml", produced)
    expected should be(produced)
  }
  "generated oracle sql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-oracle.sql")
    val produced = SqlGenerator.oracle().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-oracle-produced.sql", produced)
    expected should be(produced)
  }
  "generated postgresql file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-postgresql.sql")
    val produced = SqlGenerator.postgresql().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-postgresql-produced.sql", produced)
    expected should be(produced)
  }
  "generated h2 file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-h2.sql")
    val produced = SqlGenerator.h2().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-h2-produced.sql", produced)
    expected should be(produced)
  }
  "generated hsqldb file" should "equal sample file" in {
    val expected = fileToString(path + "/" + "tables-out-hsqldb.sql")
    val produced = SqlGenerator.hsqldb().schema(tableDefs)
    if (expected != produced)
      toFile(path + "/" + "tables-out-hsqldb-produced.sql", produced)
    expected should be(produced)
  }
  "generated h2 roundtrip file" should "almost equal sample file" in {
    Class.forName("org.h2.Driver") // fix "sbt +test" - No suitable driver found
    val dbAndConnectionInfoList = List(
      (null,       h2Ci),
      ("other_db", h2Ci2),
    )
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = dbAndConnectionInfoList.map { case (db, connectionInfo) =>
      implicit val ci = connectionInfo
      val statements =
       "create schema test_schema_1" ::
        SqlGenerator.h2().schema(tableDefs.filter(_.db == db))
          .split(";").toList.map(_.trim).filter(_ != "")
      executeStatements(statements: _*)
      val conn = getConn
      val Schema0 = "PUBLIC"
      val Schema1 = "TEST_SCHEMA_1"
      val Schemas = List(Schema0, Schema1)
      val Prefix1 = "TEST."
      val idxNames = indexNames(statements).map(_.toUpperCase)
      val rawJdbcTableDefs = {
        try
          Schemas.flatMap(schema => JdbcTableDefLoader.tableDefs(conn, null, schema, null))
        finally conn.close
      }.map(t => t.copy(
        // h2-specific synthetic index cleanup
        idx = t.idx.filter(i => idxNames.contains(i.name)),
        // h2 empty comments cleanup
        comments = Option(t.comments).filter(_ != "").orNull,
        cols = t.cols.map(c => c.copy(comments = Option(c.comments).filter(_ != "").orNull))
      ))

      val jdbcTableDefs =
        rawJdbcTableDefs
          .map(td =>
            if (td.name contains Schema1)
              td.unprefixTableNames(Prefix1)
            else
              td.toSimpleNames
          )
          .map(_.toLowerCase)
          .map(_.copy(db = db))
      YamlTableDefWriter.toYaml(jdbcTableDefs)
    }.mkString("\n")
    if (expected != produced)
      toFile(path + "/" + "tables-out-h2-jdbc-produced.yaml", produced)
    def skipSomeH2(s: String) = {
      skipSome(s).split("\\r?\\n")
        .filterNot(_ startsWith "- uk_tt2_spec_code_col2")     // h2 optimizes index count?
        .filterNot(_ startsWith "- col2                    1") // h2 empty comments roundtrip fails
        .filterNot(_ ==   "comments: \"\"")                    // h2 empty comments roundtrip fails
        .mkString(nl)
    }
    skipSomeH2(expected) should be(skipSomeH2(produced))
  }
  "generated hsqldb roundtrip file" should "almost equal sample file" in {
    val dbAndConnectionInfoList = List(
      (null,       hsqldbCi),
      ("other_db", hsqldbCi2),
    )
    val expected = fileToString(path + "/" + "tables-out.yaml")
    val produced = dbAndConnectionInfoList.map { case (db, connectionInfo) =>
      implicit val ci = connectionInfo
      val statements =
       "create schema test_schema_1" ::
       SqlGenerator.hsqldb().schema(tableDefs.filter(_.db == db))
        .split(";").toList.map(_.trim).filter(_ != "")
      executeStatements(statements: _*)
      val conn = getConn
      val Catalog = "PUBLIC"
      val Schema0 = "PUBLIC"
      val Schema1 = "TEST_SCHEMA_1"
      val Schemas = List(Schema0, Schema1)
      val Prefix1 = s"$Catalog."
      val idxNames = indexNames(statements).map(_.toUpperCase)
      val rawJdbcTableDefs = {
        try
          Schemas.flatMap(schema => JdbcTableDefLoader.tableDefs(conn, null, schema, null))
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

      val jdbcTableDefs =
        rawJdbcTableDefs
          .map(td =>
            if (td.name contains Schema1)
              td.unprefixTableNames(Prefix1)
            else
              td.toSimpleNames
          )
          .map(_.toLowerCase)
          .map(_.copy(db = db))
      YamlTableDefWriter.toYaml(jdbcTableDefs)
    }.mkString("\n")
    if (expected != produced)
      toFile(path + "/" + "tables-out-hsqldb-jdbc-produced.yaml", produced)
    skipSome(expected) should be(skipSome(produced))
  }
}

object TableDefTests {
  val nl = System.getProperty("line.separator")
  val h2Ci = ("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "SA", "")
  val h2Ci2 = ("jdbc:h2:mem:test2;DB_CLOSE_DELAY=-1", "SA", "")
  val hsqldbCi = ("jdbc:hsqldb:mem:mymemdb", "SA", "")
  val hsqldbCi2 = ("jdbc:hsqldb:mem:mymemdb2", "SA", "")
  val path = "src/test/resources"
  val mdDefs = YamlMd.fromFiles(
    path = path, filter = _.getName == "tables-in.yaml")
  val tableDefs = new YamlTableDefLoader(mdDefs).tableDefs
  def skipSome(s: String) = {
    // h2 and hsqldb ignores 'desc' on index cols, do not compare these lines
    s.split("\\r?\\n")
      .filterNot(_.contains("- code, col2"))
      .filterNot(_.contains("idx_tt1_spec_col3_col5d"))
      // do not compare extras-dependent lines
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

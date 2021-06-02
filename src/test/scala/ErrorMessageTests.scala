import org.mojoz.metadata._
import org.mojoz.metadata.in._
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.Seq

class ErrorMessageTests extends FlatSpec with Matchers {
  val path = "src/test/resources"
  val tableMd = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val viewMd = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "error-message-tests.yaml")
  val tableDefs = new YamlTableDefLoader(tableMd).tableDefs
  val tableMetadata = new TableMetadata(tableDefs)
  val nl = System.getProperty("line.separator")

  def messageStack(ex: Throwable): String =
    if (ex == null) ""
    else ex.getMessage + Option(messageStack(ex.getCause)).filter(_ != "").map("\n" + _).getOrElse("")

  viewMd.take(2).map(_.line).mkString(", ") should be ("1, 13")

  viewMd foreach { md =>
    val lines = md.body.split("\\r?\\n")
    val emKey = "expected-error-message:"
    val expectedErrorMessages =
      lines.filter(_ startsWith emKey).map(_.substring(emKey.length)).map(_.trim)
    val cleanMd = md.copy(body = lines.map(l => if (l startsWith emKey) "" else l).mkString(nl))
    val exception = intercept[RuntimeException](
      YamlViewDefLoader(tableMetadata, Seq(cleanMd)).plainViewDefs
    )
    val messages = messageStack(exception)
    expectedErrorMessages foreach { expectedMsg =>
      messages should include (expectedMsg)
    }
  }
}

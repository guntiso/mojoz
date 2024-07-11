package org.mojoz.metadata.in

import org.mojoz.metadata._
import org.mojoz.metadata.in._
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.collection.immutable.Seq
import scala.collection.mutable.Buffer

class ErrorMessageTests extends FlatSpec with Matchers {
  class FilesLazyMdSource(
    path: String,
    filter: (File) => Boolean) extends FilesMdSource(path, filter) {
    override def defs = split(defSets)
  }

  val path = "src/test/resources"
  val tableMd = YamlMd.fromFiles(
    path = path, filter = _.getName endsWith "-in.yaml")
  val yamlMd = new FilesLazyMdSource(
    path = path, filter = _.getName endsWith "error-message-tests.yaml").defs
  val tableDefs = new YamlTableDefLoader(tableMd).tableDefs
  val tableMetadata = new TableMetadata(tableDefs)
  val nl = System.getProperty("line.separator")

  def messageStack(ex: Throwable): String =
    if (ex == null) ""
    else ex.getMessage + Option(messageStack(ex.getCause)).filter(_ != "").map("\n" + _).getOrElse("")

  val mdBuffer = Buffer[YamlMd]()

  "yaml loaders" should "produce expected error messages" in {
    yamlMd foreach { md =>
      val lines = md.body.split("\\r?\\n")
      val emKey = "expected-error-message:"
      val expectedErrorMessages =
        lines.filter(_ startsWith emKey).map(_.substring(emKey.length)).map(_.trim)
      val cleanMd = md.copy(body = lines.map(l => if (l startsWith emKey) "" else l).mkString(nl))
      mdBuffer += cleanMd
      if (expectedErrorMessages.nonEmpty) {
        val exception = intercept[RuntimeException] {
          mdBuffer.toList.foreach(_.parsed)
          val ct = new YamlTypeDefLoader(mdBuffer.toList).typeDefs
          val td = new YamlTableDefLoader(mdBuffer.toList).tableDefs
          val vd = YamlViewDefLoader(tableMetadata, mdBuffer.toList).plainViewDefs
        }
        val messages = messageStack(exception)
        expectedErrorMessages foreach { expectedMsg =>
          messages should include (expectedMsg)
        }
        mdBuffer.clear()
      }
    }
  }
}

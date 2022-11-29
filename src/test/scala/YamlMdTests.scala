import org.mojoz.metadata.in._
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.io.Source

class YamlMdTests extends FlatSpec with Matchers {
  val filename = "error-message-tests.yaml"
  val filter: File => Boolean = _.getName endsWith filename

  "yaml metadata from files" should "provide line numbers" in {
    YamlMd.fromFiles("src/test", filter)
      .take(2).map(_.line).mkString(", ") should be ("1, 10")
  }

  "yaml metadata from string" should "provide index and line numbers" in {
    val f = new File(s"src/test/resources/$filename")
    val mdString = Source.fromFile(f).mkString
    YamlMd.fromString(mdString)
      .take(2).map(_.filename).head should be ("string_0")
    YamlMd.fromString(mdString)
      .take(2).map(_.line).mkString(", ") should be ("1, 10")
  }

  "yaml metadata from files" should "provide relative path and filename" in {
    YamlMd.fromFiles("src/test", filter).head.filename shouldBe (s"resources/$filename")
    YamlMd.fromFiles("src/test/resources", filter).head.filename shouldBe (s"$filename")
  }
}

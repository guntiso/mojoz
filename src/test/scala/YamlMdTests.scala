import org.mojoz.metadata.in._
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import java.io.File

class YamlMdTests extends FlatSpec with Matchers {
  "yaml metadata from files" should "provide relative path and filename" in {
    val filename = "error-message-tests.yaml"
    val filter: File => Boolean = _.getName endsWith filename
    YamlMd.fromFiles("src/test", filter).head.filename shouldBe (s"resources/$filename")
    YamlMd.fromFiles("src/test/resources", filter).head.filename shouldBe (s"$filename")
  }
}

package org.mojoz.metadata

import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

class TypeMetadataTests extends FlatSpec with Matchers {

  "type metadata" should "return custom types last" in {
    TypeMetadata.customizedTypeDefs.map(_.name) shouldBe Seq(
      "string",
      "date",
      "time",
      "short",
      "int",
      "long",
      "integer",
      "float",
      "double",
      "decimal",
      "boolean",
      "bytes",
      "anyType",
      "dateTime",
    )
  }
}

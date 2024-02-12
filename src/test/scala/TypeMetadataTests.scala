package org.mojoz.metadata


import org.mojoz.metadata.in.{YamlMd, YamlTypeDefLoader}
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import java.sql.Types

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

  "type metadata" should "merge types" in {
    val customTypes = new YamlTypeDefLoader(YamlMd.fromString("""
type:       boolean
scala name: MyBoolean
yaml:
- bool
oracle sql:
- numeric(1)

type:       float
scala name: java.lang.Double
jdbc:
- FLOAT

type:       double
scala name: java.lang.Double
jdbc:
- DOUBLE
    """.trim)).typeDefs
    val mergedTypes       = TypeMetadata.mergeTypeDefs(customTypes, TypeMetadata.defaultTypeDefs)
    val nameToType        = mergedTypes.map(t => t.name -> t).toMap

    customTypes.find(_.name == "boolean").get.withFallback(
      TypeMetadata.defaultTypeDefs.find(_.name == "boolean").get
    ) shouldBe nameToType("boolean")

    nameToType("boolean").targetNames shouldBe Map(
      "json"  -> "boolean",
      "scala" -> "MyBoolean",
      "xsd"   -> "boolean",
    )
    nameToType("boolean").yamlLoad.size shouldBe 1
    nameToType("boolean").yamlLoad.head.typeName shouldBe Some("bool")

    nameToType("float").yamlLoad.size shouldBe 1
    nameToType("float").yamlLoad.head.typeName shouldBe Some("float")

    val ddlWriteForB  = nameToType("boolean").ddlWrite
    ddlWriteForB("sql").size shouldBe 1
    ddlWriteForB("sql").head.targetNamePattern shouldBe "boolean"
    ddlWriteForB("postgresql").size shouldBe 1
    ddlWriteForB("postgresql").head.targetNamePattern shouldBe "bool"
    ddlWriteForB("oracle sql").size shouldBe 1
    ddlWriteForB("oracle sql").head.targetNamePattern shouldBe "numeric(1)"

    val jdbcInfoForB  = nameToType("boolean").jdbcLoad
    jdbcInfoForB.size shouldBe 1
    jdbcInfoForB("jdbc").size shouldBe 2
    jdbcInfoForB("jdbc").head.jdbcTypeCode shouldBe Types.BIT
    jdbcInfoForB("jdbc").head.jdbcTypeNameOrCode shouldBe "BIT"
    jdbcInfoForB("jdbc").tail.head.jdbcTypeCode shouldBe Types.BOOLEAN
    jdbcInfoForB("jdbc").tail.head.jdbcTypeNameOrCode shouldBe "BOOLEAN"

    val jdbcInfoForF  = nameToType("float").jdbcLoad
    jdbcInfoForF.size shouldBe 1
    jdbcInfoForF("jdbc").size shouldBe 1
    jdbcInfoForF("jdbc").head.jdbcTypeCode shouldBe Types.FLOAT
    jdbcInfoForF("jdbc").head.jdbcTypeNameOrCode shouldBe "FLOAT"

    val jdbcInfoForD  = nameToType("double").jdbcLoad
    jdbcInfoForD.size shouldBe 1
    jdbcInfoForD("jdbc").size shouldBe 1
    jdbcInfoForD("jdbc").head.jdbcTypeCode shouldBe Types.DOUBLE
    jdbcInfoForD("jdbc").head.jdbcTypeNameOrCode shouldBe "DOUBLE"
  }
}

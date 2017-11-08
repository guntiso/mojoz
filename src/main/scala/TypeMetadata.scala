package mojoz.metadata

import mojoz.metadata.in._
import mojoz.metadata.ColumnDef.ColumnDefBase

case class JdbcLoadInfo(
  jdbcTypeNameOrCode:   String,
  jdbcTypeCode:         Int,
  minSize:              Option[Int],
  maxSize:              Option[Int],
  minFractionDigits:    Option[Int],
  maxFractionDigits:    Option[Int],
  targetLength:         Option[Integer], // xxx Some(null) means copy from source
  targetTotalDigits:    Option[Integer], // xxx Some(null) means copy from source
  targetFractionDigits: Option[Integer]  // xxx Some(null) means copy from source
)

case class YamlLoadInfo(
  // TODO YamlLoadInfo
  // TODO naming patterns here?
)

object TypeDef {
  trait TypeDefBase {
    val name: String
    val targetNames: Map[String, String] // xsd, scala, java, db, postgres, oracle, ...?
    val jdbcLoad: Seq[JdbcLoadInfo]
    val yamlLoad: Seq[YamlLoadInfo]
    val defaults: ColumnDefBase[Type]
    val namingConventions: Seq[String]
  }
}

import TypeDef.TypeDefBase
case class TypeDef(
  name: String,
  targetNames: Map[String, String],
  jdbcLoad: Seq[JdbcLoadInfo],
  yamlLoad: Seq[YamlLoadInfo],
  defaults: ColumnDefBase[Type],
  namingConventions: Seq[String],
  extras: Map[String, Any]) extends TypeDefBase {
}

class TypeMetadata[+T <: TypeDefBase](
  val typeDefs: Seq[T] = (new YamlTypeDefLoader()).typeDefs) {
  private val typeNameToDef = typeDefs.map(td => (td.name, td)).toMap
  def typeDef(typeName: String) =
    typeNameToDef(typeName)
}

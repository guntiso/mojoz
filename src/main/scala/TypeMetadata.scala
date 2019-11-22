package mojoz.metadata

import mojoz.metadata.in._
import mojoz.metadata.ColumnDef.ColumnDefBase
import scala.collection.immutable._

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
  // TODO naming patterns here?
  typeName:             Option[String],
  minSize:              Option[Int],
  maxSize:              Option[Int],
  minFractionDigits:    Option[Int],
  maxFractionDigits:    Option[Int],
  targetLength:         Option[Integer], // xxx Some(null) means copy from source
  targetTotalDigits:    Option[Integer], // xxx Some(null) means copy from source
  targetFractionDigits: Option[Integer]  // xxx Some(null) means copy from source
)

case class SqlWriteInfo(
  minSize:              Option[Int],
  maxSize:              Option[Int],
  minFractionDigits:    Option[Int],
  maxFractionDigits:    Option[Int],
  targetNamePattern:    String
)

object TypeDef {
  trait TypeDefBase {
    val name: String
    val targetNames: Map[String, String] // xsd, scala, java, ...
    val jdbcLoad: Map[String, Seq[JdbcLoadInfo]]
    val yamlLoad: Seq[YamlLoadInfo]
    val sqlWrite: Map[String, Seq[SqlWriteInfo]]
    val defaults: ColumnDefBase[Type]
    val namingConventions: Seq[String]
  }
}

import TypeDef.TypeDefBase
case class TypeDef(
  name: String,
  targetNames: Map[String, String],
  jdbcLoad: Map[String, Seq[JdbcLoadInfo]],
  yamlLoad: Seq[YamlLoadInfo],
  sqlWrite: Map[String, Seq[SqlWriteInfo]],
  defaults: ColumnDefBase[Type],
  namingConventions: Seq[String],
  extras: Map[String, Any]
  ) extends TypeDefBase {

  def withFallback(other: TypeDef) = TypeDef(
    Option(name).getOrElse(other.name),
    other.targetNames ++ targetNames,
    jdbcLoad ++ other.jdbcLoad,
    yamlLoad ++ other.yamlLoad,
    other.sqlWrite ++ sqlWrite,
    Option(defaults).getOrElse(other.defaults),
    Option(namingConventions).filter(_.size > 0).getOrElse(other.namingConventions), // ?
    other.extras ++ extras
  )
}

object TypeMetadata {
  def mergeTypeDefs(typeDefs: Seq[TypeDef], fallbackTypeDefs: Seq[TypeDef]): Seq[TypeDef] = {
    val nameToTypeDef = typeDefs.map(t => t.name -> t).toMap
    val nameToFallbackTypeDef = fallbackTypeDefs.map(t => t.name -> t).toMap
    typeDefs.map { td =>
      val fallbackTypeDefOpt = nameToFallbackTypeDef.get(td.name)
      if (fallbackTypeDefOpt.isDefined) td.withFallback(fallbackTypeDefOpt.get) else td
    } ++ fallbackTypeDefs.filterNot(ftd => nameToTypeDef.contains(ftd.name))
  }
  lazy val defaultTypeDefs = new YamlTypeDefLoader(YamlMd.fromResource("/mojoz-default-types.yaml")).typeDefs
  lazy val customTypeDefs = new YamlTypeDefLoader(YamlMd.fromResource("/mojoz-custom-types.yaml", false)).typeDefs
  lazy val customizedTypeDefs = mergeTypeDefs(customTypeDefs, defaultTypeDefs)
}

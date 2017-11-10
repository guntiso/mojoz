package mojoz.metadata
package in

import mojoz.metadata.ColumnDef.ColumnDefBase
import org.yaml.snakeyaml.Yaml
import scala.collection.immutable._
import scala.collection.JavaConverters._
import scala.util.Try

class YamlTypeDefLoader(yamlMd: Seq[YamlMd] = YamlMd.fromResource("/mojoz-default-types.yaml")) {
  import YamlTableDefLoader._
  val sources = yamlMd.filter(YamlMd.isCustomTypeDef)
  val typeDefs = {
    val rawTypeDefs = sources map { td =>
      try loadYamlTypeDef(td.body) catch {
        case e: Exception => throw new RuntimeException(
          "Failed to load typedef from " + td.filename, e) // TODO line number
      }
    }
    val nameToTableDef = {
      val duplicateNames =
        rawTypeDefs.map(_.name).groupBy(n => n).filter(_._2.size > 1).map(_._1)
      if (duplicateNames.size > 0)
        throw new RuntimeException(
          "Duplicate type definitions: " + duplicateNames.mkString(", "))
      rawTypeDefs.map(t => (t.name, t)).toMap
    }
    // checkTypeDefs(rawTypeDefs)
    rawTypeDefs
  }
  private def toString(src: Any, thisFail: String) = {
    src match {
      case s: java.lang.String => s
      case x => throw new RuntimeException(
        " - unexpected definition class: " + x.getClass
        + "\nentry: " + x.toString)
    }
  }
  private def toMinMax(s: String): (Option[Int], Option[Int]) = {
    val parts = s.split("\\.\\.", 2)
    def toIntOpt(s: String) = s match {
      case "" | "*" => None
      case i => Some(i).map(_.toInt) // TODO try, explain
    }
    if (s == "*")
      (Some(0), None)
    else {
      val min = toIntOpt(parts(0))
      val max = if (parts.size > 1) toIntOpt(parts(1)) else min
      (min, max)
    }
  }
  private def toJdbcLoadInfo(s: String) = {
    val sParts = s.split("->", 2)
    val jdbcPart = Option(sParts(0)).map(_.trim).filter(_ != "").orNull
    if (jdbcPart == null)
      throw new RuntimeException(
        "Unexpected format for jdbc load info: " + s)
    val targetPart =
      if (sParts.size > 1) Option(sParts(1)).map(_.trim).filter(_ != "").orNull
      else null
    val targetPartParts = Option(targetPart).map(_.split(",\\s*")) getOrElse Array()
    val jdbcPartParts = jdbcPart.split("\\s+", 3)
    val jdbcNameOrCode = jdbcPartParts(0)
    val jdbcCode =
      Try(jdbcNameOrCode.toInt).toOption
        .getOrElse(JdbcTableDefLoader.jdbcTypeNameToCode.get(jdbcNameOrCode)
          .getOrElse(sys.error("Unexpected jdbc type name: " + jdbcNameOrCode)))
    val sizeInterval = if (jdbcPartParts.size > 1) jdbcPartParts(1) else ""
    val (minSize, maxSize) = toMinMax(sizeInterval)
    val fracInterval = if (jdbcPartParts.size > 2) jdbcPartParts(2) else ""
    val (minFrac, maxFrac) = toMinMax(fracInterval)
    val targetLength:         Option[Integer] =
      if (targetPartParts.size == 1)
        targetPartParts(0) match {
          case "none" => None
          case "size" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(new Integer(fxSize.toInt))
        }
      else None
    val targetTotalDigits:    Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(0) match {
          case "none" => None
          case "size" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(new Integer(fxSize.toInt))
        }
      else None
    val targetFractionDigits: Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(1) match {
          case "none" => None
          case "frac" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(new Integer(fxSize.toInt))
        }
      else None
    JdbcLoadInfo(jdbcNameOrCode, jdbcCode, minSize, maxSize, minFrac, maxFrac,
      targetLength, targetTotalDigits, targetFractionDigits)
  }
  private def toYamlLoadInfo(s: String) = {
    val sParts = s.split("->", 2)
    val yamlPart = Option(sParts(0)).map(_.trim).filter(_ != "").orNull
    if (yamlPart == null)
      throw new RuntimeException(
        "Unexpected format for yaml load info: " + s)
    val targetPart =
      if (sParts.size > 1) Option(sParts(1)).map(_.trim).filter(_ != "").orNull
      else null
    val targetPartParts = Option(targetPart).map(_.split(",\\s*")) getOrElse Array()
    val yamlPartParts = yamlPart.split("\\s+", 3)
    val yamlName = Option(yamlPartParts(0)).filter(_ != "null")
    val sizeInterval = if (yamlPartParts.size > 1) yamlPartParts(1) else ""
    val (minSize, maxSize) = toMinMax(sizeInterval)
    val fracInterval = if (yamlPartParts.size > 2) yamlPartParts(2) else ""
    val (minFrac, maxFrac) = toMinMax(fracInterval)
    val targetLength:         Option[Integer] =
      if (targetPartParts.size == 1)
        targetPartParts(0) match {
          case "none" => None
          case "size" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(new Integer(fxSize.toInt))
        }
      else None
    val targetTotalDigits:    Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(0) match {
          case "none" => None
          case "size" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(new Integer(fxSize.toInt))
        }
      else None
    val targetFractionDigits: Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(1) match {
          case "none" => None
          case "frac" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(new Integer(fxSize.toInt))
        }
      else None
    YamlLoadInfo(yamlName, minSize, maxSize, minFrac, maxFrac,
      targetLength, targetTotalDigits, targetFractionDigits)
  }
  private def toSqlWriteInfo(s: String) = {
    val sParts = s.split("->", 2)
    val typePart = Option(sParts(0)).map(_.trim).filter(_ != "").orNull
    if (typePart == null)
      throw new RuntimeException(
        "Unexpected format for sql info: " + s)
    val targetPattern =
      if (sParts.size > 1) Option(sParts(1)).map(_.trim).filter(_ != "") getOrElse typePart
      else null
    val typePartParts = if (sParts.size == 1) Array(targetPattern) else typePart.split("\\s+", 3)
    val sizeInterval = if (typePartParts.size > 1) typePartParts(1) else ""
    val (minSize, maxSize) = toMinMax(sizeInterval)
    val fracInterval = if (typePartParts.size > 2) typePartParts(2) else ""
    val (minFrac, maxFrac) = toMinMax(fracInterval)
    SqlWriteInfo(minSize, maxSize, minFrac, maxFrac, targetPattern)
  }
  private def loadYamlTypeDef(typeDef: String) = {
    val tdMap =
      (new Yaml).load(typeDef).asInstanceOf[java.util.Map[String, _]].asScala.toMap
    val typeName = tdMap.get("type").map(_.toString)
      .getOrElse(sys.error("Missing type name"))
    val targetNames: Map[String, String] = TreeMap()(math.Ordering.String) ++
      tdMap.filterKeys(_ endsWith " name").map {
        case (k, v) => (k.substring(0, k.length - "name".length - 1).trim, "" + v)
      }
    val jdbcLoad = tdMap.get("jdbc load")
      .filter(_ != null)
      .map(m => m.asInstanceOf[java.util.ArrayList[_]].asScala.toList)
      .getOrElse(Nil)
      .map(toString(_, "Failed to load jdbc load definition"))
      .map(toJdbcLoadInfo)
    val yamlLoad = tdMap.get("yaml")
      .filter(_ != null)
      .map(m => m.asInstanceOf[java.util.ArrayList[_]].asScala.toList)
      .getOrElse(Nil)
      .map(toString(_, "Failed to load yaml load definition"))
      .map(toYamlLoadInfo)
    val sqlWrite: Map[String, Seq[SqlWriteInfo]] = TreeMap()(math.Ordering.String) ++
      tdMap.filterKeys(_ endsWith "sql").map {
        case (k, v) =>
        val sqlWriteInfoSeq =
          Option(v)
            .map(m => m.asInstanceOf[java.util.ArrayList[_]].asScala.toList)
            .getOrElse(Nil)
            .map(toString(_, s"Failed to load sql write definition for $k"))
            .map(toSqlWriteInfo)
        (k, sqlWriteInfoSeq)
      }
    val defaults = null                         // TODO
    val namingConventions: Seq[String] = Nil    // TODO
    val extras: Map[String, Any] = Map.empty    // TODO val extras = tdMap -- TypeDefKeyStrings
    TypeDef(typeName, targetNames, jdbcLoad, yamlLoad, sqlWrite, defaults, namingConventions, extras)
  }
}

package org.mojoz.metadata
package in

import org.snakeyaml.engine.v2.api.LoadSettings
import org.snakeyaml.engine.v2.api.Load
import scala.collection.immutable._
import scala.jdk.CollectionConverters._
import scala.util.Try

class YamlTypeDefLoader(yamlMd: Seq[YamlMd]) {
  import YamlTableDefLoader._
  val sources = yamlMd.filter(YamlMd.isCustomTypeDef)
  val typeDefs = {
    val rawTypeDefs = sources map { td =>
      try loadYamlTypeDef(td.body, td.filename, td.line) catch {
        case e: Exception => throw new RuntimeException(
          s"Failed to load type definition from ${td.filename}, line ${td.line}", e)
      }
    }
    val nameToTableDef = {
      val duplicateNames =
        rawTypeDefs.map(_.name).groupBy(n => n).filter(_._2.size > 1).map(_._1)
      if (duplicateNames.size > 0)
        sys.error(
          "Duplicate type definitions: " + duplicateNames.mkString(", "))
      rawTypeDefs.map(t => (t.name, t)).toMap
    }
    // checkTypeDefs(rawTypeDefs)
    rawTypeDefs
  }
  private def toString(src: Any, thisFail: String) = {
    src match {
      case s: java.lang.String => s
      case x => sys.error(
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
      sys.error(
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
          case fxSize => Some(Integer.valueOf(fxSize.toInt))
        }
      else None
    val targetTotalDigits:    Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(0) match {
          case "none" => None
          case "size" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(Integer.valueOf(fxSize.toInt))
        }
      else None
    val targetFractionDigits: Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(1) match {
          case "none" => None
          case "frac" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(Integer.valueOf(fxSize.toInt))
        }
      else None
    JdbcLoadInfo(jdbcNameOrCode, jdbcCode, minSize, maxSize, minFrac, maxFrac,
      targetLength, targetTotalDigits, targetFractionDigits)
  }
  private def toYamlLoadInfo(s: String) = {
    val sParts = s.split("->", 2)
    val yamlPart = Option(sParts(0)).map(_.trim).filter(_ != "").orNull
    if (yamlPart == null)
      sys.error(
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
          case fxSize => Some(Integer.valueOf(fxSize.toInt))
        }
      else None
    val targetTotalDigits:    Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(0) match {
          case "none" => None
          case "size" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(Integer.valueOf(fxSize.toInt))
        }
      else None
    val targetFractionDigits: Option[Integer] =
      if (targetPartParts.size == 2)
        targetPartParts(1) match {
          case "none" => None
          case "frac" => Some(null) // xxx Some(null) means copy from source
          case fxSize => Some(Integer.valueOf(fxSize.toInt))
        }
      else None
    YamlLoadInfo(yamlName, minSize, maxSize, minFrac, maxFrac,
      targetLength, targetTotalDigits, targetFractionDigits)
  }
  private def toSqlWriteInfo(s: String) = {
    val sParts = s.split("->", 2)
    val typePart = Option(sParts(0)).map(_.trim).filter(_ != "").orNull
    if (typePart == null)
      sys.error(
        "Unexpected format for sql info: " + s)
    val targetPattern =
      if (sParts.size > 1) Option(sParts(1)).map(_.trim).filter(_ != "") getOrElse typePart
      else typePart
    val typePartParts = if (sParts.size == 1) Array(targetPattern) else typePart.split("\\s+", 3)
    val sizeInterval = if (typePartParts.size > 1) typePartParts(1) else ""
    val (minSize, maxSize) = toMinMax(sizeInterval)
    val fracInterval = if (typePartParts.size > 2) typePartParts(2) else ""
    val (minFrac, maxFrac) = toMinMax(fracInterval)
    DdlWriteInfo(minSize, maxSize, minFrac, maxFrac, targetPattern)
  }
  private def loadYamlTypeDef(typeDefString: String, labelOrFilename: String = null, lineNumber: Int = 0) = {
    val loaderSettings = LoadSettings.builder()
      .setLabel(Option(labelOrFilename) getOrElse "mojoz type metadata")
      .setAllowDuplicateKeys(false)
      .build();
    val lineNumberCorrection = if (lineNumber > 1) "\n" * (lineNumber - 1) else ""
    val tdMap =
      (new Load(loaderSettings)).loadFromString(lineNumberCorrection + typeDefString) match {
        case m: java.util.Map[String @unchecked, _] => m.asScala.toMap
        case x => sys.error(
          "Unexpected class: " + Option(x).map(_.getClass).orNull)
      }
    val typeName = tdMap.get("type").map(_.toString)
      .getOrElse(sys.error("Missing type name"))
    val targetNames: Map[String, String] = TreeMap()(math.Ordering.String) ++
      tdMap.filter(_._1 endsWith " name").map {
        case (k, v) => (k.substring(0, k.length - "name".length - 1).trim, "" + v)
      }
    val jdbcLoad: Map[String, Seq[JdbcLoadInfo]] = TreeMap()(math.Ordering.String) ++
      tdMap.filter(_._1 endsWith "jdbc").map {
        case (k, v) =>
        val jdbcLoadInfoSeq =
          (v match {
            case null => Nil
            case a: java.util.ArrayList[_] => a.asScala.toList
            case x => sys.error("Unexpected class: " + x.getClass)
          })
            .map(toString(_, s"Failed to load jdbc load definition for $k"))
            .map(toJdbcLoadInfo)
        (k, jdbcLoadInfoSeq)
      }
    val yamlLoad = tdMap.get("yaml")
      .map {
        case null => Nil
        case a: java.util.ArrayList[_] => a.asScala.toList
        case x => sys.error("Unexpected class: " + x.getClass)
      }
      .getOrElse(Nil)
      .map(toString(_, "Failed to load yaml load definition"))
      .map(toYamlLoadInfo)
    val ddlWrite: Map[String, Seq[DdlWriteInfo]] = TreeMap()(math.Ordering.String) ++
      tdMap.filter {
        case (k, v) =>
          k.endsWith("sql") || k.endsWith("cql")
      }.map {
        case (k, v) =>
        val ddlWriteInfoSeq =
          (v match {
            case null => Nil
            case a: java.util.ArrayList[_] => a.asScala.toList
            case x => sys.error("Unexpected class: " + x.getClass)
          })
            .map(toString(_, s"Failed to load ddl write definition for $k"))
            .map(toSqlWriteInfo)
        (k, ddlWriteInfoSeq)
      }
    val defaults = null                         // TODO
    val namingConventions: Seq[String] = Nil    // TODO
    val extras: Map[String, Any] = Map.empty    // TODO val extras = tdMap -- TypeDefKeyStrings
    TypeDef(typeName, targetNames, jdbcLoad, yamlLoad, ddlWrite, defaults, namingConventions, extras)
  }
}

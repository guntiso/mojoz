package org.mojoz.metadata.in

import org.snakeyaml.engine.v2.api.Load
import org.snakeyaml.engine.v2.api.LoadSettings

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.jar.JarFile
import scala.collection.immutable.{Map, Seq}
import scala.collection.mutable.Buffer
import scala.io.Codec
import scala.io.Source
import scala.jdk.CollectionConverters._


case class YamlMd(
  filename: String,
  line: Int,
  body: String) {
  private[in] lazy val startsWithDirectiveOrDash = {
    val it = // .nextOption() not available on scala 2.12
      body.linesIterator
        .filterNot(_ startsWith "#")// skip comments
    if (it.hasNext) {
      val line = it.next()
      line.startsWith("%") ||       // yaml directive
      line.startsWith("-")          // yaml array or yaml directives end
    } else false
  }

  lazy val parsed: Seq[Map[String, Any]] = try {
    val loaderSettings = LoadSettings.builder()
      .setLabel(Option(filename) getOrElse "mojoz metadata")
      .setAllowDuplicateKeys(false)
      .build()
    val lineNumberCorrection = if (line > 1) "\n" * (line - 1) else ""
    (new Load(loaderSettings)).loadAllFromString(lineNumberCorrection + body).iterator.asScala.toList.flatMap {
      case m: java.util.Map[String @unchecked, _] => Seq(m.asScala.toMap)
      case a: java.util.ArrayList[_] => a.asScala.map {
        case m: java.util.Map[String @unchecked, _] => m.asScala.toMap
        case x => sys.error(
          "Unexpected class: " + Option(x).map(_.getClass).orNull)
      }
      case x => sys.error(
        "Unexpected class: " + Option(x).map(_.getClass).orNull)
    }
  } catch {
    case e: Exception => throw new RuntimeException(
      s"Failed to parse yaml metadata from $filename, line $line: ${e.getMessage}", e)
  }
}

private[in] trait MdSource {
  def noSplit(mdDef: YamlMd) = // do not split if not bare documents
    mdDef.startsWithDirectiveOrDash
  def split(mdDefs: Seq[YamlMd]) = {
    def shouldSplitAt(line: String) =
      (line == "")         ||  // empty line or
      (line startsWith "...")  // yaml end-of-document marker
    mdDefs.flatMap { d =>
      val linesBuffer = Buffer[String]()
      val mdBuffer    = Buffer[YamlMd]()
      var lineNr = 0
      if (noSplit(d))
        mdBuffer += d
      else
      (d.body + "\n\n").linesIterator foreach { line =>
        lineNr += 1
        if (shouldSplitAt(line)) {
          if (linesBuffer.nonEmpty) {
            mdBuffer += d.copy(
              line = lineNr - linesBuffer.size,
              body = linesBuffer.mkString("\n"))
            linesBuffer.clear()
          }
        } else {
          linesBuffer += line
        }
      }
      mdBuffer.toSeq
    }
  }
  def defSets: Seq[YamlMd]
  def defs = {
    val md = split(defSets)
    md.foreach(_.parsed)
    md
  }
}

private[in] class FileMdSource(file: File) extends MdSource {
  override def defSets =
    Seq(YamlMd(file.getName, 0,  Source.fromFile(file).mkString))
}

private[in] class FilesMdSource(
  val path: String,
  val filter: (File) => Boolean) extends MdSource {
  require(path != null)
  private def recursiveListFiles(relativePath: String, f: File): Array[(String, File)] = {
    val these = Option(f.listFiles) getOrElse Array[File]()
    these.filter(!_.isDirectory)
      .filter(filter)
      .map(f => (relativePath + f.getName, f)) ++
     these.filter(_.isDirectory)
      .flatMap(f => recursiveListFiles(relativePath + f.getName + "/", f))
  }
  override def defSets =
    recursiveListFiles("", new File(path))
      .sortBy(_._1)
      .toList
      .map { case (relativeName, f) => YamlMd(relativeName, 0, Source.fromFile(f).mkString) }
}

private[in] class ResourcesMdSource(
  val indexPath: String,
  val nameFilter: (String) => Boolean,
  val nameMap: (String) => String) extends MdSource {
  // getClass.getClassLoader.getResources("") does not work from jar :(
  private def typedefResources =
    Option(getClass.getResourceAsStream(indexPath))
      .map(Source.fromInputStream(_)(Codec("UTF-8"))
        .getLines().toList).getOrElse(Nil)
      .filter(nameFilter).map(nameMap).toSet.toList
  override def defSets = typedefResources.map(r => YamlMd(r, 0,
    Option(getClass.getResourceAsStream(r))
      .map(Source.fromInputStream(_)("UTF-8").mkString)
      .getOrElse(sys.error("Resource not found: " + r))))
}

private[in] class ResourceMdSource(val resourcePath: String,
    requireResource: Boolean = true) extends MdSource {
  val typedefResources = Seq(resourcePath)
  override def defSets = typedefResources.map(r => YamlMd(r, 0,
    Option(getClass.getResourceAsStream(r))
      .map(Source.fromInputStream(_)("UTF-8").mkString)
      .getOrElse {
        if (requireResource)
          sys.error("Resource not found: " + resourcePath)
        else ""
    }))
}

private[in] class ResourcePathsMdSource(
    val resourcePaths: Seq[String],
    val nameFilter: (String) => Boolean) extends MdSource {
  val classLoader = Thread.currentThread().getContextClassLoader
  override def defSets = resourcePaths.flatMap { path =>
    val normalizedPath = if (path.startsWith("/")) path.drop(1) else path
    val resourceUrl = classLoader.getResource(normalizedPath)
      if  (resourceUrl == null) Nil
      else resourceUrl.getProtocol match {
        case "file" =>
          val filePath = resourceUrl.getPath
          if (Files.isDirectory(Paths.get(filePath)))
            new FilesMdSource(filePath, f => nameFilter(f.getName)).defSets
          else if (nameFilter(filePath))
            new FileMdSource(new File(filePath)).defSets
          else Nil

        case "jar" =>
          val jarPath = resourceUrl.getPath.substring(5, resourceUrl.getPath.indexOf("!"))
          val entryPath = resourceUrl.getPath.substring(resourceUrl.getPath.indexOf("!") + 2)
          val jarFile = new JarFile(jarPath)
          try {
            val entries = jarFile.entries().asScala
              .filter(entry => entry.getName.startsWith(entryPath) && !entry.isDirectory)
              .toSeq
            if (entries.isEmpty) {
              val entry = jarFile.getJarEntry(entryPath)
              if (entry != null && !entry.isDirectory)
                Seq(YamlMd(entryPath, 0, Source.fromInputStream(jarFile.getInputStream(entry))("UTF-8").mkString))
              else Nil
            } else {
              entries.map { entry =>
                YamlMd(entry.getName, 0, Source.fromInputStream(jarFile.getInputStream(entry))("UTF-8").mkString)
              }
            }
          } finally {
            jarFile.close()
          }

        case protocol =>
          throw new IllegalArgumentException(s"Unsupported protocol: $protocol for $normalizedPath")
      }
  }
}

private[in] class StringMdSource(defStrings: String*) extends MdSource {
  override def defSets = defStrings.zipWithIndex.map {
    case (s, i) => YamlMd(s"string_$i", 0, s)
  }.toVector
}

private[in] class NamedStringMdSource(nameAndMdStringPairs: (String, String)*) extends MdSource {
  override def defSets = nameAndMdStringPairs.map {
    case (n, s) => YamlMd(n, 0, s)
  }.toVector
}

object YamlMd {
  private[in] def isCustomTypeDef(d: YamlMd): Boolean = ??? // XXX for binary compatibility TODO remove   
  private[in] def isTableDef(d: YamlMd): Boolean      = ??? // XXX for binary compatibility TODO remove   
  private[in] def isViewDef(d: YamlMd): Boolean       = ??? // XXX for binary compatibility TODO remove   
  def fromFile(file: File) =
    new FileMdSource(file).defs
  def fromFiles(
    path: String,
    filter: (File) => Boolean = _.getName endsWith ".yaml") =
    new FilesMdSource(path, filter).defs
  def fromPaths(
    resourcePaths: Seq[String] = Seq("tables", "views"),
    nameFilter: (String) => Boolean = _ endsWith ".yaml") =
    new ResourcePathsMdSource(resourcePaths, nameFilter).defs
  def fromResource(resourcePath: String, requireResource: Boolean = true) =
    new ResourceMdSource(resourcePath, requireResource).defs
  def fromResources(
    indexPath: String = "/-md-files.txt",
    nameFilter: (String) => Boolean = _ endsWith ".yaml",
    nameMap: (String) => String = "/" + _) =
    new ResourcesMdSource(indexPath, nameFilter, nameMap).defs
  def fromString(mdString: String) =
    new StringMdSource(mdString).defs
  def fromStrings(mdStrings: String*) =
    new StringMdSource(mdStrings: _*).defs
  def fromNamedString(name: String, mdString: String) =
    new NamedStringMdSource((name, mdString)).defs
  def fromNamedStrings(nameAndMdStringPairs: (String, String)*) =
    new NamedStringMdSource(nameAndMdStringPairs: _*).defs
}

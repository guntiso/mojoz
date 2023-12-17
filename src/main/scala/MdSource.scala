package org.mojoz.metadata.in

import java.io.File
import scala.collection.immutable.Seq
import scala.collection.mutable.Buffer
import scala.io.Codec
import scala.io.Source

case class YamlMd(
  filename: String,
  line: Int,
  body: String)

private[in] trait MdSource {
  def split(mdDefs: Seq[YamlMd]) = {
    def shouldSplitAt(line: String) =
      (line == "")         ||  // empty line or
      (line startsWith "...")  // yaml end-of-document marker
    mdDefs.flatMap { d =>
      val linesBuffer = Buffer[String]()
      val mdBuffer    = Buffer[YamlMd]()
      var lineNr = 0
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
  def defs = split(defSets)
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
  private val customTypeDefPattern = "(^|\\n)type\\s*:".r    // XXX
  private val tableDefPattern      = "(^|\\n)columns\\s*:".r // XXX
  private val hasFieldsPattern     = "(^|\\n)fields\\s*:".r  // XXX
  private val hasExtendsPattern    = "(^|\\n)extends\\s*:".r // XXX
  private[in] def isCustomTypeDef(d: YamlMd) =
    customTypeDefPattern.findFirstIn(d.body).isDefined
  private[in] def isTableDef(d: YamlMd) =
    tableDefPattern.findFirstIn(d.body).isDefined
  private[in] def isViewDef(d: YamlMd) = !isTableDef(d) && !isCustomTypeDef(d) &&
    (hasFieldsPattern.findFirstIn(d.body).isDefined ||
     hasExtendsPattern.findFirstIn(d.body).isDefined)
  def fromFile(file: File) =
    new FileMdSource(file).defs
  def fromFiles(
    path: String,
    filter: (File) => Boolean = _.getName endsWith ".yaml") =
    new FilesMdSource(path, filter).defs
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

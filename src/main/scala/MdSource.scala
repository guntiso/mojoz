package mojoz.metadata.in

import java.io.File
import scala.collection.immutable.Seq
import scala.io.Codec
import scala.io.Source

case class YamlMd(
  filename: String,
  line: Int,
  body: String)

private[in] trait MdSource {
  def split(mdDefs: Seq[YamlMd]) = {
    // TODO set line numbers while splitting
    def split(s: String) = s.split("((\\-\\-\\-)|(\\r?\\n[\\r\\n]+))+").toSeq
    mdDefs.map { d =>
      split(d.body)
        .map(_.trim)
        .filter(_.length > 0)
        .map((d, _))
    }.flatMap(x => x)
      .map(x => x._1.copy(body = x._2))
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
  private def recursiveListFiles(f: File): Array[File] = {
    val these = Option(f.listFiles) getOrElse Array()
    these.filter(!_.isDirectory) ++
      these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  private def typedefFiles =
    recursiveListFiles(new File(path)).toList.filter(filter)
  override def defSets = typedefFiles.map(f => YamlMd(f.getName, 0,
    Source.fromFile(f).mkString))
}

private[in] class ResourcesMdSource(
  val indexPath: String,
  val nameFilter: (String) => Boolean,
  val nameMap: (String) => String) extends MdSource {
  // getClass.getClassLoader.getResources("") does not work from jar :(
  private def typedefResources =
    Option(getClass.getResourceAsStream(indexPath))
      .map(Source.fromInputStream(_)(Codec("UTF-8"))
        .getLines.toList).getOrElse(Nil)
      .filter(nameFilter).map(nameMap).toSet.toList
  override def defSets = typedefResources.map(r => YamlMd(r, 0,
    Source.fromInputStream(getClass.getResourceAsStream(r))("UTF-8").mkString))
}

object YamlMd {
  private val customTypeDefPattern = "(^|\\n)\\s*type\\s*:".r    // XXX
  private val tableDefPattern      = "(^|\\n)\\s*columns\\s*:".r // XXX
  private[in] def isCustomTypeDef(d: YamlMd) =
    customTypeDefPattern.findFirstIn(d.body).isDefined
  private[in] def isTableDef(d: YamlMd) =
    tableDefPattern.findFirstIn(d.body).isDefined
  private[in] def isViewDef(d: YamlMd) = !isTableDef(d) && !isCustomTypeDef(d)
  def fromFile(file: File) =
    new FileMdSource(file).defs
  def fromFiles(
    path: String,
    filter: (File) => Boolean = _.getName endsWith ".yaml") =
    new FilesMdSource(path, filter).defs
  def fromResources(
    indexPath: String = "/-md-files.txt",
    nameFilter: (String) => Boolean = _ endsWith ".yaml",
    nameMap: (String) => String = "/" + _) =
    new ResourcesMdSource(indexPath, nameFilter, nameMap).defs
}

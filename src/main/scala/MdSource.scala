package mojoz.metadata.in

import java.io.File
import scala.io.Codec
import scala.io.Source

case class MdDef(
  filename: String,
  line: Int,
  body: String)

private[in] object MdSource {
  def split(mdDefs: Seq[MdDef]) = {
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
  private val tableDefPattern = "\\ncolumns\\s*:".r // XXX
  def isTableDef(d: MdDef) = tableDefPattern.findFirstIn(d.body).isDefined
  def isViewDef(d: MdDef) = !isTableDef(d)
  def getTableDefs(mdDefs: Seq[MdDef]) = split(mdDefs).filter(isTableDef)
  def getViewDefs(mdDefs: Seq[MdDef]) = split(mdDefs).filter(isViewDef)
}

class FilesMdSource(
  val path: String,
  val filter: (File) => Boolean = _.getName endsWith ".yaml") {
  require(path != null)
  private def recursiveListFiles(f: File): Array[File] = {
    val these = Option(f.listFiles) getOrElse Array()
    these.filter(!_.isDirectory) ++
      these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  private def typedefFiles =
    recursiveListFiles(new File(path)).toSeq.filter(filter)
  private def defSets = typedefFiles.map(f => MdDef(f.getName, 0,
    Source.fromFile(f).mkString))
  def getRawTableDefs = MdSource.getTableDefs(defSets)
  def getRawViewDefs = MdSource.getViewDefs(defSets)
}

class ResourcesMdSource(
  val indexPath: String = "/-md-files.txt",
  val nameFilter: (String) => Boolean = _.endsWith(".yaml"),
  val nameMap: (String) => String = "/" + _) {
  // getClass.getClassLoader.getResources("") does not work from jar :(
  private def typedefResources =
    Option(getClass.getResourceAsStream(indexPath))
      .map(Source.fromInputStream(_)(Codec("UTF-8"))
        .getLines.toList).getOrElse(Nil)
      .filter(nameFilter).map(nameMap).toSet.toSeq
  private def defSets = typedefResources.map(r => MdDef(r, 0,
    Source.fromInputStream(getClass.getResourceAsStream(r))("UTF-8").mkString))
  def getRawTableDefs = MdSource.getTableDefs(defSets)
  def getRawViewDefs = MdSource.getViewDefs(defSets)
}

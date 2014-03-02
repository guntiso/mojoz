package metadata

import java.io.File
import scala.io.Source

case class MdDef(
  filename: String,
  line: Int,
  body: String)

trait RawTableDefSource {
  def getRawTableDefs: Seq[MdDef]
}

trait RawViewDefSource {
  def getRawViewDefs: Seq[MdDef]
}

object MdSource {
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

trait FilesMdSource extends RawTableDefSource with RawViewDefSource {
  def path: String = null
  def filter: (File) => Boolean = _.getName endsWith ".yaml"
  def recursiveListFiles(f: File): Array[File] = {
    val these = Option(f.listFiles) getOrElse Array()
    these.filter(!_.isDirectory) ++
      these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def typedefFiles =
    if (path == null) sys.error("FilesMdSource path not initialized")
    else recursiveListFiles(new File(path)).toSeq.filter(filter)
  def defSets = typedefFiles.map(f => MdDef(f.getName, 0,
    Source.fromFile(f).mkString))
  override def getRawTableDefs = MdSource.getTableDefs(defSets)
  override def getRawViewDefs = MdSource.getViewDefs(defSets)
}

trait ResourcesMdSource extends RawTableDefSource with RawViewDefSource {
  def indexPath: String = "/-md-files.txt"
  def nameFilter: (String) => Boolean = _.endsWith(".yaml")
  def nameMap: (String) => String = "/" + _
  // getClass.getClassLoader.getResources("") does not work from jar :(
  def typedefResources =
    Option(getClass.getResourceAsStream(indexPath))
      .map(Source.fromInputStream(_)(io.Codec("UTF-8"))
        .getLines.toList).getOrElse(Nil)
      .filter(nameFilter).map(nameMap).toSet.toSeq
  def defSets = typedefResources.map(r => MdDef(r, 0,
    Source.fromInputStream(getClass.getResourceAsStream(r))("UTF-8").mkString))
  override def getRawTableDefs = MdSource.getTableDefs(defSets)
  override def getRawViewDefs = MdSource.getViewDefs(defSets)
}

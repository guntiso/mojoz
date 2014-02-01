package metadata

import java.io.File
import scala.io.Source

case class MdDef(
  filename: String,
  line: Int,
  body: String)

trait MdSource {
  def getMdDefs: Seq[MdDef]
}

object MdFileSplitter {
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
}

trait FilesMdSource extends MdSource {
  def path: String
  def filter: (File) => Boolean
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  def typedefFiles = recursiveListFiles(new File(path)).toSeq.filter(filter)
  def defSets = typedefFiles.map(f => MdDef(f.getName, 0,
    Source.fromFile(f).mkString))
  override def getMdDefs = MdFileSplitter.split(defSets)
}

trait ResourcesMdSource extends MdSource {
  def indexPath: String
  def nameFilter: (String) => Boolean
  def nameMap: (String) => String = "/" + _
  // getClass.getClassLoader.getResources("") does not work from jar :(
  def typedefResources =
    Option(getClass.getResourceAsStream(indexPath))
      .map(Source.fromInputStream(_)(io.Codec("UTF-8"))
        .getLines.toList).getOrElse(Nil)
      .filter(nameFilter).map(nameMap).toSet.toSeq
  def defSets = typedefResources.map(r => MdDef(r, 0,
    Source.fromInputStream(getClass.getResourceAsStream(r))("UTF-8").mkString))
  override def getMdDefs = MdFileSplitter.split(defSets)
}

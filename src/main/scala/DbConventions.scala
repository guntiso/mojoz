package mojoz.metadata

import xsdgen.ElementName

object DbConventions {
  def dbNameToXsdName(dbName: String) = {
    val parts = dbName.split("[_\\.]+")
    parts.toList.map(_.toLowerCase match {
      case "usr" => "user"
      case "grp" => "group"
      case "rle" => "role"
      case x => x
    }).map(_.capitalize).mkString
  }
  def xsdNameToDbName(xsdName: String) = {
    val parts = ElementName.get(xsdName).split("[\\-\\_]")
    val hadPrefix = xsdName.startsWith("_")
    val hadSuffix = xsdName.endsWith("_")
    val clean = parts.toList.map(_.toLowerCase match {
      case "user" => "usr"
      case "group" => "grp"
      case "role" => "rle"
      case x => x
    }).mkString("_").replace("__", "_")
      .replace("_1", "1") // no underscore before 1 in our database names
      .replace("_2", "2") // no underscore before 2 in our database names
    (hadPrefix, hadSuffix) match {
      case (true, true) => "_" + clean + "_"
      case (true, false) => "_" + clean
      case (false, true) => clean + "_"
      case (false, false) => clean
    }
  }
}

package metadata

import xsdgen.ElementName

object DbConventions {
  def dbNameToXsdName(dbName: String) = {
    dbName.split("[_\\.]+").map(_.toLowerCase match {
      case "usr" => "user"
      case "grp" => "group"
      case "rle" => "role"
      case x => x
    }).map(_.capitalize) mkString
  }
  def xsdNameToDbName(xsdName: String) = {
    ElementName.get(xsdName).split("[\\-\\_]").map(_.toLowerCase match {
      case "user" => "usr"
      case "group" => "grp"
      case "role" => "rle"
      case x => x
    }).mkString("_").replace("__", "_")
      .replace("_1", "1") // no underscore before 1 in our database names
      .replace("_2", "2") // no underscore before 2 in our database names
  }
}

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
    }).mkString("_") match {
      case x if x endsWith "_2" => x.replace("_2", "2") // XXX dirty fix phone_2
      case x if x contains "_1_" => x.replace("_1_", "1") // XXX dirty fix
      case x if x contains "_2_" => x.replace("_2_", "2") // XXX dirty fix
      case x => x
    }
  }
}

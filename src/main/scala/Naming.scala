package mojoz.metadata

object Naming {
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
    val parts = dasherize(xsdName).split("[\\-\\_]")
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
      .replace("_3", "3") // no underscore before 3 in our database names
    (hadPrefix, hadSuffix) match {
      case (true, true) => "_" + clean + "_"
      case (true, false) => "_" + clean
      case (false, true) => clean + "_"
      case (false, false) => clean
    }
  }

  def dasherize(name: String) = {
    val (upper, digit, other) = (1, 2, 3)
    val buf = new StringBuilder(name.length() * 2)
    var charType = 0
    var prevCharType = 0
    for (i <- 0 to (name.length - 1)) {
      val c = name.charAt(i)
      if (Character.isUpperCase(c)) charType = upper
      else if (Character.isDigit(c)) charType = digit
      else charType = other
      if (i > 0
        && charType != prevCharType
        && !(prevCharType == upper && charType == other)) {
        buf.append('-')
      }
      if (charType == upper) buf.append(Character.toLowerCase(c))
      else buf.append(c)
      prevCharType = charType
    }
    buf.toString
  }
}

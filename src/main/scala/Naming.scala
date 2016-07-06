package mojoz.metadata

object Naming {
  def dbNameToXsdName(dbName: String) = {
    val parts = dbName.split("[_\\.]+")
    parts.toList
      .map(_.toLowerCase)
      .map(_.capitalize)
      .mkString
  }
  def dbName(name: String) = {
    val parts = dasherize(name).split("[\\-\\_]")
    val hadPrefix = name.startsWith("_")
    val hadSuffix = name.endsWith("_")
    val clean = parts.toList
      .map(_.toLowerCase)
      .mkString("_")
      .replace("__", "_")
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

package mojoz.metadata

object Naming {
  def camelize(name: String) = {
    val parts = name.split("[_\\-\\.]+")
    parts.toList
      .map(_.toLowerCase)
      .map(_.capitalize)
      .mkString
  }
  def camelizeLower(name: String) = camelize(name) match {
    case x if x.length == 1 || (x.length > 1 && (x(1).isLower || x(1).isDigit)) =>
      s"${x(0).toLower}${x.substring(1)}"
    case x => x
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

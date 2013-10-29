package metadata

import scala.xml.PrettyPrinter

import metadata.DbConventions.{ dbNameToXsdName => xsdName }

object ViewDefVisualizer {
  def getType(s: String) = YamlViewDefLoader.nameToExtendedViewDef(s)
  def printComplexType(indent: String, typeDef: XsdTypeDef) {
    val nextIndent = indent + "  "
    typeDef.fields.foreach(f => {
      println(nextIndent + (xsdName(Option(f.alias) getOrElse f.name)))
      if (f.isComplexType)
        printComplexType(nextIndent, getType(f.xsdType.name))
    })
  }
  def print(typeName: String) = {
    val t = getType(typeName)
    println(xsdName(t.name))
    printComplexType("", t)
  }
}

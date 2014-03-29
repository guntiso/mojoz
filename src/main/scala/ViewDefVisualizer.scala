package mojoz.metadata.out

import scala.xml.PrettyPrinter

import mojoz.metadata._
import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }

trait ViewDefVisualizer { this: ViewDefSource =>
  def getType(s: String) = nameToExtendedViewDef(s)
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

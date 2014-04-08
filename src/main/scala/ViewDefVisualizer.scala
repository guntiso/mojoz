package mojoz.metadata.out

import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }
import mojoz.metadata.Metadata
import mojoz.metadata.XsdTypeDef

class ViewDefVisualizer(metadata: Metadata) {
  private def getType(s: String) = metadata.extendedViewDef(s)
  private def printComplexType(indent: String, typeDef: XsdTypeDef) {
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

package mojoz.metadata.out

import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }
import mojoz.metadata.Metadata
import mojoz.metadata.ViewDef
import mojoz.metadata.XsdType

class ViewDefVisualizer(metadata: Metadata[XsdType]) {
  private def getType(s: String) = metadata.extendedViewDef(s)
  private def printComplexType(indent: String, typeDef: ViewDef[XsdType]) {
    val nextIndent = indent + "  "
    typeDef.fields.foreach(f => {
      println(nextIndent + (xsdName(Option(f.alias) getOrElse f.name)))
      if (f.type_ != null && f.type_.isComplexType)
        printComplexType(nextIndent, getType(f.type_.name))
    })
  }
  def print(typeName: String) = {
    val t = getType(typeName)
    println(xsdName(t.name))
    printComplexType("", t)
  }
}

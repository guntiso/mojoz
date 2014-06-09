package mojoz.metadata.out

import mojoz.metadata.DbConventions.{ dbNameToXsdName => xsdName }
import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.Metadata
import mojoz.metadata.Type
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }

class ViewDefVisualizer(metadata: Metadata[Type]) {
  private def getType(s: String) = metadata.extendedViewDef(s)
  private def printComplexType(indent: String, typeDef: ViewDef[FieldDef[Type]]) {
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

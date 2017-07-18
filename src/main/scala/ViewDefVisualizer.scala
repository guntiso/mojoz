package mojoz.metadata.out

import mojoz.metadata.FieldDef.{ FieldDefBase => FieldDef }
import mojoz.metadata.Type
import mojoz.metadata.ViewDef.{ ViewDefBase => ViewDef }

class ViewDefVisualizer(nameToExtendedViewDef: Map[String, ViewDef[FieldDef[Type]]]) {
  private def getType(s: String) = nameToExtendedViewDef(s)
  private def printComplexType(indent: String, typeDef: ViewDef[FieldDef[Type]]) {
    val nextIndent = indent + "  "
    typeDef.fields.foreach(f => {
      println(nextIndent + (Option(f.alias) getOrElse f.name))
      if (f.type_ != null && f.type_.isComplexType)
        printComplexType(nextIndent, getType(f.type_.name))
    })
  }
  def print(typeName: String) = {
    val t = getType(typeName)
    println(t.name)
    printComplexType("", t)
  }
}

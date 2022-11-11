package org.mojoz.metadata

import scala.collection.immutable.Seq

package object in {
  case class Join(alias: String, table: String, isOuterJoin: Boolean, columns: Seq[ColumnDef])
  /** db and basetable and joins to parsed joins */
  type JoinsParser = (String, String, Seq[String]) => Seq[Join]
}

package object io {
  type SimplePatternMdConventions = MdConventions.SimplePatternMdConventions
}

package mojoz.metadata

import java.io.File
import scala.io.Codec
import scala.io.Source
import scala.math.max
import scala.annotation.tailrec

trait TableDefSource {
  val entities: Seq[TableDef]
}

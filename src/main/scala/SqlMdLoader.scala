package mojoz.metadata

import java.io.File
import scala.io.Codec
import scala.io.Source
import scala.math.max
import scala.annotation.tailrec

trait TableDefSource {
  val entities: Seq[TableDef]
}

package io {

case class DbTableDef(
  name: String,
  comment: String,
  cols: Seq[DbColumnDef],
  pk: Option[DbIndex],
  uk: Seq[DbIndex],
  idx: Seq[DbIndex],
  refs: Seq[Ref])
case class DbColumnDef(
  name: String,
  dbType: String,
  nullable: Boolean,
  dbDefault: String,
  check: String,
  comment: String)

}

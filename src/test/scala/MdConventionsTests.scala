import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.Seq

import org.mojoz.metadata.ColumnDef
import org.mojoz.metadata.TableDef
import org.mojoz.metadata.TableDef
import org.mojoz.metadata.TableMetadata.Ref
import org.mojoz.metadata.Type
import org.mojoz.metadata.io.MdConventions

class MdConventionsTests extends FlatSpec with Matchers {
  def ref(col: String, refTable: String, refCol: String) =
    Ref(null, Seq(col), refTable, Seq(refCol), null, null, null, null)
  def tableDef(db: String, tableName: String, col: String, ref: Ref) =
    TableDef(db, tableName, null, Seq(colDef(col)), None, Nil, Nil, Nil, Seq(ref), Map.empty)
  def colDef(colName: String) =
    ColumnDef(colName, new Type("test"), true, null, null, null, Map.empty)
  "metadata conventions" should "encode ref cols properly" in {
    val tests = List(
      "table, col, refTable, refCol -> col, refTable.refCol",
      "table, reftab_refcol, reftab, refcol -> reftab.refcol",
      "t, person_id, person, id -> person.id",
      "t, mother_id, person, id -> mother.id, person.id",
      "t, col_blah, rt, blah -> col.blah, rt.blah",
      //sch.table,col_blah, sch.rt, blah -> col.blah,     rt.blah",   // de-optimized
      "sch.table, col_blah, sch.rt, blah -> col.blah, sch.rt.blah",
      "sch1.table, col_blah, sch2.rt, blah -> col.blah, sch2.rt.blah",
      "sch1.table, col_blax, sch2.rt, blah -> col_blax, sch2.rt.blah",
      "table, col_baxy_blah, rt, baxy_blah -> col.baxy_blah, rt.baxy_blah",
      // "s.table, reftab_refcol, s.reftab, refcol -> reftab.refcol", // de-optimized
      "s1.table, reftab_refcol, s2.reftab, refcol -> reftab.refcol, s2.reftab.refcol",
      "s1.table, s2_reftab_refcol, s2.reftab, refcol -> s2.reftab.refcol",
      "table, s2_reftab_refcol, s2.reftab, refcol -> s2.reftab.refcol")
      .map(_.split("->"))
      .map(a => (a(0).split(",").toList.map(_.trim),
        a(1).split(",").toList.map(_.trim)))
    val db = null
    tests foreach {
      case (d @ List(tab, col, refTab, refCol), List(ccol)) =>
        val td = tableDef(db, tab, col, ref(col, refTab, refCol))
        val cd = MdConventions.toExternal(td, td.cols(0))
        val expected = (d, ccol, None)
        val produced = (d, cd.name, cd.type_.type_)
        expected should be(produced)
      case (d @ List(tab, col, refTab, refCol), List(ccol, refType)) =>
        val td = tableDef(db, tab, col, ref(col, refTab, refCol))
        val cd = MdConventions.toExternal(td, td.cols(0))
        val expected = (d, ccol, Some(refType))
        val produced = (d, cd.name, cd.type_.type_.map(_.name))
        expected should be(produced)
      case x => sys.error("Unexpected test format: " + x)
    }
  }
}

package mojoz.metadata.out

import scala.annotation.tailrec
import scala.math.max
import mojoz.metadata._
import mojoz.metadata.io._
import mojoz.metadata.TableDef._
import SqlWriter._

object SqlWriter {

trait ConstraintNamingRules {
  def pkName(tableName: String): String
  def ukName(tableName: String, uk: DbIndex): String
  def idxName(tableName: String, idx: DbIndex): String
  def fkName(tableName: String, ref: Ref): String
}

class SimpleConstraintNamingRules extends ConstraintNamingRules {
  val maxNameLen = 60
  val pkPrefix = "pk_"
  val pkSuffix = ""
  val ukPrefix = "uk_"
  val ukSuffix = ""
  val ukTableNameSep = "_"
  val ukColNameSep = "_"
  val idxPrefix = "idx_"
  val idxSuffix = ""
  val idxTableNameSep = "_"
  val idxColNameSep = "_"
  val fkPrefix = "fk_"
  val fkSuffix = ""
  val fkTableNameSep = "_"
  val fkColNameSep = "_"
  def pkUsableLen =
    maxNameLen - pkPrefix.length - pkSuffix.length
  def usableLen(prefix: String, sep: String, suffix: String) =
    maxNameLen - prefix.length - sep.length - suffix.length
  def shorten(s: String, maxLen: Int) = {
    if (s.length <= maxLen) s
    else if (maxLen < 4) s.substring(0, maxLen)
    else {
      val leftS = s.substring(0, maxLen / 2) match {
        case s if s endsWith "_" => s.substring(0, s.length - 1)
        case s => s
      }
      val rightS = s.substring(s.length + leftS.length + 1 - maxLen) match {
        case s if s startsWith "_" => s substring 1
        case s => s
      }
      leftS + "_" + rightS
    }
  }
  def makeName(prefix: String, leftName: String, sep: String,
    rightName: String, suffix: String) = {
    val usable = usableLen(prefix, sep, suffix)
    val lNameLen = usable / 2
    val rNameLen = usable - lNameLen
    val lnRaw = leftName
    val rnRaw = rightName
    val rawLen =
      prefix.length + lnRaw.length + sep.length + rnRaw.length + suffix.length
    val isOverLen = lnRaw.length + rnRaw.length > usable
    val lnOverLen = lnRaw.length > lNameLen
    val rnOverLen = rnRaw.length > rNameLen
    val (ln, rn) = (isOverLen, lnOverLen, rnOverLen) match {
      case (false, _, _) => (lnRaw, rnRaw)
      case (_, _, true) =>
        val ln = shorten(lnRaw, lNameLen)
        val rn = shorten(rnRaw, usable - ln.length)
        (ln, rn)
      case _ => (shorten(lnRaw, usable - rnRaw.length), rnRaw)
    }
    s"$prefix$ln$sep$rn$suffix"
  }
  private def unprefix(s: String) =
    if (s.indexOf(".") < 0) s else s.substring(s.lastIndexOf(".") + 1)
  private def colsToName(cols: Seq[String], sep: String) =
    cols.map(_.split("\\s+")(0)).mkString(sep)
  override def pkName(tableName: String) =
    pkPrefix + shorten(unprefix(tableName), pkUsableLen) + pkSuffix
  override def ukName(tableName: String, uk: DbIndex) = makeName(ukPrefix,
    unprefix(tableName), ukTableNameSep, colsToName(uk.cols, ukColNameSep), ukSuffix)
  override def idxName(tableName: String, idx: DbIndex) = makeName(idxPrefix,
    unprefix(tableName), idxTableNameSep, colsToName(idx.cols, idxColNameSep), idxSuffix)
  override def fkName(tableName: String, r: Ref) = makeName(fkPrefix,
    unprefix(tableName), fkTableNameSep,
    colsToName(r.cols, fkColNameSep), fkSuffix)
  }

class OracleConstraintNamingRules extends SimpleConstraintNamingRules {
  override val maxNameLen = 30
}

def apply(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new StandardSqlWriter(constraintNamingRules)
def h2(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new HsqldbSqlWriter(constraintNamingRules)
def hsqldb(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new HsqldbSqlWriter(constraintNamingRules)
def oracle(constraintNamingRules: ConstraintNamingRules = new OracleConstraintNamingRules): SqlWriter =
  new OracleSqlWriter(constraintNamingRules)
def postgresql(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new PostgreSqlWriter(constraintNamingRules)

}

trait SqlWriter { this: ConstraintNamingRules =>
  private[out] def idxCols(cols: Seq[String]) = cols.map(c =>
    if (c.toLowerCase endsWith " asc") c.substring(0, c.length - 4) else c)
  def schema(tables: Seq[TableDef[Type]]) = List(
    tables.map(tableAndCommentsAndIndexes).mkString("\n\n"),
    foreignKeys(tables).mkString("\n")).mkString("\n\n") +
    (if (tables.size > 0) "\n" else "")
  def tableAndCommentsAndIndexes(t: TableDef[Type]): String =
    List[Iterable[String]](
      Some(table(t)), tableComment(t), columnComments(t),
      uniqueIndexes(t), indexes(t))
      .flatten.mkString("\n")
  def table(t: TableDef[Type]) =
    (t.cols.map(column) ++ primaryKey(t))
      .mkString("create table " + t.name + "(\n  ", ",\n  ", "\n);")
  def primaryKey(t: TableDef[_]) = t.pk map { pk =>
    "constraint " + Option(pk.name).getOrElse(pkName(t.name)) +
      " primary key (" + idxCols(pk.cols).mkString(", ") + ")"
  }
  def uniqueIndexes(t: TableDef[_]) = t.uk map { uk =>
    "create unique index " + Option(uk.name).getOrElse(ukName(t.name, uk)) +
      s" on ${t.name}(${idxCols(uk.cols).mkString(", ")});"
  }
  def indexes(t: TableDef[_]) = t.idx map { idx =>
    "create index " + Option(idx.name).getOrElse(idxName(t.name, idx)) +
      s" on ${t.name}(${idxCols(idx.cols).mkString(", ")});"
  }
  private def column(c: ColumnDef[Type]) = {
    // XXX FIXME!!! what if db default is function name?
    val dbDefault = (c.type_.name, c.dbDefault) match {
      case (_, null) => null
      case ("string", d) if !(d startsWith ".") => s"'$d'"
      case (_, d) => d
    }
    c.name + " " + dbType(c) +
      (if (dbDefault == null) "" else " default " + dbDefault) +
      (if (c.nullable || c.name == "id") "" else " not null") + //XXX name != id
      check(c)
  }
  def tableComment(t: TableDef[_]) = Option(t.comments).filter(_ != "").map(c =>
    s"comment on table ${t.name} is '$c';")
  def columnComments(t: TableDef[_]) =
    t.cols.filter(_.comments != null).filter(_.comments != "").map(c =>
      s"comment on column ${t.name}.${c.name} is '${c.comments}';")
  def foreignKeys(tables: Seq[TableDef[_]]) = tables.map { t =>
    t.refs map foreignKey(t)
  }.flatten
  def foreignKey(t: TableDef[_])(r: TableDef.Ref) =
    s"alter table ${t.name} add constraint ${
      Option(r.name) getOrElse fkName(t.name, r)
    } foreign key (${
      r.cols mkString ", "
    }) references ${r.refTable}(${r.refCols mkString ", "})" +
      Option(r.onDeleteAction).map(" on delete " + _).getOrElse("") +
      Option(r.onUpdateAction).map(" on update " + _).getOrElse("") +
      ";"
  def dbType(c: ColumnDef[Type]): String
  def check(c: ColumnDef[Type]): String
}

private[out] class HsqldbSqlWriter(
  constraintNamingRules: ConstraintNamingRules)
  extends StandardSqlWriter(constraintNamingRules) {
  // drop "desc" keyword - hsqldb ignores it, fails metadata roundtrip test
  override def idxCols(cols: Seq[String]) = super.idxCols(cols.map(c =>
    if (c.toLowerCase endsWith " desc") c.substring(0, c.length - 5) else c))
}

private[out] class OracleSqlWriter(
  constraintNamingRules: ConstraintNamingRules)
  extends StandardSqlWriter(constraintNamingRules) {
  override def dbType(c: ColumnDef[Type]) = {
    val xt = c.type_
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    xt.name match {
      case "long" =>
        "numeric(" + (xt.totalDigits getOrElse 18) + ")"
      case "int" =>
        "numeric(" + (xt.totalDigits getOrElse 9) + ")"
      case "string" =>
        if (xt.length.isDefined && xt.length.get <= 4000)
          "varchar2" + s"(${xt.length.get} char)"
        else "clob"
      case "boolean" => "char"
      case _ => super.dbType(c)
    }
  }
  override def check(c: ColumnDef[Type]) = {
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    c.type_.name match {
      case "boolean" => " check (" + dbColumnName + " in ('N','Y'))"
      // TODO do not add enum to col, or you will get uninformative msg from ora,
      // like: ORA-02290: check constraint (KPS.SYS_C0090768) violated
      case _ => super.check(c)
    }
  }
  override def foreignKey(t: TableDef[_])(r: TableDef.Ref) =
    // oracle does not have on update rule
    super.foreignKey(t)(
      if (r.onUpdateAction != null) r.copy(onUpdateAction = null) else r)
}

private[out] class StandardSqlWriter(
  constraintNamingRules: ConstraintNamingRules) extends SqlWriter with ConstraintNamingRules {
  override def pkName(tableName: String) =
    constraintNamingRules.pkName(tableName)
  override def ukName(tableName: String, uk: DbIndex) =
    constraintNamingRules.ukName(tableName, uk)
  override def idxName(tableName: String, idx: DbIndex) =
    constraintNamingRules.idxName(tableName, idx)
  override def fkName(tableName: String, ref: Ref) =
    constraintNamingRules.fkName(tableName, ref)
  override def dbType(c: ColumnDef[Type]) = {
    val xt = c.type_
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    xt.name match {
      case "integer" =>
        "numeric" + xt.totalDigits.map(l => s"($l)").getOrElse("")
      case "long" => xt.totalDigits match {
        case None => "bigint"
        case Some(d) => s"numeric($d)"
      }
      case "int" => xt.totalDigits match {
        case None => "integer"
        case Some(d) => s"numeric($d)"
      }
      case "decimal" =>
        "numeric" + ((xt.totalDigits, xt.fractionDigits) match {
          case (None, None) => ""
          case (Some(t), None) => s"($t)"
          case (None, Some(f)) => s"(*, $f)"
          case (Some(t), Some(f)) => s"($t, $f)"
        })
      case "date" => "date"
      case "dateTime" => "timestamp"
      case "string" =>
        xt.length.map(l => s"varchar($l)") getOrElse ("clob")
      case "boolean" => "boolean"
      case "base64Binary" => "blob"
      case x =>
        throw new RuntimeException("Unexpected type: " + xt + " for " + c.name)
    }
  }
  override def check(c: ColumnDef[Type]) = {
    val xt = c.type_
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    xt.name match {
      case "string" if c.enum != null =>
        c.enum.map("'" + _ + "'")
          .mkString(" check (" + dbColumnName + " in (", ", ", "))")
      case _ => ""
    }
  }
}

private[out] class PostgreSqlWriter(
  constraintNamingRules: ConstraintNamingRules)
  extends StandardSqlWriter(constraintNamingRules) {
  override def dbType(c: ColumnDef[Type]) = {
    c.type_.name match {
      case "boolean" => "bool"
      case "base64Binary" => "bytea"
      case "string" =>
        "varchar" + c.type_.length.map(l => s"($l)").getOrElse("")
      case x => super.dbType(c)
    }
  }
}

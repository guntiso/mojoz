package org.mojoz.metadata.out

import scala.annotation.tailrec
import scala.math.max
import org.mojoz.metadata._
import org.mojoz.metadata.io._
import org.mojoz.metadata.TableMetadata._
import scala.collection.immutable.{ Map, Seq }
import DdlGenerator._

object DdlGenerator {

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

def apply(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): DdlGenerator =
  new StandardSqlDdlGenerator(constraintNamingRules, typeDefs)
def h2(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): DdlGenerator =
  new H2DdlGenerator(constraintNamingRules, typeDefs)
def hsqldb(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): DdlGenerator =
  new HsqldbDdlGenerator(constraintNamingRules, typeDefs)
def oracle(constraintNamingRules: ConstraintNamingRules = new OracleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): DdlGenerator =
  new OracleDdlGenerator(constraintNamingRules, typeDefs)
def postgresql(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): DdlGenerator =
  new PostgreDdlGenerator(constraintNamingRules, typeDefs)

}

abstract class DdlGenerator(typeDefs: Seq[TypeDef]) { this: ConstraintNamingRules =>
  private[out] def idxCols(cols: Seq[String]) = cols.map(c =>
    if (c.toLowerCase endsWith " asc") c.substring(0, c.length - 4) else c)
  /** Returns full sql schema string (tables, comments, keys, indices, refs)
    */
  def schema(tables: Seq[TableDef]) = List(
    tablesAndComments(tables),
    keysAndIndexes(tables),
    foreignKeys(tables)).filter(_ != "").mkString("\n\n") +
    (if (tables.size > 0) "\n" else "")
  def tableAndComments(t: TableDef): String =
    List[Iterable[String]](
      Some(table(t)), tableComment(t), columnComments(t))
      .flatten.mkString("\n")
  def tablesAndComments(tables: Seq[TableDef]): String =
    tables.map(tableAndComments).mkString("\n\n")
  def keysAndIndexes(t: TableDef): String =
    List[Iterable[String]](
      primaryKey(t), uniqueIndexes(t), indexes(t))
      .flatten.mkString("\n")
  def keysAndIndexes(tables: Seq[TableDef]): String =
    tables.map(keysAndIndexes).filter(_ != "").mkString("\n\n")
  def tableAndCommentsAndIndexes(t: TableDef): String =
    (tableAndComments(t) + keysAndIndexes(t)).trim
  def table(t: TableDef) =
    // pk separated from table definition to fit large data imports etc.
    (t.cols.map(column(t)) ++ tableChecks(t)) // ++ primaryKey(t))
      .mkString("create table " + t.name + "(\n  ", ",\n  ", "\n);")
  def primaryKey(t: TableDef_[_]) = t.pk map { pk =>
    "alter table " + t.name + " add " +
    "constraint " + Option(pk.name).getOrElse(pkName(t.name)) +
      " primary key (" + idxCols(pk.cols).mkString(", ") + ");"
  }
  def uniqueIndex(t: TableDef_[_])(uk: DbIndex) =
    s"create unique index ${
      Option(uk.name).getOrElse(ukName(t.name, uk))
    } on ${t.name}(${idxCols(uk.cols).mkString(", ")});"
  def uniqueKey(t: TableDef_[_])(uk: DbIndex) =
    s"alter table ${t.name} add constraint ${
      Option(uk.name).getOrElse(ukName(t.name, uk))
    } unique(${idxCols(uk.cols).mkString(", ")});"
  def uniqueIndexes(t: TableDef_[_]) = t.uk map { uk =>
    if (uk.cols.exists(_.toLowerCase endsWith " desc")) uniqueIndex(t)(uk)
    // on some dbs, unique constraint (not index) is required to add fk
    else uniqueKey(t)(uk)
  }
  def indexes(t: TableDef_[_]) = t.idx map { idx =>
    "create index " + Option(idx.name).getOrElse(idxName(t.name, idx)) +
      s" on ${t.name}(${idxCols(idx.cols).mkString(", ")});"
  }
  def dbDefault(c: ColumnDef) = c.dbDefault
  private def column(t: TableDef_[_])(c: ColumnDef) = {
    c.name + " " + dbType(c) +
      (dbDefault(c) match { case null => "" case d => s" default $d" }) +
      (if (explicitNotNullForColumn(t, c)) " not null" else "") +
      colCheck(c)
  }
  private[out] def explicitNotNullForColumn(t: TableDef_[_], c: ColumnDef) =
    !c.nullable && !t.pk.exists(_.cols.contains(c.name))
  def tableComment(t: TableDef_[_]) = Option(t.comments).map(c =>
    s"comment on table ${t.name} is '${c.replace("'", "''")}';")
  def columnComments(t: TableDef_[ColumnDef_[_]]) =
    t.cols.filter(_.comments != null).map(c =>
      s"comment on column ${t.name}.${c.name} is '${c.comments.replace("'", "''")}';")
  def foreignKeys(tables: Seq[TableDef_[_]]) = tables.map { t =>
    t.refs map foreignKey(t.name)
  }.flatten.mkString("\n")
  def foreignKey(tableName: String)(r: TableMetadata.Ref) =
    s"alter table $tableName add constraint ${
      Option(r.name) getOrElse fkName(tableName, r)
    } foreign key (${
      r.cols mkString ", "
    }) references ${r.refTable}(${r.refCols mkString ", "})" +
      Option(r.onDeleteAction).map(" on delete " + _).getOrElse("") +
      Option(r.onUpdateAction).map(" on update " + _).getOrElse("") +
      ";"
  val ddlWriteInfoKey = "sql"
  lazy val typeNameToSqlWriteInfoSeq: Map[String, Seq[DdlWriteInfo]] =
    typeDefs.map(td =>
      td.name ->
        td.ddlWrite.get(ddlWriteInfoKey).orElse(td.ddlWrite.get("sql")).getOrElse(Nil)
    ).toMap
  def dbType(c: ColumnDef): String = {
    val t = c.type_
    typeNameToSqlWriteInfoSeq.get(t.name).getOrElse(Nil).find { info =>
      sizeOptionMatch(info.minSize, info.maxSize, t.length.orElse(t.totalDigits)) &&
      sizeOptionMatch(info.minFractionDigits, info.maxFractionDigits, t.fractionDigits)
    }.map { info =>
      (t.length.orElse(t.totalDigits), t.fractionDigits) match {
        case (None, _) => info.targetNamePattern
        case (Some(size), None) => info.targetNamePattern.replace("size", "" + size)
        case (Some(size), Some(frac)) =>
          info.targetNamePattern.replace("size", "" + size).replace("frac", "" + frac)
      }
    }.getOrElse {
      sys.error(s"Missing sql info (key '$ddlWriteInfoKey' or 'sql') for type $t in ${c.name}")
    }
  }
  def colCheck(c: ColumnDef): String
  def tableChecks(t: TableDef): Seq[String] =
    t.ck.map(ck =>
      if (ck.name != null) s"constraint ${ck.name} check (${ck.expression})"
      else s"check (${ck.expression})")
  private def sizeOptionMatch(min: Option[Int], max: Option[Int], value: Option[Int]) =
    min.isEmpty && value.isEmpty ||
      min.isDefined && value.isDefined && min.get <= value.get &&
      max.map(_ >= value.get).getOrElse(true)
}

private[out] class HsqldbDdlGenerator(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends StandardSqlDdlGenerator(constraintNamingRules, typeDefs) {
  override val ddlWriteInfoKey = "hsqldb sql"
  // drop "desc" keyword - hsqldb ignores it, fails metadata roundtrip test
  override def idxCols(cols: Seq[String]) = super.idxCols(cols.map(c =>
    if (c.toLowerCase endsWith " desc") c.substring(0, c.length - 5) else c))
}

private[out] class H2DdlGenerator(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends HsqldbDdlGenerator(constraintNamingRules, typeDefs) {
  override val ddlWriteInfoKey = "h2 sql"
  override def explicitNotNullForColumn(t: TableDef_[_], c: ColumnDef) =
    !c.nullable || t.pk.exists(_.cols.contains(c.name))
}

private[out] class OracleDdlGenerator(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends StandardSqlDdlGenerator(constraintNamingRules, typeDefs) {
  override val ddlWriteInfoKey = "oracle sql"
  override def dbDefault(c: ColumnDef) = (c.type_.name, c.dbDefault) match {
    case (_, null) => null
    case ("boolean", f) if "false" equalsIgnoreCase f => "'N'"
    case ("boolean", t) if "true" equalsIgnoreCase t => "'Y'"
    case _ => super.dbDefault(c)
  }
  override def colCheck(c: ColumnDef) = {
    c.type_.name match {
      case "boolean" => " check (" + c.name + " in ('N','Y'))"
      // TODO do not add enum to col, or you will get uninformative msg from ora,
      // like: ORA-02290: check constraint (KPS.SYS_C0090768) violated
      case _ => super.colCheck(c)
    }
  }
  override def foreignKey(tableName: String)(r: TableMetadata.Ref) =
    // oracle does not have on update rule
    super.foreignKey(tableName)(
      if (r.onUpdateAction != null) r.copy(onUpdateAction = null) else r)
}

private[out] class StandardSqlDdlGenerator(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef]) extends DdlGenerator(typeDefs) with ConstraintNamingRules {
  override def pkName(tableName: String) =
    constraintNamingRules.pkName(tableName)
  override def ukName(tableName: String, uk: DbIndex) =
    constraintNamingRules.ukName(tableName, uk)
  override def idxName(tableName: String, idx: DbIndex) =
    constraintNamingRules.idxName(tableName, idx)
  override def fkName(tableName: String, ref: Ref) =
    constraintNamingRules.fkName(tableName, ref)
  override def colCheck(c: ColumnDef) = {
    val xt = c.type_
    xt.name match {
      case "string" if c.enum_ != null =>
        c.enum_.map("'" + _ + "'")
          .mkString(" check (" + c.name + " in (", ", ", "))")
      case _ if c.enum_ != null =>
        c.enum_
          .mkString(" check (" + c.name + " in (", ", ", "))")
      case _ => ""
    }
  }
}

private[out] class PostgreDdlGenerator(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends StandardSqlDdlGenerator(constraintNamingRules, typeDefs) {
  override val ddlWriteInfoKey = "postgresql"
}
package mojoz.metadata.out

import scala.annotation.tailrec
import scala.math.max
import mojoz.metadata._
import mojoz.metadata.io._
import mojoz.metadata.TableDef._
import mojoz.metadata.TableDef.{ TableDefBase => TableDef }
import mojoz.metadata.ColumnDef.{ ColumnDefBase => ColumnDef }
import scala.collection.immutable.{ Map, Seq }
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

def apply(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): SqlWriter =
  new StandardSqlWriter(constraintNamingRules, typeDefs)
def h2(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): SqlWriter =
  new H2SqlWriter(constraintNamingRules, typeDefs)
def hsqldb(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): SqlWriter =
  new HsqldbSqlWriter(constraintNamingRules, typeDefs)
def oracle(constraintNamingRules: ConstraintNamingRules = new OracleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): SqlWriter =
  new OracleSqlWriter(constraintNamingRules, typeDefs)
def postgresql(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules,
    typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs): SqlWriter =
  new PostgreSqlWriter(constraintNamingRules, typeDefs)

}

abstract class SqlWriter(typeDefs: Seq[TypeDef]) { this: ConstraintNamingRules =>
  private[out] def idxCols(cols: Seq[String]) = cols.map(c =>
    if (c.toLowerCase endsWith " asc") c.substring(0, c.length - 4) else c)
  /** Returns full sql schema string (tables, comments, keys, indices, refs)
    */
  def schema(tables: Seq[TableDef[ColumnDef[Type]]]) = List(
    tablesAndComments(tables),
    keysAndIndexes(tables),
    foreignKeys(tables)).filter(_ != "").mkString("\n\n") +
    (if (tables.size > 0) "\n" else "")
  def tableAndComments(t: TableDef[ColumnDef[Type]]): String =
    List[Iterable[String]](
      Some(table(t)), tableComment(t), columnComments(t))
      .flatten.mkString("\n")
  def tablesAndComments(tables: Seq[TableDef[ColumnDef[Type]]]): String =
    tables.map(tableAndComments).mkString("\n\n")
  def keysAndIndexes(t: TableDef[ColumnDef[Type]]): String =
    List[Iterable[String]](
      primaryKey(t), uniqueIndexes(t), indexes(t))
      .flatten.mkString("\n")
  def keysAndIndexes(tables: Seq[TableDef[ColumnDef[Type]]]): String =
    tables.map(keysAndIndexes).filter(_ != "").mkString("\n\n")
  def tableAndCommentsAndIndexes(t: TableDef[ColumnDef[Type]]): String =
    (tableAndComments(t) + keysAndIndexes(t)).trim
  def table(t: TableDef[ColumnDef[Type]]) =
    // pk separated from table definition to fit large data imports etc.
    (t.cols.map(column(t)) ++ tableChecks(t)) // ++ primaryKey(t))
      .mkString("create table " + t.name + "(\n  ", ",\n  ", "\n);")
  def primaryKey(t: TableDef[_]) = t.pk map { pk =>
    "alter table " + t.name + " add " +
    "constraint " + Option(pk.name).getOrElse(pkName(t.name)) +
      " primary key (" + idxCols(pk.cols).mkString(", ") + ");"
  }
  def uniqueIndex(t: TableDef[_])(uk: DbIndex) =
    s"create unique index ${
      Option(uk.name).getOrElse(ukName(t.name, uk))
    } on ${t.name}(${idxCols(uk.cols).mkString(", ")});"
  def uniqueKey(t: TableDef[_])(uk: DbIndex) =
    s"alter table ${t.name} add constraint ${
      Option(uk.name).getOrElse(ukName(t.name, uk))
    } unique(${idxCols(uk.cols).mkString(", ")});"
  def uniqueIndexes(t: TableDef[_]) = t.uk map { uk =>
    if (uk.cols.exists(_.toLowerCase endsWith " desc")) uniqueIndex(t)(uk)
    // on some dbs, unique constraint (not index) is required to add fk
    else uniqueKey(t)(uk)
  }
  def indexes(t: TableDef[_]) = t.idx map { idx =>
    "create index " + Option(idx.name).getOrElse(idxName(t.name, idx)) +
      s" on ${t.name}(${idxCols(idx.cols).mkString(", ")});"
  }
  def dbDefault(c: ColumnDef[Type]) = c.dbDefault
  private def column(t: TableDef[_])(c: ColumnDef[Type]) = {
    c.name + " " + dbType(c) +
      (dbDefault(c) match { case null => "" case d => s" default $d" }) +
      (if (explicitNotNullForColumn(t, c)) " not null" else "") +
      colCheck(c)
  }
  private[out] def explicitNotNullForColumn(t: TableDef[_], c: ColumnDef[Type]) =
    !c.nullable && !t.pk.exists(_.cols.contains(c.name))
  def tableComment(t: TableDef[_]) = Option(t.comments).filter(_ != "").map(c =>
    s"comment on table ${t.name} is '${c.replace("'", "''")}';")
  def columnComments(t: TableDef[ColumnDef[_]]) =
    t.cols.filter(_.comments != null).filter(_.comments != "").map(c =>
      s"comment on column ${t.name}.${c.name} is '${c.comments.replace("'", "''")}';")
  def foreignKeys(tables: Seq[TableDef[_]]) = tables.map { t =>
    t.refs map foreignKey(t.name)
  }.flatten.mkString("\n")
  def foreignKey(tableName: String)(r: TableDef.Ref) =
    s"alter table $tableName add constraint ${
      Option(r.name) getOrElse fkName(tableName, r)
    } foreign key (${
      r.cols mkString ", "
    }) references ${r.refTable}(${r.refCols mkString ", "})" +
      Option(r.onDeleteAction).map(" on delete " + _).getOrElse("") +
      Option(r.onUpdateAction).map(" on update " + _).getOrElse("") +
      ";"
  val sqlWriteInfoKey = "sql"
  lazy val typeNameToSqlWriteInfoSeq: Map[String, Seq[SqlWriteInfo]] =
    typeDefs.map(td =>
      td.name ->
        td.sqlWrite.get(sqlWriteInfoKey).orElse(td.sqlWrite.get("sql")).getOrElse(Nil)
    ).toMap
  def dbType(c: ColumnDef[Type]): String = {
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
      sys.error(s"Missing sql info (key '$sqlWriteInfoKey' or 'sql') for type $t in ${c.name}")
    }
  }
  def colCheck(c: ColumnDef[Type]): String
  def tableChecks(t: TableDef[ColumnDef[Type]]): Seq[String] =
    t.ck.map(ck =>
      if (ck.name != null) s"constraint ${ck.name} check (${ck.expression})"
      else s"check (${ck.expression})")
  private def sizeOptionMatch(min: Option[Int], max: Option[Int], value: Option[Int]) =
    min.isEmpty && value.isEmpty ||
      min.isDefined && value.isDefined && min.get <= value.get &&
      max.map(_ >= value.get).getOrElse(true)
}

private[out] class HsqldbSqlWriter(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends StandardSqlWriter(constraintNamingRules, typeDefs) {
  override val sqlWriteInfoKey = "hsqldb sql"
  // drop "desc" keyword - hsqldb ignores it, fails metadata roundtrip test
  override def idxCols(cols: Seq[String]) = super.idxCols(cols.map(c =>
    if (c.toLowerCase endsWith " desc") c.substring(0, c.length - 5) else c))
}

private[out] class H2SqlWriter(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends HsqldbSqlWriter(constraintNamingRules, typeDefs) {
  override val sqlWriteInfoKey = "h2 sql"
  // for index name jdbc roundtrip
  override def uniqueIndexes(t: TableDef[_]) = t.uk.map(uniqueIndex(t))
  // how to retrieve column check constraint for h2? add to table instead 
  override def colCheck(c: ColumnDef[Type]): String = ""
  override def explicitNotNullForColumn(t: TableDef[_], c: ColumnDef[Type]) =
    !c.nullable || t.pk.exists(_.cols.contains(c.name))
  override def tableChecks(t: TableDef[ColumnDef[Type]]): Seq[String] = t.cols.map { c =>
    val xt = c.type_
    xt.name match {
      case "string" if c.enum != null =>
        c.enum.map("'" + _ + "'")
          .mkString("check " + c.name + " in (", ", ", ")")
      case _ => ""
    }
  }.filter(_ != "") ++ super.tableChecks(t)
}

private[out] class OracleSqlWriter(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends StandardSqlWriter(constraintNamingRules, typeDefs) {
  override val sqlWriteInfoKey = "oracle sql"
  override def dbDefault(c: ColumnDef[Type]) = (c.type_.name, c.dbDefault) match {
    case (_, null) => null
    case ("boolean", f) if "false" equalsIgnoreCase f => "'N'"
    case ("boolean", t) if "true" equalsIgnoreCase t => "'Y'"
    case _ => super.dbDefault(c)
  }
  override def colCheck(c: ColumnDef[Type]) = {
    c.type_.name match {
      case "boolean" => " check (" + c.name + " in ('N','Y'))"
      // TODO do not add enum to col, or you will get uninformative msg from ora,
      // like: ORA-02290: check constraint (KPS.SYS_C0090768) violated
      case _ => super.colCheck(c)
    }
  }
  override def foreignKey(tableName: String)(r: TableDef.Ref) =
    // oracle does not have on update rule
    super.foreignKey(tableName)(
      if (r.onUpdateAction != null) r.copy(onUpdateAction = null) else r)
}

private[out] class StandardSqlWriter(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef]) extends SqlWriter(typeDefs) with ConstraintNamingRules {
  override def pkName(tableName: String) =
    constraintNamingRules.pkName(tableName)
  override def ukName(tableName: String, uk: DbIndex) =
    constraintNamingRules.ukName(tableName, uk)
  override def idxName(tableName: String, idx: DbIndex) =
    constraintNamingRules.idxName(tableName, idx)
  override def fkName(tableName: String, ref: Ref) =
    constraintNamingRules.fkName(tableName, ref)
  override def colCheck(c: ColumnDef[Type]) = {
    val xt = c.type_
    xt.name match {
      case "string" if c.enum != null =>
        c.enum.map("'" + _ + "'")
          .mkString(" check (" + c.name + " in (", ", ", "))")
      case _ => ""
    }
  }
}

private[out] class PostgreSqlWriter(
    constraintNamingRules: ConstraintNamingRules,
    typeDefs: Seq[TypeDef])
  extends StandardSqlWriter(constraintNamingRules, typeDefs) {
  override val sqlWriteInfoKey = "postgresql"
}

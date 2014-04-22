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
  def fkName(tableName: String, ref: Ref): String
}

class SimpleConstraintNamingRules extends ConstraintNamingRules {
  val maxNameLen = 60
  val pkPrefix = "pk_"
  val pkSuffix = ""
  val fkPrefix = "fk_"
  val fkSuffix = ""
  val fkTableNameSep = "_"
  def pkUsableLen =
    maxNameLen - pkPrefix.length - pkSuffix.length
  def fkUsableLen =
    maxNameLen - fkPrefix.length - fkTableNameSep.length - fkSuffix.length
  def fkTableNameLen = fkUsableLen / 2
  def fkRefTableNameLen = fkUsableLen - fkTableNameLen
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
  override def fkName(tableName: String, r: Ref) = {
    val tnRaw = tableName
    val rnRaw = Option(r.defaultRefTableAlias).getOrElse(r.refTable)
    val rawLen =
      fkPrefix.length + tnRaw.length + fkTableNameSep.length + rnRaw.length +
        fkSuffix.length
    val overLen = tnRaw.length + rnRaw.length > fkUsableLen
    val tnOverLen = tnRaw.length > fkTableNameLen
    val rnOverLen = rnRaw.length > fkRefTableNameLen
    val (tn, rn) = (overLen, tnOverLen, rnOverLen) match {
      case (false, _, _) => (tnRaw, rnRaw)
      case (_, _, true) =>
        val tn = shorten(tnRaw, fkTableNameLen)
        val rn = shorten(rnRaw, fkUsableLen - tn.length)
        (tn, rn)
      case _ => (shorten(tnRaw, fkUsableLen - rnRaw.length), rnRaw)
    }
    s"$fkPrefix$tn$fkTableNameSep$rn$fkSuffix"
  }
  override def pkName(tableName: String) =
    pkPrefix + shorten(tableName, pkUsableLen) + pkSuffix
}

class OracleConstraintNamingRules extends SimpleConstraintNamingRules {
  override val maxNameLen = 30
}

def apply(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new StandardSqlWriter(constraintNamingRules)
def hsqldb(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new StandardSqlWriter(constraintNamingRules)
def oracle(constraintNamingRules: ConstraintNamingRules = new OracleConstraintNamingRules): SqlWriter =
  new OracleSqlWriter(constraintNamingRules)
def postgresql(constraintNamingRules: ConstraintNamingRules = new SimpleConstraintNamingRules): SqlWriter =
  new PostgreSqlWriter(constraintNamingRules)

}

trait SqlWriter { this: ConstraintNamingRules =>
  def createStatements(tables: Seq[TableDef[Type]]) = {
    List(createTablesStatements(tables), foreignKeys(tables)).mkString("\n")
  }
  def createTablesStatements(tables: Seq[TableDef[Type]]) = {
    tables.map(t => List(
      createTableStatement(t), tableComment(t), columnComments(t))
      .mkString("\n")).mkString("\n\n") + (if (tables.size > 0) "\n" else "")
  }
  def createTableStatement(t: TableDef[Type]) =
    (t.cols.map(createColumn) ++ primaryKey(t))
      .mkString("create table " + t.name + "(\n  ", ",\n  ", "\n);")
  def primaryKey(t: TableDef[_]) = t.pk map { pk =>
    "constraint " + Option(pk.name).getOrElse(pkName(t.name)) +
      " primary key (" + pk.cols.mkString(", ") + ")"
  }
  private def createColumn(c: ColumnDef[Type]) =
    c.name + " " + dbType(c) +
      (if (c.dbDefault == null) "" else " default " + c.dbDefault) +
      (if (c.nullable || c.name == "id") "" else " not null") + //XXX name != id
      check(c)
  def tableComment(t: TableDef[_]) = "comment on table " + t.name +
    " is '" + Option(t.comments).getOrElse("") + "';"
  def columnComments(t: TableDef[_]) = t.cols.map(c =>
    "comment on column " + t.name + "." + c.name +
      " is '" + Option(c.comments).getOrElse("") + "';") mkString "\n"
  def foreignKeys(tables: Seq[TableDef[_]]) = tables.map { t =>
    t.refs map { r =>
      // TODO cascade
      s"alter table ${t.name} add constraint ${fkName(t.name, r)} foreign key (${
        r.cols mkString ", "
      }) references ${r.refTable}(${r.refCols mkString ", "});"
    }
  }.flatMap(x => x)
    .mkString("", "\n", if (tables.exists(_.refs.size > 0)) "\n" else "")
  def dbType(c: ColumnDef[Type]): String
  def check(c: ColumnDef[Type]): String
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
}

private[out] class StandardSqlWriter(
  constraintNamingRules: ConstraintNamingRules) extends SqlWriter with ConstraintNamingRules {
  override def pkName(tableName: String) =
    constraintNamingRules.pkName(tableName)
  override def fkName(tableName: String, ref: Ref) =
    constraintNamingRules.fkName(tableName, ref)
  override def dbType(c: ColumnDef[Type]) = {
    val xt = c.type_
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    xt.name match {
      case "integer" =>
        "numeric" + xt.totalDigits.map(l => s"($l)").getOrElse("")
      case "long" =>
        "bigint" + xt.totalDigits.map(l => s"($l)").getOrElse("")
      case "int" =>
        "integer" + xt.totalDigits.map(l => s"($l)").getOrElse("")
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

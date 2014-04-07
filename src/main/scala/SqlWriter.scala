package mojoz.metadata.out

import scala.annotation.tailrec
import scala.math.max
import mojoz.metadata._
import mojoz.metadata.io._

trait ConstraintNamingRules {
  def pkName(tableName: String): String
  def fkName(tableName: String, ref: Ref): String
}

trait SimpleConstraintNamingRules extends ConstraintNamingRules {
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

trait OracleConstraintNamingRules extends SimpleConstraintNamingRules {
  override val maxNameLen = 30
}

trait OracleSqlWriter { this: ConstraintNamingRules =>
  def createStatements(tables: Seq[TableDef]) = {
    List(createTablesStatements(tables), foreignKeys(tables)).mkString("\n")
  }
  def createTablesStatements(tables: Seq[TableDef]) = {
    tables.map(dbTableDef).map(t => List(
      createTableStatement(t), tableComment(t), columnComments(t))
      .mkString("\n")).mkString("\n\n") + (if (tables.size > 0) "\n" else "")
  }
  def createTableStatement(t: DbTableDef) =
    (t.cols.map(createColumn) ++ primaryKey(t))
      .mkString("create table " + t.name + "(\n  ", ",\n  ", "\n);")
  def primaryKey(t: DbTableDef) = t.pk map { pk =>
    "constraint " + Option(pk.name).getOrElse(pkName(t.name)) +
      " primary key (" + pk.cols.mkString(", ") + ")"
  }
  private def createColumn(c: DbColumnDef) =
    c.name + " " + c.dbType +
      (if (c.dbDefault == null) "" else " default " + c.dbDefault) +
      (if (c.nullable || c.name == "id") "" else " not null") + //XXX name != id
      c.check
  def tableComment(t: DbTableDef) = "comment on table " + t.name +
    " is '" + Option(t.comment).getOrElse("") + "';"
  def columnComments(t: DbTableDef) = t.cols.map(c =>
    "comment on column " + t.name + "." + c.name +
      " is '" + Option(c.comment).getOrElse("") + "';") mkString "\n"
  def foreignKeys(tables: Seq[TableDef]) = tables.map(dbTableDef).map { t =>
    t.refs map { r =>
      // TODO cascade
      s"alter table ${t.name} add constraint ${fkName(t.name, r)} foreign key (${
        r.cols mkString ", "
      }) references ${r.refTable}(${r.refCols mkString ", "});"
    }
  }.flatMap(x => x) mkString "\n"
  def dbTableDef(t: TableDef) = t match {
    case TableDef(name, comment, cols, pk, uk, idx, refs) => DbTableDef(
      DbConventions.xsdNameToDbName(name), comment, cols.map(dbColumnDef), pk, uk, idx, refs)
  }
  def dbColumnDef(c: ColumnDef) = c match {
    case ColumnDef(name, xsdType, nullable, dbDefault, enum, comment) => DbColumnDef(
      DbConventions.xsdNameToDbName(name),
      dbType(c), nullable, dbDefault, check(c), comment)
  }
  def dbType(c: ColumnDef) = {
    val xt = c.xsdType
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    xt.name match {
      case "integer" =>
        "numeric(" + (xt.totalDigits getOrElse 20) + ")" // len?
      case "long" =>
        "numeric(" + (xt.totalDigits getOrElse 18) + ")"
      case "int" =>
        "numeric(" + (xt.totalDigits getOrElse 9) + ")"
      case "decimal" =>
        "numeric(" + xt.totalDigits.get + ", " + xt.fractionDigits.get + ")"
      case "date" => "date"
      case "dateTime" => "timestamp"
      case "string" =>
        "varchar2" + xt.length.map(l => s"($l char)").getOrElse("")
      case "boolean" => "char"
      case "base64Binary" => "blob"
      case x => throw new RuntimeException("Unexpected xsd type: " + xt)
    }
  }
  def check(c: ColumnDef) = {
    val xt = c.xsdType
    def dbColumnName = DbConventions.xsdNameToDbName(c.name)
    xt.name match {
      case "boolean" => " check (" + dbColumnName + " in ('N','Y'))"
      // TODO do not add enum to col, or you will get uninformative msg from ora,
      // like: ORA-02290: check constraint (KPS.SYS_C0090768) violated
      case "string" if c.enum != null =>
        c.enum.map("'" + _ + "'")
          .mkString(" check (" + dbColumnName + " in (", ", ", "))")
      case _ => ""
    }
  }
}

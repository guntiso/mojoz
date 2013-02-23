package metadata

import scala.annotation.tailrec
import scala.math.max

object OracleSqlWriter {
  def createTablesStatements(tables: Seq[TableDef]) = {
    tables.map(dbTableDef).map(t => List(
      createTableStatement(t), tableComment(t), columnComments(t))
      .mkString("\n")).mkString("\n\n") + (if (tables.size > 0) "\n" else "")
  }
  // TODO default values
  def createTableStatement(t: DbTableDef) =
    (t.cols.map(createColumn) ++ primaryKey(t))
      .mkString("create table " + t.name + "(\n  ", ",\n  ", "\n);")
  def primaryKey(t: DbTableDef) = t.pk map { pk =>
    "constraint " + pk.name + " primary key (" + pk.cols.mkString(", ") + ")"
  }
  // TODO default
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
  // TODO
  def foreignKeys() {}
  def dbTableDef(t: TableDef) = t match {
    case TableDef(name, comment, cols, pk) => DbTableDef(
      DbConventions.xsdNameToDbName(name), comment, cols.map(dbColumnDef), pk)
  }
  def dbColumnDef(c: ColumnDef) = c match {
    case ColumnDef(name, xsdType, nullable, _, _, dbDefault, comment) => DbColumnDef(
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
      case "string" => "varchar2(" + xt.length.get + " char)"
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
      case _ => ""
    }
  }
}

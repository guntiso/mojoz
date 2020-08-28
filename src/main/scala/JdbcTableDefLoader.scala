package org.mojoz.metadata.in

import java.lang.Integer
import java.sql.Connection
import java.sql.{ DatabaseMetaData => DM }
import java.sql.ResultSet
import java.sql.{ ResultSet => RS }
import java.sql.Types

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Seq

import org.mojoz.metadata.ColumnDef
import org.mojoz.metadata.JdbcLoadInfo
import org.mojoz.metadata.TableDef
import org.mojoz.metadata.TableDef._
import org.mojoz.metadata.Type
import org.mojoz.metadata.TypeDef
import org.mojoz.metadata.TypeMetadata

abstract class JdbcTableDefLoader(typeDefs: Seq[TypeDef]) {
  import JdbcTableDefLoader._
  def jdbcTableDefs(conn: Connection,
    catalog: String, schemaPattern: String, tableNamePattern: String,
    types: String*) = {
    val tableDefs = ListBuffer[TableDef[ColumnDef[JdbcColumnType]]]()
    val dmd = conn.getMetaData
    val rs = dmd.getTables(catalog, schemaPattern, tableNamePattern,
      if (types.size == 0) null else types.toArray)
    while (rs.next) {
      val catalog = rs.getString("TABLE_CAT")
      val schema = rs.getString("TABLE_SCHEM")
      val tableName = rs.getString("TABLE_NAME")
      val comments = rs.getString("REMARKS")
      val cols = colDefs(dmd.getColumns(catalog, schema, tableName, null))
      val pk = this.pk(dmd.getPrimaryKeys(catalog, schema, tableName))
      val (uk, idx) =
        ukAndIdx(dmd.getIndexInfo(catalog, schema, tableName, false, true))
      val refs = this.refs(dmd.getImportedKeys(catalog, schema, tableName))
      val ck = this.checkConstraints(conn, catalog, schema, tableName)
        .filterNot(c => CkParser.isNotNullCheck(c.expression))
      def findCol(name: String) =
        cols.find(c => name.equalsIgnoreCase(c.name) ||
          name.equalsIgnoreCase(tableName + "." + c.name) ||
          name.equalsIgnoreCase(schema + "." + tableName + "." + c.name))
      val checkColEnum = ck
        .map(c => (c, CkParser.colAndEnum(c.expression)))
        .filter(_._2.isDefined)
        .map(cce => (cce._1, findCol(cce._2.get._1), cce._2.get._2))
        .filter(_._2.isDefined)
        .map(cce => (cce._1, cce._2.get, cce._3))
      // FIXME if multiple matching checks on the same column
      val enumCk = checkColEnum.map(_._1).toSet
      val unmappedCk = ck.filterNot(enumCk.contains)
      val colToEnum = checkColEnum.map(cce => (cce._2, cce._3)).toMap
      val mappedCols = cols map {c =>
        val enum = colToEnum.get(c)
        if (enum.isDefined) c.copy(enum = enum.get) else c
      }

      val tableFullName =
        List(catalog, schema, tableName)
          .filter(_ != null).filter(_ != "").mkString(".")
      val extras = Map[String, Any]()
      tableDefs += TableDef(tableFullName, comments, mappedCols,
          pk, uk, unmappedCk, idx, refs, extras)
    }

    // work around oracle bugs
    if (conn.getClass.getName.startsWith("oracle")) {
      if (!tableDefs.exists(_.comments != null)) {
        val st = conn.prepareStatement(
          "select comments from all_tab_comments" +
            " where owner || '.' || table_name = ?",
          RS.TYPE_FORWARD_ONLY, RS.CONCUR_READ_ONLY, RS.CLOSE_CURSORS_AT_COMMIT)
        tableDefs transform { td =>
          st.setString(1, td.name)
          val rs = st.executeQuery()
          val comments = if (rs.next) rs.getString(1) else null
          rs.close()
          st.clearParameters()
          if (comments == null) td else td.copy(comments = comments)
        }
      }
      if (!tableDefs.exists(_.cols.exists(_.comments != null))) {
        val st = conn.prepareStatement(
          "select column_name, comments from all_col_comments" +
            " where owner || '.' || table_name = ?",
          RS.TYPE_FORWARD_ONLY, RS.CONCUR_READ_ONLY, RS.CLOSE_CURSORS_AT_COMMIT)
        tableDefs transform { td =>
          st.setString(1, td.name)
          val rs = st.executeQuery()
          var cList: List[(String, String)] = Nil
          while (rs.next)
            cList = (rs.getString(1), rs.getString(2)) :: cList
          rs.close()
          st.clearParameters()
          val cMap = cList.toMap
          if (!cMap.values.exists(c => c != null && c != "")) td
          else td.copy(cols = td.cols.map(c =>
            c.copy(comments = cMap.get(c.name).orNull)))
        }
      }
      // XXX booleans emulated on oracle
      val emulatedBooleanEnums = Set(
        List("N", "Y"), List("Y", "N"))
      def isEmulatedBoolean(c: ColumnDef[JdbcColumnType]) =
        c.type_.jdbcTypeCode == Types.CHAR && c.type_.size == 1 &&
          emulatedBooleanEnums.contains(c.enum.toList)
      tableDefs transform { td =>
        if (td.cols.exists(isEmulatedBoolean))
          td.copy(cols = td.cols.map { c =>
            if (isEmulatedBoolean(c)) c.copy(
              type_ = c.type_.copy(jdbcTypeCode = Types.BOOLEAN),
              enum = null,
              dbDefault =
                if (c.dbDefault == null) null
                else c.dbDefault.trim match {
                  case "'N'" => "false" case "'Y'" => "true" case d => d
                })
            else c
          })
        else td
      }
    }
    tableDefs.toList
  }
  def checkConstraints(conn: Connection, catalog: String,
    schemaPattern: String, tableNamePattern: String): Seq[CheckConstraint]
  def checkConstraints(rs: ResultSet) = {
    var checks: List[CheckConstraint] = Nil
    while (rs.next) {
      val name = rs.getString("CONSTRAINT_NAME")
      val expr = rs.getString("CHECK_CLAUSE")
      checks = CheckConstraint(name, expr) :: checks
    }
    rs.close
    checks
  }
  private[in] def standardCheckConstraints(conn: Connection,
    catalog: String, schemaPattern: String, tableNamePattern: String) = {
    val ps = conn.prepareStatement("""
      |select c.constraint_name, c.check_clause
      |  -- TODO? is_deferrable, initially_deferred
      |from information_schema.check_constraints c
      |join information_schema.table_constraints t
      |on c.constraint_catalog = t.constraint_catalog
      |and  c.constraint_schema = t.constraint_schema
      |and  c.constraint_name = t.constraint_name
      |where constraint_type = 'CHECK'
      |  and table_catalog like ?
      |  and table_schema like ?
      |  and table_name like ?
      """.stripMargin.trim)
    ps.setString(1, if (catalog == null) "%" else catalog)
    ps.setString(2, schemaPattern)
    ps.setString(3, tableNamePattern)
    val rs = ps.executeQuery()
    val checks = checkConstraints(rs)
    checks
  }
  def colDefs(rs: ResultSet) = {
    val cols = ListBuffer[ColumnDef[JdbcColumnType]]()
    while (rs.next) {
      val name = rs.getString("COLUMN_NAME")
      val jdbcTypeCode = rs.getInt("DATA_TYPE")
      val dbTypeName = rs.getString("TYPE_NAME")
      val size = rs.getInt("COLUMN_SIZE")
      val fractionDigits = rs.getInt("DECIMAL_DIGITS")
      val nullable = rs.getInt("NULLABLE") == DM.columnNullable
      val dbDefault = (jdbcTypeCode, rs.getString("COLUMN_DEF")) match {
        case (_, null) => null
        case (Types.BOOLEAN, "TRUE") => "true"
        case (Types.BOOLEAN, "FALSE") => "false"
        // XXX remove postgresql type safety garbage
        // FIXME check db is postgresql?
        case (_, d) if (d.indexOf("::") > 0) => d.substring(0, d.indexOf("::"))
        case (_, d) => d
      }
      val comments = rs.getString("REMARKS")
      val check = null
      val dbType =
        JdbcColumnType(dbTypeName, jdbcTypeCode, size, fractionDigits)
      val extras = Map[String, Any]()
      cols += ColumnDef(name, dbType, nullable, dbDefault, check, comments, extras)
    }
    rs.close
    cols.toList
  }
  def pk(rs: ResultSet) = {
    var cols: List[(Short, String, String)] = Nil
    while (rs.next) {
      val keySeq = rs.getShort("KEY_SEQ")
      val colName = rs.getString("COLUMN_NAME")
      val pkName = rs.getString("PK_NAME")
      cols = (keySeq, colName, pkName) :: cols
    }
    rs.close
    val pkName = cols.map(_._3).headOption.orNull
    if (cols.size == 0) None else Some(DbIndex(pkName, cols.sorted.map(_._2)))
  }
  def ukAndIdx(rs: ResultSet) = {
    var uk: List[(Short, String, String)] = Nil
    var idx: List[(Short, String, String)] = Nil
    while (rs.next) {
      val isIndex = rs.getShort("TYPE") match {
        case DM.tableIndexStatistic => false
        case _ => true
      }
      if (isIndex) {
        val ascDesc = rs.getString("ASC_OR_DESC") match {
          case "A" => "ASC"
          case "D" => "DESC"
          case null => null
        }
        val nonUnique = rs.getBoolean("NON_UNIQUE")
        val ordinal = rs.getShort("ORDINAL_POSITION")
        val colName = rs.getString("COLUMN_NAME")
        val idxName = rs.getString("INDEX_NAME")
        // TODO INDEX_QUALIFIER?
        // TODO FILTER_CONDITION?
        val colExpr = List(colName, ascDesc).filter(_ != null).mkString(" ")
        val t = (ordinal, colExpr, idxName)
        if (nonUnique) idx = t :: idx else uk = t :: uk
      }
    }
    rs.close
    val uks = uk.groupBy(_._3).map(t => DbIndex(t._1, t._2.sorted.map(_._2)))
    val idxs = idx.groupBy(_._3).map(t => DbIndex(t._1, t._2.sorted.map(_._2)))
    (uks.toList.sortBy(_.name), idxs.toList.sortBy(_.name))
  }
  def refs(rs: ResultSet) = {
    def fkRule(rule: Short) = rule match {
      case DM.importedKeyNoAction | DM.importedKeyRestrict => "no action"
      case DM.importedKeyCascade => "cascade"
      case DM.importedKeySetNull => "set null"
      case DM.importedKeySetDefault => "set default"
      case _ => sys.error("Unexpected fk rule: " + rule)
    }
    val refMap = scala.collection.mutable.Map[(String, String), Ref]()
    while (rs.next) {
      val pkTabCat = rs.getString("PKTABLE_CAT")
      val pkTabSch = rs.getString("PKTABLE_SCHEM")
      val pkTabName = rs.getString("PKTABLE_NAME")
      val pkColName = rs.getString("PKCOLUMN_NAME")
      val fkColName = rs.getString("FKCOLUMN_NAME")
      val keySeq = rs.getShort("KEY_SEQ")
      val updateRule = fkRule(rs.getShort("UPDATE_RULE"))
      val deleteRule = fkRule(rs.getShort("DELETE_RULE"))
      val fkName = rs.getString("FK_NAME")
      val deferrability = rs.getShort("DEFERRABILITY") match {
        case DM.importedKeyInitiallyDeferred => "deferred"
        case DM.importedKeyInitiallyImmediate => "immediate"
        case DM.importedKeyNotDeferrable => "not deferrable"
        case _ => null
      }
      val pkTabFullName =
        List(pkTabCat, pkTabSch, pkTabName)
          .filter(_ != null).filter(_ != "").mkString(".")
      refMap.get(pkTabFullName, fkName) match {
        case Some(r) =>
          refMap += (pkTabFullName, fkName) ->
            r.copy(
              cols = r.cols :+ fkColName,
              refCols = r.refCols :+ pkColName)
        case None =>
          // TODO deferrability to ref!
          refMap += (pkTabFullName, fkName) ->
            Ref(fkName, Seq(fkColName), pkTabFullName,
              Seq(pkColName), null, null, deleteRule, updateRule)
      }
    }
    rs.close
    refMap.values
      .toList
      .sortBy(_.name) // TODO sorting refs somehow for test stability, improve?
  }
  val jdbcLoadInfoKey = "jdbc"
  lazy val jdbcLoadInfoToTypeDef: Seq[(JdbcLoadInfo, TypeDef)] =
    typeDefs.flatMap { td =>
      val specific = td.jdbcLoad.get(jdbcLoadInfoKey).getOrElse(Nil)
      val generic = td.jdbcLoad.get("jdbc").getOrElse(Nil)
      (specific ++ (generic filterNot specific.contains)).map(_ -> td)
    }
  def jdbcTypeToMojozType(jdbcTypeCode: Int, size: Int, frac: Int) =
    jdbcLoadInfoToTypeDef.find { case (jl, td) =>
      jl.jdbcTypeCode == jdbcTypeCode &&
      jl.minSize.map(_ <= size).getOrElse(true) &&
      jl.maxSize.map(_ >= size).getOrElse(true) &&
      jl.minFractionDigits.map(_ <= frac).getOrElse(true) &&
      jl.maxFractionDigits.map(_ >= frac).getOrElse(true)
    }.map { case (jl, td) =>
      val length = jl.targetLength match {
        case Some(null) => Some(size) // xxx Some(null) means copy from source
        case x => x.map(_.intValue)
      }
      val totalDigits = jl.targetTotalDigits match {
        case Some(null) => Some(size) // xxx Some(null) means copy from source
        case x => x.map(_.intValue)
      }
      val fractionDigits = jl.targetFractionDigits match {
        case Some(null) => Some(frac) // xxx Some(null) means copy from source
        case x => x.map(_.intValue)
      }
      val isComplexType = false
      Type(td.name, length, totalDigits, fractionDigits, isComplexType)
    }.getOrElse {
      val jdbcTypeName = jdbcCodeToTypeName.get(jdbcTypeCode) getOrElse ""
      throw new RuntimeException(
        s"Failed to convert jdbc type (code: $jdbcTypeCode, name: $jdbcTypeName, size: $size, fractionDigits: $frac)" +
          " to mojoz type - mo match found")
    }
  def toMojozType(jdbcColumnType: JdbcColumnType): Type =
    jdbcTypeToMojozType(jdbcColumnType.jdbcTypeCode, jdbcColumnType.size, jdbcColumnType.fractionDigits)
  def toMojozTypeTableDef(tableDef: TableDef[ColumnDef[JdbcColumnType]]): TableDef[ColumnDef[Type]] =
    tableDef.copy(cols = tableDef.cols.map(c => c.copy(type_ = toMojozType(c.type_))))
}

// java.sun.com/j2se/1.5.0/docs/guide/jdbc/getstart/GettingStartedTOC.fm.html
object JdbcTableDefLoader {
  case class JdbcColumnType(
    dbTypeName: String,
    jdbcTypeCode: Int,
    size: Int,
    fractionDigits: Int)
  class H2(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends JdbcTableDefLoader(typeDefs) {
    override val jdbcLoadInfoKey = "h2 jdbc"
    override def checkConstraints(conn: Connection,
        catalog: String, schemaPattern: String, tableNamePattern: String) = {
      // FIXME table def loaders: use like or equals (like may fail!)? escape _?
      // FIXME table def loaders: ignore pars if null!
      val ps = conn.prepareStatement("""
        |select constraint_name, check_expression check_clause
        |  from information_schema.constraints
        | where constraint_type = 'CHECK'
        |   and table_catalog like ?
        |   and table_schema like ?
        |   and table_name like ?
        """.stripMargin.trim)
      ps.setString(1, catalog)
      ps.setString(2, schemaPattern)
      ps.setString(3, tableNamePattern)
      val rs = ps.executeQuery()
      val checks = checkConstraints(rs)
      checks
    }
  }
  class Hsqldb(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends JdbcTableDefLoader(typeDefs) {
    override val jdbcLoadInfoKey = "hsqldb jdbc"
    override def checkConstraints(conn: Connection,
      catalog: String, schemaPattern: String, tableNamePattern: String) =
      standardCheckConstraints(conn, catalog, schemaPattern, tableNamePattern)
  }
  class Oracle(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends JdbcTableDefLoader(typeDefs) {
    override val jdbcLoadInfoKey = "oracle jdbc"
    override def checkConstraints(conn: Connection,
        catalog: String, schemaPattern: String, tableNamePattern: String) = {
      val ps = conn.prepareStatement("""
        |select constraint_name, search_condition check_clause
        |  from all_constraints
        |  where constraint_type = 'C' and owner like ? and table_name like ?
        """.stripMargin.trim)
      ps.setString(1, schemaPattern)
      ps.setString(2, tableNamePattern)
      val rs = ps.executeQuery()
      val checks = checkConstraints(rs)
      checks
    }
  }
  class Postgresql(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends JdbcTableDefLoader(typeDefs) {
    override val jdbcLoadInfoKey = "postgresql jdbc"
    override def checkConstraints(conn: Connection,
      catalog: String, schemaPattern: String, tableNamePattern: String) =
      standardCheckConstraints(conn, catalog, schemaPattern, tableNamePattern)
  }
  private[in] class Other(typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) extends JdbcTableDefLoader(typeDefs) {
    override def checkConstraints(conn: Connection,
        catalog: String, schemaPattern: String, tableNamePattern: String) = {
      Nil
    }
  }
  def jdbcTableDefLoader(conn: Connection, typeDefs: Seq[TypeDef] = TypeMetadata.customizedTypeDefs) = {
    conn.getMetaData.getDatabaseProductName match {
      case "H2" => new H2(typeDefs)
      case "HSQL Database Engine" => new Hsqldb(typeDefs)
      case "Oracle" => new Oracle(typeDefs)
      case "PostgreSQL" => new Postgresql(typeDefs)
      case x =>
        println("JdbcTableDefLoader - unsupported database " + x +
          ", ignoring check constraints!")
        new Other(typeDefs)
    }
  }
  def tableDefs(conn: Connection,
    catalog: String, schemaPattern: String, tableNamePattern: String,
    types: String*) = {
    val loader = jdbcTableDefLoader(conn)
    loader.jdbcTableDefs(conn, catalog, schemaPattern, tableNamePattern, types: _*)
      .map(loader.toMojozTypeTableDef)
  }
  private[in] val jdbcTypeNameToCode: Map[String, Int] = Map(
    "ARRAY" -> Types.ARRAY,
    "BIGINT" -> Types.BIGINT,
    "BINARY" -> Types.BINARY,
    "BIT" -> Types.BIT,
    "BLOB" -> Types.BLOB,
    "BOOLEAN" -> Types.BOOLEAN,
    "CHAR" -> Types.CHAR,
    "CLOB" -> Types.CLOB,
    "DATALINK" -> Types.DATALINK,
    "DATE" -> Types.DATE,
    "DECIMAL" -> Types.DECIMAL,
    "DISTINCT" -> Types.DISTINCT,
    "DOUBLE" -> Types.DOUBLE,
    "FLOAT" -> Types.FLOAT,
    "INTEGER" -> Types.INTEGER,
    "JAVA_OBJECT" -> Types.JAVA_OBJECT,
    "LONGNVARCHAR" -> Types.LONGNVARCHAR,
    "LONGVARBINARY" -> Types.LONGVARBINARY,
    "LONGVARCHAR" -> Types.LONGVARCHAR,
    "NCHAR" -> Types.NCHAR,
    "NCLOB" -> Types.NCLOB,
    "NULL" -> Types.NULL,
    "NUMERIC" -> Types.NUMERIC,
    "NVARCHAR" -> Types.NVARCHAR,
    "OTHER" -> Types.OTHER,
    "REAL" -> Types.REAL,
    "REF" -> Types.REF,
    "REF_CURSOR" -> 2012, // Types.REF_CURSOR - since java 8
    "ROWID" -> Types.ROWID,
    "SMALLINT" -> Types.SMALLINT,
    "SQLXML" -> Types.SQLXML,
    "STRUCT" -> Types.STRUCT,
    "TIME" -> Types.TIME,
    "TIME_WITH_TIMEZONE" -> 2013, // Types.TIME_WITH_TIMEZONE - since java 8
    "TIMESTAMP" -> Types.TIMESTAMP,
    "TIMESTAMP_WITH_TIMEZONE" -> 2014, // Types.TIMESTAMP_WITH_TIMEZONE - since java 8,
    "TINYINT" -> Types.TINYINT,
    "VARBINARY" -> Types.VARBINARY,
    "VARCHAR" -> Types.VARCHAR
  )
  private[in] val jdbcCodeToTypeName: Map[Int, String] =
    jdbcTypeNameToCode.map(_.swap)
}

private[in] object CkParser {
  val s = "\\s*"
  val ident = "[_\\p{IsLatin}][_\\p{IsLatin}0-9]*"
  val qi = s"$ident(\\.$ident)*"
  val in = "[iI][nN]"
  val enum = """\(?'?[\+\-_\p{IsLatin}0-9\.\s]+'?\)?"""
  val cast = """::[\w\.\s]+"""
  val qiQuotQi = s"""$qi|"$qi""""
  val checkIn1 = s"""_(($qiQuotQi))_ $in _(($enum( , $enum)*))_"""
  val checkIn2 = s"""_(($qiQuotQi))_ ($cast)? = (_($enum($cast)?)_ ($cast)?))_"""
  val checkIn3 = s"""_(($qiQuotQi))_ ($cast)? = ANY_(ARRAY_[(_($enum($cast)?)_ ($cast)?( , $enum($cast)?)_ ($cast)?)*)]_)_ ($cast)?(_[]_)?)_"""
  val checkNotNull = s"$s($qiQuotQi)(?i) is not null$s".replace(" ", "\\s+")
  val castR = """::\w+""".r
  private def regex(pattern: String) =
    ("^" + pattern
      .replace("_[", "\\[")
      .replace("]_", "\\]")
      .replace("_(", "[\\s\\(]*")
      .replace(")_", "[\\s\\)]*")
      .replace(" ", "\\s*") +
    "$").r
  val CheckIn1 = regex(checkIn1)
  val CheckIn2 = regex(checkIn2)
  val CheckIn3 = regex(checkIn3)
  val CheckNotNull = regex(checkNotNull)
  private def toColEnum(col: String, values: String) =
    (col.replace(""""""", ""),
      values.split("[\\(,\\)]+").toList.map(_.trim.replace("'", "")).filter(_.trim != ""))
  def colAndEnum(ck: String) = ck match {
    case CheckIn1(col, _, _, values, _) =>
      Some(toColEnum(col, values))
    case CheckIn2(col, _, _, _, values, _, _) =>
      Some(toColEnum(col, castR.replaceAllIn(values.replace("::character varying", ""), "")))
    case CheckIn3(col, _, _, _, values, _, _, _, _, _, _, _) =>
      Some(toColEnum(col, castR.replaceAllIn(values.replace("::character varying", ""), "")))
    case _ =>
      None
  }
  def isNotNullCheck(ck: String) = ck match {
    case CheckNotNull(_, _, _) => true
    case _ => false
  }
}

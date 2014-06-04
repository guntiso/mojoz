package mojoz.metadata.in

import java.lang.Integer
import java.sql.Connection
import java.sql.{ DatabaseMetaData => DM }
import java.sql.ResultSet
import java.sql.{ ResultSet => RS }
import java.sql.Types

import scala.collection.mutable.ListBuffer

import mojoz.metadata.ColumnDef
import mojoz.metadata.TableDef
import mojoz.metadata.TableDef._
import mojoz.metadata.Type

abstract class JdbcTableDefLoader {
  import JdbcTableDefLoader._
  def jdbcTableDefs(conn: Connection,
    catalog: String, schemaPattern: String, tableNamePattern: String,
    types: String*) = {
    val tableDefs = ListBuffer[TableDef[JdbcColumnType]]()
    val dmd = conn.getMetaData
    val rs = dmd.getTables(catalog, schemaPattern, tableNamePattern,
      if (types.size == 0) null else types.toArray)
    while (rs.next) {
      val catalog = rs.getString("TABLE_CAT")
      val schema = rs.getString("TABLE_SCHEM")
      val tableName = rs.getString("TABLE_NAME")
      val comment = rs.getString("REMARKS")
      val cols = colDefs(dmd.getColumns(catalog, schema, tableName, null))
      val pk = this.pk(dmd.getPrimaryKeys(catalog, schema, tableName))
      val (uk, idx) =
        ukAndIdx(dmd.getIndexInfo(catalog, schema, tableName, false, true))
      val refs = this.refs(dmd.getImportedKeys(catalog, schema, tableName))
      val ck = this.checkConstraints(conn, catalog, schema, tableName)
      val tableFullName =
        List(catalog, schema, tableName)
          .filter(_ != null).filter(_ != "").mkString(".")
      tableDefs += TableDef(tableFullName, comment, cols, pk, uk, ck, idx, refs)
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
          val comment = if (rs.next) rs.getString(1) else null
          rs.close()
          st.clearParameters()
          if (comment == null) td else td.copy(comments = comment)
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
    ps.setString(1, catalog)
    ps.setString(2, schemaPattern)
    ps.setString(3, tableNamePattern)
    val rs = ps.executeQuery()
    val checks = checkConstraints(rs)
    checks
  }
  def checkToEnum(check: String) =
    // TODO properly, regexp at least
    Option(check).map(_ split "[\\s'\\(\\),]+")
      .map(_.toList.filter(_ != ""))
      .map {
        case "check" :: _ :: "in" :: tail => tail
        case x => Nil
      }.filter(_.size > 0).orNull
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
        // XXX booleans emulated on oracle
        // FIXME check enum = [Y, N], check db is oracle?
        case (Types.CHAR, d) if size == 1 => d.trim match {
          case "'N'" => "false" case "'Y'" => "true" case d => d
        }
        // XXX remove postgresql type safety garbage
        // FIXME check db is postgresql?
        case (_, d) if (d.indexOf("::") > 0) => d.substring(0, d.indexOf("::"))
        case (_, d) => d
      }
      val comment = rs.getString("REMARKS")
      val check = null // TODO check constraint for column (enum)!
      val dbType =
        JdbcColumnType(dbTypeName, jdbcTypeCode, size, fractionDigits)
      cols += ColumnDef(name, dbType, nullable, dbDefault, check, comment)
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
          r.cols.asInstanceOf[ListBuffer[String]] += fkColName
          r.refCols.asInstanceOf[ListBuffer[String]] += pkColName
        case None =>
          // TODO deferrability to ref!
          refMap += (pkTabFullName, fkName) ->
            Ref(fkName, ListBuffer(fkColName), pkTabFullName,
              ListBuffer(pkColName), null, null, deleteRule, updateRule)
      }
    }
    rs.close
    refMap.values
      .map(r => r.copy(cols = r.cols.toList, refCols = r.refCols.toList))
      .toList
      .sortBy(_.name) // TODO sorting refs somehow for test stability, improve?
  }
}

// java.sun.com/j2se/1.5.0/docs/guide/jdbc/getstart/GettingStartedTOC.fm.html
object JdbcTableDefLoader {
  case class JdbcColumnType(
    dbTypeName: String,
    jdbcTypeCode: Int,
    size: Int,
    fractionDigits: Int)
  class H2 extends JdbcTableDefLoader {
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
  class Hsqldb extends JdbcTableDefLoader {
    override def checkConstraints(conn: Connection,
      catalog: String, schemaPattern: String, tableNamePattern: String) =
      standardCheckConstraints(conn, catalog, schemaPattern, tableNamePattern)
  }
  class Oracle extends JdbcTableDefLoader {
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
      // TODO filter sys not null constraints, or not here?
      checks
    }
  }
  class Postgresql extends JdbcTableDefLoader {
    override def checkConstraints(conn: Connection,
      catalog: String, schemaPattern: String, tableNamePattern: String) =
      standardCheckConstraints(conn, catalog, schemaPattern, tableNamePattern)
  }
  private[in] class Other extends JdbcTableDefLoader {
    override def checkConstraints(conn: Connection,
        catalog: String, schemaPattern: String, tableNamePattern: String) = {
      Nil
    }
  }
  private[in] def integerOrSubtype(len: Int) =
    if (len > 18) new Type("integer", None, Some(len.toInt), None, false)
    else if (len > 9) new Type("long", None, Some(len.toInt), None, false)
    else new Type("int", None, Some(len.toInt), None, false)
  def jdbcTableDefs(conn: Connection,
    catalog: String, schemaPattern: String, tableNamePattern: String,
    types: String*) = {
    val loader = conn.getMetaData.getDatabaseProductName match {
      case "H2" => new H2
      case "HSQL Database Engine" => new Hsqldb
      case "Oracle" => new Oracle
      case "PostgreSQL" => new Postgresql
      case x =>
        println("JdbcTableDefLoader - unsupported database " + x +
          ", ignoring check constraints!")
        new Other
    }
    loader.jdbcTableDefs(
      conn, catalog, schemaPattern, tableNamePattern, types: _*)
  }
  def tableDefs(conn: Connection,
    catalog: String, schemaPattern: String, tableNamePattern: String,
    types: String*) =
    jdbcTableDefs(conn, catalog, schemaPattern, tableNamePattern, types: _*)
      .map(mapType)
  def mapType(jdbcColumnType: JdbcColumnType): Type =
    map(jdbcColumnType.jdbcTypeCode, jdbcColumnType.size, jdbcColumnType.fractionDigits)
  def mapType(tableDef: TableDef[JdbcColumnType]): TableDef[Type] =
    tableDef.copy(cols = tableDef.cols.map(c => c.copy(type_ = mapType(c.type_))))
  private[in] def map(jdbcTypeCode: Int, size: Int, fractionDigits: Int) =
    jdbcTypeCode match {
      case Types.ARRAY => new Type("base64Binary")
      case Types.BIGINT => new Type("long")
      case Types.BINARY => new Type("base64Binary")
      case Types.BIT => new Type("boolean")
      case Types.BLOB => new Type("base64Binary")
      case Types.BOOLEAN => new Type("boolean")
      case Types.CHAR =>
        // XXX booleans emulated on oracle
        // FIXME check enum = [Y, N], check db is oracle?
        if (size == 1) new Type("boolean")
        else new Type("string", size)
      case Types.CLOB => new Type("string", size)
      case Types.DATALINK => new Type("string", size) // anyURI instead?
      case Types.DATE => new Type("date")
      case Types.DECIMAL | Types.NUMERIC =>
        if (fractionDigits == 0) integerOrSubtype(size)
        else new Type("decimal", size, fractionDigits)
      case Types.DISTINCT =>
        // TODO need base type to solve this correctly
        new Type("string")
      case Types.DOUBLE => new Type("double")
      case Types.FLOAT => new Type("double") // Yes, double, not float! I mean it.
      case Types.INTEGER => new Type("int") // Yes, int, not integer! I mean it.
      case Types.JAVA_OBJECT => new Type("base64Binary")
      case Types.LONGNVARCHAR => new Type("string")
      case Types.LONGVARBINARY => new Type("base64Binary")
      case Types.LONGVARCHAR =>
        if (size > 0) new Type("string", size) else new Type("string")
      case Types.NCHAR => new Type("string", size)
      case Types.NCLOB => new Type("string", size)
      case Types.NULL => new Type("string")
      case Types.NVARCHAR => new Type("string", size)
      case Types.OTHER => new Type("base64Binary")
      case Types.REAL => new Type("float")
      case Types.REF => new Type("string", size)
      case Types.ROWID => new Type("string", size)
      case Types.SMALLINT => new Type("short")
      case Types.SQLXML => new Type("string")
      case Types.STRUCT => new Type("base64Binary")
      case Types.TIME => new Type("time")
      case Types.TIMESTAMP => new Type("dateTime")
      case Types.TINYINT =>
        // TODO? signed/unsigned byte?
        new Type("short")
      case Types.VARBINARY => new Type("base64Binary")
      case Types.VARCHAR => new Type("string", size)
      case x => sys.error("Unexpected jdbc type code: " + x)
    }
}

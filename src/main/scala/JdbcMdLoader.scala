package mojoz.metadata.in

import java.lang.Integer
import java.sql.Types

import mojoz.metadata._
import mojoz.metadata.{ XsdType => Type }

// java.sun.com/j2se/1.5.0/docs/guide/jdbc/getstart/GettingStartedTOC.fm.html
object JdbcToXsdTypeMapper {
  def map(jdbcTypeCode: Int, size: Integer, fractionDigits: Integer) =
    jdbcTypeCode match {
      case Types.ARRAY => new Type("base64Binary")
      case Types.BIGINT => new Type("long")
      case Types.BINARY => new Type("base64Binary")
      case Types.BIT => new Type("boolean")
      case Types.BLOB => new Type("base64Binary")
      case Types.BOOLEAN => new Type("boolean")
      case Types.CHAR => new Type("string", size)
      case Types.CLOB => new Type("string", size)
      case Types.DATALINK => new Type("string", size) // anyURI instead?
      case Types.DATE => new Type("date")
      case Types.DECIMAL | Types.NUMERIC =>
        // TODO choose best base type?
        new Type("decimal", size, fractionDigits)
      case Types.DISTINCT =>
        // TODO need base type to solve this correctly
        new Type("string")
      case Types.DOUBLE => new Type("double")
      case Types.FLOAT => new Type("double") // Yes, double, not float! I mean it.
      case Types.INTEGER => new Type("int") // Yes, int, not integer! I mean it.
      case Types.JAVA_OBJECT => new Type("base64Binary")
      case Types.LONGNVARCHAR => new Type("string")
      case Types.LONGVARBINARY => new Type("base64Binary")
      case Types.LONGVARCHAR => new Type("string", size)
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

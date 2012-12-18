package xsdgen;

import java.sql.Types;

// java.sun.com/j2se/1.5.0/docs/guide/jdbc/getstart/GettingStartedTOC.fm.html
public class JdbcToXsdTypeMapper {
    public static Type map(int jdbcTypeCode, Integer size,
            Integer fractionDigits) {
        switch (jdbcTypeCode) {
        case Types.ARRAY:
            return new Type("base64Binary");
        case Types.BIGINT:
            return new Type("long");
        case Types.BINARY:
            return new Type("base64Binary");
        case Types.BIT:
            return new Type("boolean");
        case Types.BLOB:
            return new Type("base64Binary");
        case Types.BOOLEAN:
            return new Type("boolean");
        case Types.CHAR:
            return new Type("string", size);
        case Types.CLOB:
            return new Type("string", size);
        case Types.DATALINK:
            return new Type("string", size); // anyURI instead?
        case Types.DATE:
            return new Type("date");
        case Types.DECIMAL:
        case Types.NUMERIC:
            // TODO choose best base type?
            return new Type("decimal", null, size, fractionDigits);
        case Types.DISTINCT:
            // TODO need base type to solve this correctly
            return new Type("string");
        case Types.DOUBLE:
            return new Type("double");
        case Types.FLOAT:
            return new Type("double"); // Yes, double, not float! I mean it.
        case Types.INTEGER:
            return new Type("int"); // Yes, int, not integer! I mean it.
        case Types.JAVA_OBJECT:
            return new Type("base64Binary");
        case Types.LONGNVARCHAR:
            return new Type("string");
        case Types.LONGVARBINARY:
            return new Type("base64Binary");
        case Types.LONGVARCHAR:
            return new Type("string", size);
        case Types.NCHAR:
            return new Type("string", size);
        case Types.NCLOB:
            return new Type("string", size);
        case Types.NULL:
            return new Type("string");
        case Types.NVARCHAR:
            return new Type("string", size);
        case Types.OTHER:
            return new Type("base64Binary");
        case Types.REAL:
            return new Type("float");
        case Types.REF:
            return new Type("string", size);
        case Types.ROWID:
            return new Type("string", size);
        case Types.SMALLINT:
            return new Type("short");
        case Types.SQLXML:
            return new Type("string");
        case Types.STRUCT:
            return new Type("base64Binary");
        case Types.TIME:
            return new Type("time");
        case Types.TIMESTAMP:
            return new Type("dateTime");
        case Types.TINYINT:
            // TODO? signed/unsigned byte?
            return new Type("short");
        case Types.VARBINARY:
            return new Type("base64Binary");
        case Types.VARCHAR:
            return new Type("string", size);
        default:
            throw new RuntimeException("Unexpected jdbc type code: "
                    + jdbcTypeCode);
        }
    }
}

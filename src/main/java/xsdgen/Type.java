package xsdgen;

public class Type {
    final String xsdTypeName;

    final Integer length;

    final Integer totalDigits;

    final Integer fractionDigits;

    public Type(String xsdTypeName) {
        this(xsdTypeName, null, null, null);
    }

    public Type(String xsdTypeName, Integer length) {
        this(xsdTypeName, length, null, null);
    }

    Type(String xsdTypeName, Integer length, Integer totalDigits,
            Integer fractionDigits) {
        this.xsdTypeName = xsdTypeName;
        this.length = length;
        this.totalDigits = totalDigits;
        this.fractionDigits = fractionDigits;
    }
}

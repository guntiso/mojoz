package xsdgen;

import java.util.*;

/**
 * @author Guntis Ozols 2005.09.15
 */
public class ElementName {

    private enum CharType {
        UPPER, DIGIT, OTHER
    }

    public static String get(String name) {
        StringBuilder buf = new StringBuilder(name.length() * 2);
        CharType charType;
        CharType prevCharType = null;
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isUpperCase(c)) {
                charType = CharType.UPPER;
            } else if (Character.isDigit(c)) {
                charType = CharType.DIGIT;
            } else {
                charType = CharType.OTHER;
            }
            if (i > 0
                    && charType != prevCharType
                    && !(prevCharType == CharType.UPPER && charType == CharType.OTHER)) {
                buf.append('-');
            }
            if (charType == CharType.UPPER) {
                buf.append(Character.toLowerCase(c));
            } else {
                buf.append(c);
            }
            prevCharType = charType;
        }
        name = buf.toString();
        return name;
    }   
}

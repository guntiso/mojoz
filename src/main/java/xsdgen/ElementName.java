package xsdgen;

import java.util.*;

/**
 * @author Guntis Ozols 2005.09.15
 */
public class ElementName {

    private enum CharType {
        UPPER, DIGIT, OTHER
    }

    private static Map<Class<?>, String> elementNameCache = Collections
            .synchronizedMap(new HashMap<Class<?>, String>());

    public static String get(Class<?> elementClass) {
        String name = elementNameCache.get(elementClass);
        if (name != null) {
            return name;
        }
        name = elementClass.getSimpleName();
        if (name.indexOf("_$$_") > 0) {
            // javassist (newer hibernate)
            name = name.substring(0, name.indexOf("_$$_"));
        }
        if (name.indexOf('$') > 0) {
            // cglib (older hibernate)
            name = name.substring(0, name.indexOf('$'));
        }
        name = get(name);
        elementNameCache.put(elementClass, name);
        return name;
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

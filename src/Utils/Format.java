package Utils;


import Common.*;
import Tokenizer.Token;

import java.util.List;

public class Format {

    public static boolean isAlpha(Integer input) {
        return (input >= 'a' && input <= 'z') || (input >= 'A' && input <= 'Z');
    }

    public static boolean isDigit(Integer input) {
        return input >= '0' && input <= '9';
    }

    public static boolean isEscapeSequence(Integer input) {
        if (input == '\'' || input == '\"' ||
                input == '\\' || input == 'n' ||
                input == 't' || input == 'r') return true;
        return false;
    }

    public static boolean isRegularChar(Integer input) {
        if (input != '\r' && input != '\t' && input != '\n' && input != '\\' && input != '"') return true;
        return false;
    }

    public static boolean isSpace(Integer input) {
        if (input == ' ' || input == '\n' || input == '\t' || input == '\r') return true;
        return false;
    }

    public static boolean isStaticFunction(String name) {
        if (name.equals("getint") || name.equals("getdouble") || name.equals("getchar") ||
            name.equals("putint") || name.equals("putdouble") || name.equals("putchar") ||
            name.equals("putstr") || name.equals("putln"))
            return true;
        return false;
    }

    public static boolean checkStaticFunctionParam(String type) {
        boolean result = false;
        switch (type) {
            case "getint":
                result = type.equals("int");
                break;
            case "getdouble":
                result = type.equals("double");
                break;
            case "getchar":
                result = type.equals("int");
                break;
            default:
                result = type.equals("void");
        }
        return result;
    }

    public static boolean isValidName(List<Function> functions, List<Constant> constants, List<Variable> variables, String name, Integer level) {
        if (isStaticFunction(name)) return false;
        for (Function function : functions) {
            if (function.getName().equals(name) && level == 1)
                return false;
        }
        for (Constant constant : constants) {
            if (constant.getName().equals(name) && level == constant.getLevel())
                return false;

        }
        for (Variable variable : variables) {
            if (variable.getName().equals(name) && level == variable.getLevel())
                return false;
        }
        return true;
    }

    public static boolean isVariable(String name, List<Variable> variables) {
        for (Variable variable : variables) {
            if (variable.getName().equals(name)) return true;
        }
        return false;
    }

    public static boolean isConstant(String name, List<Constant> constants) {
        for (Constant constant : constants) {
            if (constant.getName().equals(name)) return true;
        }
        return false;
    }

    public static boolean isFunction(String name, List<Function> functions) {
        if (isStaticFunction(name)) return true;
        for (Function function : functions) {
            if (function.getName().equals(name)) return true;
        }
        return false;
    }

    public static boolean isParam(String name, List<Param> params) {
        for (Param param : params) {
            if (param.getName().equals(name)) return true;
        }
        return false;
    }

    public static boolean checkParam(String name, List<Function> functions, Integer num) {
        if (isStaticFunction(name)) {
            if (name.equals("getint") || name.equals("getdouble") || name.equals("getchar") || name.equals("putln")) {
                return num==0;
            }
            else return num==1;
        }
        for (Function function : functions) {
            if (function.getName().equals(name)) {
                if (num == function.getParams().size()) return true;
            }
        }
        return false;
    }

    public static boolean isOperator(Token symbol) {
        if (    symbol.getType() != TokenType.PLUS &&
                symbol.getType() != TokenType.MINUS &&
                symbol.getType() != TokenType.MUL &&
                symbol.getType() != TokenType.DIV &&
                symbol.getType() != TokenType.EQ &&
                symbol.getType() != TokenType.NEQ &&
                symbol.getType() != TokenType.LE &&
                symbol.getType() != TokenType.LT &&
                symbol.getType() != TokenType.GE &&
                symbol.getType() != TokenType.GT) return false;
        return true;
    }


    public static Global functionNameToGlobalInformation(String name) {
        char[] arr = name.toCharArray();
        int len = arr.length;
        String[] items = new String[len];
        for (int i = 0; i < arr.length; ++i) {
            int asc = (int) arr[i];
            items[i] = String.format("%2X", asc);
        }
        Global global = new Global(1, arr.length, items);
        return global;
    }

    public static boolean isInitLibrary(String name, List<LibraryFunction> libraryFunctions) {
        for (LibraryFunction function : libraryFunctions) {
            if (function.getName().equals(name)) return true;
        }
        return false;
    }

    public static void clearLocal(List<Variable> variables, List<Constant> constants) {
        for (Variable variable : variables) {
            if (variable.getLevel() > 1) variables.remove(variable);
        }
        for (Constant constant : constants) {
            if (constant.getLevel() > 1) constants.remove(constant);
        }
    }
    public static boolean isLocal(String name, List<Constant> constants, List<Variable> variables) {
        for (Constant constant : constants) {
            if (constant.getName().equals(name) && constant.getLevel() > 1) return true;
        }
        for (Variable variable : variables) {
            if (variable.getName().equals(name) && variable.getLevel() > 1) return true;
        }
        return false;
    }

    public static void getId() {

    }
}

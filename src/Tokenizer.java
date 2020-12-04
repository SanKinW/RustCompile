import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

public class Tokenizer {
    private static int input;
    private static HashMap<String, String> KeyWords;
    private static List<Token> tokenList;
    private static Iterator<Token> it;

    public static void processSource(InputStream inputStream) throws IOException {
        KeyWords = Type.init();
        tokenList = new ArrayList<>();
        input = inputStream.read();
        while (input != -1) {
            if (!Format.isSpace(input)) {
                try {
                    Token token = getToken(inputStream);
                    if (token != null) {
                        tokenList.add(token);
                        System.out.print(token.getVal() + " ");
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            else input = inputStream.read();
        }
        initIterator();
    }

    public static Token getToken(InputStream inputStream) throws Exception {
        Token token = null;
        String val = "";
        char temp = (char) input;
        if (Format.isAlpha(input)) {
            while (true) {
                val = val + temp;
                input = inputStream.read();
                if (!(Format.isAlpha(input) || Format.isDigit(input))) {
                    String type = KeyWords.get(val);
                    if (type == null) token = new Token(TokenType.IDENT, val);
                    else {
                        if (type.equals("FN_KW")) token = new Token(TokenType.FN_KW, val);
                        else if (type.equals("LET_KW")) token = new Token(TokenType.LET_KW, val);
                        else if (type.equals("CONST_KW")) token = new Token(TokenType.CONST_KW, val);
                        else if (type.equals("AS_KW")) token = new Token(TokenType.AS_KW, val);
                        else if (type.equals("WHILE_KW")) token = new Token(TokenType.WHILE_KW, val);
                        else if (type.equals("IF_KW")) token = new Token(TokenType.IF_KW, val);
                        else if (type.equals("ELSE_KW")) token = new Token(TokenType.ELSE_KW, val);
                        else if (type.equals("RETURN_KW")) token = new Token(TokenType.RETURN_KW, val);
                        else if (type.equals("BREAK_KW")) token = new Token(TokenType.BREAK_KW, val);
                        else if (type.equals("CONTINUE_KW")) token = new Token(TokenType.CONTINUE_KW, val);
                    }
                    break;
                }
                temp = (char) input;
            }
        }
        else if (Format.isDigit(input)) {
            while (true) {
                val = val + temp;
                input = inputStream.read();
                if (!Format.isDigit(input)) {
                    Integer num = Integer.parseInt(val);
                    token = new Token(TokenType.UINT_LITERAL, num);
                    break;
                }
                temp = (char) input;
            }
        }
        else if (input == '"') {
            while (true) {
                input = inputStream.read();
                if (input == '\\') {
                    input = inputStream.read();
                    if(!Format.isEscapeSequence(input)) throw new Exception();
                    else  {
                        switch (input) {
                            case '\'':
                                temp =  '\'';
                                break;
                            case '\"':
                                temp = '\"';
                                break;
                            case '\\':
                                temp = '\\';
                                break;
                            case 'n':
                                temp = '\n';
                                break;
                            case 'r':
                                temp = '\r';

                                break;
                            case 't':
                                temp = '\t';
                                break;
                        }
                    }
                }
                else if (Format.isRegularChar(input)) {
                    temp = (char) input;
                }
                else if(input == '\"'){
                    token = new Token(TokenType.STRING_LITERAL, val);
                    input = inputStream.read();
                    break;
                }
                else throw new Exception();
                val = val + temp;
            }
        }
        else if(input == '=') {
            input = inputStream.read();
            if (input == '=') {
                token = new Token(TokenType.EQ, "==");
                input = inputStream.read();
            }
            else token = new Token(TokenType.ASSIGN, "=");
        }
        else if (input == '!') {
            input = inputStream.read();
            if (input == '=') {
                token = new Token(TokenType.NEQ, "!=");
                input = inputStream.read();
            }
            else throw new Exception();
        }
        else if (input == '<') {
            input = inputStream.read();
            if (input == '=') {
                token = new Token(TokenType.LE, "<=");
                input = inputStream.read();
            }
            else token = new Token(TokenType.LT, "<");
        }
        else if (input == '>') {
            input = inputStream.read();
            if (input == '=') {
                token = new Token(TokenType.GE, ">=");
                input = inputStream.read();
            }
            else token = new Token(TokenType.GT, ">");
        }
        else if (input == '-') {
            input = inputStream.read();
            if (input == '>') {
                token = new Token(TokenType.ARROW, "->");
                input = inputStream.read();
            }
            else token = new Token(TokenType.MINUS, "-");
        }
        else if (input == '/') {
            input = inputStream.read();
            if (input == '/') {
                while (true) {
                    input = inputStream.read();
                    if (input == '\n') {
                        input = inputStream.read();
                        break;
                    }
                }
            }else throw new Exception();
        }
        else {
                if (input == '+') token = new Token(TokenType.PLUS, "+");
                else if (input == '*') token = new Token(TokenType.MUL, "*");
                else if (input == '/') token = new Token(TokenType.DIV, "/");
                else if (input == '(') token = new Token(TokenType.L_PAREN, "(");
                else if (input == ')') token = new Token(TokenType.R_PAREN, ")");
                else if (input == '{') token = new Token(TokenType.L_BRACE, "{");
                else if (input == '}') token = new Token(TokenType.R_BRACE, "}");
                else if (input == ',') token = new Token(TokenType.COMMA, ",");
                else if (input == ':') token = new Token(TokenType.COLON, ":");
                else if (input == ';') token = new Token(TokenType.SEMICOLON, ";");
                else throw new Exception();
                input = inputStream.read();
        }
        return token;
    }

    public static List<Token> getTokenList() {
        return tokenList;
    }

    public static void initIterator() {
        it = tokenList.iterator();
    }

    public static boolean hasNext() {return it.hasNext();}

    public static Token readToken() {
        if (it.hasNext()) {
            Token token = it.next();
            //System.out.println(token);
            return token;
        }
        return null;
    }

}

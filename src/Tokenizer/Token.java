package Tokenizer;

import Common.TokenType;

public class Token {
    private TokenType type;
    private Object val;

    public Object getVal() {
        return val;
    }

    public TokenType getType() {
        return type;
    }

    public Token(TokenType type, Object val) {
        this.type = type;
        this.val = val;
    }

    @Override
    public String toString() {
        return "Token{" +
                "type='" + type + '\'' +
                ", val=" + val +
                '}';
    }
}

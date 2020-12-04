public enum TokenType {
    FN_KW("FN_KW"),
    LET_KW("LET_KW"),
    CONST_KW("CONST_KW"),
    AS_KW("AS_KW"),
    WHILE_KW("WHILE_KW"),
    IF_KW("IF_KW"),
    ELSE_KW("ELSE_KW"),
    RETURN_KW("RETURN_KW"),
    BREAK_KW("BREAK_KW"),
    CONTINUE_KW("CONTINUE_KW"),
    UINT_LITERAL("UINT_LITERAL"),
    STRING_LITERAL("STRING_LITERAL"),
    IDENT("IDENT"),
    PLUS("PLUS"),
    MINUS("MINUS"),
    MUL("MUL"),
    DIV("DIV"),
    ASSIGN("ASSIGN"),
    EQ("EQ"),
    NEQ("NEQ"),
    LT("LT"),
    GT("GT"),
    LE("LE"),
    GE("GE"),
    L_PAREN("L_PAREN"),// (
    R_PAREN("R_PAREN"),// )
    L_BRACE("L_BRACE"),// {
    R_BRACE("R_BRACE"),// }
    ARROW("ARROW"),
    COMMA("COMMA"),
    COLON("COLON"),
    SEMICOLON("SEMICOLON");

    private String kind;
    TokenType(String kind) {
        this.kind = kind;
    }

}

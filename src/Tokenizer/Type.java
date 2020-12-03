package Tokenizer;

import java.util.HashMap;
import java.util.Map;

public class Type {

    public static HashMap<String, String> init() {
        HashMap<String, String> hashMap = new HashMap();
        hashMap.put("fn", "FN_KW");
        hashMap.put("let", "LET_KW");
        hashMap.put("const", "CONST_KW");
        hashMap.put("as", "AS_KW");
        hashMap.put("while", "WHILE_KW");
        hashMap.put("if", "IF_KW");
        hashMap.put("else", "ELSE_KW");
        hashMap.put("return", "RETURN_KW");
        hashMap.put("break", "BREAK_KW");
        hashMap.put("continue", "CONTINUE_KW");
        return hashMap;
    }

}

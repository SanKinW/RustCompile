import Analyser.Analyser;
import Binary.OutToBinary;
import Common.FunctionDef;
import Common.Global;
import Tokenizer.Tokenizer;
import Tokenizer.Token;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

public class Test {
    //https://SanKinW:45dc826862b18c003d799faf2d133cf920985c41@github.com/SanKinW/Compile.git
    public static void main(String[] args) throws Exception {
        String file_path = "src/test.txt";
        Tokenizer.processSource(file_path);
        for (Token token:Tokenizer.getTokenList()) {
            //System.out.println(token);
        }
        System.out.println("------------------Analyser Start");
        Analyser.analyseProgram();
        for (Global global : Analyser.getGlobals()) {
            System.out.println(global);
        }
        for (FunctionDef functionDef: Analyser.getFunctionDefs()) {
            System.out.println(functionDef);
        }
        System.out.println("---------------生成二进制");
        OutToBinary binary = new OutToBinary(Analyser.getGlobals(), Analyser.getFunctionDefs());
        binary.generate();
    }
}

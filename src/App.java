import Analyser.Analyser;
import Binary.OutToBinary;
import Common.FunctionDef;
import Common.Global;
import Tokenizer.Tokenizer;
import Tokenizer.Token;

import java.io.*;
import java.util.List;

public class App {
    //https://SanKinW:45dc826862b18c003d799faf2d133cf920985c41@github.com/SanKinW/Compile.git
    public static void main(String[] args) throws Exception {
        //String file_path = "src/test.txt";
        if (args.length < 2) throw new Exception();
        InputStream inputStream = new FileInputStream(args[0]);
        //InputStream inputStream = new FileInputStream(file_path);
        Tokenizer.processSource(inputStream);
        for (Token token : Tokenizer.getTokenList()) {
            //System.out.println(token);
        }
        System.out.println("------------------Analyser Start");
        Analyser.analyseProgram();
        System.out.println(Analyser.getStartFunction());
        for (FunctionDef functionDef : Analyser.getFunctionDefs()) {
            System.out.println(functionDef);
        }
        System.out.println("---------------生成二进制");
        OutToBinary binary = new OutToBinary(Analyser.getGlobals(), Analyser.getStartFunction(), Analyser.getFunctionDefs());

        DataOutputStream out = new DataOutputStream(new FileOutputStream(new File(args[1])));
        List<Byte> bytes = binary.generate();
        byte[] resultBytes = new byte[bytes.size()];
        for (int i = 0; i < bytes.size(); ++i) {
            resultBytes[i] = bytes.get(i);
        }
        out.write(resultBytes);
    }
}
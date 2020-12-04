import java.io.*;
import java.util.List;

public class App {
    //https://SanKinW:45dc826862b18c003d799faf2d133cf920985c41@github.com/SanKinW/Compile.git
    public static void main(String[] args) throws Exception {
        //String file_path = "src/test.txt";
        if (args.length < 3) throw new Exception();
        InputStream inputStream = new FileInputStream(args[1]);
        //InputStream inputStream = new FileInputStream(file_path);
        Tokenizer.processSource(inputStream);
        for (Token token : Tokenizer.getTokenList()) {
            System.out.print(token.getVal());
        }
        System.out.println("------------------Analyser Start");
        Analyser.analyseProgram();
        System.out.println(Analyser.getStartFunction());
        for (FunctionDef functionDef : Analyser.getFunctionDefs()) {
            System.out.println(functionDef);
        }
        System.out.println("---------------生成二进制");
        OutToBinary binary = new OutToBinary(Analyser.getGlobals(), Analyser.getStartFunction(), Analyser.getFunctionDefs());

        DataOutputStream out = new DataOutputStream(new FileOutputStream(new File(args[2])));
        List<Byte> bytes = binary.generate();
        byte[] resultBytes = new byte[bytes.size()];
        for (int i = 0; i < bytes.size(); ++i) {
            resultBytes[i] = bytes.get(i);
        }
        out.write(resultBytes);
    }
}

import java.io.*;
import java.util.List;

public class App {
    public static void main(String[] args)  {
        try {
            InputStream inputStream = new FileInputStream(args[0]);
            Tokenizer.processSource(inputStream);
            System.out.println("\n------------------Analyser Start");
            Analyser.analyseProgram();
            System.out.println(Analyser.getGlobals().size());
            for (Global global : Analyser.getGlobals()) {
                System.out.println(global);
            }
            System.out.println("-----------------------------function");
            System.out.println(Analyser.getStartFunction());
            for (FunctionDef functionDef : Analyser.getFunctionDefs()) {
                System.out.println(functionDef);
            }
            System.out.println("\n----------------------------生成二进制");
            OutToBinary binary = new OutToBinary(Analyser.getGlobals(), Analyser.getStartFunction(), Analyser.getFunctionDefs());

            DataOutputStream out = new DataOutputStream(new FileOutputStream(new File(args[1])));
            List<Byte> bytes = binary.generate();
            byte[] resultBytes = new byte[bytes.size()];
            for (int i = 0; i < bytes.size(); ++i) {
                resultBytes[i] = bytes.get(i);
            }
            out.write(resultBytes);
        }catch (Exception e) {
            System.exit(-1);
        }
    }
}

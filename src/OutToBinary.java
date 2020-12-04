import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class OutToBinary {
    private List<Global> globals;
    private FunctionDef start;
    private List<FunctionDef> functionDefs;
    private List<Byte> output;

    //DataOutputStream out = new DataOutputStream(new FileOutputStream(new File("src/out.txt")));

    int magic=0x72303b3e;
    int version=0x00000001;

    public OutToBinary(List<Global> globals, FunctionDef start, List<FunctionDef> functionDefs) throws FileNotFoundException {
        this.globals = globals;
        this.start = start;
        this.functionDefs = functionDefs;
        output = new ArrayList<>();
    }

    public List<Byte> generate() throws IOException {
        //magic
        List<Byte> magic=int2bytes(4,this.magic);
        output.addAll(magic);
        //version
        List<Byte> version=int2bytes(4,this.version);
        output.addAll(version);

        //globals.count
        List<Byte> globalCount=int2bytes(4,globals.size());
        output.addAll(globalCount);

        //out.writeBytes(globalCount.toString());

        for(Global global : globals){
            //isConst
            List<Byte> isConst=int2bytes(1, global.getIsConst());
            output.addAll(isConst);

            //out.writeBytes(isConst.toString());

            // value count
            List<Byte> globalValueCount=int2bytes(4, global.getValueCount());
            output.addAll(globalValueCount);
            //out.writeBytes(globalValueCount.toString());

            //value items
            List<Byte> globalValue;
            if (global.getValueItems() == null) globalValue = long2bytes(8,0L);
            else globalValue = String2bytes(global.getValueItems());
            output.addAll(globalValue);
            //out.writeBytes(globalValue.toString());
            System.out.println(global.getValueItems());
        }

        //functions.count
        List<Byte> functionsCount=int2bytes(4, functionDefs.size() + 1);
        output.addAll(functionsCount);
        //out.writeBytes(functionsCount.toString());

        generateFunction(start);

        for(FunctionDef functionDef : functionDefs){
            generateFunction(functionDef);
        }
        return output;
    }

    private void generateFunction(FunctionDef functionDef) throws IOException {
        //name
        List<Byte> name = int2bytes(4,functionDef.getId());
        output.addAll(name);
        //out.writeBytes(name.toString());

        //retSlots
        List<Byte> retSlots = int2bytes(4,functionDef.getReturnSlots());
        output.addAll(retSlots);
        //out.writeBytes(retSlots.toString());

        //paramsSlots;
        List<Byte> paramsSlots=int2bytes(4,functionDef.getParamSlots());
        output.addAll(paramsSlots);
        //out.writeBytes(paramsSlots.toString());

        //locSlots;
        List<Byte> locSlots=int2bytes(4,functionDef.getLocalSlots());
        output.addAll(locSlots);
        //out.writeBytes(locSlots.toString());

        List<Instructions> instructions = functionDef.getBody();

        //bodyCount
        List<Byte> bodyCount=int2bytes(4, instructions.size());
        output.addAll(bodyCount);
        //out.writeBytes(bodyCount.toString());

        //instructions
        for(Instructions instruction : instructions){
            //type
            List<Byte> type = int2bytes(1, instruction.getInstruction());
            output.addAll(type);
            //out.writeBytes(type.toString());

            if(instruction.getParam() != null){
                List<Byte>  x;
                if(instruction.getInstruction() == 1)
                    x = long2bytes(8,instruction.getParam());
                else
                    x = int2bytes(4,instruction.getParam());
                output.addAll(x);
                //out.writeBytes(x.toString());
            }
        }
    }

    private List<Byte> Char2bytes(char value) {
        List<Byte>  AB=new ArrayList<>();
        AB.add((byte)(value&0xff));
        return AB;
    }

    private List<Byte> String2bytes(String valueString) {
        List<Byte>  AB=new ArrayList<>();
        for (int i=0;i<valueString.length();i++){
            char ch=valueString.charAt(i);
            if (ch!='\\')
                AB.add((byte)(ch&0xff));
            else {
                i++;
                ch=valueString.charAt(i);
                if (ch=='\\')
                    AB.add((byte)('\\'&0xff));
                else if (ch=='\"')
                    AB.add((byte)('\"'&0xff));
                else if (ch=='\'')
                    AB.add((byte)('\''&0xff));
                else if (ch=='n')
                    AB.add((byte)('\n'&0xff));
                else if (ch=='r')
                    AB.add((byte)('\r'&0xff));
                else if (ch=='t')
                    AB.add((byte)('\t'&0xff));
            }
        }
        return AB;
    }

    private List<Byte> long2bytes(int length, long target) {
        ArrayList<Byte> bytes = new ArrayList<>();
        int start = 8*(length-1);
        for(int i = 0 ; i < length; i++){
            bytes.add((byte) (( target >> ( start - i * 8 )) & 0xFF ));
        }
        return bytes;
    }

    private ArrayList<Byte> int2bytes(int length,int target){
        ArrayList<Byte> bytes = new ArrayList<>();
        int start = 8*(length-1);
        for(int i = 0 ; i < length; i++){
            bytes.add((byte) (( target >> ( start - i * 8 )) & 0xFF ));
        }
        return bytes;
    }
}

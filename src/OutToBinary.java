import java.util.ArrayList;
import java.util.List;

public class OutToBinary {
    private List<Global> globals;
    private FunctionDef start;
    private List<FunctionDef> functionDefs;
    private List<Byte> output;

    int magic=0x72303b3e;
    int version=0x00000001;

    public OutToBinary(List<Global> globals, FunctionDef start, List<FunctionDef> functionDefs) {
        this.globals = globals;
        this.start = start;
        this.functionDefs = functionDefs;
        output = new ArrayList<>();
    }

    public List<Byte> generate() {
        //magic
        List<Byte> magic=int2bytes(4,this.magic);
        output.addAll(magic);
        //version
        List<Byte> version=int2bytes(4,this.version);
        output.addAll(version);

        //globals.count
        List<Byte> globalCount=int2bytes(4,globals.size());
        output.addAll(globalCount);

        for(Global global : globals){
            //isConst
            List<Byte> isConst=int2bytes(1,global.getIsConst());
            output.addAll(isConst);

            // value count
            List<Byte> globalValueCount=int2bytes(4,global.getValueCount());
            output.addAll(globalValueCount);

            //value items
            List<Byte> globalValue;
            if (global.getValueItems() == null) globalValue = long2bytes(8,0);
            else globalValue = String2bytes(global.getValueItems());
            output.addAll(globalValue);
        }

        //functions.count
        List<Byte> functions_count=int2bytes(4, functionDefs.size() + 1);
        output.addAll(functions_count);

        generateFunction(start);

        for(FunctionDef functionDef : functionDefs){
            generateFunction(functionDef);
        }
        return output;
    }

    private void generateFunction(FunctionDef functionDef) {
        //name
        List<Byte> name = int2bytes(4,functionDef.getId());
        output.addAll(name);

        //retSlots
        List<Byte> retSlots = int2bytes(4,functionDef.getReturnSlots());
        output.addAll(retSlots);

        //paramsSlots;
        List<Byte> paramsSlots=int2bytes(4,functionDef.getParamSlots());
        output.addAll(paramsSlots);

        //locSlots;
        List<Byte> locSlots=int2bytes(4,functionDef.getLocalSlots());
        output.addAll(locSlots);

        List<Instructions> instructions = functionDef.getBody();
        //body_count
        List<Byte> body_count=int2bytes(4, instructions.size());
        output.addAll(body_count);

        //instructions
        for(Instructions instruction : instructions){
            //type
            List<Byte> type=int2bytes(1,instruction.getInstruction());
            output.addAll(type);
            if(instruction.getParam() != null){
                List<Byte>  x;
                if(instruction.getInstruction() == 1)
                    x = long2bytes(8,instruction.getParam());
                else
                    x = int2bytes(4,instruction.getParam());
                output.addAll(x);
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
        int start = 8 * (length-1);
        for(int i = 0 ; i < length; i++){
            bytes.add((byte) (( target >> ( start - i * 8 )) & 0xFF ));
        }
        return bytes;
    }

    private ArrayList<Byte> int2bytes(int length,int target){
        ArrayList<Byte> bytes = new ArrayList<>();
        int start = 8 * (length-1);
        for(int i = 0 ; i < length; i++){
            bytes.add((byte) (( target >> ( start - i * 8 )) & 0xFF ));
        }
        return bytes;
    }
}

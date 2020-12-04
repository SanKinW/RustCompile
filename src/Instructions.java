public class Instructions {
    private Instruction instruction;
    private Integer param;

    public Instructions() {}

    public Instructions(Instruction instruction, Integer paramIds) {
        this.instruction = instruction;
        this.param = paramIds;
    }

    public Integer getInstruction() {
        return instruction.getNum();
    }

    public void setInstruction(Instruction instruction) {
        this.instruction = instruction;
    }

    public Integer getParam() {
        return param;
    }

    public void setParam(Integer param) {
        this.param = param;
    }

    @Override
    public String toString() {
        return "\n      Instructions{\n" +
                "           instruction=" + instruction +
                ",\n        paramIds=" + param + '\n'+
                '}';
    }
}

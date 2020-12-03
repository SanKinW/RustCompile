package Common;

public class Instructions {
    private Instruction instruction;
    private Long[] paramIds;

    public Instructions(Instruction instruction, Long[] paramIds) {
        this.instruction = instruction;
        this.paramIds = paramIds;
    }
}

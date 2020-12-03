package Common;

import java.util.Arrays;

public class Instructions {
    private Instruction instruction;
    private Long[] paramIds;

    public Instructions(Instruction instruction, Long[] paramIds) {
        this.instruction = instruction;
        this.paramIds = paramIds;
    }

    public void setParamIds(Long[] paramIds) {
        this.paramIds = paramIds;
    }

    @Override
    public String toString() {
        return "Instructions{" +
                "instruction=" + instruction +
                ", paramIds=" + Arrays.toString(paramIds) +
                '}';
    }
}

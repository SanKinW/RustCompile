package Common;

import java.util.Arrays;

public class Global {
    private Integer isConst;
    private Integer valueCount;
    private String[] valueItems;
    public Global(Integer isConst) {
        this.isConst = isConst;
        valueCount = 0;
    }
    public Global(Integer isConst, Integer valueCount, String[] valueItems) {
        this.isConst = isConst;
        this.valueCount = valueCount;
        this.valueItems = valueItems;
    }

    @Override
    public String toString() {
        return "Global{" +
                "isConst=" + isConst +
                ", valueCount=" + valueCount +
                ", valueItems=" + Arrays.toString(valueItems) +
                '}';
    }
}

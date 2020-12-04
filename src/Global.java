import java.util.Arrays;

public class Global {
    private Integer isConst;
    private Integer valueCount;
    private String valueItems;
    public Global(Integer isConst) {
        this.isConst = isConst;
        this.valueCount = 0;
        this.valueItems = null;
    }
    public Global(Integer isConst, Integer valueCount, String valueItems) {
        this.isConst = isConst;
        this.valueCount = valueCount;
        this.valueItems = valueItems;
    }

    public Integer getIsConst() {
        return isConst;
    }

    public Integer getValueCount() {
        return valueCount;
    }

    public String getValueItems() {
        return valueItems;
    }

    @Override
    public String toString() {
        return "Global{" +
                "isConst=" + isConst +
                ", valueCount=" + valueCount +
                ", valueItems=" + valueItems +
                '}';
    }
}

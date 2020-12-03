package Common;

import java.util.List;

public class FunctionDef {
    private Long id;
    private Integer returnSlots;
    private Integer paramSlots;
    private Integer localSlots;
    private List<Instructions> body;

    public FunctionDef(Long name, Integer returnSlots, Integer paramSlots, Integer localSlots, List<Instructions> body) {
        this.id = name;
        this.returnSlots = returnSlots;
        this.paramSlots = paramSlots;
        this.localSlots = localSlots;
        this.body = body;
    }

    public List<Instructions> getBody() {
        return body;
    }

    @Override
    public String toString() {
        return "FunctionDef{" +
                "id=" + id +
                ", returnSlots=" + returnSlots +
                ", paramSlots=" + paramSlots +
                ", localSlots=" + localSlots +
                ", body=" + body +
                '}';
    }
}

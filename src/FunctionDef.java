import java.util.List;

public class FunctionDef {
    private Integer id;
    private Integer returnSlots;
    private Integer paramSlots;
    private Integer localSlots;
    private List<Instructions> body;

    public FunctionDef(Integer name, Integer returnSlots, Integer paramSlots, Integer localSlots, List<Instructions> body) {
        this.id = name;
        this.returnSlots = returnSlots;
        this.paramSlots = paramSlots;
        this.localSlots = localSlots;
        this.body = body;
    }

    public Integer getId() {
        return id;
    }

    public Integer getLocalSlots() {
        return localSlots;
    }

    public Integer getParamSlots() {
        return paramSlots;
    }

    public Integer getReturnSlots() {
        return returnSlots;
    }


    public List<Instructions> getBody() {
        return body;
    }

    @Override
    public String toString() {
        return "FunctionDef{\n" +
                "   id=" + id +
                ",\n    returnSlots=" + returnSlots +
                ",\n    paramSlots=" + paramSlots +
                ",\n    localSlots=" + localSlots +
                ",\n    body=" + body +'\n'+
                '}';
    }
}

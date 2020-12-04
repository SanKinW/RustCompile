package Common;

import java.util.List;

public class Function {
    private String type;
    private String name;
    private List<Param> params;
    private Integer id;

    public Function(String type, String name, List<Param> params, Integer id) {
        this.type = type;
        this.name = name;
        this.params = params;
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public List<Param> getParams() {
        return params;
    }

    public Integer getId() {
        return id;
    }
}

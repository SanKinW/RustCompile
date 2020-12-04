package Common;

import java.security.acl.LastOwnerException;

public class Constant {
    private String name;
    private Integer id;
    private Integer level;

    public Constant(String name, Integer id, Integer level) {
        this.name = name;
        this.id = id;
        this.level = level;
    }

    public String getName() {
        return name;
    }

    public Integer getId() {
        return id;
    }

    public Integer getLevel() {
        return level;
    }
}

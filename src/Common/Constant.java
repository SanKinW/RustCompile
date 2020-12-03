package Common;

import java.security.acl.LastOwnerException;

public class Constant {
    private String name;
    private Long id;
    private Integer level;

    public Constant(String name, Long val, Integer level) {
        this.name = name;
        this.id = val;
        this.level = level;
    }

    public String getName() {
        return name;
    }

    public Long getId() {
        return id;
    }

    public Integer getLevel() {
        return level;
    }
}

package Common;

public class Variable {
    private String name;
    private Long id;
    private Integer level;

    public Variable(String name, Integer level) {
        this.name = name;
        this.id = null;
        this.level = level;
    }

    public Variable(String name, Long val, Integer level) {
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

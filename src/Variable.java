public class Variable {
    private String name;
    private Integer id;
    private Integer level;

    public Variable(String name, Integer level) {
        this.name = name;
        this.id = null;
        this.level = level;
    }

    public Variable(String name, Integer id, Integer level) {
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

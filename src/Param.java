public class Param {
    private String type;
    private String name;

    public Param(String type, String name) {
        this.type = type;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }
}

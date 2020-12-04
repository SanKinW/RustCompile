package Common;

public enum Instruction {
    nop(0x00),
    push(0x01),
    pop(0x02),
    popn(0x03),
    dup(0x04),
    loca(0x0a),
    arga(0x0b),
    globa(0x0c),
    load(0x13),
    store(0x17),
    alloc(0x18),
    free(0x19),
    stackalloc(0x1a),
    add(0x20),
    sub(0x21),
    mul(0x22),
    div(0x23),
    shl(0x29),
    shr(0x2a),
    and(0x2b),
    or(0x2c),
    xor(0x2d),
    not(0x2e),
    cmp(0x30),
    neg(0x34),
    shrl(0x38),
    setLt(0x39),
    setGt(0x3a),
    br(0x41),
    brFalse(0x42),
    brTrue(0x43),
    call(0x48),
    ret(0x49),
    callname(0x4a),
    sacn(0x50),
    print(0x54),
    println(0x58),
    panic(0xfe);

    private Integer num;

    Instruction(Integer num) {
        this.num = num;
    }

    public Integer getNum() {
        return num;
    }
}

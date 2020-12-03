package error;


public class TokenizeError extends CompileError {
    // auto-generated
    private static final long serialVersionUID = 1L;

    private ErrorCode err;

    public TokenizeError(ErrorCode err) {
        super();
        this.err = err;
    }

    public ErrorCode getErr() {
        return err;
    }


    @Override
    public String toString() {
        return new StringBuilder().append("Tokenize Error: ").append(err).append(", at: ").toString();
    }
}

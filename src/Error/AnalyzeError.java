package Error;


public class AnalyzeError extends CompileError {
    private static final long serialVersionUID = 1L;

    ErrorCode code;

    @Override
    public ErrorCode getErr() {
        return code;
    }


    /**
     *
     * @param code
     */
    public AnalyzeError(ErrorCode code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return new StringBuilder().append("Analyze Error: ").append(code).append(", at: ").toString();
    }
}

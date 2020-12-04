import java.util.ArrayList;
import java.util.List;

public class ExpectedTokenError extends CompileError {
    private static final long serialVersionUID = 1L;

    List<TokenType> expectTokenType;
    Token token;

    @Override
    public ErrorCode getErr() {
        return ErrorCode.ExpectedToken;
    }


    /**
     * @param expectedTokenType
     * @param token
     */
    public ExpectedTokenError(TokenType expectedTokenType, Token token) {
        this.expectTokenType = new ArrayList<>();
        this.expectTokenType.add(expectedTokenType);
        this.token = token;
    }

    /**
     * @param expectedTokenType
     * @param token
     */
    public ExpectedTokenError(List<TokenType> expectedTokenType, Token token) {
        this.expectTokenType = expectedTokenType;
        this.token = token;
    }

    @Override
    public String toString() {
        return new StringBuilder().append("Analyse error. Expected ").append(expectTokenType).append(" at ").toString();
    }
}

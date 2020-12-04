package Error;

public enum ErrorCode {
    NoError, // Should be only used internally.
    StreamError, EOF, InvalidInput, InvalidIdentifier, IntegerOverflow, // int32_t overflow.
    NoBegin, NoEnd, NeedIdentifier, ConstantNeedValue, NoSemicolon, InvalidVariableDeclaration, InvalidType, InvalidReturn,
    IncompleteExpression, NotDeclared, AssignToConstant, DuplicateDeclaration, NotInitialized, InvalidAssignment, InvalidOperator,
    InvalidPrint, ExpectedToken, NoLeftParen, NoRightParen, NoLeftBrace, NoRightBrace, NoArrow, NoLetKeyWord, NoColon,
    NoConstantKeyWord, NoIfKeyWord, NoWhileKeyWord, NoReturn, InvalidParam,
    InValidFunction;
}

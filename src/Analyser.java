import java.util.ArrayList;
import java.util.List;
import java.util.Stack;


public class Analyser {
    //变量
    private static List<Variable> Variables = new ArrayList<>();  //变量
    //常量
    private static List<Constant> Constants = new ArrayList<>();  //常量
    //函数
    private static List<Function> Functions = new ArrayList<>();  //函数

    private static Token symbol;

    private static Stack<TokenType> stackOp = new Stack<>(); // 操作符栈

    private static int priority[][] = OperatorPrecedence.getPriority(); // 算符优先矩阵

    private static int globalCount = 0; //全局变量个数

    private static int functionCount = 1; //函数个数

    private static int localSlot = 0; //局部变量个数

    private static int alloc = 0; //参数起始地址

    private static boolean isReturn = false; //函数是否有返回值

    private static boolean onAssign = false; //是否为赋值表达式

    private static List<Param> params;  //当前函数参数

    private static List<Global> globals = new ArrayList<>(); //全局符号表

    private static List<FunctionDef> functionDefs = new ArrayList<>(); //函数输出列表

    private static FunctionDef startFunction;

    private static List<Instructions> instructionsList; //指令集列表

    private static List<LibraryFunction> libraryFunctions = new ArrayList<>(); //库函数列表



    //程序
    public static void analyseProgram() throws Exception {

        symbol = Tokenizer.readToken();
        instructionsList = new ArrayList<>();
        while (symbol.getType() == TokenType.LET_KW || symbol.getType() == TokenType.CONST_KW) {
            analyseDeclStmt(1);
        }
        List<Instructions> initInstruction = instructionsList;
        while (symbol != null) {
            if (symbol.getType() != TokenType.FN_KW)
                throw new AnalyzeError(ErrorCode.ExpectedToken);
            //更新指令集
            instructionsList = new ArrayList<>();
            //当前参数
            params = new ArrayList<>();
            //将值初始化
            localSlot = 0;
            isReturn = false;

            analyseFunction();

            //全局变量个数加一
            globalCount++;
            //函数个数加一
            functionCount++;
        }
        if (!Format.hasMain(Functions))
            throw new AnalyzeError(ErrorCode.NoMainFunction);

        //向全局变量填入口程序_start
        Global global = new Global(1, 6, "_start");
        globals.add(global);
        //add stacklloc
        Instructions instruction = new Instructions(Instruction.stackalloc, 0);
        initInstruction.add(instruction);
        //add call main
        instruction = new Instructions(Instruction.call, functionCount-1);
        initInstruction.add(instruction);
        if (isReturn) {
            instruction = new Instructions(Instruction.popn, 1);
            initInstruction.add(instruction);
        }
        startFunction = new FunctionDef(globalCount, 0, 0, 0, initInstruction);
        globalCount++;
    }

    //function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
    public static void analyseFunction() throws Exception {
        //函数名
        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.ExpectedToken);
        //检查函数名
        String name = (String) symbol.getVal();
        if (Format.isFunction(name, Functions))
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration);



        //左括号
        symbol = Tokenizer.readToken();
        if (!(symbol.getType() == TokenType.L_PAREN))
            throw new AnalyzeError(ErrorCode.NoLeftParen);
        //参数列表
        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.R_PAREN)
            analyseFunctionParamList();

        //右括号
        if (symbol.getType() != TokenType.R_PAREN)
            throw new AnalyzeError(ErrorCode.NoRightParen);
        //箭头
        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.ARROW)
            throw new AnalyzeError(ErrorCode.NoArrow);
        //返回值
        symbol = Tokenizer.readToken();
        String type = analyseTy();
        Integer returnSlot;
        if (type.equals("int")) {
            returnSlot = 1;
            alloc = 1;
        }
        else {
            returnSlot = 0;
            alloc = 0;
            isReturn = true;
        }


        Function function = new Function(type, name, params, functionCount);
        Functions.add(function);

        //分析代码块
        analyseBlockStmt(type, 2);

        if (!isReturn)
            throw new AnalyzeError(ErrorCode.NoReturn);

        if (type.equals("void")) {
            //ret
            Instructions instructions = new Instructions(Instruction.ret, null);
            instructionsList.add(instructions);
        }

        Global global = Format.functionNameToGlobalInformation(name);
        globals.add(global);

        FunctionDef functionDef = new FunctionDef(globalCount, returnSlot, params.size(), localSlot, instructionsList);
        functionDefs.add(functionDef);

        //从列表中去掉局部变量
        Format.clearLocal(Variables, Constants);
    }


    //function_param_list -> function_param (',' function_param)*
    public static void analyseFunctionParamList() throws Exception {
        analyseFunctionParam();
        while (symbol.getType() == TokenType.COMMA) {
            symbol = Tokenizer.readToken();
            analyseFunctionParam();
        }
    }

    //function_param -> 'const'? IDENT ':' ty
    public static void analyseFunctionParam() throws Exception {
        if (symbol.getType() == TokenType.CONST_KW) symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.ExpectedToken);
        //处理参数
        String name = (String) symbol.getVal();

        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.COLON)
            throw new AnalyzeError(ErrorCode.ExpectedToken);

        symbol = Tokenizer.readToken();
        String type = analyseTy();

        Param param = new Param(type, name);
        params.add(param);

    }

    //block 代码块
    public static void analyseBlockStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.L_BRACE)
            throw new AnalyzeError(ErrorCode.NoLeftBrace);
        symbol = Tokenizer.readToken();
        while (symbol.getType() != TokenType.R_BRACE) {
            analyseStmt(type, level);
        }

        symbol = Tokenizer.readToken();
    }

    //语句
    public static void analyseStmt(String type, Integer level) throws Exception {
        if (symbol.getType() == TokenType.CONST_KW || symbol.getType() == TokenType.LET_KW)
            analyseDeclStmt(level);
        else if (symbol.getType() == TokenType.IF_KW)
            analyseIfStmt(type, level);
        else if (symbol.getType() == TokenType.WHILE_KW)
            analyseWhileStmt(type, level);
        else if (symbol.getType() == TokenType.RETURN_KW)
            analyseReturnStmt(type, level);
        else if (symbol.getType() == TokenType.SEMICOLON)
            analyseEmptyStmt();
        else if (symbol.getType() == TokenType.L_BRACE)
            analyseBlockStmt(type, level + 1);
        else
            analyseExprStmt(level);
    }

    //表达式
    public static void analyseExpr(Integer level) throws Exception {
        if (symbol.getType() == TokenType.MINUS) {
            Instructions negInstruction = new Instructions(Instruction.neg, 0);
            symbol = Tokenizer.readToken();
            if (symbol.getType() == TokenType.MINUS) {
                analyseExpr(level);
                instructionsList.add(negInstruction);
            }
            else if (symbol.getType() == TokenType.UINT_LITERAL) {
                analyseLiteralExpr();
                instructionsList.add(negInstruction);
                if (Format.isOperator(symbol)) {
                    analyseOperatorExpr(level);
                }
            }else if (symbol.getType() == TokenType.IDENT) {
                String name = (String) symbol.getVal();
                symbol = Tokenizer.readToken();
                if (symbol.getType() == TokenType.L_PAREN) {
                    stackOp.push(TokenType.L_PAREN);
                    if (Format.isFunction(name, Functions)) {
                        Integer id;
                        Instructions instruction;
                        // 是库函数
                        if (Format.isStaticFunction(name)) {
                            LibraryFunction function = new LibraryFunction(name, globalCount);
                            libraryFunctions.add(function);
                            id = globalCount;
                            globalCount++;

                            Global global = Format.functionNameToGlobalInformation(name);
                            globals.add(global);
                            instruction = new Instructions(Instruction.callname, id);
                        }
                        //自定义函数
                        else {
                            id = Format.getFunctionId(name, Functions);
                            instruction = new Instructions(Instruction.call, id);
                        }
                        analyseCallExpr(name, level);

                        //弹栈
                        while (stackOp.peek() != TokenType.L_PAREN) {
                            TokenType tokenType = stackOp.pop();
                            Format.instructionGenerate(tokenType, instructionsList);
                        }
                        stackOp.pop();

                        instructionsList.add(instruction);
                    }else {
                        throw new AnalyzeError(ErrorCode.InValidFunction);
                    }
                    instructionsList.add(negInstruction);
                    if (Format.isOperator(symbol)) {
                        analyseOperatorExpr(level);
                    }
                }else if (Format.isOperator(symbol)) {
                    analyseIdentExpr(name, level);
                    instructionsList.add(negInstruction);
                    analyseOperatorExpr(level);
                }
                else {
                    analyseIdentExpr(name, level);
                    instructionsList.add(negInstruction);
                }
            }
        }
        else if (symbol.getType() == TokenType.IDENT) {
            String name = (String) symbol.getVal();
            symbol = Tokenizer.readToken();
            if (symbol.getType() == TokenType.ASSIGN) {
                if (onAssign)
                    throw new AnalyzeError(ErrorCode.InvalidAssignment);

                if ((!Format.isConstant(name, Constants) && Format.isVariable(name, Variables)) || Format.isParam(name, params)) {
                    if (Format.isLocal(name, Constants, Variables)) {
                        Integer id = Format.getId(name, level, Constants, Variables);
                        //取出值
                        Instructions instruction = new Instructions(Instruction.loca, id);
                        instructionsList.add(instruction);
                    }else if (Format.isParam(name, params)) {
                        Integer id = Format.getParamPos(name, params);
                        //取出值
                        Instructions instruction = new Instructions(Instruction.arga, alloc + id);
                        instructionsList.add(instruction);
                    }
                    else {
                        Integer id = Format.getId(name, level, Constants, Variables);
                        Instructions instruction = new Instructions(Instruction.globa, id);
                        instructionsList.add(instruction);
                    }
                    onAssign = true;
                    analyseAssignExpr(name, level);
                }else {
                    throw new AnalyzeError(ErrorCode.InvalidAssignment);
                }
                onAssign = false;
                if (Format.isOperator(symbol)) {
                    analyseOperatorExpr(level);
                }
            }
            else if (symbol.getType() == TokenType.L_PAREN) {
                stackOp.push(TokenType.L_PAREN);
                if (Format.isFunction(name, Functions)) {
                    Integer id;
                    Instructions instruction;
                    // 是库函数
                    if (Format.isStaticFunction(name)) {
                        LibraryFunction function = new LibraryFunction(name, globalCount);
                        libraryFunctions.add(function);
                        id = globalCount;
                        globalCount++;

                        Global global = Format.functionNameToGlobalInformation(name);
                        globals.add(global);
                        instruction = new Instructions(Instruction.callname, id);
                    }
                    //自定义函数
                    else {
                        id = Format.getFunctionId(name, Functions);
                        instruction = new Instructions(Instruction.call, id);
                    }
                    analyseCallExpr(name, level);

                    //弹栈
                    while (stackOp.peek() != TokenType.L_PAREN) {
                        TokenType tokenType = stackOp.pop();
                        Format.instructionGenerate(tokenType, instructionsList);
                    }
                    stackOp.pop();

                    instructionsList.add(instruction);

                }else {
                    throw new AnalyzeError(ErrorCode.InValidFunction);
                }
                if (Format.isOperator(symbol)) {
                    analyseOperatorExpr(level);
                }
            }else if (Format.isOperator(symbol)) {
                analyseIdentExpr(name, level);
                analyseOperatorExpr(level);
            }else if (symbol.getType() == TokenType.AS_KW) {
                analyseAsExpr();
            }
            else {
                analyseIdentExpr(name, level);
            }
        }
        else if (symbol.getType() == TokenType.UINT_LITERAL ||
                symbol.getType() == TokenType.STRING_LITERAL) {
            analyseLiteralExpr();
            if (Format.isOperator(symbol)) analyseOperatorExpr(level);
        }
        else if (symbol.getType() == TokenType.L_PAREN) {
            stackOp.push(TokenType.L_PAREN);
            analyseGroupExpr(level);
            if (Format.isOperator(symbol)) {
                analyseOperatorExpr(level);
            }
        }
        else throw new AnalyzeError(ErrorCode.InvalidType);
    }

    //
    public static void analyseBinaryOperator() throws Exception {
        if (symbol.getType() != TokenType.PLUS &&
            symbol.getType() != TokenType.MINUS &&
            symbol.getType() != TokenType.MUL &&
            symbol.getType() != TokenType.DIV &&
            symbol.getType() != TokenType.EQ &&
            symbol.getType() != TokenType.NEQ &&
            symbol.getType() != TokenType.LE &&
            symbol.getType() != TokenType.LT &&
            symbol.getType() != TokenType.GE &&
            symbol.getType() != TokenType.GT) {
            throw new AnalyzeError(ErrorCode.InvalidOperator);
        }
    }

    //operator_expr -> expr binary_operator expr
    public static void analyseOperatorExpr(Integer level) throws Exception {
        analyseBinaryOperator();
        if (!stackOp.empty()) {
            int front = OperatorPrecedence.getOrder(stackOp.peek());
            int next = OperatorPrecedence.getOrder(symbol.getType());
            if (priority[front][next] > 0) {
                TokenType type = stackOp.pop();
                Format.instructionGenerate(type, instructionsList);
            }
        }
        stackOp.push(symbol.getType());

        symbol = Tokenizer.readToken();
        analyseExpr(level);
    }

    //negate_expr -> '-' expr
    public static void analyseNegateExpr(Integer level) throws Exception {
        symbol = Tokenizer.readToken();
        analyseExpr(level);
    }

    //assign_expr -> l_expr '=' expr
    public static void analyseAssignExpr(String name, Integer level) throws Exception {
        symbol = Tokenizer.readToken();
        analyseExpr(level);
        while (!stackOp.empty()) {
            Format.instructionGenerate(stackOp.pop(), instructionsList);
        }
        //存储到地址中
        Instructions instruction = new Instructions(Instruction.store, null);
        instructionsList.add(instruction);
    }

    //as_expr -> expr 'as' ty
    public static void analyseAsExpr() throws Exception {
        symbol = Tokenizer.readToken();
        String type = analyseTy();
        if (!type.equals("int"))
            throw new AnalyzeError(ErrorCode.InvalidType);
    }

    //call_param_list -> expr (',' expr)*
    public static int analyseCallParamList(Integer level) throws Exception {
        analyseExpr(level);
        while (!stackOp.empty() && stackOp.peek() != TokenType.L_PAREN) {
            Format.instructionGenerate(stackOp.pop(), instructionsList);
        }
        int count = 1;
        while (symbol.getType() == TokenType.COMMA) {
            symbol = Tokenizer.readToken();
            analyseExpr(level);
            while (!stackOp.empty() && stackOp.peek() != TokenType.L_PAREN) {
                Format.instructionGenerate(stackOp.pop(), instructionsList);
            }
            count++;
        }
        return count;
    }

    //call_expr -> IDENT '(' call_param_list? ')'
    public static void analyseCallExpr(String name, Integer level) throws Exception {
        Instructions instruction;
        int count = 0; //参数个数
        //分配返回值空间
        if (Format.hasReturn(name, Functions)) {
            instruction = new Instructions(Instruction.stackalloc, 1);
        }else {
            if (onAssign)
                throw new AnalyzeError(ErrorCode.InvalidAssignment);
            instruction = new Instructions(Instruction.stackalloc, 0);
        }

        instructionsList.add(instruction);

        symbol = Tokenizer.readToken();

        if (symbol.getType() != TokenType.R_PAREN)
            count = analyseCallParamList(level);

        if (!Format.checkParam(name, Functions, count))
            throw new AnalyzeError(ErrorCode.InvalidParam);

        if (symbol.getType() != TokenType.R_PAREN)
            throw new AnalyzeError(ErrorCode.NoRightParen);

        symbol = Tokenizer.readToken();
    }

    //literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
    public static void analyseLiteralExpr() throws Exception {
        if (symbol.getType() == TokenType.UINT_LITERAL) {
            //加载常数
            Instructions instructions = new Instructions(Instruction.push, (Integer) symbol.getVal());
            instructionsList.add(instructions);
        }
        else if (symbol.getType() == TokenType.STRING_LITERAL) {
            //填入全局符号表
            Global global = Format.functionNameToGlobalInformation((String) symbol.getVal());
            globals.add(global);

            //加入指令集
            Instructions instruction = new Instructions(Instruction.push, globalCount);
            instructionsList.add(instruction);
            globalCount++;

        }
        else
            throw new AnalyzeError(ErrorCode.ExpectedToken);

        symbol = Tokenizer.readToken();
    }

    //ident_expr -> IDENT
    public static void analyseIdentExpr(String name, Integer level) throws Exception {
        if (!(Format.isVariable(name, Variables) || Format.isConstant(name, Constants) || Format.isParam(name, params)))
            throw new AnalyzeError(ErrorCode.NotDeclared);
        Instructions instruction;
        //局部变量
        int id;
        if (Format.isLocal(name, Constants, Variables)) {
            id = Format.getId(name, level, Constants, Variables);
            instruction = new Instructions(Instruction.loca, id);
            instructionsList.add(instruction);
        }
        //参数
        else if (Format.isParam(name, params)) {
            id = Format.getParamPos(name, params);
            instruction = new Instructions(Instruction.arga, alloc + id);
            instructionsList.add(instruction);
        }
        //全局变量
        else {
            id = Format.getId(name, level, Constants, Variables);
            instruction = new Instructions(Instruction.globa, id);
            instructionsList.add(instruction);
        }
        instruction = new Instructions(Instruction.load, null);
        instructionsList.add(instruction);
    }

    //group_expr -> '(' expr ')'
    public static void analyseGroupExpr(Integer level) throws Exception {
        if (symbol.getType() != TokenType.L_PAREN)
            throw new AnalyzeError(ErrorCode.NoLeftParen);

        symbol = Tokenizer.readToken();
        analyseExpr(level);


        if (symbol.getType() != TokenType.R_PAREN)
            throw new AnalyzeError(ErrorCode.NoRightParen);

        while (stackOp.peek() != TokenType.L_PAREN) {
            TokenType type = stackOp.pop();
            Format.instructionGenerate(type, instructionsList);
        }
        stackOp.pop();

        symbol = Tokenizer.readToken();
    }


    //类型
    public static String analyseTy() throws Exception {
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.ExpectedToken);

        String type = (String) symbol.getVal();

        //int
        if (!(type.equals("int") || type.equals("void")))
            throw new AnalyzeError(ErrorCode.InvalidIdentifier);

        symbol = Tokenizer.readToken();
        return type;
    }

    //expr_stmt -> expr ';'
    public static void analyseExprStmt(Integer level) throws Exception {
        analyseExpr(level);
        //弹栈
        while (!stackOp.empty()) {
            TokenType tokenType = stackOp.pop();
            Format.instructionGenerate(tokenType, instructionsList);
        }
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        symbol = Tokenizer.readToken();
    }

    //decl_stmt -> let_decl_stmt | const_decl_stmt
    public static void analyseDeclStmt(Integer level) throws Exception {
        if (symbol.getType() == TokenType.CONST_KW)
            analyseConstDeclStmt(level);
        else if (symbol.getType() == TokenType.LET_KW)
            analyseLetDeclStmt(level);
        else throw new AnalyzeError(ErrorCode.ExpectedToken);
        if (level == 1) globalCount++;
        else localSlot++;
    }

    //let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
    public static void analyseLetDeclStmt(Integer level) throws Exception {
        if (symbol.getType() != TokenType.LET_KW)
            throw new AnalyzeError(ErrorCode.NoLetKeyWord);

        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.NeedIdentifier);
        String name = (String) symbol.getVal();
        //填表
        if (!Format.isValidName(Functions, Constants, Variables, name, level))
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration);
        //全局变量
        if (level == 1) {
            Variable variable = new Variable(name, globalCount, level);
            Variables.add(variable);
            Global global = new Global(0);
            globals.add(global);

        }
        //局部变量
        else {
            Variable variable = new Variable(name, localSlot, level);
            Variables.add(variable);
        }

        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.COLON)
            throw new AnalyzeError(ErrorCode.NoColon);

        symbol = Tokenizer.readToken();
        String type = analyseTy();
        if (!type.equals("int"))
            throw new AnalyzeError(ErrorCode.InvalidType);

        if (symbol.getType() == TokenType.ASSIGN) {
            onAssign = true;
            Instructions instruction;
            if (level == 1) {
                //取地址
                instruction = new Instructions(Instruction.globa, globalCount);
                instructionsList.add(instruction);
            }
            else {
                instruction = new Instructions(Instruction.loca, localSlot);
                instructionsList.add(instruction);
            }

            symbol = Tokenizer.readToken();
            analyseExpr(level);
            //弹栈
            while (!stackOp.empty()) {
                TokenType tokenType = stackOp.pop();
                Format.instructionGenerate(tokenType, instructionsList);
            }

            //存值
            instruction = new Instructions(Instruction.store, null);
            instructionsList.add(instruction);

            onAssign = false;
        }
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        symbol = Tokenizer.readToken();
    }

    //const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
    public static void analyseConstDeclStmt(Integer level) throws Exception {
        if (symbol.getType() != TokenType.CONST_KW)
            throw new AnalyzeError(ErrorCode.NoConstantKeyWord);

        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.NeedIdentifier);
        String name = (String) symbol.getVal();
        if (!Format.isValidName(Functions, Constants, Variables, name, level))
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration);
        //向全局变量表里填入
        if (level == 1) {
            Constant constant = new Constant(name, globalCount, level);
            Constants.add(constant);
            Global global = new Global(1);
            globals.add(global);
            //生成 globa 指令，准备赋值
            Instructions instruction = new Instructions(Instruction.globa, globalCount);
            instructionsList.add(instruction);
        }
        //局部变量
        else {
            Constant constant = new Constant(name, localSlot, level);
            Constants.add(constant);

            //生成 loca 指令，准备赋值
            Instructions instruction = new Instructions(Instruction.loca, localSlot);
            instructionsList.add(instruction);
        }


        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.COLON)
            throw new AnalyzeError(ErrorCode.NoColon);

        symbol = Tokenizer.readToken();
        String type = analyseTy();
        if (!type.equals("int"))
            throw new AnalyzeError(ErrorCode.InvalidType);

        if (symbol.getType() != TokenType.ASSIGN)
            throw new AnalyzeError(ErrorCode.ConstantNeedValue);

        onAssign = true;
        symbol = Tokenizer.readToken();
        analyseExpr(level);
        //弹栈
        while (!stackOp.empty()) {
            TokenType tokenType = stackOp.pop();
            Format.instructionGenerate(tokenType, instructionsList);
        }
        onAssign = false;

        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        Instructions instruction = new Instructions(Instruction.store, null);
        instructionsList.add(instruction);
        symbol = Tokenizer.readToken();
    }


    //if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
    public static void analyseIfStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.IF_KW)
            throw new AnalyzeError(ErrorCode.NoIfKeyWord);
        symbol = Tokenizer.readToken();
        analyseExpr(level);
        //弹栈
        while (!stackOp.empty()) {
            TokenType tokenType = stackOp.pop();
            Format.instructionGenerate(tokenType, instructionsList);
        }

        //brTrue
        Instructions instruction = new Instructions(Instruction.brTrue, 1);
        instructionsList.add(instruction);
        //br
        Instructions ifInstruction = new Instructions(Instruction.br, 0);
        instructionsList.add(ifInstruction);
        int index = instructionsList.size();

        analyseBlockStmt(type, level + 1);


        int size = instructionsList.size();

        if (instructionsList.get(size -1).getInstruction() == 0x49) {
            int dis = instructionsList.size() - index;
            ifInstruction.setParam(dis);

            if (symbol.getType() == TokenType.ELSE_KW) {
                symbol = Tokenizer.readToken();
                if (symbol.getType() == TokenType.IF_KW)
                    analyseIfStmt(type, level);
                else {
                    analyseBlockStmt(type, level + 1);
                    size = instructionsList.size();

                    if (instructionsList.get(size -1).getInstruction() != 0x49) {
                        instruction = new Instructions(Instruction.br, 0);
                        instructionsList.add(instruction);
                    }
                }
            }
        }
        else {
            Instructions jumpInstruction = new Instructions(Instruction.br, null);
            instructionsList.add(jumpInstruction);
            int jump = instructionsList.size();

            int dis = instructionsList.size() - index;
            ifInstruction.setParam(dis);

            if (symbol.getType() == TokenType.ELSE_KW) {
                symbol = Tokenizer.readToken();
                if (symbol.getType() == TokenType.IF_KW)
                    analyseIfStmt(type, level);
                else {
                    analyseBlockStmt(type, level + 1);
                        instruction = new Instructions(Instruction.br, 0);
                        instructionsList.add(instruction);
                }
            }
            dis = instructionsList.size() - jump;
            jumpInstruction.setParam(dis);
        }
    }

    //while_stmt -> 'while' expr block_stmt
    public static void analyseWhileStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.WHILE_KW)
            throw new AnalyzeError(ErrorCode.NoWhileKeyWord);

        Instructions instruction = new Instructions(Instruction.br, 0);
        instructionsList.add(instruction);

        int whileStart = instructionsList.size();
        symbol = Tokenizer.readToken();
        analyseExpr(level);
        //弹栈
        while (!stackOp.empty()) {
            TokenType tokenType = stackOp.pop();
            Format.instructionGenerate(tokenType, instructionsList);
        }

        //brTrue
        instruction = new Instructions(Instruction.brTrue, 1);
        instructionsList.add(instruction);
        //br
        Instructions jumpInstruction = new Instructions(Instruction.br, 0);
        instructionsList.add(jumpInstruction);
        int index = instructionsList.size();

        analyseBlockStmt(type, level + 1);


        //跳至while 判断语句
        instruction = new Instructions(Instruction.br, 0);
        instructionsList.add(instruction);
        int whileEnd = instructionsList.size();
        int dis = whileStart - whileEnd;
        instruction.setParam(dis);


        dis = instructionsList.size() - index;
        jumpInstruction.setParam(dis);

    }

    //return_stmt -> 'return' expr? ';'
    public static void analyseReturnStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.RETURN_KW)
            throw new AnalyzeError(ErrorCode.NoReturn);

        symbol = Tokenizer.readToken();
        if (symbol.getType() != TokenType.SEMICOLON) {
            if (type.equals("int")) {
                //取返回地址
                Instructions instructions = new Instructions(Instruction.arga, 0);
                instructionsList.add(instructions);

                System.out.println("return num="+symbol.getVal());
                analyseExpr(level);

                while (!stackOp.empty()) {
                    Format.instructionGenerate(stackOp.pop(), instructionsList);
                }
                //放入地址中
                instructions = new Instructions(Instruction.store, null);
                instructionsList.add(instructions);
                isReturn = true;
            }
            else if (type.equals("void"))
                throw new AnalyzeError(ErrorCode.InvalidReturn);
        }
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);
        while (!stackOp.empty()) {
            Format.instructionGenerate(stackOp.pop(), instructionsList);
        }
        //ret
        Instructions instructions = new Instructions(Instruction.ret, null);
        instructionsList.add(instructions);
        symbol = Tokenizer.readToken();
    }

    //空语句
    public static void analyseEmptyStmt() throws Exception {
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        symbol = Tokenizer.readToken();
    }




    public static List<Global> getGlobals() {
        return globals;
    }

    public static FunctionDef getStartFunction() {
        return startFunction;
    }

    public static List<FunctionDef> getFunctionDefs() {
        return functionDefs;
    }

    public static List<Param> getParams() {
        return params;
    }
}

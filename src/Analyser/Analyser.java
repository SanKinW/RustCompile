package Analyser;

import Common.*;
import Tokenizer.Token;
import Utils.*;
import error.AnalyzeError;
import error.ErrorCode;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import static Tokenizer.Tokenizer.*;
import static Utils.Format.*;
import static Utils.OperatorPrecedence.getOrder;

public class Analyser {
    //变量
    private static List<Variable> Variables = new ArrayList<>();  //变量
    //常量
    private static List<Constant> Constants = new ArrayList<>();  //常量
    //函数
    private static List<Function> Functions = new ArrayList<>();  //函数

    private static Token symbol;

    private static List<String> list = new ArrayList<>();

    private static Stack<TokenType> stackOp = new Stack<>(); // 操作符栈

    private static int priority[][] = OperatorPrecedence.getPriority(); // 算符优先矩阵

    private static long globalCount = 0; //全局变量个数

    private static long functionCount = 1; //函数个数

    private static int localSlot = 0; //局部变量个数

    private static int alloc = 0; //参数起始地址

    private static boolean isReturn = false; //函数是否有返回值

    private static List<Param> params;  //当前函数参数

    private static List<Global> globals = new ArrayList<>(); //全局符号表

    private static List<FunctionDef> functionDefs = new ArrayList<>(); //函数输出列表

    private static List<Instructions> instructionsList; //指令集列表

    private static List<LibraryFunction> libraryFunctions = new ArrayList<>(); //库函数列表



    //程序
    public static void analyseProgram() throws Exception {

        symbol = readToken();
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
            analyseFunction();
            //全局变量个数加一
            globalCount++;
            //函数个数加一
            functionCount++;
        }

        //向全局变量填入口程序_start
        String[] unicode = new String[]{"5F", "73",  "74", "61", "72", "74"}; //_start
        Global global = new Global(1, unicode.length, unicode);
        globals.add(global);
        //add stacklloc
        Instructions instruction = new Instructions(Instruction.stackalloc, null);
        initInstruction.add(instruction);
        //add call main
        Long[] para = new Long[]{functionCount};
        instruction = new Instructions(Instruction.call, para);
        initInstruction.add(instruction);
        FunctionDef functionDef = new FunctionDef(globalCount, 0, 0, 0, initInstruction);
        functionDefs.add(functionDef);
        globalCount++;
    }

    //function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
    public static void analyseFunction() throws Exception {
        //函数名
        symbol = readToken();
        //当前参数
        params = new ArrayList<>();
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.ExpectedToken);
        //检查函数名
        String name = (String) symbol.getVal();
        if (isFunction(name, Functions))
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration);

        Global global = Format.functionNameToGlobalInformation(name);
        globals.add(global);


        //左括号
        symbol = readToken();
        if (!(symbol.getType() == TokenType.L_PAREN))
            throw new AnalyzeError(ErrorCode.NoLeftParen);
        //参数列表
        symbol = readToken();
        if (symbol.getType() != TokenType.R_PAREN)
            analyseFunctionParamList();

        //右括号
        if (symbol.getType() != TokenType.R_PAREN)
            throw new AnalyzeError(ErrorCode.NoRightParen);
        //箭头
        symbol = readToken();
        if (symbol.getType() != TokenType.ARROW)
            throw new AnalyzeError(ErrorCode.NoArrow);
        //返回值
        symbol = readToken();
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

        //分析代码块
        analyseBlockStmt(type, 2);

        if (!isReturn)
            throw new AnalyzeError(ErrorCode.NoReturn);


        Function function = new Function(type, name, params, functionCount);
        Functions.add(function);

        FunctionDef functionDef = new FunctionDef(globalCount, returnSlot, params.size(), localSlot, instructionsList);
        functionDefs.add(functionDef);

        //从列表中去掉局部变量
        Format.clearLocal(Variables, Constants);
        //将值重新初始化
        localSlot = 0;
        isReturn = false;
    }


    //function_param_list -> function_param (',' function_param)*
    public static void analyseFunctionParamList() throws Exception {
        analyseFunctionParam();
        while (symbol.getType() == TokenType.COMMA) {
            analyseFunctionParam();
        }
    }

    //function_param -> 'const'? IDENT ':' ty
    public static void analyseFunctionParam() throws Exception {
        if (symbol.getType() == TokenType.CONST_KW) symbol = readToken();
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.ExpectedToken);
        //处理参数
        String name = (String) symbol.getVal();

        symbol = readToken();
        if (symbol.getType() != TokenType.COLON)
            throw new AnalyzeError(ErrorCode.ExpectedToken);
        symbol = readToken();
        String type = analyseTy();
        Param param = new Param(type, name);
        params.add(param);
    }

    //block 代码块
    public static void analyseBlockStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.L_BRACE) throw new AnalyzeError(ErrorCode.NoLeftBrace);
        symbol = readToken();
        while (symbol.getType() != TokenType.R_BRACE) {
            analyseStmt(type, level);
        }
        symbol = readToken();
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
            Instructions instruction = new Instructions(Instruction.push, new Long[0]);
            instructionsList.add(instruction);
            if (stackOp.empty()) stackOp.push(TokenType.MINUS);
            else {
                int front = getOrder(stackOp.peek());
                int next = getOrder(TokenType.MINUS);
                if (priority[front][next] > 0) {
                    TokenType type = stackOp.pop();
                    instructionGenerate(type, instructionsList);
                }
                stackOp.push(TokenType.MINUS);
            }
            analyseNegateExpr(level);

        }
        else if (symbol.getType() == TokenType.IDENT) {
            String name = (String) symbol.getVal();
            symbol = readToken();
            if (symbol.getType() == TokenType.ASSIGN) {
                //考虑对参数赋值？
                if (!isConstant(name, Constants) && isVariable(name, Variables)) {
                    if (isLocal(name, Constants, Variables)) {
                        Long id = getId(name, level, Constants, Variables);
                        //取出值
                        Instructions instruction = new Instructions(Instruction.loca, new Long[]{id});
                        instructionsList.add(instruction);
                    }else if (isParam(name, params)) {
                        Long id = getParamPos(name, params);
                        Instructions instruction = new Instructions(Instruction.arga, new Long[]{id});
                        instructionsList.add(instruction);
                    }
                    else {
                        Long id = getId(name, level, Constants, Variables);
                        Instructions instruction = new Instructions(Instruction.globa, new Long[]{id});
                        instructionsList.add(instruction);
                    }
                    analyseAssignExpr(name, level);
                }else {
                    throw new AnalyzeError(ErrorCode.InvalidAssignment);
                }
            }
            else if (symbol.getType() == TokenType.L_PAREN) {
                if (Format.isFunction(name, Functions)) {
                    Long id;
                    Instructions instruction;
                    // 是库函数
                    if (Format.isStaticFunction(name)) {
                        // 未填入全局变量表
                        if (!Format.isInitLibrary(name, libraryFunctions)) {
                            LibraryFunction function = new LibraryFunction(name, globalCount);
                            libraryFunctions.add(function);
                            id = globalCount++;

                            Global global = Format.functionNameToGlobalInformation(name);
                            globals.add(global);
                        }
                        // 已填入
                        else {
                            id = getKuId(name, libraryFunctions);
                        }
                        instruction = new Instructions(Instruction.callname, new Long[]{id});
                    }
                    //自定义函数
                    else {
                        id = getFunctionId(name, Functions);
                        instruction = new Instructions(Instruction.call, new Long[]{id});
                    }
                    analyseCallExpr(name, level);
                    instructionsList.add(instruction);
                }else {
                    throw new AnalyzeError(ErrorCode.InValidFunction);
                }
            }else if (Format.isOperator(symbol)) {
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
        int front = getOrder(stackOp.peek());
        int next = getOrder(symbol.getType());
        if (priority[front][next] > 0) {
            TokenType type = stackOp.pop();
            instructionGenerate(type, instructionsList);
        }
        stackOp.push(symbol.getType());

        symbol = readToken();
        analyseExpr(level);
    }

    //negate_expr -> '-' expr
    public static void analyseNegateExpr(Integer level) throws Exception {
        symbol = readToken();
        analyseExpr(level);
    }

    //assign_expr -> l_expr '=' expr
    public static void analyseAssignExpr(String name, Integer level) throws Exception {
        symbol = readToken();
        analyseExpr(level);
        //存储到地址中
        Instructions instruction = new Instructions(Instruction.store, null);
        instructionsList.add(instruction);
    }

    //as_expr -> expr 'as' ty
    public static void analyseAsExpr() throws Exception {
        symbol = readToken();
        String type = analyseTy();
        if (!type.equals("int"))
            throw new AnalyzeError(ErrorCode.InvalidType);
    }

    //call_param_list -> expr (',' expr)*
    public static int analyseCallParamList(String name, Integer level) throws Exception {
        analyseExpr(level);
        int count = 1;
        while (symbol.getType() == TokenType.COMMA) {
            symbol = readToken();
            analyseExpr(level);
            count++;
        }
        return count;
    }

    //call_expr -> IDENT '(' call_param_list? ')'
    public static void analyseCallExpr(String name, Integer level) throws Exception {
        Instructions instruction;
        int count = 0; //参数个数
        //分配返回值空间
        if (hasReturn(name, Functions)) {
            instruction = new Instructions(Instruction.stackalloc, new Long[]{1L});
        }else {
            instruction = new Instructions(Instruction.stackalloc, new Long[]{0L});
        }

        instructionsList.add(instruction);

        symbol = readToken();

        if (symbol.getType() != TokenType.R_PAREN)
            count = analyseCallParamList(name, level);

        if (!Format.checkParam(name, Functions, count))
            throw new AnalyzeError(ErrorCode.InvalidParam);

        if (symbol.getType() != TokenType.R_PAREN)
            throw new AnalyzeError(ErrorCode.NoRightParen);

        symbol = readToken();
    }

    //literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
    public static void analyseLiteralExpr() throws Exception {
        if (symbol.getType() == TokenType.UINT_LITERAL) {
            //加载常数
            Long[] param = new Long[]{(Long) symbol.getVal()};
            Instructions instructions = new Instructions(Instruction.push, param);
            instructionsList.add(instructions);
        }
        else if (symbol.getType() == TokenType.STRING_LITERAL) {
            //填入全局符号表
            Global global = Format.functionNameToGlobalInformation((String) symbol.getVal());
            globals.add(global);

            //加入指令集
            Instructions instruction = new Instructions(Instruction.push, new Long[]{globalCount});
            instructionsList.add(instruction);
            globalCount++;

        }
        else throw new AnalyzeError(ErrorCode.ExpectedToken);

        symbol = readToken();
    }

    //ident_expr -> IDENT
    public static void analyseIdentExpr(String name, Integer level) throws Exception {
        if (!(Format.isVariable(name, Variables) || Format.isConstant(name, Constants) || Format.isParam(name, params)))
            throw new AnalyzeError(ErrorCode.NotDeclared);
        Instructions instruction;
        //局部变量
        if (isLocal(name, Constants, Variables)) {
            Long id = getId(name, level, Constants, Variables);
            instruction = new Instructions(Instruction.loca, new Long[]{id});
            instructionsList.add(instruction);
        }
        //参数
        else if (isParam(name, params)) {
            Long id = getParamPos(name, params);
            instruction = new Instructions(Instruction.arga, new Long[]{alloc+id});
            instructionsList.add(instruction);
        }
        //全局变量
        else {
            Long id = getId(name, level, Constants, Variables);
            instruction = new Instructions(Instruction.globa, new Long[]{id});
            instructionsList.add(instruction);
        }
        instruction = new Instructions(Instruction.load, null);
        instructionsList.add(instruction);
    }

    //group_expr -> '(' expr ')'
    public static void analyseGroupExpr(Integer level) throws Exception {
        if (symbol.getType() != TokenType.L_PAREN)
            throw new AnalyzeError(ErrorCode.NoLeftParen);

        symbol = readToken();
        analyseExpr(level);

        if (symbol.getType() != TokenType.R_PAREN)
            throw new AnalyzeError(ErrorCode.NoRightParen);
        while (stackOp.peek() != TokenType.L_PAREN) {
            list.add(stackOp.pop().getKind());
        }
        stackOp.pop();

        symbol = readToken();
    }


    //类型
    public static String analyseTy() throws Exception {
        if (symbol.getType() != TokenType.IDENT)
            throw new AnalyzeError(ErrorCode.ExpectedToken);

        String type = (String) symbol.getVal();

        //int
        if (!(type.equals("int") || type.equals("void")))
            throw new AnalyzeError(ErrorCode.InvalidIdentifier);

        symbol = readToken();
        return type;
    }

    //expr_stmt -> expr ';'
    public static void analyseExprStmt(Integer level) throws Exception {
        analyseExpr(level);
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);
        symbol = readToken();
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

        symbol = readToken();
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
            Variable variable = new Variable(name, (long)localSlot, level);
            Variables.add(variable);
        }

        symbol = readToken();
        if (symbol.getType() != TokenType.COLON)
            throw new AnalyzeError(ErrorCode.NoColon);

        symbol = readToken();
        String type = analyseTy();
        if (!type.equals("int"))
            throw new AnalyzeError(ErrorCode.InvalidType);

        if (symbol.getType() == TokenType.ASSIGN) {
            Instructions instruction;
            if (level == 1) {
                Long[] count = new Long[]{globalCount};
                //取地址
                instruction = new Instructions(Instruction.globa, count);
                instructionsList.add(instruction);
            }
            else {
                Long[] count = new Long[]{(long) localSlot};
                instruction = new Instructions(Instruction.loca, count);
                instructionsList.add(instruction);
                localSlot++;
            }

            symbol = readToken();
            analyseExpr(level);

            //存值
            instruction = new Instructions(Instruction.store, null);
            instructionsList.add(instruction);

        }
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        symbol = readToken();
    }

    //const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
    public static void analyseConstDeclStmt(Integer level) throws Exception {
        if (symbol.getType() != TokenType.CONST_KW)
            throw new AnalyzeError(ErrorCode.NoConstantKeyWord);

        symbol = readToken();
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
            Long[] count = new Long[]{globalCount};
            Instructions instruction = new Instructions(Instruction.globa, count);
            instructionsList.add(instruction);
        }
        //局部变量
        else {
            Constant constant = new Constant(name, (long) localSlot, level);
            Constants.add(constant);

            //生成 loca 指令，准备赋值
            Long[] count = new Long[]{(long) localSlot};
            Instructions instruction = new Instructions(Instruction.loca, count);
            instructionsList.add(instruction);

            localSlot++;
        }


        symbol = readToken();
        if (symbol.getType() != TokenType.COLON)
            throw new AnalyzeError(ErrorCode.NoColon);

        symbol = readToken();
        String type = analyseTy();
        if (!type.equals("int"))
            throw new AnalyzeError(ErrorCode.InvalidType);

        if (symbol.getType() != TokenType.ASSIGN)
            throw new AnalyzeError(ErrorCode.ConstantNeedValue);

        symbol = readToken();
        analyseExpr(level);

        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        Instructions instruction = new Instructions(Instruction.store, null);
        instructionsList.add(instruction);
        symbol = readToken();
    }


    //if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
    public static void analyseIfStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.IF_KW)
            throw new AnalyzeError(ErrorCode.NoIfKeyWord);
        symbol = readToken();
        analyseExpr(level);
        //弹栈
        while (!stackOp.empty()) {
            TokenType tokenType = stackOp.pop();
            instructionGenerate(tokenType, instructionsList);
        }

        //brTrue
        Instructions instruction = new Instructions(Instruction.brTrue, new Long[]{1L});
        instructionsList.add(instruction);
        //br
        Instructions ifInstruction = new Instructions(Instruction.br, null);
        instructionsList.add(ifInstruction);
        int index = instructionsList.size();

        analyseBlockStmt(type, level + 1);

        Instructions jumpInstruction = new Instructions(Instruction.br, null);
        instructionsList.add(jumpInstruction);
        int ifEnd = instructionsList.size();

        long dis = instructionsList.size() - index;
        ifInstruction.setParamIds(new Long[]{dis});

        if (symbol.getType() == TokenType.ELSE_KW) {
            symbol = readToken();
            if (symbol.getType() == TokenType.IF_KW)
                analyseIfStmt(type, level);
            else
                analyseBlockStmt(type, level + 1);
        }
        dis = instructionsList.size() - ifEnd;
        jumpInstruction.setParamIds(new Long[]{dis});
    }

    //while_stmt -> 'while' expr block_stmt
    public static void analyseWhileStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.WHILE_KW)
            throw new AnalyzeError(ErrorCode.NoWhileKeyWord);

        int whileStart = instructionsList.size() - 1;
        symbol = readToken();
        analyseExpr(level);


        //把 0 压入栈
        Long[] param = new Long[]{0L};
        Instructions instruction = new Instructions(Instruction.push, param);
        instructionsList.add(instruction);
        // cmp
        instruction = new Instructions(Instruction.cmp, null);
        instructionsList.add(instruction);
        // set.lt
        instruction = new Instructions(Instruction.set, null);
        instructionsList.add(instruction);
        //brTrue
        instruction = new Instructions(Instruction.brTrue, new Long[]{1L});
        instructionsList.add(instruction);
        //br
        Instructions jumpInstruction = new Instructions(Instruction.br, null);
        instructionsList.add(jumpInstruction);
        int index = instructionsList.size();

        analyseBlockStmt(type, level + 1);

        int whileEnd = instructionsList.size();
        long dis = whileStart - whileEnd;
        //跳至while 判断语句
        instruction = new Instructions(Instruction.br, new Long[]{dis});
        instructionsList.add(instruction);


        dis = instructionsList.size() - index;
        jumpInstruction.setParamIds(new Long[]{dis});

    }

    //return_stmt -> 'return' expr? ';'
    public static void analyseReturnStmt(String type, Integer level) throws Exception {
        if (symbol.getType() != TokenType.RETURN_KW)
            throw new AnalyzeError(ErrorCode.NoReturn);

        symbol = readToken();
        if (symbol.getType() != TokenType.SEMICOLON) {
            if (type.equals("int")) {
                //取返回地址
                Instructions instructions = new Instructions(Instruction.arga, new Long[]{0L});
                instructionsList.add(instructions);
                analyseExpr(level);
                //放入地址中
                instructions = new Instructions(Instruction.store, null);
                instructionsList.add(instructions);
                //ret
                instructions = new Instructions(Instruction.ret, null);
                instructionsList.add(instructions);
                isReturn = true;
            }
            else if (type.equals("void"))
                throw new AnalyzeError(ErrorCode.InvalidReturn);
        }
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        symbol = readToken();
    }

    //空语句
    public static void analyseEmptyStmt() throws Exception {
        if (symbol.getType() != TokenType.SEMICOLON)
            throw new AnalyzeError(ErrorCode.NoSemicolon);

        symbol = readToken();
    }




    public static List<Global> getGlobals() {
        return globals;
    }

    public static List<FunctionDef> getFunctionDefs() {
        return functionDefs;
    }
}

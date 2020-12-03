package Utils;

import Common.TokenType;


import java.util.Stack;

public class InterCode {

    public static void Operator(TokenType type, Stack<Integer> stack) {
        Integer next = stack.pop();
        Integer front = stack.pop();
        switch (type) {
            case PLUS:
                front = front + next;
                break;
            case MINUS:
                front = front - next;
                break;
            case MUL:
                front = front * next;
                break;
            case DIV:
                front = front / next;
                break;
            case EQ:
                if (next == front) front = 1;
                else front = 0;
                break;
            case NEQ:
                if (front != next) front = 1;
                else front = 0;
                break;
            case LE:
                if (front <= next) front = 1;
                else front = 0;
                break;
            case LT:
                if (front < next) front = 1;
                else front = 0;
                break;
            case GE:
                if (front >= next) front = 1;
                else front = 0;
                break;
            case GT:
                if (front > next) front = 1;
                else front = 0;
                break;
        }
        stack.push(front);
    }
}

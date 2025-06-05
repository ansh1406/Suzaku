#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <vector>
#include "lexer.hpp"
#include "grammar.hpp"
#include "errorHandler.hpp"

namespace parser
{
    enum NodeType
    {
        INTEGER,
        STRING,
        VARIABLE,
        BINARY_OPERATION,
        UNARY_OPERATION,
        FUNCTION_CALL,
        BLOCK,
        STATEMENT,
        EXPRESSION_STATEMENT,
        VARIABLE_DECLARATION,
        CONSTANT_DECLARATION,
        FUNCTION_DEFINITION,
        RETURN_STATEMENT,
        IFELSE_STATEMENT,
        SWITCH_STATEMENT,
        CASE_STATEMENT,
        DEFAULT_CASE_STATEMENT,
        WHILE_LOOP_STATEMENT,
        FOR_LOOP_STATEMENT,
        DO_WHILE_LOOP_STATEMENT,
        BREAK_STATEMENT,
        CONTINUE_STATEMENT
    };

    class BaseASTNode
    {
    public:
        virtual ~BaseASTNode() = default;
        virtual parser::NodeType getNodeType() const = 0;
    };

    class ExpressionNode : public BaseASTNode
    {
    };

    class VariableNode : public ExpressionNode
    {
    public:
        Token *name;
        std::vector<ExpressionNode *> dimensions;
        VariableNode(Token *name, std::vector<ExpressionNode *> dims) : name(name), dimensions(dims) {}
        parser::NodeType getNodeType() const override { return parser::VARIABLE; }
    };

    class IntegerNode : public ExpressionNode
    {
    public:
        int value;
        IntegerNode(int val) : value(val) {}
        parser::NodeType getNodeType() const override { return parser::INTEGER; }
    };

    class StringNode : public ExpressionNode
    {
    public:
        std::string value;
        StringNode(std::string val) : value(val) {}
        parser::NodeType getNodeType() const override { return parser::STRING; }
    };

    class BinaryOperationNode : public ExpressionNode
    {
    public:
        Token *operatorSymbol;
        ExpressionNode *leftOperand;
        ExpressionNode *rightOperand;
        BinaryOperationNode(Token *op, ExpressionNode *left, ExpressionNode *right) : operatorSymbol(op), leftOperand(left), rightOperand(right) {}
        parser::NodeType getNodeType() const override { return parser::BINARY_OPERATION; }
    };

    class UnaryOperationNode : public ExpressionNode
    {
    public:
        Token *operatorSymbol;
        ExpressionNode *operand;
        UnaryOperationNode(Token *op, ExpressionNode *opnd) : operatorSymbol(op), operand(opnd) {}
        parser::NodeType getNodeType() const override { return parser::UNARY_OPERATION; }
    };

    class FunctionCallNode : public ExpressionNode
    {
    public:
        Token *functionName;
        std::vector<ExpressionNode *> arguments;
        FunctionCallNode(Token *funcName, std::vector<ExpressionNode *> args) : functionName(funcName), arguments(args) {}
        parser::NodeType getNodeType() const override { return parser::FUNCTION_CALL; }
    };

    class StatementNode : public BaseASTNode
    {
    public:
        parser::NodeType getNodeType() const override { return parser::STATEMENT; }
        virtual ~StatementNode() = default;
    };

    class ExpressionStatementNode : public StatementNode
    {
    public:
        ExpressionNode *expr;
        ExpressionStatementNode(ExpressionNode *expr) : expr(expr) {}
        parser::NodeType getNodeType() const override { return parser::EXPRESSION_STATEMENT; }
    };

    class Block : public StatementNode
    {
    public:
        std::vector<StatementNode *> statements;
        Block(std::vector<StatementNode *> stmts) : statements(stmts) {}
        parser::NodeType getNodeType() const override { return parser::BLOCK; }
    };

    class VariableDeclarationNode : public StatementNode
    {
    public:
        VariableNode *var;
        std::vector<ExpressionNode *> expression;
        VariableDeclarationNode(VariableNode *v, std::vector<ExpressionNode *> expr) : var(v), expression(expr) {}
        parser::NodeType getNodeType() const override { return parser::VARIABLE_DECLARATION; }
    };

    class ConstantDeclarationNode : public StatementNode
    {
    public:
        VariableNode *var;
        std::vector<ExpressionNode *> expression;
        ConstantDeclarationNode(VariableNode *v, std::vector<ExpressionNode *> expr) : var(v), expression(expr) {}
        parser::NodeType getNodeType() const override { return parser::CONSTANT_DECLARATION; }
    };

    class ReturnNode : public StatementNode
    {
    public:
        ExpressionNode *expression;
        ReturnNode(ExpressionNode *expr) : expression(expr) {}
        parser::NodeType getNodeType() const override { return parser::RETURN_STATEMENT; }
    };

    class IfElseNode : public StatementNode
    {
    public:
        ExpressionNode *condition;
        StatementNode *trueBlock;
        StatementNode *falseBlock;
        IfElseNode(ExpressionNode *cond, StatementNode *thenB, StatementNode *elseB) : condition(cond), trueBlock(thenB), falseBlock(elseB) {}
        parser::NodeType getNodeType() const override { return parser::IFELSE_STATEMENT; }
    };

    class SwitchNode : public StatementNode
    {
    public:
        ExpressionNode *matchValue;
        Block *body;
        SwitchNode(ExpressionNode *val, Block *b) : matchValue(val), body(b) {}
        parser::NodeType getNodeType() const override { return parser::SWITCH_STATEMENT; }
    };

    class CaseNode : public StatementNode
    {
    public:
        ExpressionNode *caseValue;
        CaseNode(ExpressionNode *value) : caseValue(value) {}
        parser::NodeType getNodeType() const override { return parser::CASE_STATEMENT; }
    };

    class DefaultCaseNode : public StatementNode
    {
    public:
        DefaultCaseNode() {}
        parser::NodeType getNodeType() const override { return parser::DEFAULT_CASE_STATEMENT; }
    };

    class WhileLoopNode : public StatementNode
    {
    public:
        ExpressionNode *condition;
        StatementNode *body;
        WhileLoopNode(ExpressionNode *cond, StatementNode *b) : condition(cond), body(b) {}
        parser::NodeType getNodeType() const override { return parser::WHILE_LOOP_STATEMENT; }
    };

    class ForLoopNode : public StatementNode
    {
    public:
        StatementNode *initExp;
        ExpressionNode *updateExp;
        ExpressionNode *condition;
        StatementNode *body;
        ForLoopNode(StatementNode *init, ExpressionNode *update, ExpressionNode *cond, StatementNode *b) : initExp(init), updateExp(update), condition(cond), body(b) {}
        parser::NodeType getNodeType() const override { return parser::FOR_LOOP_STATEMENT; }
    };

    class DoWhileLoopNode : public StatementNode
    {
    public:
        StatementNode *body;
        ExpressionNode *condition;
        DoWhileLoopNode(StatementNode *b, ExpressionNode *cond) : body(b), condition(cond) {}
        parser::NodeType getNodeType() const override { return parser::DO_WHILE_LOOP_STATEMENT; }
    };

    class BreakNode : public StatementNode
    {
    public:
        BreakNode() {}
        parser::NodeType getNodeType() const override { return parser::BREAK_STATEMENT; }
    };

    class ContinueNode : public StatementNode
    {
    public:
        ContinueNode() {}
        parser::NodeType getNodeType() const override { return parser::CONTINUE_STATEMENT; }
    };

    class FunctionDefinitionNode : public BaseASTNode
    {
    public:
        Token *functionName;
        std::vector<parser::VariableDeclarationNode *> parameters;
        Block *body;
        FunctionDefinitionNode(Token *funcName, std::vector<parser::VariableDeclarationNode *> params, Block *b) : functionName(funcName), parameters(params), body(b) {}
        parser::NodeType getNodeType() const override { return parser::FUNCTION_DEFINITION; }
    };

    class ParsedProgram
    {
    public:
        std::vector<parser::VariableDeclarationNode *> globalVariables;
        std::vector<parser::ConstantDeclarationNode *> globalConstants;
        std::vector<parser::FunctionDefinitionNode *> functions;
        ParsedProgram(std::vector<parser::VariableDeclarationNode *> globalVars, std::vector<parser::ConstantDeclarationNode *> globalConsts, std::vector<parser::FunctionDefinitionNode *> funcs) : globalVariables(globalVars), globalConstants(globalConsts), functions(funcs) {}
    };
}
class ASTBuilder
{
public:
    ASTBuilder() {}
    parser::IntegerNode *createIntegerNode(const int value)
    {
        return new parser::IntegerNode(value);
    }

    parser::StringNode *createStringNode(const std::string &value)
    {
        return new parser::StringNode(value);
    }

    parser::VariableNode *createVariableNode(Token *name, const std::vector<parser::ExpressionNode *> &dimensions)
    {
        return new parser::VariableNode(name, dimensions);
    }

    parser::BinaryOperationNode *createBinaryOperationNode(Token *op, parser::ExpressionNode *left, parser::ExpressionNode *right)
    {
        return new parser::BinaryOperationNode(op, left, right);
    }
    parser::UnaryOperationNode *createUnaryOperationNode(Token *op, parser::ExpressionNode *operand)
    {
        return new parser::UnaryOperationNode(op, operand);
    }
    parser::FunctionCallNode *createFunctionCallNode(Token *funcName, const std::vector<parser::ExpressionNode *> &args)
    {
        return new parser::FunctionCallNode(funcName, args);
    }
    parser::ExpressionStatementNode *createExpressionStatementNode(parser::ExpressionNode *expr)
    {
        return new parser::ExpressionStatementNode(expr);
    }
    parser::VariableDeclarationNode *createVariableDeclarationNode(parser::VariableNode *v, std::vector<parser::ExpressionNode *> expr)
    {
        return new parser::VariableDeclarationNode(v, expr);
    }

    parser::ConstantDeclarationNode *createConstantDeclarationNode(parser::VariableNode *v, std::vector<parser::ExpressionNode *> expr)
    {
        return new parser::ConstantDeclarationNode(v, expr);
    }

    parser::ReturnNode *createReturnNode(parser::ExpressionNode *expr)
    {
        return new parser::ReturnNode(expr);
    }
    parser::IfElseNode *createIfElseNode(parser::ExpressionNode *cond, parser::StatementNode *thenBlock, parser::StatementNode *elseBlock)
    {
        return new parser::IfElseNode(cond, thenBlock, elseBlock);
    }

    parser::SwitchNode *createSwitchNode(parser::ExpressionNode *matchValue, parser::Block *body)
    {
        return new parser::SwitchNode(matchValue, body);
    }

    parser::CaseNode *createCaseNode(parser::ExpressionNode *caseValue)
    {
        return new parser::CaseNode(caseValue);
    }

    parser::DefaultCaseNode *createDefaultCaseNode()
    {
        return new parser::DefaultCaseNode();
    }

    parser::WhileLoopNode *createWhileLoopNode(parser::ExpressionNode *cond, parser::StatementNode *body)
    {
        return new parser::WhileLoopNode(cond, body);
    }
    parser::ForLoopNode *createForLoopNode(parser::StatementNode *init, parser::ExpressionNode *update, parser::ExpressionNode *cond, parser::StatementNode *body)
    {
        return new parser::ForLoopNode(init, update, cond, body);
    }
    parser::DoWhileLoopNode *createDoWhileLoopNode(parser::StatementNode *body, parser::ExpressionNode *cond)
    {
        return new parser::DoWhileLoopNode(body, cond);
    }
    parser::BreakNode *createBreakNode()
    {
        return new parser::BreakNode();
    }
    parser::ContinueNode *createContinueNode()
    {
        return new parser::ContinueNode();
    }
    parser::FunctionDefinitionNode *createFunctionDefinitionNode(Token *funcName, const std::vector<parser::VariableDeclarationNode *> &params, parser::Block *body)
    {
        return new parser::FunctionDefinitionNode(funcName, params, body);
    }
    parser::Block *createBlockNode(const std::vector<parser::StatementNode *> &statements)
    {
        return new parser::Block(statements);
    }
};

class Parser
{

public:
    Parser(Lexer &lexer, ErrorHandler &errorHandler);
    parser::ParsedProgram *parse();

private:
    int getPrecedence(Token *t);
    Lexer &lexer;
    ErrorHandler &errorHandler;
    ASTBuilder builder;

    bool match(Token *token, grammar::TYPE expectedType, const std::string &expectedValue);
    bool match(Token *token, std::vector<grammar::TYPE> expectedTypes);
    void sync(grammar::TYPE expectedType , const std::string &expectedValue = "");

    parser::StatementNode *parseStatement();
    parser::ExpressionNode *parsePrimaryExpression();
    parser::ExpressionNode *parseBinaryExpression(int precedence);
    parser::ExpressionNode *parseForFunctionCallArguments();
    parser::ExpressionNode *parseUnaryExpression();
    parser::FunctionCallNode *parseFunctionCall(Token *name);
    parser::StatementNode *parseExpressionStatement();
    parser::Block *parseBlock();
    parser::IfElseNode *parseIfElseStatement();
    parser::SwitchNode *parseSwitchStatement();
    parser::CaseNode *parseCaseStatement();
    parser::WhileLoopNode *parseWhileStatement();
    parser::ForLoopNode *parseForStatement();
    parser::DoWhileLoopNode *parseDoWhileStatement();
    parser::ReturnNode *parseReturnStatement();
    parser::BreakNode *parseBreakStatement();
    parser::ContinueNode *parseContinueStatement();
    parser::DefaultCaseNode *parseDefaultCaseStatement();
    parser::VariableDeclarationNode *parseVariableDeclaration();
    parser::ConstantDeclarationNode *parseConstantDeclaration();
    parser::FunctionDefinitionNode *parseFunctionDefinition();
    parser::IntegerNode *parseInteger();
    parser::ExpressionNode *parseIdentifier();

    Token *peekNext();
    Token *getNextToken();
    Token *peekBack();
    void consumeToken();
};

#endif
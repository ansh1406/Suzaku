#ifndef IRGENERATOR_H
#define IRGENERATOR_H

#include "lexer.hpp"
#include "parser.hpp"
#include "grammar.hpp"

#include <vector>
#include <string>
#include <unordered_map>
#include <ostream>

namespace irgenerator
{
    struct Variable
    {
        union
        {
            int address;
            int offset;
        };
        std::vector<int> dimensions;
    };

    struct ProgramContext
    {
        std::unordered_map<std::string, Variable *> globalVariables;
        std::unordered_map<std::string, int> functionIds;
        std::unordered_map<std::string, std::vector<int>> functionArgumentSizes;
        int nextFreeGlobalAddress;
    };

    struct FunctionContext
    {
        std::unordered_map<std::string, Variable *> localVariables;
        int nextFreeLocalOffset;
        int currentStackOffset;
        int returnAddressOffsetFromBasePointer;
    };

    enum class IRCodeBlockType
    {
        SWITCH,
        FOR,
        WHILE,
        DO_WHILE,
    };

    struct CodeBlockContext
    {
        IRCodeBlockType type;
        int codeBlockId;
        CodeBlockContext(IRCodeBlockType blockType, int id) : type(blockType), codeBlockId(id) {}
    };

    enum class IRNodeType
    {
        INTEGER = 1,
        STRING,
        GLOBAL_VARIABLE,
        LOCAL_VARIABLE,
        GLOBAL_VARIABLE_ADDRESS,
        LOCAL_VARIABLE_ADDRESS,
        FUNCTION_CALL,
        ARITHMATIC_ADDTION_EXPRESSION,
        ARITHMATIC_SUBTRACTION_EXPRESSION,
        ARITHMATIC_MULTIPLICATION_EXPRESSION,
        ARITHMATIC_DIVISION_EXPRESSION,
        ARITHMATIC_MODULO_EXPRESSION,
        BITWISE_AND_EXPRESSION,
        BITWISE_OR_EXPRESSION,
        BITWISE_XOR_EXPRESSION,
        BITWISE_NAND_EXPRESSION,
        BITWISE_NOR_EXPRESSION,
        BITWISE_LEFT_SHIFT_EXPRESSION,
        BITWISE_RIGHT_SHIFT_EXPRESSION,
        RELATIONAL_LESS_THAN_EXPRESSION,
        RELATIONAL_GREATER_THAN_EXPRESSION,
        RELATIONAL_LESS_THAN_EQUALS_EXPRESSION,
        RELATIONAL_GREATER_THAN_EQUALS_EXPRESSION,
        RELATIONAL_EQUALS_EXPRESSION,
        RELATIONAL_NOT_EQUALS_EXPRESSION,
        LOGICAL_AND_EXPRESSION,
        LOGICAL_OR_EXPRESSION,
        ASSIGNMENT,
        UNARY_PLUS_EXPRESSION,
        UNARY_NEGATION_EXPRESSION,
        UNARY_LOGICAL_NOT_EXPRESSION,
        UNARY_BITWISE_NOT_EXPRESSION,
        UNARY_DEREFERENCE_EXPRESSION,
        VARIABLE_DECLARATION,
        GLOBAL_VARIABLE_DECLARATION,
        LOCAL_VARIABLE_DECLARATION,
        EXPRESSION_STATEMENT,
        IFELSE_STATEMENT,
        SWITCH_STATEMENT,
        CASE_STATEMENT,
        CASE_DEFAULT_STATEMENT,
        WHILE_LOOP,
        FOR_LOOP,
        DO_WHILE_LOOP,
        FOR_LOOP_BREAK_STATEMENT,
        WHILE_LOOP_BREAK_STATEMENT,
        DO_WHILE_LOOP_BREAK_STATEMENT,
        SWITCH_BREAK_STATEMENT,
        FOR_LOOP_CONTINUE_STATEMENT,
        WHILE_LOOP_CONTINUE_STATEMENT,
        DO_WHILE_LOOP_CONTINUE_STATEMENT,
        RETURN_STATEMENT,
        FUNCTION_DEFINITION
    };
    class IRNode
    {
    public:
        virtual ~IRNode() = default;
        virtual IRNodeType getNodeType() const = 0;
    };
    class ExpressionNode : public IRNode
    {
    public:
        virtual ~ExpressionNode() = default;
        virtual IRNodeType getNodeType() const override = 0;
    };
    class StatementNode : public IRNode
    {
    public:
        virtual ~StatementNode() = default;
        virtual IRNodeType getNodeType() const override = 0;
    };
    class IntegerNode : public ExpressionNode
    {
    public:
        int value;
        IntegerNode(int val) : value(val) {}
        IRNodeType getNodeType() const override { return IRNodeType::INTEGER; }
    };
    class StringNode : public ExpressionNode
    {
    public:
        std::string value;
        int baseAddress; // For string literals, this is the address in the global data segment
        StringNode(const std::string &val, int addr) : value(val), baseAddress(addr) {}
        IRNodeType getNodeType() const override { return IRNodeType::STRING; }
    };
    class VariableNode : public ExpressionNode
    {
    public:
        std::vector<ExpressionNode *> dimensions;
        VariableNode(std::vector<ExpressionNode *> dims)
            : dimensions(dims) {}

        virtual IRNodeType getNodeType() const override = 0;
    };
    class GlobalVariableNode : public VariableNode
    {
    public:
        int address;
        GlobalVariableNode(int addr, std::vector<ExpressionNode *> dims)
            : VariableNode(dims), address(addr) {}
        IRNodeType getNodeType() const override { return IRNodeType::GLOBAL_VARIABLE; }
    };
    class LocalVariableNode : public VariableNode
    {
    public:
        int offset;
        LocalVariableNode(int off, std::vector<ExpressionNode *> dims)
            : VariableNode(dims), offset(off) {}
        IRNodeType getNodeType() const override { return IRNodeType::LOCAL_VARIABLE; }
    };
    class VariableAddressNode : public ExpressionNode
    {
    public:
        std::vector<ExpressionNode *> dimensions;
        VariableAddressNode(std::vector<ExpressionNode *> dims)
            : dimensions(dims) {}
        virtual IRNodeType getNodeType() const override = 0;
    };
    class GlobalVariableAddressNode : public VariableAddressNode
    {
    public:
        int address;
        GlobalVariableAddressNode(int addr, const std::vector<ExpressionNode *> &dims)
            : VariableAddressNode(dims), address(addr) {}
        IRNodeType getNodeType() const override { return IRNodeType::GLOBAL_VARIABLE_ADDRESS; }
    };
    class LocalVariableAddressNode : public VariableAddressNode
    {
    public:
        int offset;
        LocalVariableAddressNode(int off, const std::vector<ExpressionNode *> &dims)
            : VariableAddressNode(dims), offset(off) {}
        IRNodeType getNodeType() const override { return IRNodeType::LOCAL_VARIABLE_ADDRESS; }
    };

    class FunctionCallNode : public ExpressionNode
    {
    public:
        int functionId;
        std::vector<std::vector<ExpressionNode *>> arguments;
        std::vector<int> uninitializedCount;
        FunctionCallNode(int funcId, const std::vector<std::vector<ExpressionNode *>> &args, std::vector<int> uninit)
            : functionId(funcId), arguments(args), uninitializedCount(uninit) {}
        IRNodeType getNodeType() const override { return IRNodeType::FUNCTION_CALL; }
    };
    class BinaryExpressionNode : public ExpressionNode
    {
    public:
        BinaryExpressionNode() = default;
        virtual IRNodeType getNodeType() const override = 0;
    };
    class ArithmeticAdditionNode : public BinaryExpressionNode
    {
    public:
        ArithmeticAdditionNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::ARITHMATIC_ADDTION_EXPRESSION; }
    };
    class ArithmeticSubtractionNode : public BinaryExpressionNode
    {
    public:
        ArithmeticSubtractionNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::ARITHMATIC_SUBTRACTION_EXPRESSION; }
    };
    class ArithmeticMultiplicationNode : public BinaryExpressionNode
    {
    public:
        ArithmeticMultiplicationNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::ARITHMATIC_MULTIPLICATION_EXPRESSION; }
    };
    class ArithmeticDivisionNode : public BinaryExpressionNode
    {
    public:
        ArithmeticDivisionNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::ARITHMATIC_DIVISION_EXPRESSION; }
    };
    class ArithmeticModuloNode : public BinaryExpressionNode
    {
    public:
        ArithmeticModuloNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::ARITHMATIC_MODULO_EXPRESSION; }
    };
    class BitwiseAndNode : public BinaryExpressionNode
    {
    public:
        BitwiseAndNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_AND_EXPRESSION; }
    };
    class BitwiseOrNode : public BinaryExpressionNode
    {
    public:
        BitwiseOrNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_OR_EXPRESSION; }
    };
    class BitwiseXorNode : public BinaryExpressionNode
    {
    public:
        BitwiseXorNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_XOR_EXPRESSION; }
    };
    class BitwiseNandNode : public BinaryExpressionNode
    {
    public:
        BitwiseNandNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_NAND_EXPRESSION; }
    };
    class BitwiseNorNode : public BinaryExpressionNode
    {
    public:
        BitwiseNorNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_NOR_EXPRESSION; }
    };
    class BitwiseLeftShiftNode : public BinaryExpressionNode
    {
    public:
        static int leftShiftCounter;
        int leftShiftId;
        BitwiseLeftShiftNode() : leftShiftId(leftShiftCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_LEFT_SHIFT_EXPRESSION; }
    };
    class BitwiseRightShiftNode : public BinaryExpressionNode
    {
    public:
        static int rightShiftCounter;
        int rightShiftId;
        BitwiseRightShiftNode() : rightShiftId(rightShiftCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::BITWISE_RIGHT_SHIFT_EXPRESSION; }
    };
    class RelationalLessThanNode : public BinaryExpressionNode
    {
    public:
        static int lessThanCounter;
        int lessThanId;
        RelationalLessThanNode() : lessThanId(lessThanCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::RELATIONAL_LESS_THAN_EXPRESSION; }
    };
    class RelationalGreaterThanNode : public BinaryExpressionNode
    {
    public:
        static int greaterThanCounter;
        int greaterThanId;
        RelationalGreaterThanNode() : greaterThanId(greaterThanCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::RELATIONAL_GREATER_THAN_EXPRESSION; }
    };
    class RelationalLessThanEqualsNode : public BinaryExpressionNode
    {
    public:
        static int lessThanEqualsCounter;
        int lessThanEqualsId;
        RelationalLessThanEqualsNode() : lessThanEqualsId(lessThanEqualsCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::RELATIONAL_LESS_THAN_EQUALS_EXPRESSION; }
    };
    class RelationalGreaterThanEqualsNode : public BinaryExpressionNode
    {
    public:
        static int greaterThanEqualsCounter;
        int greaterThanEqualsId;
        RelationalGreaterThanEqualsNode() : greaterThanEqualsId(greaterThanEqualsCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::RELATIONAL_GREATER_THAN_EQUALS_EXPRESSION; }
    };
    class RelationalEqualsNode : public BinaryExpressionNode
    {
    public:
        static int equalsCounter;
        int equalsId;
        RelationalEqualsNode() : equalsId(equalsCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::RELATIONAL_EQUALS_EXPRESSION; }
    };
    class RelationalNotEqualsNode : public BinaryExpressionNode
    {
    public:
        static int notEqualsCounter;
        int notEqualsId;
        RelationalNotEqualsNode() : notEqualsId(notEqualsCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::RELATIONAL_NOT_EQUALS_EXPRESSION; }
    };
    class LogicalAndNode : public BinaryExpressionNode
    {
    public:
        static int logicalAndCounter;
        int logicalAndId;
        LogicalAndNode() : logicalAndId(logicalAndCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::LOGICAL_AND_EXPRESSION; }
    };
    class LogicalOrNode : public BinaryExpressionNode
    {
    public:
        static int logicalOrCounter;
        int logicalOrId;
        LogicalOrNode() : logicalOrId(logicalOrCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::LOGICAL_OR_EXPRESSION; }
    };
    class AssignmentNode : public ExpressionNode
    {
    public:
        AssignmentNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::ASSIGNMENT; }
    };

    class UnaryExpressionNode : public ExpressionNode
    {
    public:
        UnaryExpressionNode() = default;
        virtual IRNodeType getNodeType() const override = 0;
    };
    class UnaryPlusNode : public UnaryExpressionNode
    {
    public:
        UnaryPlusNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::UNARY_PLUS_EXPRESSION; }
    };
    class UnaryNegationNode : public UnaryExpressionNode
    {
    public:
        UnaryNegationNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::UNARY_NEGATION_EXPRESSION; }
    };
    class UnaryLogicalNotNode : public UnaryExpressionNode
    {
    public:
        static int logicalNotCounter;
        int logicalNotId;
        UnaryLogicalNotNode() : logicalNotId(logicalNotCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::UNARY_LOGICAL_NOT_EXPRESSION; }
    };
    class UnaryBitwiseNotNode : public UnaryExpressionNode
    {
    public:
        UnaryBitwiseNotNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::UNARY_BITWISE_NOT_EXPRESSION; }
    };
    class UnaryDereferenceNode : public UnaryExpressionNode
    {
    public:
        UnaryDereferenceNode() = default;
        IRNodeType getNodeType() const override { return IRNodeType::UNARY_DEREFERENCE_EXPRESSION; }
    };

    class VariableDeclarationNode : public StatementNode
    {
    public:
        std::string name;
        std::vector<int> dimensions;
        VariableDeclarationNode(const std::string &varName, const std::vector<int> &dims)
            : name(varName), dimensions(dims) {}
        IRNodeType getNodeType() const override { return IRNodeType::VARIABLE_DECLARATION; }
    };

    class GlobalVariableDeclarationNode : public VariableDeclarationNode
    {
    public:
        int baseAddress;
        std::vector<int> initializationValues;
        GlobalVariableDeclarationNode(const std::string &varName, std::vector<int> &initVals, const std::vector<int> &dims, int addr)
            : VariableDeclarationNode(varName, dims), initializationValues(initVals), baseAddress(addr) {}
        IRNodeType getNodeType() const override { return IRNodeType::GLOBAL_VARIABLE_DECLARATION; }
    };

    class LocalVariableDeclarationNode : public VariableDeclarationNode
    {
    public:
        int baseOffset;
        int uninitializedCount;
        std::vector<std::vector<ExpressionNode *>> initializationValues;
        LocalVariableDeclarationNode(const std::string &varName, const std::vector<std::vector<ExpressionNode *>> &initVals, const std::vector<int> &dims, int offset, int uninitializedCount)
            : VariableDeclarationNode(varName, dims), initializationValues(initVals), baseOffset(offset), uninitializedCount(uninitializedCount) {}
        IRNodeType getNodeType() const override { return IRNodeType::LOCAL_VARIABLE_DECLARATION; }
    };

    class ExpressionStatementNode : public StatementNode
    {
    public:
        std::vector<ExpressionNode *> expressions;
        ExpressionStatementNode(const std::vector<ExpressionNode *> &exprs) : expressions(exprs) {}
        IRNodeType getNodeType() const override { return IRNodeType::EXPRESSION_STATEMENT; }
    };

    class IfElseNode : public StatementNode
    {
    public:
        static int ifCounter;
        int ifid;
        std::vector<ExpressionNode *> condition;
        std::vector<StatementNode *> trueBlock;
        std::vector<StatementNode *> falseBlock;
        IfElseNode(const std::vector<ExpressionNode *> &cond, const std::vector<StatementNode *> &trueBlk, const std::vector<StatementNode *> &falseBlk)
            : condition(cond), trueBlock(trueBlk), falseBlock(falseBlk), ifid(ifCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::IFELSE_STATEMENT; }
    };
    class SwitchNode : public StatementNode
    {
    public:
        static int switchCounter;
        int switchId;
        std::vector<ExpressionNode *> matchValue;
        std::vector<StatementNode *> body;
        std::vector<int> caseValues;
        SwitchNode(std::vector<ExpressionNode *> matchVal, const std::vector<StatementNode *> &bodyNodes, const std::vector<int> &caseVals)
            : matchValue(matchVal), body(bodyNodes), caseValues(caseVals), switchId(switchCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::SWITCH_STATEMENT; }
    };
    class CaseNode : public StatementNode
    {
    public:
        static int caseCounter;
        int caseid;
        int switchId;
        CaseNode(int id) : switchId(id), caseid(caseCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::CASE_STATEMENT; }
    };

    class DefaultCaseNode : public StatementNode
    {
    public:
        int switchId;
        DefaultCaseNode(int id) : switchId(id) {}
        IRNodeType getNodeType() const override { return IRNodeType::CASE_DEFAULT_STATEMENT; }
    };

    class WhileLoopNode : public StatementNode
    {
    public:
        static int whileCounter;
        int whileid;
        std::vector<ExpressionNode *> condition;
        std::vector<StatementNode *> body;
        WhileLoopNode(std::vector<ExpressionNode *> cond, const std::vector<StatementNode *> &body)
            : condition(cond), body(body), whileid(whileCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::WHILE_LOOP; }
    };
    class ForLoopNode : public StatementNode
    {
    public:
        static int forCounter;
        int forid;
        StatementNode *init;
        ExpressionStatementNode *update;
        std::vector<ExpressionNode *> condition;
        std::vector<StatementNode *> body;
        ForLoopNode(StatementNode *init, ExpressionStatementNode *upd, std::vector<ExpressionNode *> cond, const std::vector<StatementNode *> &body)
            : init(init), update(upd), condition(cond), body(body), forid(forCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::FOR_LOOP; }
    };
    class DoWhileLoopNode : public StatementNode
    {
    public:
        static int doWhileCounter;
        int doWhileid;
        std::vector<StatementNode *> body;
        std::vector<ExpressionNode *> condition;
        DoWhileLoopNode(const std::vector<StatementNode *> &body, std::vector<ExpressionNode *> cond)
            : body(body), condition(cond), doWhileid(doWhileCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::DO_WHILE_LOOP; }
    };
    class BreakStatementNode : public StatementNode
    {
    public:
        BreakStatementNode() = default;
        virtual IRNodeType getNodeType() const override = 0;
    };
    class ForLoopBreakStatementNode : public BreakStatementNode
    {
    public:
        int forLoopId;
        ForLoopBreakStatementNode(int loopId) : forLoopId(loopId) {}
        IRNodeType getNodeType() const override { return IRNodeType::FOR_LOOP_BREAK_STATEMENT; }
    };
    class WhileLoopBreakStatementNode : public BreakStatementNode
    {
    public:
        int whileLoopId;
        WhileLoopBreakStatementNode(int loopId) : whileLoopId(loopId) {}
        IRNodeType getNodeType() const override { return IRNodeType::WHILE_LOOP_BREAK_STATEMENT; }
    };
    class DoWhileLoopBreakStatementNode : public BreakStatementNode
    {
    public:
        int doWhileLoopId;
        DoWhileLoopBreakStatementNode(int loopId) : doWhileLoopId(loopId) {}
        IRNodeType getNodeType() const override { return IRNodeType::DO_WHILE_LOOP_BREAK_STATEMENT; }
    };
    class SwitchBreakStatementNode : public BreakStatementNode
    {
    public:
        int switchId;
        SwitchBreakStatementNode(int id) : switchId(id) {}
        IRNodeType getNodeType() const override { return IRNodeType::SWITCH_BREAK_STATEMENT; }
    };
    class ContinueStatementNode : public StatementNode
    {
    public:
        ContinueStatementNode() = default;
        virtual IRNodeType getNodeType() const override = 0;
    };
    class ForLoopContinueStatementNode : public ContinueStatementNode
    {
    public:
        int forLoopId;
        ForLoopContinueStatementNode(int loopId) : forLoopId(loopId) {}
        IRNodeType getNodeType() const override { return IRNodeType::FOR_LOOP_CONTINUE_STATEMENT; }
    };
    class WhileLoopContinueStatementNode : public ContinueStatementNode
    {
    public:
        int whileLoopId;
        WhileLoopContinueStatementNode(int loopId) : whileLoopId(loopId) {}
        IRNodeType getNodeType() const override { return IRNodeType::WHILE_LOOP_CONTINUE_STATEMENT; }
    };
    class DoWhileLoopContinueStatementNode : public ContinueStatementNode
    {
    public:
        int doWhileLoopId;
        DoWhileLoopContinueStatementNode(int loopId) : doWhileLoopId(loopId) {}
        IRNodeType getNodeType() const override { return IRNodeType::DO_WHILE_LOOP_CONTINUE_STATEMENT; }
    };
    class ReturnStatementNode : public StatementNode
    {
    public:
        int returnAddressOffset;
        int basePointerOffsetBefore;
        int basePointerOffsetAfter;
        std::vector<ExpressionNode *> returnValue;
        ReturnStatementNode(int returnAddrOffset, int basePtrOffsetBefore, int basePtrOffsetAfter, const std::vector<ExpressionNode *> &retValue)
            : returnAddressOffset(returnAddrOffset), basePointerOffsetBefore(basePtrOffsetBefore), basePointerOffsetAfter(basePtrOffsetAfter), returnValue(retValue) {}
        IRNodeType getNodeType() const override { return IRNodeType::RETURN_STATEMENT; }
    };
    class FunctionDefinitionNode : public StatementNode
    {
    public:
        static int functionCounter;
        int functionId;
        std::string name;
        std::vector<VariableDeclarationNode *> parameters;
        std::vector<StatementNode *> body;
        FunctionDefinitionNode(const std::string &funcName, const std::vector<VariableDeclarationNode *> &params, const std::vector<StatementNode *> &bodyNodes)
            : name(funcName), parameters(params), body(bodyNodes), functionId(functionCounter++) {}
        IRNodeType getNodeType() const override { return IRNodeType::FUNCTION_DEFINITION; }
    };

    class IntermediateRepresentation
    {
    public:
        std::vector<irgenerator::IRNode *> nodes;

        void addNode(irgenerator::IRNode *node)
        {
            nodes.push_back(node);
        }

        void clear()
        {
            for (auto node : nodes)
            {
                delete node;
            }
            nodes.clear();
        }

        ~IntermediateRepresentation()
        {
            clear();
        }
    };
}
class IRGenerator
{
public:
    IRGenerator(parser::ParsedProgram *program)
        : program(program) {}

    irgenerator::IntermediateRepresentation *generateIR();

private:
    irgenerator::FunctionDefinitionNode *generateFunctionDefinition(parser::FunctionDefinitionNode *funcDef);

    irgenerator::GlobalVariableDeclarationNode *generateGlobalVariableDeclaration(parser::VariableDeclarationNode *varDecl);
    irgenerator::LocalVariableDeclarationNode *generateLocalVariableDeclaration(parser::VariableDeclarationNode *varDecl);

    irgenerator::StatementNode *generateStatement(parser::StatementNode *stmt);
    irgenerator::StatementNode *generateStatement(parser::StatementNode *stmt, irgenerator::CodeBlockContext *codeBlockContext);
    irgenerator::ExpressionNode *generatePrimaryExpression(parser::ExpressionNode *expr);

    irgenerator::ExpressionNode *generateIntegerNode(parser::IntegerNode *intNode);
    irgenerator::ExpressionNode *generateStringNode(parser::StringNode *strNode);
    irgenerator::ExpressionNode *generateVariableNode(parser::VariableNode *varNode);
    irgenerator::ExpressionNode *generateVariableAddressNode(parser::VariableNode *varNode);
    irgenerator::ExpressionNode *generateFunctionCall(parser::FunctionCallNode *funcCall);

    irgenerator::ExpressionNode *generateBinaryExpression(parser::BinaryOperationNode *binExpr);
    irgenerator::ExpressionNode *generateUnaryExpression(parser::UnaryOperationNode *unaryExpr);

    irgenerator::StatementNode *generateExpressionStatement(parser::ExpressionStatementNode *exprStmt);
    irgenerator::StatementNode *generateIfElseStatement(parser::IfElseNode *ifStmt);
    irgenerator::StatementNode *generateSwitchStatement(parser::SwitchNode *switchStmt);
    irgenerator::StatementNode *generateWhileLoop(parser::WhileLoopNode *whileLoop);
    irgenerator::StatementNode *generateForLoop(parser::ForLoopNode *forLoop);
    irgenerator::StatementNode *generateDoWhileLoop(parser::DoWhileLoopNode *doWhileLoop);
    irgenerator::StatementNode *generateReturnStatement(parser::ReturnNode *returnStmt);

    irgenerator::StatementNode *generateCaseStatement(irgenerator::CodeBlockContext *codeBlockContext);
    irgenerator::StatementNode *generateDefaultCaseStatement(irgenerator::CodeBlockContext *codeBlockContext);
    irgenerator::StatementNode *generateBreakStatement(irgenerator::CodeBlockContext *codeBlockContext);
    irgenerator::StatementNode *generateContinueStatement(irgenerator::CodeBlockContext *codeBlockContext);

    int calculateArraySize(const std::vector<int> &dimensions);
    void serializeExpression(parser::ExpressionNode *expr, std::vector<irgenerator::ExpressionNode *> &expressions);

    parser::ParsedProgram *program;
    irgenerator::ProgramContext *programContext;
    irgenerator::FunctionContext *currentFunctionContext;
};

class IRFileWriter
{
public:
    IRFileWriter(std::ofstream &irFile, irgenerator::IntermediateRepresentation *ir) : irFile(irFile), ir(ir) {}

    void write();

private:
    std::ofstream &irFile;
    irgenerator::IntermediateRepresentation *ir;

    void writeIntegerNode(irgenerator::IntegerNode *node);
    void writeStringNode(irgenerator::StringNode *node);

    void writeGlobalVariableNode(irgenerator::GlobalVariableNode *node);
    void writeLocalVariableNode(irgenerator::LocalVariableNode *node);
    void writeGlobalVariableAddressNode(irgenerator::GlobalVariableAddressNode *node);
    void writeLocalVariableAddressNode(irgenerator::LocalVariableAddressNode *node);
    void writeFunctionCallNode(irgenerator::FunctionCallNode *node);

    void writeExpressionNode(irgenerator::ExpressionNode *node);

    void writeArithmeticAdditionNode(irgenerator::ArithmeticAdditionNode *node);
    void writeArithmeticSubtractionNode(irgenerator::ArithmeticSubtractionNode *node);
    void writeArithmeticMultiplicationNode(irgenerator::ArithmeticMultiplicationNode *node);
    void writeArithmeticDivisionNode(irgenerator::ArithmeticDivisionNode *node);
    void writeArithmeticModuloNode(irgenerator::ArithmeticModuloNode *node);
    void writeBitwiseAndNode(irgenerator::BitwiseAndNode *node);
    void writeBitwiseOrNode(irgenerator::BitwiseOrNode *node);
    void writeBitwiseXorNode(irgenerator::BitwiseXorNode *node);
    void writeBitwiseNandNode(irgenerator::BitwiseNandNode *node);
    void writeBitwiseNorNode(irgenerator::BitwiseNorNode *node);
    void writeBitwiseLeftShiftNode(irgenerator::BitwiseLeftShiftNode *node);
    void writeBitwiseRightShiftNode(irgenerator::BitwiseRightShiftNode *node);
    void writeRelationalLessThanNode(irgenerator::RelationalLessThanNode *node);
    void writeRelationalGreaterThanNode(irgenerator::RelationalGreaterThanNode *node);
    void writeRelationalLessThanEqualsNode(irgenerator::RelationalLessThanEqualsNode *node);
    void writeRelationalGreaterThanEqualsNode(irgenerator::RelationalGreaterThanEqualsNode *node);
    void writeRelationalEqualsNode(irgenerator::RelationalEqualsNode *node);
    void writeRelationalNotEqualsNode(irgenerator::RelationalNotEqualsNode *node);
    void writeLogicalAndNode(irgenerator::LogicalAndNode *node);
    void writeLogicalOrNode(irgenerator::LogicalOrNode *node);
    void writeAssignmentNode(irgenerator::AssignmentNode *node);

    void writeUnaryPlusNode(irgenerator::UnaryPlusNode *node);
    void writeUnaryNegationNode(irgenerator::UnaryNegationNode *node);
    void writeUnaryLogicalNotNode(irgenerator::UnaryLogicalNotNode *node);
    void writeUnaryBitwiseNotNode(irgenerator::UnaryBitwiseNotNode *node);
    void writeUnaryDereferenceNode(irgenerator::UnaryDereferenceNode *node);

    void writeGlobalVariableDeclarationNode(irgenerator::GlobalVariableDeclarationNode *node);
    void writeLocalVariableDeclarationNode(irgenerator::LocalVariableDeclarationNode *node);

    void writeStatementNode(irgenerator::StatementNode *node);

    void writeExpressionStatementNode(irgenerator::ExpressionStatementNode *node);
    void writeIfElseNode(irgenerator::IfElseNode *node);
    void writeSwitchNode(irgenerator::SwitchNode *node);
    void writeCaseNode(irgenerator::CaseNode *node);
    void writeDefaultCaseNode(irgenerator::DefaultCaseNode *node);
    void writeWhileLoopNode(irgenerator::WhileLoopNode *node);
    void writeForLoopNode(irgenerator::ForLoopNode *node);
    void writeDoWhileLoopNode(irgenerator::DoWhileLoopNode *node);

    void writeForLoopBreakStatementNode(irgenerator::ForLoopBreakStatementNode *node);
    void writeWhileLoopBreakStatementNode(irgenerator::WhileLoopBreakStatementNode *node);
    void writeDoWhileLoopBreakStatementNode(irgenerator::DoWhileLoopBreakStatementNode *node);
    void writeSwitchBreakStatementNode(irgenerator::SwitchBreakStatementNode *node);

    void writeForLoopContinueStatementNode(irgenerator::ForLoopContinueStatementNode *node);
    void writeWhileLoopContinueStatementNode(irgenerator::WhileLoopContinueStatementNode *node);
    void writeDoWhileLoopContinueStatementNode(irgenerator::DoWhileLoopContinueStatementNode *node);

    void writeReturnStatementNode(irgenerator::ReturnStatementNode *node);
    void writeFunctionDefinitionNode(irgenerator::FunctionDefinitionNode *node);

    void writeBuiltInFunctions();
};

#endif

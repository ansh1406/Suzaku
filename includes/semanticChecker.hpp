#ifndef SEMANTICCHECKER_H
#define SEMANTICCHECKER_H

#include"parser.hpp"
#include"errorHandler.hpp"
#include"grammar.hpp"
#include"lexer.hpp"
#include<vector>
#include<map>

namespace semantic_checker
{
    enum IdentifierType
    {
        LOCAL_VARIABLE,
        LOCAL_CONSTANT,
        GLOBAL_VARIABLE,
        GLOBAL_CONSTANT,
        FUNCTION_CALL,
        INVALID
    };
    class Constant{
        public:
        std::string name;
        std::vector<int> &values;
        std::vector<int> &dimensions;
        Constant(std::string name) : name(name), values(*new std::vector<int>()), dimensions(*new std::vector<int>()) {}
    };
    class Variable{
        public:
        std::string name;
        std::vector<int> &dimensions;
        Variable(std::string name) : name(name), dimensions(*new std::vector<int>()) {}
    };
}

class SemanticChecker
{
    public:
    SemanticChecker(parser::ParsedProgram *program, ErrorHandler &errorHandler) : program(program), errorHandler(errorHandler) {}
    void check();

    private:
    parser::ParsedProgram *program;
    ErrorHandler &errorHandler;
    
    std::unordered_map<std::string,int> globalVariables;
    std::unordered_map<std::string,int> globalConstants;
    std::unordered_map<std::string,int> localVariables;
    std::unordered_map<std::string,int> localConstants;
    std::unordered_map<std::string,int> functions;
    std::unordered_map<std::string,semantic_checker::Constant*> globalConstantValues;
    std::unordered_map<std::string,semantic_checker::Constant*> localConstantValues;
    std::unordered_map<std::string, semantic_checker::Variable*> globalVariableValues;
    std::unordered_map<std::string, semantic_checker::Variable*> localVariableValues;

    void checkGlobalVariableDeclaration();
    void checkGlobalConstantDeclaration();
    void checkFunctions();
    bool checkExpression(parser::ExpressionNode *&expr);
    bool checkUnaryOperation(parser::UnaryOperationNode *stmt);
    bool checkBinaryOperation(parser::BinaryOperationNode *stmt);
    bool checkFunctionCall(parser::FunctionCallNode *funcCall);
    bool checkForConstantExpression(parser::ExpressionNode *expr);

    bool validateIdentifierName(const std::string &name);
    semantic_checker::IdentifierType getIdentifierType(Token* token);

    void checkStatement(parser::StatementNode *stmt , bool declarationAllowed = false);
    void checkBlock(parser::Block *block , bool declarationAllowed = false);
    void checkExpressionStatement(parser::ExpressionStatementNode *exprStmt);
    void checkVariableDeclaration(parser::VariableDeclarationNode *varDecl);
    void checkConstantDeclaration(parser::ConstantDeclarationNode *constDecl);
    void checkIfElseStatement(parser::IfElseNode *ifElse);
    void checkSwitchStatement(parser::SwitchNode *switchStatement);
    void checkCaseStatement(parser::CaseNode *caseStmt);
    void checkForLoop(parser::ForLoopNode *forLoop);
    void checkWhileLoop(parser::WhileLoopNode *whileLoop);
    void checkDoWhileLoop(parser::DoWhileLoopNode *doWhileLoop);
    void checkReturnStatement(parser::ReturnNode *returnStmt);

    int getDimentionalOffset(parser::VariableNode *var, std::vector<int> &dimensions);

    parser::ExpressionNode* solveConstantExpression(parser::ExpressionNode *expr);


};

#endif
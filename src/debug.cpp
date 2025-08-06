#include "debug.hpp"

void printProgram(parser::ParsedProgram *program)
{
    std::cout << "No of Global Variables = " << program->globalVariables.size() << std::endl;
    for (auto globalVar : program->globalVariables)
    {
        std::cout << globalVar->var->name->value << ", ";
    }
    std::cout << std::endl;
    std::cout << "No of Global Constants = " << program->globalConstants.size() << std::endl;
    for (auto globalConst : program->globalConstants)
    {
        std::cout << globalConst->var->name->value << ", ";
    }
    std::cout << std::endl;
    std::cout << "No of Functions = " << program->functions.size() << std::endl;
    for (auto func : program->functions)
    {
        std::cout << func->functionName->value << ", ";
    }
    std::cout << "\n\n\n\n";
    for (auto globalVar : program->globalVariables)
    {
        printStatement(globalVar);
    }
    for (auto globalConst : program->globalConstants)
    {
        printStatement(globalConst);
    }
    for (auto fxn : program->functions)
    {
        printFunction(fxn);
    }
    std::cout << "End of Program" << std::endl;
    std::cout << "\n\n\n\n";
}

void printExpression(parser::ExpressionNode *expr)
{
    if (!expr)
    {
        std::cout << expr << std::endl;
        return;
    }
    switch (expr->getNodeType())
    {
    case parser::INTEGER:
        std::cout << " " << static_cast<parser::IntegerNode *>(expr)->value;
        break;
    case parser::STRING:
        std::cout << " \"" << static_cast<parser::StringNode *>(expr)->value << "\"";
        break;
    case parser::VARIABLE:
        std::cout << " " << static_cast<parser::VariableNode *>(expr)->name->value;
        for (auto dim : static_cast<parser::VariableNode *>(expr)->dimensions)
        {
            std::cout << "[";
            printExpression(dim);
            std::cout << "]";
        }
        break;
    case parser::BINARY_OPERATION:
        std::cout << "( ";
        printExpression(static_cast<parser::BinaryOperationNode *>(expr)->leftOperand);
        std::cout << " " << static_cast<parser::BinaryOperationNode *>(expr)->operatorSymbol->value;
        printExpression(static_cast<parser::BinaryOperationNode *>(expr)->rightOperand);
        std::cout << " )";
        break;
    case parser::UNARY_OPERATION:
        std::cout << " " << static_cast<parser::UnaryOperationNode *>(expr)->operatorSymbol->value;
        printExpression(static_cast<parser::UnaryOperationNode *>(expr)->operand);
        break;
    case parser::FUNCTION_CALL:
        std::cout << " " << static_cast<parser::FunctionCallNode *>(expr)->functionName->value;
        std::cout << "(";
        for (auto arg : static_cast<parser::FunctionCallNode *>(expr)->arguments)
        {
            printExpression(arg);
            std::cout << ", ";
        }
        std::cout << ")";
        break;
    default:
        std::cout << "Unknown expression type." << std::endl;
        break;
    }
}

void printStatement(parser::StatementNode *statement)
{
    if (statement == nullptr)
    {
        std::cout << "null" << std::endl;
        return;
    }
    switch (statement->getNodeType())
    {
    case parser::VARIABLE_DECLARATION:
        std::cout << "var " << static_cast<parser::VariableDeclarationNode *>(statement)->var->name->value;
        for (auto expr : static_cast<parser::VariableDeclarationNode *>(statement)->var->dimensions)
        {
            std::cout << "[";
            printExpression(expr);
            std::cout << "]";
        }
        std::cout << " = ";
        for (auto expr : static_cast<parser::VariableDeclarationNode *>(statement)->expression)
        {
            std::cout << " { ";
            printExpression(expr);
            std::cout << " } ";
        }
        break;
    case parser::CONSTANT_DECLARATION:
        std::cout << "const " << static_cast<parser::ConstantDeclarationNode *>(statement)->var->name->value;
        for (auto expr : static_cast<parser::ConstantDeclarationNode *>(statement)->var->dimensions)
        {
            std::cout << "[";
            printExpression(expr);
            std::cout << "]";
        }
        std::cout << " = ";
        for (auto expr : static_cast<parser::ConstantDeclarationNode *>(statement)->expression)
        {
            std::cout << " { ";
            printExpression(expr);
            std::cout << " } ";
        }
        break;
    case parser::EXPRESSION_STATEMENT:
        printExpression(static_cast<parser::ExpressionStatementNode *>(statement)->expr);
        break;

    case parser::IFELSE_STATEMENT:
        std::cout << "if (";
        printExpression(static_cast<parser::IfElseNode *>(statement)->condition);
        std::cout << ") {\n";
        if (static_cast<parser::IfElseNode *>(statement)->trueBlock->getNodeType() == parser::STATEMENT)
        {
            printStatement(static_cast<parser::IfElseNode *>(statement)->trueBlock);
        }
        else
        {
            for (auto stmt : static_cast<parser::Block *>(static_cast<parser::IfElseNode *>(statement)->trueBlock)->statements)
            {
                printStatement(stmt);
            }
        }
        std::cout << "}\n";
        if (static_cast<parser::IfElseNode *>(statement)->falseBlock != nullptr)
        {
            std::cout << "else {\n";
            if (static_cast<parser::IfElseNode *>(statement)->falseBlock->getNodeType() == parser::STATEMENT)
            {
                printStatement(static_cast<parser::IfElseNode *>(statement)->falseBlock);
            }
            else
            {
                for (auto stmt : static_cast<parser::Block *>(static_cast<parser::IfElseNode *>(statement)->falseBlock)->statements)
                {
                    printStatement(stmt);
                }
            }
            std::cout << "}";
        }
        break;

    case parser::FOR_LOOP_STATEMENT:
        std::cout << "for (";
        printStatement(static_cast<parser::ForLoopNode *>(statement)->initExp);
        std::cout << "; ";
        printExpression(static_cast<parser::ForLoopNode *>(statement)->condition);
        std::cout << "; ";
        printExpression(static_cast<parser::ForLoopNode *>(statement)->updateExp);
        std::cout << ") {\n";
        if (static_cast<parser::ForLoopNode *>(statement)->body->getNodeType() != parser::BLOCK)
        {
            printStatement(static_cast<parser::ForLoopNode *>(statement)->body);
        }
        else
        {
            for (auto stmt : static_cast<parser::Block *>(static_cast<parser::ForLoopNode *>(statement)->body)->statements)
            {
                printStatement(stmt);
            }
        }
        std::cout << "}";
        break;

    case parser::WHILE_LOOP_STATEMENT:
        std::cout << "while (";
        printExpression(static_cast<parser::WhileLoopNode *>(statement)->condition);
        std::cout << ") {\n";
        if (static_cast<parser::WhileLoopNode *>(statement)->body->getNodeType() != parser::BLOCK)
        {
            printStatement(static_cast<parser::WhileLoopNode *>(statement)->body);
        }
        else
        {
            for (auto stmt : static_cast<parser::Block *>(static_cast<parser::WhileLoopNode *>(statement)->body)->statements)
            {
                printStatement(stmt);
            }
        }
        std::cout << "}";
        break;

    case parser::DO_WHILE_LOOP_STATEMENT:
        std::cout << "do {\n";
        if (static_cast<parser::DoWhileLoopNode *>(statement)->body->getNodeType() != parser::BLOCK)
        {
            printStatement(static_cast<parser::DoWhileLoopNode *>(statement)->body);
        }
        else
        {
            for (auto stmt : static_cast<parser::Block *>(static_cast<parser::DoWhileLoopNode *>(statement)->body)->statements)
            {
                printStatement(stmt);
            }
        }
        std::cout << "} while (";
        printExpression(static_cast<parser::DoWhileLoopNode *>(statement)->condition);
        std::cout << ");";
        break;
    case parser::RETURN_STATEMENT:
        std::cout << "return ";
        printExpression(static_cast<parser::ReturnNode *>(statement)->expression);
        break;

    case parser::CASE_STATEMENT:
        std::cout << "case ";
        printExpression(static_cast<parser::CaseNode *>(statement)->caseValue);
        std::cout << ":";
        break;

    case parser::BREAK_STATEMENT:
        std::cout << "break;";
        break;
    case parser::CONTINUE_STATEMENT:
        std::cout << "continue;";
        break;
    case parser::SWITCH_STATEMENT:
        std::cout << "switch (";
        printExpression(static_cast<parser::SwitchNode *>(statement)->matchValue);
        std::cout << ") {\n";
        for (auto stmt : static_cast<parser::Block *>(static_cast<parser::SwitchNode *>(statement)->body)->statements)
        {
            printStatement(stmt);
        }
        std::cout << "}";
        break;
    case parser::DEFAULT_CASE_STATEMENT:
        std::cout << "default:";
        break;
    default:
        std::cout << "Unknown statement type." << std::endl;
        break;
    }
    std::cout << std::endl;
}

void printFunction(parser::FunctionDefinitionNode *function)
{
    bool flag = false;
    for (auto &builtIn : grammar::BuiltInFunctions)
    {
        if (builtIn == function->functionName->value)
        {
            flag = true;
            break;
        }
    }
    if (flag)
        return;
    std::cout << function->functionName->value << "(";
    for (auto param : function->parameters)
    {
        printStatement(param);
    }
    std::cout << "){" << std::endl;
    for (auto stmt : static_cast<parser::Block *>(function->body)->statements)
    {
        printStatement(stmt);
    }
    std::cout << "}" << std::endl;
}
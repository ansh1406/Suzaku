#include "semanticChecker.hpp"

void SemanticChecker::check()
{
    checkGlobalConstantDeclaration();
    checkGlobalVariableDeclaration();
    checkFunctions();
}

void SemanticChecker::checkGlobalVariableDeclaration()
{
    for (auto &varDecl : program->globalVariables)
    {
        if (validateIdentifierName(varDecl->var->name->value))
        {
            if (getIdentifierType(varDecl->var->name) != semantic_checker::INVALID)
            {
                errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Identifier name already used: " + varDecl->var->name->value);
                continue;
            }
            globalVariables[varDecl->var->name->value] = 1;
            globalVariableValues[varDecl->var->name->value] = new semantic_checker::Variable(varDecl->var->name->value);
        }
        else
        {
            errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Invalid identifeir name: " + varDecl->var->name->value);
        }
        for (auto &dim : varDecl->var->dimensions)
        {
            if (dim == nullptr || !checkForConstantExpression(dim))
            {
                errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Invalid dimension in variable declaration: " + varDecl->var->name->value);
                globalVariableValues[varDecl->var->name->value]->dimensions.push_back(0);
            }
            else
            {
                dim = solveConstantExpression(dim);
                globalVariableValues[varDecl->var->name->value]->dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
            }
        }
        for (auto &expr : varDecl->expression)
        {
            if (!checkForConstantExpression(expr))
            {
                errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Invalid expression in global variable declaration: " + varDecl->var->name->value);
            }
            else
            {
                expr = solveConstantExpression(expr);
            }
        }
    }
}

void SemanticChecker::checkGlobalConstantDeclaration()
{
    for (auto &constDecl : program->globalConstants)
    {
        if (validateIdentifierName(constDecl->var->name->value))
        {
            if (getIdentifierType(constDecl->var->name) != semantic_checker::INVALID)
            {
                errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Identifier name already used: " + constDecl->var->name->value);
                continue;
            }
            globalConstants[constDecl->var->name->value] = 1;
            globalConstantValues[constDecl->var->name->value] = new semantic_checker::Constant(constDecl->var->name->value);
        }
        else
        {
            errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Invalid identifier name: " + constDecl->var->name->value);
        }
        for (auto &dim : constDecl->var->dimensions)
        {
            if (dim == nullptr || !checkForConstantExpression(dim))
            {
                errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Invalid dimension in constant declaration: " + constDecl->var->name->value);
                globalConstantValues[constDecl->var->name->value]->dimensions.push_back(0);
            }
            else
            {
                dim = solveConstantExpression(dim);
                globalConstantValues[constDecl->var->name->value]->dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
            }
        }
        for (auto &expr : constDecl->expression)
        {
            if (!checkForConstantExpression(expr))
            {
                errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Invalid expression in constant declaration: " + constDecl->var->name->value);
                globalConstantValues[constDecl->var->name->value]->values.push_back(0);
            }
            else
            {
                expr = solveConstantExpression(expr);
                globalConstantValues[constDecl->var->name->value]->values.push_back(static_cast<parser::IntegerNode *>(expr)->value);
            }
        }
    }
}

void SemanticChecker::checkFunctions()
{
    for (auto &funcDef : program->functions)
    {
        // Exclude the built-in function __scanString from semantic checks
        if (funcDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::PRINT_CHAR))
        {
            functions[funcDef->functionName->value] = 2; // it will have one argument
            continue;
        }
        if (funcDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::PRINT_INT))
        {
            functions[funcDef->functionName->value] = 2;
            continue;
        }
        if (funcDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::PRINT_STRING))
        {
            functions[funcDef->functionName->value] = 2;
            continue;
        }
        if (funcDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::READ_INT))
        {
            functions[funcDef->functionName->value] = 2; 
            continue;
        }
        if (funcDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::READ_CHAR))
        {
            functions[funcDef->functionName->value] = 2;
            continue;
        }
        if (funcDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::READ_STRING))
        {
            functions[funcDef->functionName->value] = 2;
            continue;
        }
        if (validateIdentifierName(funcDef->functionName->value))
        {
            if (getIdentifierType(funcDef->functionName) != semantic_checker::INVALID)
            {
                errorHandler.reportError(funcDef->functionName->lineNumber, funcDef->functionName->columnNumber, "Identifier name already used: " + funcDef->functionName->value);
                continue;
            }
            functions[funcDef->functionName->value] = funcDef->parameters.size() + 1;
        }
    }
    for (auto &function : program->functions)
    {
        bool flag = false;
        for (auto const &builtIn : grammar::BuiltInFunctions)
        {
            if (function->functionName->value == builtIn)
            {
                flag = true;
                break;
            }
        }
        if (flag)
            continue;
        for (auto &param : function->parameters)
        {
            if (validateIdentifierName(param->var->name->value))
            {
                if (localVariables.find(param->var->name->value) != localVariables.end() || localConstants.find(param->var->name->value) != localConstants.end())
                {
                    errorHandler.reportError(param->var->name->lineNumber, param->var->name->columnNumber, "Parameter name already used: " + param->var->name->value);
                    continue;
                }
                localVariables[param->var->name->value] = 1;
                localVariableValues[param->var->name->value] = new semantic_checker::Variable(param->var->name->value);
                for(auto &dim : param->var->dimensions)
                {
                    if (dim == nullptr || !checkForConstantExpression(dim))
                    {
                        errorHandler.reportError(param->var->name->lineNumber, param->var->name->columnNumber, "Invalid dimension in parameter declaration: " + param->var->name->value);
                        localVariableValues[param->var->name->value]->dimensions.push_back(0);
                    }
                    else
                    {
                        dim = solveConstantExpression(dim);
                        localVariableValues[param->var->name->value]->dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
                    }
                }
            }
            else
            {
                errorHandler.reportError(param->var->name->lineNumber, param->var->name->columnNumber, "Invalid parameter name: " + param->var->name->value);
            }
            for (auto &dim : param->var->dimensions)
            {
                if (dim == nullptr || !checkForConstantExpression(dim))
                {
                    errorHandler.reportError(param->var->name->lineNumber, param->var->name->columnNumber, "Invalid dimension in parameter declaration: " + param->var->name->value);
                }
                else
                {
                    dim = solveConstantExpression(dim);
                }
            }
        }
        checkBlock(function->body,true);

        localVariables.clear();
        localConstants.clear();
    }
}

bool SemanticChecker::checkUnaryOperation(parser::UnaryOperationNode *stmt)
{
    if (stmt == nullptr)
    {
        return false;
    }
    if (stmt->operand == nullptr)
    {
        return false;
    }
    if (stmt->operatorSymbol == nullptr)
    {
        return false;
    }
    if (stmt->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN))
    {
        if (stmt->operand->getNodeType() != parser::VARIABLE)
        {
            errorHandler.reportError(stmt->operatorSymbol->lineNumber, stmt->operatorSymbol->columnNumber, "Invalid unary operation on non-variable: " + stmt->operatorSymbol->value);
            return false;
        }
        else
        {
            parser::VariableNode *var = static_cast<parser::VariableNode *>(stmt->operand);
            semantic_checker::IdentifierType type = getIdentifierType(var->name);
            if (type == semantic_checker::LOCAL_CONSTANT || type == semantic_checker::GLOBAL_CONSTANT)
            {
                errorHandler.reportError(var->name->lineNumber, var->name->columnNumber, "Cannot take address of a constant: " + var->name->value);
                return false;
            }
            if (type == semantic_checker::LOCAL_VARIABLE)
            {
                if (localVariableValues[var->name->value]->dimensions.size() != var->dimensions.size())
                {
                    errorHandler.reportError(var->name->lineNumber, var->name->columnNumber, "Variable dimensions do not match: " + var->name->value);
                    return false;
                }
            }
            if (type == semantic_checker::GLOBAL_VARIABLE)
            {
                if (globalVariableValues[var->name->value]->dimensions.size() != var->dimensions.size())
                {
                    errorHandler.reportError(var->name->lineNumber, var->name->columnNumber, "Variable dimensions do not match: " + var->name->value);
                    return false;
                }
            }
        }
    }
    return checkExpression(stmt->operand);
}

bool SemanticChecker::checkBinaryOperation(parser::BinaryOperationNode *stmt)
{
    if (stmt == nullptr)
    {
        return true;
    }
    if (stmt->leftOperand == nullptr || stmt->rightOperand == nullptr)
    {
        return false;
    }
    if (stmt->operatorSymbol == nullptr)
    {
        return false;
    }
    if (stmt->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN))
    {
        if (stmt->leftOperand && stmt->leftOperand->getNodeType() == parser::UNARY_OPERATION)
        {
            parser::UnaryOperationNode *unaryOp = static_cast<parser::UnaryOperationNode *>(stmt->leftOperand);
            if (unaryOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::ASTERISK_SIGN))
            {
                return checkExpression(stmt->leftOperand) && checkExpression(stmt->rightOperand);
            }
        }
        if (stmt->leftOperand->getNodeType() != parser::VARIABLE)
        {
            errorHandler.reportError(stmt->operatorSymbol->lineNumber, stmt->operatorSymbol->columnNumber, "Invalid assignment to non-variable.");
            return false;
        }
        Token *token = static_cast<parser::VariableNode *>(stmt->leftOperand)->name;
        semantic_checker::IdentifierType type = getIdentifierType(token);
        if (type == semantic_checker::INVALID)
        {
            errorHandler.reportError(token->lineNumber, token->columnNumber, "Undefined variable: " + token->value);
            return false;
        }
        if (type == semantic_checker::LOCAL_CONSTANT || type == semantic_checker::GLOBAL_CONSTANT)
        {
            errorHandler.reportError(token->lineNumber, token->columnNumber, "Cannot assign to a constant: " + token->value);
            return false;
        }
    }

    return checkExpression(stmt->leftOperand) && checkExpression(stmt->rightOperand);
}

bool SemanticChecker::checkFunctionCall(parser::FunctionCallNode *funcCall)
{
    if (funcCall == nullptr || funcCall->functionName == nullptr)
    {
        return false;
    }
    for (auto &arg : funcCall->arguments)
    {
        if (arg->getNodeType() != parser::STRING)
            checkExpression(arg);
    }
    if ((funcCall->arguments.size() + 1) != functions[funcCall->functionName->value])
    {
        errorHandler.reportError(funcCall->functionName->lineNumber, funcCall->functionName->columnNumber, "Function call argument count mismatch: ");
        return false;
    }
    return true;
}

bool SemanticChecker::checkExpression(parser::ExpressionNode *&expr)
{
    if (expr == nullptr)
    {
        return false;
    }
    if (expr->getNodeType() == parser::UNARY_OPERATION)
    {
        bool temp = checkUnaryOperation(static_cast<parser::UnaryOperationNode *>(expr));
        if (static_cast<parser::UnaryOperationNode *>(expr)->operand->getNodeType() == parser::INTEGER)
        {
            expr = solveConstantExpression(expr);
        }
        return temp;
    }
    if (expr->getNodeType() == parser::BINARY_OPERATION)
    {
        bool temp = checkBinaryOperation(static_cast<parser::BinaryOperationNode *>(expr));
        if (static_cast<parser::BinaryOperationNode *>(expr)->leftOperand->getNodeType() == parser::INTEGER &&
            static_cast<parser::BinaryOperationNode *>(expr)->rightOperand->getNodeType() == parser::INTEGER)
        {
            expr = solveConstantExpression(expr);
        }
        return temp;
    }
    if (expr->getNodeType() == parser::FUNCTION_CALL)
    {
        semantic_checker::IdentifierType type = getIdentifierType(static_cast<parser::FunctionCallNode *>(expr)->functionName);
        if (type == semantic_checker::INVALID)
        {
            errorHandler.reportError(static_cast<parser::FunctionCallNode *>(expr)->functionName->lineNumber, static_cast<parser::FunctionCallNode *>(expr)->functionName->columnNumber, "Undefined function: " + static_cast<parser::FunctionCallNode *>(expr)->functionName->value);
            return false;
        }
        return checkFunctionCall(static_cast<parser::FunctionCallNode *>(expr));
    }
    if (expr->getNodeType() == parser::VARIABLE)
    {
        Token *token = static_cast<parser::VariableNode *>(expr)->name;
        semantic_checker::IdentifierType type = getIdentifierType(token);
        if (type == semantic_checker::INVALID || type == semantic_checker::FUNCTION_CALL)
        {
            errorHandler.reportError(token->lineNumber, token->columnNumber, "Undefined variable: " + token->value);
            return false;
        }
        for (auto &dim : static_cast<parser::VariableNode *>(expr)->dimensions)
        {
            if (dim == nullptr || !checkExpression(dim))
            {
                errorHandler.reportError(token->lineNumber, token->columnNumber, "Invalid dimension in variable: " + token->value);
                return false;
            }
        }
        if (type == semantic_checker::LOCAL_CONSTANT || type== semantic_checker::GLOBAL_CONSTANT)
        {
           expr = solveConstantExpression(expr);
        }
        if (type == semantic_checker::LOCAL_VARIABLE)
        {
            semantic_checker::Variable *varValue = localVariableValues[token->value];
            if (varValue->dimensions.size() < static_cast<parser::VariableNode *>(expr)->dimensions.size())
            {
                errorHandler.reportError(token->lineNumber, token->columnNumber, "Variable dimension count exceed declaration: " + token->value);
                return false;
            }
        }
        if (type == semantic_checker::GLOBAL_VARIABLE)
        {
            semantic_checker::Variable *globalVarValue = globalVariableValues[token->value];
            if (globalVarValue->dimensions.size() < static_cast<parser::VariableNode *>(expr)->dimensions.size())
            {
                errorHandler.reportError(token->lineNumber, token->columnNumber, "Variable dimension count exceed declaration: " + token->value);
                return false;
            }
        }
    }
    if (expr->getNodeType() == parser::INTEGER)
    {
        return true;
    }
    if (expr->getNodeType() == parser::FUNCTION_CALL)
    {
        Token *token = static_cast<parser::FunctionCallNode *>(expr)->functionName;
        semantic_checker::IdentifierType type = getIdentifierType(token);
        if (type != semantic_checker::FUNCTION_CALL)
        {
            errorHandler.reportError(token->lineNumber, token->columnNumber, "Undefined function call: " + token->value);
            return false;
        }
    }
    return true;
}

void SemanticChecker::checkStatement(parser::StatementNode *stmt , bool declarationAllowed)
{
    switch (stmt->getNodeType())
    {
    case parser::EXPRESSION_STATEMENT:
        checkExpressionStatement(static_cast<parser::ExpressionStatementNode *>(stmt));
        break;
    case parser::VARIABLE_DECLARATION:
        if(!declarationAllowed)
        {
            errorHandler.reportError(static_cast<parser::VariableDeclarationNode *>(stmt)->var->name->lineNumber,static_cast<parser::VariableDeclarationNode *> (stmt)->var->name->columnNumber, "Variable declaration is not allowed in blocks.");
            return;
        }
        checkVariableDeclaration(static_cast<parser::VariableDeclarationNode *>(stmt));
        break;
    case parser::CONSTANT_DECLARATION:
        if(!declarationAllowed)
        {
            errorHandler.reportError(static_cast<parser::ConstantDeclarationNode *>(stmt)->var->name->lineNumber,static_cast<parser::ConstantDeclarationNode *>(stmt)->var->name->columnNumber, "Constant declaration is not allowed in blocks.");
            return;
        }
        checkConstantDeclaration(static_cast<parser::ConstantDeclarationNode *>(stmt));
        break;
    case parser::RETURN_STATEMENT:
        checkReturnStatement(static_cast<parser::ReturnNode *>(stmt));
        break;
    case parser::IFELSE_STATEMENT:
        checkIfElseStatement(static_cast<parser::IfElseNode *>(stmt));
        break;
    case parser::SWITCH_STATEMENT:
        checkSwitchStatement(static_cast<parser::SwitchNode *>(stmt));
        break;
    }
}

void SemanticChecker::checkBlock(parser::Block *block , bool declarationAllowed)
{
    for (auto &stmt : block->statements)
    {
        checkStatement(stmt,declarationAllowed);
    }
}

void SemanticChecker::checkExpressionStatement(parser::ExpressionStatementNode *exprStmt)
{
    if (exprStmt == nullptr || exprStmt->expr == nullptr)
    {
        return;
    }
    checkExpression(exprStmt->expr);
}

void SemanticChecker::checkVariableDeclaration(parser::VariableDeclarationNode *varDecl)
{
    if (varDecl == nullptr || varDecl->var == nullptr || varDecl->var->name == nullptr)
    {
        return;
    }
    if (!validateIdentifierName(varDecl->var->name->value))
    {
        errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Invalid variable name: " + varDecl->var->name->value);
        return;
    }
    if (localVariables.find(varDecl->var->name->value) != localVariables.end() || localConstants.find(varDecl->var->name->value) != localConstants.end())
    {
        errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Variable name already used: " + varDecl->var->name->value);
        return;
    }
    localVariables[varDecl->var->name->value] = 1;
    localVariableValues[varDecl->var->name->value] = new semantic_checker::Variable(varDecl->var->name->value);
    for (auto &dim : varDecl->var->dimensions)
    {
        if (dim == nullptr || !checkForConstantExpression(dim))
        {
            errorHandler.reportError(varDecl->var->name->lineNumber, varDecl->var->name->columnNumber, "Invalid dimension in variable declaration: " + varDecl->var->name->value);
            localVariableValues[varDecl->var->name->value]->dimensions.push_back(0);
        }
        else
        {
            dim = solveConstantExpression(dim);
            localVariableValues[varDecl->var->name->value]->dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
        }
    }
    for (auto &expr : varDecl->expression)
    {
        checkExpression(expr);
    }
}

void SemanticChecker::checkConstantDeclaration(parser::ConstantDeclarationNode *constDecl)
{
    if (constDecl == nullptr || constDecl->var == nullptr || constDecl->var->name == nullptr)
    {
        return;
    }
    if (!validateIdentifierName(constDecl->var->name->value))
    {
        errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Invalid constant name: " + constDecl->var->name->value);
        return;
    }
    if (localVariables.find(constDecl->var->name->value) != localVariables.end() || localConstants.find(constDecl->var->name->value) != localConstants.end())
    {
        errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Constant name already used: " + constDecl->var->name->value);
        return;
    }
    localConstants[constDecl->var->name->value] = 1;
    localConstantValues[constDecl->var->name->value] = new semantic_checker::Constant(constDecl->var->name->value);
    for (auto &dim : constDecl->var->dimensions)
    {
        if (dim == nullptr || !checkForConstantExpression(dim))
        {
            errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Invalid dimension in constant declaration: " + constDecl->var->name->value);
            localConstantValues[constDecl->var->name->value]->dimensions.push_back(0);
        }
        else
        {
            dim = solveConstantExpression(dim);
            localConstantValues[constDecl->var->name->value]->dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
        }
    }
    for (auto &expr : constDecl->expression)
    {
        if (!checkForConstantExpression(expr))
        {
            errorHandler.reportError(constDecl->var->name->lineNumber, constDecl->var->name->columnNumber, "Invalid expression in constant declaration: " + constDecl->var->name->value);
            localConstantValues[constDecl->var->name->value]->values.push_back(0);
        }
        else
        {
            expr = solveConstantExpression(expr);
            localConstantValues[constDecl->var->name->value]->values.push_back(static_cast<parser::IntegerNode *>(expr)->value);
        }
    }
}

void SemanticChecker::checkIfElseStatement(parser::IfElseNode *ifElse)
{
    if (ifElse == nullptr || ifElse->condition == nullptr || ifElse->trueBlock == nullptr)
    {
        return;
    }
    checkExpression(ifElse->condition);
    if (ifElse->trueBlock->getNodeType() == parser::BLOCK)
    {
        checkBlock(static_cast<parser::Block *>(ifElse->trueBlock));
    }
    else if (ifElse->trueBlock->getNodeType() == parser::STATEMENT)
    {
        checkExpression(static_cast<parser::ExpressionStatementNode *>(ifElse->trueBlock)->expr);
    }
    if (ifElse->falseBlock != nullptr)
    {
        if (ifElse->falseBlock->getNodeType() == parser::BLOCK)
        {
            checkBlock(static_cast<parser::Block *>(ifElse->falseBlock));
        }
        else if (ifElse->falseBlock->getNodeType() == parser::STATEMENT)
        {
            checkExpression(static_cast<parser::ExpressionStatementNode *>(ifElse->falseBlock)->expr);
        }
    }
}

void SemanticChecker::checkSwitchStatement(parser::SwitchNode *switchStatement)
{
    if (switchStatement == nullptr || switchStatement->matchValue == nullptr || switchStatement->body == nullptr)
    {
        return;
    }
    checkExpression(switchStatement->matchValue);
    checkBlock(switchStatement->body);
}

void SemanticChecker::checkCaseStatement(parser::CaseNode *caseStmt)
{
    if (caseStmt == nullptr || caseStmt->caseValue == nullptr)
    {
        return;
    }
    if (!checkForConstantExpression(caseStmt->caseValue))
    {
        return;
    }
    else
    {
        caseStmt->caseValue = solveConstantExpression(caseStmt->caseValue);
    }
}

void SemanticChecker::checkForLoop(parser::ForLoopNode *forLoop)
{
    if (forLoop->initExp)
    {
        if (forLoop->initExp->getNodeType() == parser::EXPRESSION_STATEMENT)
            checkExpression(static_cast<parser::ExpressionStatementNode *>(forLoop->initExp)->expr);
        if (forLoop->initExp->getNodeType() == parser::VARIABLE_DECLARATION)
            checkVariableDeclaration(static_cast<parser::VariableDeclarationNode *>(forLoop->initExp));
        if (forLoop->initExp->getNodeType() == parser::CONSTANT_DECLARATION)
            checkConstantDeclaration(static_cast<parser::ConstantDeclarationNode *>(forLoop->initExp));
    }
    if (forLoop->updateExp)
        checkExpression(forLoop->updateExp);
    if (forLoop->condition)
        checkExpression(forLoop->condition);
    if (forLoop->body && forLoop->body->getNodeType() == parser::BLOCK)
        checkBlock(static_cast<parser::Block *>(forLoop->body));
    else if (forLoop->body && forLoop->body->getNodeType() == parser::STATEMENT)
        checkStatement(static_cast<parser::StatementNode *>(forLoop->body));
}

void SemanticChecker::checkWhileLoop(parser::WhileLoopNode *whileLoop)
{
    if (whileLoop == nullptr || whileLoop->condition == nullptr || whileLoop->body == nullptr)
    {
        return;
    }
    checkExpression(whileLoop->condition);
    if (whileLoop->body->getNodeType() == parser::BLOCK)
    {
        checkBlock(static_cast<parser::Block *>(whileLoop->body));
    }
    else if (whileLoop->body->getNodeType() == parser::STATEMENT)
    {
        checkStatement(static_cast<parser::StatementNode *>(whileLoop->body));
    }
}

void SemanticChecker::checkDoWhileLoop(parser::DoWhileLoopNode *doWhileLoop)
{
    if (doWhileLoop == nullptr || doWhileLoop->body == nullptr || doWhileLoop->condition == nullptr)
    {
        return;
    }
    if (doWhileLoop->body->getNodeType() == parser::BLOCK)
    {
        checkBlock(static_cast<parser::Block *>(doWhileLoop->body));
    }
    else if (doWhileLoop->body->getNodeType() == parser::STATEMENT)
    {
        checkStatement(static_cast<parser::StatementNode *>(doWhileLoop->body));
    }
    checkExpression(doWhileLoop->condition);
}

void SemanticChecker::checkReturnStatement(parser::ReturnNode *returnStmt)
{
    if (returnStmt == nullptr)
    {
        return;
    }
    if (returnStmt->expression != nullptr)
    {
        checkExpression(returnStmt->expression);
    }
}

bool SemanticChecker::validateIdentifierName(const std::string &name)
{
    if (name.empty())
    {
        return false;
    }
    if (!isalpha(name[0]) && name[0] != '_')
    {
        return false;
    }
    for (char c : name)
    {
        if (!isalnum(c) && c != '_')
        {
            return false;
        }
    }
    if (name.length() > 32)
    {
        return false;
    }
    return true;
}

bool SemanticChecker::checkForConstantExpression(parser::ExpressionNode *expr)
{
    if (expr == nullptr)
    {
        return true;
    }
    if (expr->getNodeType() == parser::INTEGER)
    {
        return true;
    }
    if (expr->getNodeType() == parser::VARIABLE)
    {
        Token *token = static_cast<parser::VariableNode *>(expr)->name;
        semantic_checker::IdentifierType type = getIdentifierType(token);
        if (type == semantic_checker::GLOBAL_CONSTANT || type == semantic_checker::LOCAL_CONSTANT)
        {
            for (auto &dim : static_cast<parser::VariableNode *>(expr)->dimensions)
            {
                if (dim == nullptr || !checkForConstantExpression(dim))
                {
                    errorHandler.reportError(token->lineNumber, token->columnNumber, "Invalid dimension in constant: " + token->value);
                    return false;
                }
            }
            return true;
        }
    }
    if (expr->getNodeType() == parser::UNARY_OPERATION)
    {
        return checkUnaryOperation(static_cast<parser::UnaryOperationNode *>(expr)) && checkForConstantExpression(static_cast<parser::UnaryOperationNode *>(expr)->operand);
    }
    if (expr->getNodeType() == parser::BINARY_OPERATION)
    {
        return checkBinaryOperation(static_cast<parser::BinaryOperationNode *>(expr)) &&
               checkForConstantExpression(static_cast<parser::BinaryOperationNode *>(expr)->leftOperand) &&
               checkForConstantExpression(static_cast<parser::BinaryOperationNode *>(expr)->rightOperand);
    }
    if (expr->getNodeType() == parser::FUNCTION_CALL)
    {
        return false;
    }
    return false;
}

semantic_checker::IdentifierType SemanticChecker::getIdentifierType(Token *token)
{
    if (localVariables.find(token->value) != localVariables.end())
    {
        return semantic_checker::LOCAL_VARIABLE;
    }
    if (localConstants.find(token->value) != localConstants.end())
    {
        return semantic_checker::LOCAL_CONSTANT;
    }
    if (globalVariables.find(token->value) != globalVariables.end())
    {
        return semantic_checker::GLOBAL_VARIABLE;
    }
    if (globalConstants.find(token->value) != globalConstants.end())
    {
        return semantic_checker::GLOBAL_CONSTANT;
    }
    if (functions.find(token->value) != functions.end())
    {
        return semantic_checker::FUNCTION_CALL;
    }
    return semantic_checker::INVALID;
}

parser::ExpressionNode *SemanticChecker::solveConstantExpression(parser::ExpressionNode *expr)
{
    if (expr == nullptr)
    {
        return nullptr;
    }
    if (expr->getNodeType() == parser::INTEGER)
    {
        return expr;
    }
    if (expr->getNodeType() == parser::VARIABLE)
    {
        Token *token = static_cast<parser::VariableNode *>(expr)->name;
        semantic_checker::IdentifierType type = getIdentifierType(token);
        semantic_checker::Constant *constant = nullptr;
        if (type == semantic_checker::GLOBAL_CONSTANT)
        {
            constant = globalConstantValues[token->value];
        }
        else if (type == semantic_checker::LOCAL_CONSTANT)
        {
            constant = localConstantValues[token->value];
        }
        if (constant->dimensions.size() != static_cast<parser::VariableNode *>(expr)->dimensions.size())
        {
            errorHandler.reportError(token->lineNumber, token->columnNumber, "Dimension mismatch in constant: " + token->value);
            delete expr;
            return new parser::IntegerNode(0);
        }
        int dimensional_offset = getDimentionalOffset(static_cast<parser::VariableNode *>(expr), constant->dimensions);
        delete expr;
        return new parser::IntegerNode(constant->values[dimensional_offset]);
    }
    if (expr->getNodeType() == parser::BINARY_OPERATION)
    {
        parser::BinaryOperationNode *binOp = static_cast<parser::BinaryOperationNode *>(expr);
        binOp->leftOperand = solveConstantExpression(binOp->leftOperand);
        binOp->rightOperand = solveConstantExpression(binOp->rightOperand);
        if (binOp->leftOperand->getNodeType() == parser::INTEGER && binOp->rightOperand->getNodeType() == parser::INTEGER)
        {
            int leftValue = static_cast<parser::IntegerNode *>(binOp->leftOperand)->value;
            int rightValue = static_cast<parser::IntegerNode *>(binOp->rightOperand)->value;
            int result = 0;
            if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN))
            {
                result = leftValue + rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN))
            {
                result = leftValue - rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::ASTERISK_SIGN))
            {
                result = leftValue * rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::FORWARD_SLASH_SIGN))
            {
                if (rightValue == 0)
                {
                    errorHandler.reportError(binOp->operatorSymbol->lineNumber, binOp->operatorSymbol->columnNumber, "Cannot divide by 0.");
                    return nullptr;
                }
                result = leftValue / rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::PERCENT_SIGN))
            {
                result = leftValue % rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN))
            {
                result = leftValue < rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_EQUALS_SIGN))
            {
                result = leftValue <= rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN))
            {
                result = leftValue > rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_EQUALS_SIGN))
            {
                result = leftValue >= rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_EQUALS_SIGN))
            {
                result = leftValue == rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::NOT_EQUALS_SIGN))
            {
                result = leftValue != rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::CARET_SIGN))
            {
                result = leftValue ^ rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::VERTICAL_BAR_SIGN))
            {
                result = leftValue | rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_VERTICAL_BAR_SIGN))
            {
                result = leftValue || rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_AMPERSAND_SIGN))
            {
                result = leftValue && rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_LESS_THAN_SIGN))
            {
                result = leftValue << rightValue;
            }
            else if (binOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_GREATER_THAN_SIGN))
            {
                result = leftValue >> rightValue;
            }
            delete expr;
            return new parser::IntegerNode(result);
        }
    }
    else if (expr->getNodeType() == parser::UNARY_OPERATION)
    {
        parser::UnaryOperationNode *unaryOp = static_cast<parser::UnaryOperationNode *>(expr);
        unaryOp->operand = solveConstantExpression(unaryOp->operand);
        int operand = static_cast<parser::IntegerNode *>(unaryOp->operand)->value;
        delete unaryOp;
        if (unaryOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN))
        {
            return new parser::IntegerNode(operand);
        }
        if (unaryOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN))
        {
            return new parser::IntegerNode(-operand);
        }
        if (unaryOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::TILDE))
        {
            return new parser::IntegerNode(~operand);
        }
        if (unaryOp->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::EXCLAMATION_MARK))
        {
            return new parser::IntegerNode(!operand);
        }
    }
}

int SemanticChecker::getDimentionalOffset(parser::VariableNode *var, std::vector<int> &dimensions)
{
    if (var == nullptr || var->dimensions.size() != dimensions.size())
    {
        return 0;
    }
    int offset = 0;
    int accumulated_offset = 1;
    for (int i = dimensions.size() - 1; i >= 0; i--)
    {
        if (dimensions[i] <= static_cast<parser::IntegerNode *>(var->dimensions[i])->value)
        {
            errorHandler.reportError(var->name->lineNumber, var->name->columnNumber, "Index out of bound: " + var->name->value);
            return 0;
        }
        offset = offset * accumulated_offset + (static_cast<parser::IntegerNode *>(var->dimensions[i])->value);
        accumulated_offset *= dimensions[i];
    }
    return offset;
}
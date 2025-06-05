#include "IRGenerator.hpp"

int irgenerator::IfElseNode::ifCounter = 0;
int irgenerator::ForLoopNode::forCounter = 0;
int irgenerator::WhileLoopNode::whileCounter = 0;
int irgenerator::DoWhileLoopNode::doWhileCounter = 0;
int irgenerator::SwitchNode::switchCounter = 0;
int irgenerator::CaseNode::caseCounter = 0;
int irgenerator::BitwiseLeftShiftNode::leftShiftCounter = 0;
int irgenerator::BitwiseRightShiftNode::rightShiftCounter = 0;
int irgenerator::RelationalLessThanNode::lessThanCounter = 0;
int irgenerator::RelationalLessThanEqualsNode::lessThanEqualsCounter = 0;
int irgenerator::RelationalGreaterThanNode::greaterThanCounter = 0;
int irgenerator::RelationalGreaterThanEqualsNode::greaterThanEqualsCounter = 0;
int irgenerator::RelationalEqualsNode::equalsCounter = 0;
int irgenerator::RelationalNotEqualsNode::notEqualsCounter = 0;
int irgenerator::LogicalAndNode::logicalAndCounter = 0;
int irgenerator::LogicalOrNode::logicalOrCounter = 0;
int irgenerator::UnaryLogicalNotNode::logicalNotCounter = 0;
int irgenerator::FunctionDefinitionNode::functionCounter = 0;

irgenerator::IntermediateRepresentation *IRGenerator::generateIR()
{
    programContext = new irgenerator::ProgramContext();
    currentFunctionContext = new irgenerator::FunctionContext();
    irgenerator::IntermediateRepresentation *ir = new irgenerator::IntermediateRepresentation();

    for (const auto &globalVar : program->globalVariables)
    {
        ir->addNode(generateGlobalVariableDeclaration(globalVar));
    }
    for (const auto &functionDef : program->functions)
    {
        if (functionDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::PRINT_CHAR))
        {
            programContext->functionIds[functionDef->functionName->value] = 32767;         // Assign a special ID for built-in functions
            programContext->functionArgumentSizes[functionDef->functionName->value] = {1}; // Built-in printChar takes one argument
        }
        else if (functionDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::PRINT_INT))
        {
            programContext->functionIds[functionDef->functionName->value] = 32766;         // Assign a special ID for built-in functions
            programContext->functionArgumentSizes[functionDef->functionName->value] = {1}; // Built-in printInt takes one argument
        }
        else if (functionDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::PRINT_STRING))
        {
            programContext->functionIds[functionDef->functionName->value] = 32765;         // Assign a special ID for built-in functions
            programContext->functionArgumentSizes[functionDef->functionName->value] = {1}; // Built-in printString takes one argument
        }
        else if (functionDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::READ_CHAR))
        {
            programContext->functionIds[functionDef->functionName->value] = 32764;        // Assign a special ID for built-in functions
            programContext->functionArgumentSizes[functionDef->functionName->value] = {1}; // Built-in readInt takes 1 arguments
        }
        else if (functionDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::READ_INT))
        {
            programContext->functionIds[functionDef->functionName->value] = 32763;        // Assign a special ID for built-in functions
            programContext->functionArgumentSizes[functionDef->functionName->value] = {1}; // Built-in readChar takes 1 arguments
        }
        else if (functionDef->functionName->value == grammar::BuiltInFunctionAsString.at(grammar::BUILT_IN_FUNCTIONS::READ_STRING))
        {
            programContext->functionIds[functionDef->functionName->value] = 32762;        // Assign a special ID for built-in functions
            programContext->functionArgumentSizes[functionDef->functionName->value] = {1}; // Built-in readString takes 1 arguments
        }
        else
        {
            programContext->functionIds[functionDef->functionName->value] = irgenerator::FunctionDefinitionNode::functionCounter++;
            std::vector<int> argumentDimensions;
            for (auto &param : functionDef->parameters)
            {
                int argumentSize = 1;
                for (auto &dim : param->var->dimensions)
                {
                    if (dim->getNodeType() == parser::NodeType::INTEGER)
                    {
                        argumentSize *= static_cast<parser::IntegerNode *>(dim)->value;
                    }
                }
                argumentDimensions.push_back(argumentSize);
            }
            programContext->functionArgumentSizes[functionDef->functionName->value] = argumentDimensions;
        }
    }
    irgenerator::FunctionDefinitionNode::functionCounter = 0;
    for (const auto &functionDef : program->functions)
    {
        int flag = 0;
        for (auto const &builtInFxn : grammar::BuiltInFunctions)
        {
            if (functionDef->functionName->value == builtInFxn)
            {
                flag = 1;
            }
        }
        if (flag)
            continue;
        ir->addNode(generateFunctionDefinition(functionDef));
    }
    return ir;
}

irgenerator::GlobalVariableDeclarationNode *IRGenerator::generateGlobalVariableDeclaration(parser::VariableDeclarationNode *varDecl)
{
    std::vector<int> dimensions;
    std::vector<int> initializationValues;
    for (const auto &dim : varDecl->var->dimensions)
    {
        if (dim->getNodeType() == parser::NodeType::INTEGER)
        {
            dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
        }
    }
    for (const auto &expr : varDecl->expression)
    {
        if (expr->getNodeType() == parser::NodeType::INTEGER)
        {
            initializationValues.push_back(static_cast<parser::IntegerNode *>(expr)->value);
        }
    }
    int address = programContext->nextFreeGlobalAddress;
    programContext->nextFreeGlobalAddress += calculateArraySize(dimensions);
    programContext->globalVariables[varDecl->var->name->value] = new irgenerator::Variable();
    programContext->globalVariables[varDecl->var->name->value]->address = address;
    programContext->globalVariables[varDecl->var->name->value]->dimensions = dimensions;
    return new irgenerator::GlobalVariableDeclarationNode(varDecl->var->name->value, initializationValues, dimensions, address);
}

irgenerator::LocalVariableDeclarationNode *IRGenerator::generateLocalVariableDeclaration(parser::VariableDeclarationNode *varDecl)
{
    std::vector<int> dimensions;
    std::vector<std::vector<irgenerator::ExpressionNode *>> initializationValues;
    for (const auto &dim : varDecl->var->dimensions)
    {
        if (dim->getNodeType() == parser::NodeType::INTEGER)
        {
            dimensions.push_back(static_cast<parser::IntegerNode *>(dim)->value);
        }
    }
    for (const auto &expr : varDecl->expression)
    {
        std::vector<irgenerator::ExpressionNode *> initValue;
        serializeExpression(expr, initValue);
        initializationValues.push_back(initValue);
    }
    currentFunctionContext->nextFreeLocalOffset += calculateArraySize(dimensions);
    int offset = currentFunctionContext->nextFreeLocalOffset-1;
    int uninitializedCount = calculateArraySize(dimensions) - varDecl->expression.size();
    currentFunctionContext->currentStackOffset = currentFunctionContext->nextFreeLocalOffset;
    currentFunctionContext->localVariables[varDecl->var->name->value] = new irgenerator::Variable();
    currentFunctionContext->localVariables[varDecl->var->name->value]->offset = offset;
    currentFunctionContext->localVariables[varDecl->var->name->value]->dimensions = dimensions;
    return new irgenerator::LocalVariableDeclarationNode(varDecl->var->name->value, initializationValues, dimensions, offset, uninitializedCount);
}

irgenerator::FunctionDefinitionNode *IRGenerator::generateFunctionDefinition(parser::FunctionDefinitionNode *funcDef)
{
    irgenerator::FunctionDefinitionNode *functionNode = new irgenerator::FunctionDefinitionNode(funcDef->functionName->value, {}, {});
    programContext->functionIds[funcDef->functionName->value] = functionNode->functionId;

    currentFunctionContext->currentStackOffset = 0;
    currentFunctionContext->localVariables.clear();
    currentFunctionContext->nextFreeLocalOffset = 0;

    for (auto &param : funcDef->parameters)
    {
        functionNode->parameters.push_back(generateLocalVariableDeclaration(param));
    }

    currentFunctionContext->returnAddressOffsetFromBasePointer = currentFunctionContext->nextFreeLocalOffset;
    currentFunctionContext->nextFreeLocalOffset++; // Reserve space for the return address
    currentFunctionContext->currentStackOffset++;
    for (const auto &stmt : funcDef->body->statements)
    {
        functionNode->body.push_back(generateStatement(stmt));
    }
    functionNode->body.push_back(generateReturnStatement(nullptr)); // Default return statement if none is triggered

    return functionNode;
}

irgenerator::StatementNode *IRGenerator::generateStatement(parser::StatementNode *stmt)
{
    switch (stmt->getNodeType())
    {
    case parser::NodeType::VARIABLE_DECLARATION:
        return generateLocalVariableDeclaration(static_cast<parser::VariableDeclarationNode *>(stmt));
    case parser::NodeType::CONSTANT_DECLARATION:
        // Constants are not supported in IR generation, so we skip them
        return nullptr;
    case parser::NodeType::EXPRESSION_STATEMENT:
        return generateExpressionStatement(static_cast<parser::ExpressionStatementNode *>(stmt));
    case parser::NodeType::RETURN_STATEMENT:
        return generateReturnStatement(static_cast<parser::ReturnNode *>(stmt));
    case parser::NodeType::IFELSE_STATEMENT:
        return generateIfElseStatement(static_cast<parser::IfElseNode *>(stmt));
    case parser::NodeType::SWITCH_STATEMENT:
        return generateSwitchStatement(static_cast<parser::SwitchNode *>(stmt));
    case parser::NodeType::FOR_LOOP_STATEMENT:
        return generateForLoop(static_cast<parser::ForLoopNode *>(stmt));
    case parser::NodeType::WHILE_LOOP_STATEMENT:
        return generateWhileLoop(static_cast<parser::WhileLoopNode *>(stmt));
    case parser::NodeType::DO_WHILE_LOOP_STATEMENT:
        return generateDoWhileLoop(static_cast<parser::DoWhileLoopNode *>(stmt));
    default:
        throw std::runtime_error("Unsupported statement type");
    }
}

irgenerator::StatementNode *IRGenerator::generateStatement(parser::StatementNode *stmt, irgenerator::CodeBlockContext *codeBlockContext)
{
    switch (stmt->getNodeType())
    {
    case parser::NodeType::CASE_STATEMENT:
        return generateCaseStatement(codeBlockContext);
    case parser::NodeType::DEFAULT_CASE_STATEMENT:
        return generateDefaultCaseStatement(codeBlockContext);
    case parser::NodeType::BREAK_STATEMENT:
        return generateBreakStatement(codeBlockContext);
    case parser::NodeType::CONTINUE_STATEMENT:
        return generateContinueStatement(codeBlockContext);
    case parser::NodeType::VARIABLE_DECLARATION:
        return generateLocalVariableDeclaration(static_cast<parser::VariableDeclarationNode *>(stmt));
    case parser::NodeType::EXPRESSION_STATEMENT:
        return generateExpressionStatement(static_cast<parser::ExpressionStatementNode *>(stmt));
    case parser::NodeType::RETURN_STATEMENT:
        return generateReturnStatement(static_cast<parser::ReturnNode *>(stmt));
    case parser::NodeType::IFELSE_STATEMENT:
        return generateIfElseStatement(static_cast<parser::IfElseNode *>(stmt));
    case parser::NodeType::SWITCH_STATEMENT:
        return generateSwitchStatement(static_cast<parser::SwitchNode *>(stmt));
    case parser::NodeType::FOR_LOOP_STATEMENT:
        return generateForLoop(static_cast<parser::ForLoopNode *>(stmt));
    case parser::NodeType::WHILE_LOOP_STATEMENT:
        return generateWhileLoop(static_cast<parser::WhileLoopNode *>(stmt));
    case parser::NodeType::DO_WHILE_LOOP_STATEMENT:
        return generateDoWhileLoop(static_cast<parser::DoWhileLoopNode *>(stmt));
    case parser::NodeType::CONSTANT_DECLARATION:
        // Constants are not supported in IR generation, so we skip them
        return nullptr;
    }
}

irgenerator::StatementNode *IRGenerator::generateCaseStatement(irgenerator::CodeBlockContext *codeBlockContext)
{
    return new irgenerator::CaseNode(codeBlockContext->codeBlockId);
}

irgenerator::StatementNode *IRGenerator::generateDefaultCaseStatement(irgenerator::CodeBlockContext *codeBlockContext)
{
    return new irgenerator::DefaultCaseNode(codeBlockContext->codeBlockId);
}

irgenerator::StatementNode *IRGenerator::generateBreakStatement(irgenerator::CodeBlockContext *codeBlockContext)
{
    switch (codeBlockContext->type)
    {
    case irgenerator::IRCodeBlockType::FOR:
        return new irgenerator::ForLoopBreakStatementNode(codeBlockContext->codeBlockId);
    case irgenerator::IRCodeBlockType::WHILE:
        return new irgenerator::WhileLoopBreakStatementNode(codeBlockContext->codeBlockId);
    case irgenerator::IRCodeBlockType::DO_WHILE:
        return new irgenerator::DoWhileLoopBreakStatementNode(codeBlockContext->codeBlockId);
    case irgenerator::IRCodeBlockType::SWITCH:
        return new irgenerator::SwitchBreakStatementNode(codeBlockContext->codeBlockId);
    }
}

irgenerator::StatementNode *IRGenerator::generateContinueStatement(irgenerator::CodeBlockContext *codeBlockContext)
{
    switch (codeBlockContext->type)
    {
    case irgenerator::IRCodeBlockType::FOR:
        return new irgenerator::ForLoopContinueStatementNode(codeBlockContext->codeBlockId);
    case irgenerator::IRCodeBlockType::WHILE:
        return new irgenerator::WhileLoopContinueStatementNode(codeBlockContext->codeBlockId);
    case irgenerator::IRCodeBlockType::DO_WHILE:
        return new irgenerator::DoWhileLoopContinueStatementNode(codeBlockContext->codeBlockId);
    default:
        throw std::runtime_error("Unsupported continue statement type");
    }
}

irgenerator::StatementNode *IRGenerator::generateReturnStatement(parser::ReturnNode *returnStmt)
{
    irgenerator::ReturnStatementNode *retStmt = new irgenerator::ReturnStatementNode(0, 0, 0, {});
    std::vector<irgenerator::ExpressionNode *> returnValues;
    if (returnStmt && returnStmt->expression)
    {
        serializeExpression(returnStmt->expression, returnValues);
    }
    else
    {
        // If no expression is provided, we still need to push a return value to the stack so I chose 0 as default
        returnValues.push_back(new irgenerator::IntegerNode(0));
        currentFunctionContext->currentStackOffset++; // Return value will be pushed on to the stack
    }
    retStmt->returnValue = returnValues;
    retStmt->basePointerOffsetBefore = currentFunctionContext->currentStackOffset;
    currentFunctionContext->currentStackOffset--;                                                                                               // Return value will already have been popped from the stack
    retStmt->returnAddressOffset = currentFunctionContext->currentStackOffset - currentFunctionContext->returnAddressOffsetFromBasePointer - 1; // -1 Due to architecture
    retStmt->basePointerOffsetAfter = currentFunctionContext->currentStackOffset;
    return retStmt;
}

irgenerator::StatementNode *IRGenerator::generateIfElseStatement(parser::IfElseNode *ifElseStmt)
{
    irgenerator::IfElseNode *ifElseNode = new irgenerator::IfElseNode({}, {}, {});
    std::vector<irgenerator::ExpressionNode *> conditionExpressions;
    serializeExpression(ifElseStmt->condition, conditionExpressions);
    currentFunctionContext->currentStackOffset--;
    ifElseNode->condition = conditionExpressions;

    if (ifElseStmt->trueBlock->getNodeType() == parser::NodeType::BLOCK)
    {
        auto trueBlock = static_cast<parser::Block *>(ifElseStmt->trueBlock);
        for (const auto &stmt : trueBlock->statements)
        {
            ifElseNode->trueBlock.push_back(generateStatement(stmt));
        }
    }
    else
    {
        ifElseNode->trueBlock.push_back(generateStatement(ifElseStmt->trueBlock));
    }
    if (ifElseStmt->falseBlock)
    {
        if (ifElseStmt->falseBlock->getNodeType() == parser::NodeType::BLOCK)
        {
            auto falseBlock = static_cast<parser::Block *>(ifElseStmt->falseBlock);
            for (const auto &stmt : falseBlock->statements)
            {
                ifElseNode->falseBlock.push_back(generateStatement(stmt));
            }
        }
        else
        {
            ifElseNode->falseBlock.push_back(generateStatement(ifElseStmt->falseBlock));
        }
    }
    return ifElseNode;
}

irgenerator::StatementNode *IRGenerator::generateWhileLoop(parser::WhileLoopNode *whileLoop)
{

    irgenerator::WhileLoopNode *whileNode = new irgenerator::WhileLoopNode({}, {});
    irgenerator::CodeBlockContext *codeBlockContext = new irgenerator::CodeBlockContext(irgenerator::IRCodeBlockType::WHILE, whileNode->whileid);
    std::vector<irgenerator::ExpressionNode *> conditionExpressions;
    serializeExpression(whileLoop->condition, conditionExpressions);
    currentFunctionContext->currentStackOffset--; // The condition expression will push a value onto the stack that would've been popped of by now
    whileNode->condition = conditionExpressions;

    if (whileLoop->body->getNodeType() == parser::NodeType::BLOCK)
    {
        auto bodyBlock = static_cast<parser::Block *>(whileLoop->body);
        for (const auto &stmt : bodyBlock->statements)
        {
            whileNode->body.push_back(generateStatement(stmt, codeBlockContext));
        }
    }
    else
    {
        whileNode->body.push_back(generateStatement(whileLoop->body, codeBlockContext));
    }
    return whileNode;
}

irgenerator::StatementNode *IRGenerator::generateDoWhileLoop(parser::DoWhileLoopNode *doWhileLoop)
{
    irgenerator::DoWhileLoopNode *doWhileNode = new irgenerator::DoWhileLoopNode({}, {});
    irgenerator::CodeBlockContext *codeBlockContext = new irgenerator::CodeBlockContext(irgenerator::IRCodeBlockType::DO_WHILE, doWhileNode->doWhileid);
    std::vector<irgenerator::ExpressionNode *> conditionExpressions;
    serializeExpression(doWhileLoop->condition, conditionExpressions);
    currentFunctionContext->currentStackOffset--;
    doWhileNode->condition = conditionExpressions;

    if (doWhileLoop->body->getNodeType() == parser::NodeType::BLOCK)
    {
        auto bodyBlock = static_cast<parser::Block *>(doWhileLoop->body);
        for (const auto &stmt : bodyBlock->statements)
        {
            doWhileNode->body.push_back(generateStatement(stmt, codeBlockContext));
        }
    }
    else
    {
        doWhileNode->body.push_back(generateStatement(doWhileLoop->body, codeBlockContext));
    }
    return doWhileNode;
}

irgenerator::StatementNode *IRGenerator::generateForLoop(parser::ForLoopNode *forLoop)
{
    irgenerator::ForLoopNode *forNode = new irgenerator::ForLoopNode(nullptr, nullptr, {}, {});
    irgenerator::CodeBlockContext *codeBlockContext = new irgenerator::CodeBlockContext(irgenerator::IRCodeBlockType::FOR, forNode->forid);
    if (forLoop->initExp)
    {
        forNode->init = generateStatement(forLoop->initExp, codeBlockContext);
    }
    if (forLoop->condition)
    {
        std::vector<irgenerator::ExpressionNode *> conditionExpressions;
        serializeExpression(forLoop->condition, conditionExpressions);
        currentFunctionContext->currentStackOffset--;
        forNode->condition = conditionExpressions;
    }
    else
    {
        // If no condition is provided, we assume the loop runs indefinitely
        forNode->condition.push_back(new irgenerator::IntegerNode(1));
    }
    if (forLoop->updateExp)
    {
        std::vector<irgenerator::ExpressionNode *> updateExpressions;
        serializeExpression(forLoop->updateExp, updateExpressions);
        currentFunctionContext->currentStackOffset--; // The update expression will push a value onto the stack that we won't use
        forNode->update = new irgenerator::ExpressionStatementNode(updateExpressions);
    }
    if (forLoop->body->getNodeType() == parser::NodeType::BLOCK)
    {
        auto bodyBlock = static_cast<parser::Block *>(forLoop->body);
        for (const auto &stmt : bodyBlock->statements)
        {
            forNode->body.push_back(generateStatement(stmt, codeBlockContext));
        }
    }
    else
    {
        forNode->body.push_back(generateStatement(forLoop->body, codeBlockContext));
    }
    return forNode;
}

irgenerator::StatementNode *IRGenerator::generateSwitchStatement(parser::SwitchNode *switchStmt)
{
    irgenerator::CaseNode::caseCounter = 0;
    irgenerator::SwitchNode *switchNode = new irgenerator::SwitchNode({}, {}, {});
    irgenerator::CodeBlockContext *codeBlockContext = new irgenerator::CodeBlockContext(irgenerator::IRCodeBlockType::SWITCH, switchNode->switchId);

    std::vector<irgenerator::ExpressionNode *> matchValueExpressions;
    serializeExpression(switchStmt->matchValue, matchValueExpressions);
    currentFunctionContext->currentStackOffset--;
    switchNode->matchValue = matchValueExpressions;

    bool hasDefaultCase = false;

    for (const auto &caseStmt : switchStmt->body->statements)
    {
        if (caseStmt->getNodeType() == parser::NodeType::CASE_STATEMENT)
        {
            switchNode->caseValues.push_back(static_cast<parser::IntegerNode *>(static_cast<parser::CaseNode *>(caseStmt)->caseValue)->value);
        }
        else if (caseStmt->getNodeType() == parser::NodeType::DEFAULT_CASE_STATEMENT)
        {
            hasDefaultCase = true;
        }
    }
    for (const auto &caseStmt : switchStmt->body->statements)
    {
        switchNode->body.push_back(generateStatement(caseStmt, codeBlockContext));
    }
    if (!hasDefaultCase)
    {
        switchNode->body.push_back(generateDefaultCaseStatement(codeBlockContext));
    }
    return switchNode;
}

irgenerator::StatementNode *IRGenerator::generateExpressionStatement(parser::ExpressionStatementNode *exprStmt)
{
    irgenerator::ExpressionStatementNode *stmt = new irgenerator::ExpressionStatementNode({});
    std::vector<irgenerator::ExpressionNode *> expressions;
    serializeExpression(exprStmt->expr, expressions);
    currentFunctionContext->currentStackOffset--; // The return value pushed by the last expression will not be used so we'll decrement the stack by one
    return new irgenerator::ExpressionStatementNode(expressions);
}

void IRGenerator::serializeExpression(parser::ExpressionNode *expr, std::vector<irgenerator::ExpressionNode *> &expressions)
{
    if (expr == nullptr)
        return;
    if (expr->getNodeType() == parser::NodeType::BINARY_OPERATION)
    {
        parser::BinaryOperationNode *binaryOperation = static_cast<parser::BinaryOperationNode *>(expr);
        if (binaryOperation->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN))
        {
            if (binaryOperation->leftOperand->getNodeType() == parser::NodeType::UNARY_OPERATION)
            {
                // Semantic checker ensures that the left operand is a dereference operation in case of assignment to a unary operation
                parser::UnaryOperationNode *unaryOperation = static_cast<parser::UnaryOperationNode *>(binaryOperation->leftOperand);
                serializeExpression(unaryOperation->operand, expressions);
            }
            if (binaryOperation->leftOperand->getNodeType() == parser::NodeType::VARIABLE)
            {
                // Semantic checker ensures that the left operand is a variable in case of assignment
                parser::VariableNode *variableNode = static_cast<parser::VariableNode *>(binaryOperation->leftOperand);
                expressions.push_back(generateVariableAddressNode(variableNode));
                currentFunctionContext->currentStackOffset++;
            }
        }
        else
            serializeExpression(binaryOperation->leftOperand, expressions);
        serializeExpression(binaryOperation->rightOperand, expressions);
        expressions.push_back(generateBinaryExpression(binaryOperation));
        currentFunctionContext->currentStackOffset--;
        return;
    }
    else if (expr->getNodeType() == parser::NodeType::UNARY_OPERATION)
    {
        parser::UnaryOperationNode *unaryOperation = static_cast<parser::UnaryOperationNode *>(expr);
        if (unaryOperation->operatorSymbol->value == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN))
        {
            // Semantic checker ensures that the operand is a variable in case of address-of operation
            parser::VariableNode *variableNode = static_cast<parser::VariableNode *>(unaryOperation->operand);
            expressions.push_back(generateVariableAddressNode(variableNode));
            currentFunctionContext->currentStackOffset++;
        }
        else
        {
            serializeExpression(unaryOperation->operand, expressions);
            expressions.push_back(generateUnaryExpression(unaryOperation));
        }
        return;
    }
    expressions.push_back(generatePrimaryExpression(expr));
    currentFunctionContext->currentStackOffset++;
}

irgenerator::ExpressionNode *IRGenerator::generatePrimaryExpression(parser::ExpressionNode *expr)
{
    switch (expr->getNodeType())
    {
    case parser::NodeType::INTEGER:
        return generateIntegerNode(static_cast<parser::IntegerNode *>(expr));
    case parser::NodeType::STRING:
        return generateStringNode(static_cast<parser::StringNode *>(expr));
    case parser::NodeType::VARIABLE:
        return generateVariableNode(static_cast<parser::VariableNode *>(expr));
    case parser::NodeType::FUNCTION_CALL:
        return generateFunctionCall(static_cast<parser::FunctionCallNode *>(expr));
    default:
        throw std::runtime_error("Unsupported expression type");
    }
}

irgenerator::ExpressionNode *IRGenerator::generateBinaryExpression(parser::BinaryOperationNode *binExpr)
{
    std::string op = binExpr->operatorSymbol->value;
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN))
    {
        return new irgenerator::AssignmentNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN))
    {
        return new irgenerator::ArithmeticAdditionNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN))
    {
        return new irgenerator::ArithmeticSubtractionNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::ASTERISK_SIGN))
    {
        return new irgenerator::ArithmeticMultiplicationNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::FORWARD_SLASH_SIGN))
    {
        return new irgenerator::ArithmeticDivisionNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::PERCENT_SIGN))
    {
        return new irgenerator::ArithmeticModuloNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN))
    {
        return new irgenerator::BitwiseAndNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::CARET_SIGN))
    {
        return new irgenerator::BitwiseXorNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::VERTICAL_BAR_SIGN))
    {
        return new irgenerator::BitwiseOrNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_LESS_THAN_SIGN))
    {
        return new irgenerator::BitwiseLeftShiftNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_GREATER_THAN_SIGN))
    {
        return new irgenerator::BitwiseRightShiftNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN))
    {
        return new irgenerator::RelationalLessThanNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_EQUALS_SIGN))
    {
        return new irgenerator::RelationalLessThanEqualsNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN))
    {
        return new irgenerator::RelationalGreaterThanNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_EQUALS_SIGN))
    {
        return new irgenerator::RelationalGreaterThanEqualsNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_EQUALS_SIGN))
    {
        return new irgenerator::RelationalEqualsNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::NOT_EQUALS_SIGN))
    {
        return new irgenerator::RelationalNotEqualsNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_AMPERSAND_SIGN))
    {
        return new irgenerator::LogicalAndNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_VERTICAL_BAR_SIGN))
    {
        return new irgenerator::LogicalOrNode();
    }
    throw std::runtime_error("Unsupported binary operation type: " + op);
}

irgenerator::ExpressionNode *IRGenerator::generateUnaryExpression(parser::UnaryOperationNode *unaryExpr)
{
    std::string op = unaryExpr->operatorSymbol->value;
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN))
    {
        return new irgenerator::UnaryPlusNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN))
    {
        return new irgenerator::UnaryNegationNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::ASTERISK_SIGN))
    {
        return new irgenerator::UnaryDereferenceNode();
    }
    if (op == grammar::SymbolAsString.at(grammar::SYMBOL::EXCLAMATION_MARK))
    {
        return new irgenerator::UnaryLogicalNotNode();
    }
    throw std::runtime_error("Unsupported unary operation type: " + op);
}

irgenerator::ExpressionNode *IRGenerator::generateIntegerNode(parser::IntegerNode *intNode)
{
    return new irgenerator::IntegerNode(intNode->value);
}

irgenerator::ExpressionNode *IRGenerator::generateStringNode(parser::StringNode *strNode)
{
    int address = programContext->nextFreeGlobalAddress;
    programContext->nextFreeGlobalAddress += strNode->value.size() + 1; // +1 for null terminator
    return new irgenerator::StringNode(strNode->value, address);
}

irgenerator::ExpressionNode *IRGenerator::generateVariableNode(parser::VariableNode *varNode)
{
    if (currentFunctionContext->localVariables.find(varNode->name->value) != currentFunctionContext->localVariables.end())
    {
        irgenerator::Variable *declVar = currentFunctionContext->localVariables[varNode->name->value];
        int offset = currentFunctionContext->currentStackOffset - declVar->offset - 1; // -1 Due to architechture
        if (varNode->dimensions.size() == declVar->dimensions.size())
        {

            std::vector<irgenerator::ExpressionNode *> dimensionsExprs;
            int dimOffsetAccumulator = 1;
            for (int i = varNode->dimensions.size() - 1; i >= 0; i--)
            {
                std::vector<irgenerator::ExpressionNode *> singleDimensionExprs;
                serializeExpression(varNode->dimensions[i], singleDimensionExprs);
                dimensionsExprs.insert(dimensionsExprs.end(), singleDimensionExprs.begin(), singleDimensionExprs.end());
                if (i != varNode->dimensions.size() - 1)
                {
                    dimensionsExprs.push_back(new irgenerator::IntegerNode(dimOffsetAccumulator));
                    dimensionsExprs.push_back(new irgenerator::ArithmeticMultiplicationNode());
                    dimensionsExprs.push_back(new irgenerator::ArithmeticAdditionNode());
                    currentFunctionContext->currentStackOffset--; // Integer node will add 1 and then both binary operations will subtract 1-1 each
                }
                dimOffsetAccumulator *= declVar->dimensions[i];
            }
            if (varNode->dimensions.size() > 0)
                currentFunctionContext->currentStackOffset--; // The calculated dimentions will have been used so it would've been pushed off the stack
            return new irgenerator::LocalVariableNode(offset, dimensionsExprs);
        }
        else
        {
            return generateVariableAddressNode(varNode);
        }
    }
    else
    {
        int address = programContext->globalVariables[varNode->name->value]->address;
        if (varNode->dimensions.size() == programContext->globalVariables[varNode->name->value]->dimensions.size())
        {
            std::vector<irgenerator::ExpressionNode *> dimensionsExprs;
            int dimOffsetAccumulator = 1;
            for (int i = varNode->dimensions.size() - 1; i >= 0; i--)
            {
                std::vector<irgenerator::ExpressionNode *> singleDimensionExprs;
                serializeExpression(varNode->dimensions[i], singleDimensionExprs);
                dimensionsExprs.insert(dimensionsExprs.end(), singleDimensionExprs.begin(), singleDimensionExprs.end());
                if (i != varNode->dimensions.size() - 1)
                {
                    dimensionsExprs.push_back(new irgenerator::IntegerNode(dimOffsetAccumulator));
                    dimensionsExprs.push_back(new irgenerator::ArithmeticMultiplicationNode());
                    dimensionsExprs.push_back(new irgenerator::ArithmeticAdditionNode());
                    currentFunctionContext->currentStackOffset--;
                }
                dimOffsetAccumulator *= programContext->globalVariables[varNode->name->value]->dimensions[i];
            }
            if (varNode->dimensions.size() > 0)
                currentFunctionContext->currentStackOffset--;
            return new irgenerator::GlobalVariableNode(address, dimensionsExprs);
        }
        else
        {
            return generateVariableAddressNode(varNode);
        }
    }
}

irgenerator::ExpressionNode *IRGenerator::generateVariableAddressNode(parser::VariableNode *varNode)
{
    if (currentFunctionContext->localVariables.find(varNode->name->value) != currentFunctionContext->localVariables.end())
    {
        irgenerator::Variable *declVar = currentFunctionContext->localVariables[varNode->name->value];
        int offset = currentFunctionContext->currentStackOffset - declVar->offset - 1; // -1 Due to architecture
        std::vector<irgenerator::ExpressionNode *> dimensionsExprs;
        int dimOffsetAccumulator = 1;
        for (int i = declVar->dimensions.size() - 1; i >= 0; i--)
        {
            if (i < varNode->dimensions.size())
            {
                std::vector<irgenerator::ExpressionNode *> singleDimensionExprs;
                serializeExpression(varNode->dimensions[i], singleDimensionExprs);
                dimensionsExprs.insert(dimensionsExprs.end(), singleDimensionExprs.begin(), singleDimensionExprs.end());
                if (i != varNode->dimensions.size() - 1)
                {
                    dimensionsExprs.push_back(new irgenerator::IntegerNode(dimOffsetAccumulator));
                    dimensionsExprs.push_back(new irgenerator::ArithmeticMultiplicationNode());
                    if (i != varNode->dimensions.size() - 1)
                    {
                        dimensionsExprs.push_back(new irgenerator::ArithmeticAdditionNode());
                        currentFunctionContext->currentStackOffset--;
                    }
                }
            }
            dimOffsetAccumulator *= declVar->dimensions[i];
        }
        if (varNode->dimensions.size() > 0)
            currentFunctionContext->currentStackOffset--;
        return new irgenerator::LocalVariableAddressNode(offset, dimensionsExprs);
    }
    else
    {
        irgenerator::Variable *declVar = programContext->globalVariables[varNode->name->value];
        int address = declVar->address;
        std::vector<irgenerator::ExpressionNode *> dimensionsExprs;
        int dimOffsetAccumulator = 1;
        for (int i = declVar->dimensions.size() - 1; i >= 0; i--)
        {
            if (i < varNode->dimensions.size())
            {
                std::vector<irgenerator::ExpressionNode *> singleDimensionExprs;
                serializeExpression(varNode->dimensions[i], singleDimensionExprs);
                dimensionsExprs.insert(dimensionsExprs.end(), singleDimensionExprs.begin(), singleDimensionExprs.end());
                if (i != varNode->dimensions.size() - 1)
                {
                    dimensionsExprs.push_back(new irgenerator::IntegerNode(dimOffsetAccumulator));
                    dimensionsExprs.push_back(new irgenerator::ArithmeticMultiplicationNode());
                    if (i != varNode->dimensions.size() - 1)
                    {
                        dimensionsExprs.push_back(new irgenerator::ArithmeticAdditionNode());
                        currentFunctionContext->currentStackOffset--; // The calculated dimension will have been used so it would've been pushed off the stack
                    }
                }
            }
            dimOffsetAccumulator *= declVar->dimensions[i];
        }
        if (varNode->dimensions.size() > 0)
            currentFunctionContext->currentStackOffset--;
        return new irgenerator::GlobalVariableAddressNode(address, dimensionsExprs);
    }
}

irgenerator::ExpressionNode *IRGenerator::generateFunctionCall(parser::FunctionCallNode *funcCall)
{
    irgenerator::FunctionCallNode *callNode = new irgenerator::FunctionCallNode(0, {}, {});
    callNode->functionId = programContext->functionIds[funcCall->functionName->value];
    int saveStackOffset = currentFunctionContext->currentStackOffset;
    currentFunctionContext->currentStackOffset++; // Reserve space for the return value
    int argCount = 0;
    for (const auto &arg : funcCall->arguments)
    {
        std::vector<irgenerator::ExpressionNode *> argExpressions;
        serializeExpression(arg, argExpressions);
        callNode->arguments.push_back(argExpressions);
        if (arg->getNodeType() == parser::NodeType::STRING)
        {
            callNode->uninitializedCount.push_back(programContext->functionArgumentSizes[funcCall->functionName->value][argCount] - 1 - static_cast<parser::StringNode *>(arg)->value.size());
        }
        else
            callNode->uninitializedCount.push_back(programContext->functionArgumentSizes[funcCall->functionName->value][argCount] - 1);
        argCount++;
    }
    currentFunctionContext->currentStackOffset = saveStackOffset;

    return callNode;
}

int IRGenerator::calculateArraySize(const std::vector<int> &dimensions)
{
    int size = 1;
    for (int dim : dimensions)
    {
        size *= dim;
    }
    return size;
}

void IRFileWriter::write()
{
    for (const auto &node : ir->nodes)
    {
        switch (node->getNodeType())
        {
        case irgenerator::IRNodeType::GLOBAL_VARIABLE_DECLARATION:
            writeGlobalVariableDeclarationNode(static_cast<irgenerator::GlobalVariableDeclarationNode *>(node));
            break;
        case irgenerator::IRNodeType::FUNCTION_DEFINITION:
            writeFunctionDefinitionNode(static_cast<irgenerator::FunctionDefinitionNode *>(node));
            break;
        }
    }
    writeBuiltInFunctions();
}

void IRFileWriter::writeGlobalVariableDeclarationNode(irgenerator::GlobalVariableDeclarationNode *node)
{
    int currentAddress = node->baseAddress;
    for (auto const &val : node->initializationValues)
    {
        irFile << "DATA-" << currentAddress << " " << val << std::endl;
        currentAddress++;
    }
}

void IRFileWriter::writeFunctionDefinitionNode(irgenerator::FunctionDefinitionNode *node)
{
    if (node->functionId == 0)
    {
        irFile << "LDA " << 5 << std::endl;
        irFile << "SSA" << std::endl;
        irFile << "JMP FUNCTION_0" << std::endl;
        irFile << "HLT" << std::endl;
    }
    irFile << "FUNCTION_" << node->functionId << ":" << std::endl;
    for (auto const &stmt : node->body)
    {
        writeStatementNode(stmt);
    }
}

void IRFileWriter::writeLocalVariableDeclarationNode(irgenerator::LocalVariableDeclarationNode *node)
{
    int currentOffset = node->baseOffset;
    if (node->uninitializedCount > 0)
    {
        irFile << "LDD " << node->uninitializedCount << std::endl;
        irFile << "SUD" << std::endl;
    }
    for (auto val = node->initializationValues.rbegin(); val != node->initializationValues.rend(); ++val)
    {
        for (auto expr : *val)
        {
            writeExpressionNode(expr);
        }
    }
    
}

void IRFileWriter::writeStatementNode(irgenerator::StatementNode *node)
{
    if (!node)
        return;
    switch (node->getNodeType())
    {
    case irgenerator::IRNodeType::LOCAL_VARIABLE_DECLARATION:
        writeLocalVariableDeclarationNode(static_cast<irgenerator::LocalVariableDeclarationNode *>(node));
        break;
    case irgenerator::IRNodeType::RETURN_STATEMENT:
        writeReturnStatementNode(static_cast<irgenerator::ReturnStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::IFELSE_STATEMENT:
        writeIfElseNode(static_cast<irgenerator::IfElseNode *>(node));
        break;
    case irgenerator::IRNodeType::WHILE_LOOP:
        writeWhileLoopNode(static_cast<irgenerator::WhileLoopNode *>(node));
        break;
    case irgenerator::IRNodeType::DO_WHILE_LOOP:
        writeDoWhileLoopNode(static_cast<irgenerator::DoWhileLoopNode *>(node));
        break;
    case irgenerator::IRNodeType::FOR_LOOP:
        writeForLoopNode(static_cast<irgenerator::ForLoopNode *>(node));
        break;
    case irgenerator::IRNodeType::SWITCH_STATEMENT:
        writeSwitchNode(static_cast<irgenerator::SwitchNode *>(node));
        break;
    case irgenerator::IRNodeType::CASE_STATEMENT:
        writeCaseNode(static_cast<irgenerator::CaseNode *>(node));
        break;
    case irgenerator::IRNodeType::CASE_DEFAULT_STATEMENT:
        writeDefaultCaseNode(static_cast<irgenerator::DefaultCaseNode *>(node));
        break;
    case irgenerator::IRNodeType::EXPRESSION_STATEMENT:
        writeExpressionStatementNode(static_cast<irgenerator::ExpressionStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::FOR_LOOP_BREAK_STATEMENT:
        writeForLoopBreakStatementNode(static_cast<irgenerator::ForLoopBreakStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::WHILE_LOOP_BREAK_STATEMENT:
        writeWhileLoopBreakStatementNode(static_cast<irgenerator::WhileLoopBreakStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::DO_WHILE_LOOP_BREAK_STATEMENT:
        writeDoWhileLoopBreakStatementNode(static_cast<irgenerator::DoWhileLoopBreakStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::SWITCH_BREAK_STATEMENT:
        writeSwitchBreakStatementNode(static_cast<irgenerator::SwitchBreakStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::FOR_LOOP_CONTINUE_STATEMENT:
        writeForLoopContinueStatementNode(static_cast<irgenerator::ForLoopContinueStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::WHILE_LOOP_CONTINUE_STATEMENT:
        writeWhileLoopContinueStatementNode(static_cast<irgenerator::WhileLoopContinueStatementNode *>(node));
        break;
    case irgenerator::IRNodeType::DO_WHILE_LOOP_CONTINUE_STATEMENT:
        writeDoWhileLoopContinueStatementNode(static_cast<irgenerator::DoWhileLoopContinueStatementNode *>(node));
        break;
    default:
        throw std::runtime_error("Unsupported statement node type");
    }
}

void IRFileWriter::writeExpressionNode(irgenerator::ExpressionNode *node)
{
    switch (node->getNodeType())
    {
    case irgenerator::IRNodeType::INTEGER:
        writeIntegerNode(static_cast<irgenerator::IntegerNode *>(node));
        break;
    case irgenerator::IRNodeType::STRING:
        writeStringNode(static_cast<irgenerator::StringNode *>(node));
        break;
    case irgenerator::IRNodeType::LOCAL_VARIABLE:
        writeLocalVariableNode(static_cast<irgenerator::LocalVariableNode *>(node));
        break;
    case irgenerator::IRNodeType::GLOBAL_VARIABLE:
        writeGlobalVariableNode(static_cast<irgenerator::GlobalVariableNode *>(node));
        break;
    case irgenerator::IRNodeType::LOCAL_VARIABLE_ADDRESS:
        writeLocalVariableAddressNode(static_cast<irgenerator::LocalVariableAddressNode *>(node));
        break;
    case irgenerator::IRNodeType::GLOBAL_VARIABLE_ADDRESS:
        writeGlobalVariableAddressNode(static_cast<irgenerator::GlobalVariableAddressNode *>(node));
        break;
    case irgenerator::IRNodeType::FUNCTION_CALL:
        writeFunctionCallNode(static_cast<irgenerator::FunctionCallNode *>(node));
        break;
    case irgenerator::IRNodeType::ASSIGNMENT:
        writeAssignmentNode(static_cast<irgenerator::AssignmentNode *>(node));
        break;
    case irgenerator::IRNodeType::ARITHMATIC_ADDTION_EXPRESSION:
        writeArithmeticAdditionNode(static_cast<irgenerator::ArithmeticAdditionNode *>(node));
        break;
    case irgenerator::IRNodeType::ARITHMATIC_SUBTRACTION_EXPRESSION:
        writeArithmeticSubtractionNode(static_cast<irgenerator::ArithmeticSubtractionNode *>(node));
        break;
    case irgenerator::IRNodeType::ARITHMATIC_MULTIPLICATION_EXPRESSION:
        writeArithmeticMultiplicationNode(static_cast<irgenerator::ArithmeticMultiplicationNode *>(node));
        break;
    case irgenerator::IRNodeType::ARITHMATIC_DIVISION_EXPRESSION:
        writeArithmeticDivisionNode(static_cast<irgenerator::ArithmeticDivisionNode *>(node));
        break;
    case irgenerator::IRNodeType::ARITHMATIC_MODULO_EXPRESSION:
        writeArithmeticModuloNode(static_cast<irgenerator::ArithmeticModuloNode *>(node));
        break;
    case irgenerator::IRNodeType::BITWISE_AND_EXPRESSION:
        writeBitwiseAndNode(static_cast<irgenerator::BitwiseAndNode *>(node));
        break;
    case irgenerator::IRNodeType::BITWISE_XOR_EXPRESSION:
        writeBitwiseXorNode(static_cast<irgenerator::BitwiseXorNode *>(node));
        break;
    case irgenerator::IRNodeType::BITWISE_OR_EXPRESSION:
        writeBitwiseOrNode(static_cast<irgenerator::BitwiseOrNode *>(node));
        break;
    case irgenerator::IRNodeType::BITWISE_LEFT_SHIFT_EXPRESSION:
        writeBitwiseLeftShiftNode(static_cast<irgenerator::BitwiseLeftShiftNode *>(node));
        break;
    case irgenerator::IRNodeType::BITWISE_RIGHT_SHIFT_EXPRESSION:
        writeBitwiseRightShiftNode(static_cast<irgenerator::BitwiseRightShiftNode *>(node));
        break;
    case irgenerator::IRNodeType::RELATIONAL_LESS_THAN_EXPRESSION:
        writeRelationalLessThanNode(static_cast<irgenerator::RelationalLessThanNode *>(node));
        break;
    case irgenerator::IRNodeType::RELATIONAL_LESS_THAN_EQUALS_EXPRESSION:
        writeRelationalLessThanEqualsNode(static_cast<irgenerator::RelationalLessThanEqualsNode *>(node));
        break;
    case irgenerator::IRNodeType::RELATIONAL_GREATER_THAN_EXPRESSION:
        writeRelationalGreaterThanNode(static_cast<irgenerator::RelationalGreaterThanNode *>(node));
        break;
    case irgenerator::IRNodeType::RELATIONAL_GREATER_THAN_EQUALS_EXPRESSION:
        writeRelationalGreaterThanEqualsNode(static_cast<irgenerator::RelationalGreaterThanEqualsNode *>(node));
        break;
    case irgenerator::IRNodeType::RELATIONAL_EQUALS_EXPRESSION:
        writeRelationalEqualsNode(static_cast<irgenerator::RelationalEqualsNode *>(node));
        break;
    case irgenerator::IRNodeType::RELATIONAL_NOT_EQUALS_EXPRESSION:
        writeRelationalNotEqualsNode(static_cast<irgenerator::RelationalNotEqualsNode *>(node));
        break;
    case irgenerator::IRNodeType::LOGICAL_AND_EXPRESSION:
        writeLogicalAndNode(static_cast<irgenerator::LogicalAndNode *>(node));
        break;
    case irgenerator::IRNodeType::LOGICAL_OR_EXPRESSION:
        writeLogicalOrNode(static_cast<irgenerator::LogicalOrNode *>(node));
        break;
    case irgenerator::IRNodeType::UNARY_PLUS_EXPRESSION:
        writeUnaryPlusNode(static_cast<irgenerator::UnaryPlusNode *>(node));
        break;
    case irgenerator::IRNodeType::UNARY_NEGATION_EXPRESSION:
        writeUnaryNegationNode(static_cast<irgenerator::UnaryNegationNode *>(node));
        break;
    case irgenerator::IRNodeType::UNARY_DEREFERENCE_EXPRESSION:
        writeUnaryDereferenceNode(static_cast<irgenerator::UnaryDereferenceNode *>(node));
        break;
    case irgenerator::IRNodeType::UNARY_LOGICAL_NOT_EXPRESSION:
        writeUnaryLogicalNotNode(static_cast<irgenerator::UnaryLogicalNotNode *>(node));
        break;

    default:
        throw std::runtime_error("Unsupported expression node type");
    }
}

void IRFileWriter::writeReturnStatementNode(irgenerator::ReturnStatementNode *node)
{
    for (auto const &expr : node->returnValue)
    {
        writeExpressionNode(expr);
    }
    irFile << "CSP" << std::endl;
    irFile << "LDB " << node->basePointerOffsetBefore << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "SCA" << std::endl;
    irFile << "CSP" << std::endl;
    irFile << "LDB " << node->returnAddressOffset << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "RCA" << std::endl;
    irFile << "LDD " << node->basePointerOffsetAfter << std::endl;
    irFile << "SDD" << std::endl;
    irFile << "JPA" << std::endl;
}

void IRFileWriter::writeIfElseNode(irgenerator::IfElseNode *node)
{
    for (auto const &expr : node->condition)
    {
        writeExpressionNode(expr);
    }
    irFile << "LSA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE IF_" << node->ifid << "_FALSE" << std::endl;
    for (auto const &expr : node->trueBlock)
    {
        writeStatementNode(expr);
    }
    irFile << "JMP IF_" << node->ifid << "_END" << std::endl;
    irFile << "IF_" << node->ifid << "_FALSE:" << std::endl;
    for (auto const &expr : node->falseBlock)
    {
        writeStatementNode(expr);
    }
    irFile << "IF_" << node->ifid << "_END:" << std::endl;
}

void IRFileWriter::writeWhileLoopNode(irgenerator::WhileLoopNode *node)
{
    irFile << "WHILE_" << node->whileid << "_START:" << std::endl;
    for (auto const &expr : node->condition)
    {
        writeExpressionNode(expr);
    }
    irFile << "LSA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE WHILE_" << node->whileid << "_END" << std::endl;
    for (auto const &stmt : node->body)
    {
        writeStatementNode(stmt);
    }
    irFile << "JMP WHILE_" << node->whileid << "_START" << std::endl;
    irFile << "WHILE_" << node->whileid << "_END:" << std::endl;
}

void IRFileWriter::writeDoWhileLoopNode(irgenerator::DoWhileLoopNode *node)
{
    irFile << "DO_WHILE_" << node->doWhileid << "_START:" << std::endl;
    for (auto const &stmt : node->body)
    {
        writeStatementNode(stmt);
    }
    irFile << "DO_WHILE_" << node->doWhileid << "_CONDITION:" << std::endl;
    for (auto const &expr : node->condition)
    {
        writeExpressionNode(expr);
    }
    irFile << "LSA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE DO_WHILE_" << node->doWhileid << "_END" << std::endl;
    irFile << "JMP DO_WHILE_" << node->doWhileid << "_START" << std::endl;
    irFile << "DO_WHILE_" << node->doWhileid << "_END:" << std::endl;
}

void IRFileWriter::writeForLoopNode(irgenerator::ForLoopNode *node)
{
    if (node->init)
    {
        writeStatementNode(node->init);
    }
    irFile << "FOR_" << node->forid << "_CONDITION:" << std::endl;
    if (!node->condition.empty())
    {
        for (auto const &expr : node->condition)
        {
            writeExpressionNode(expr);
        }
        irFile << "LSA" << std::endl;
        irFile << "LDB 0" << std::endl;
        irFile << "CMP" << std::endl;
        irFile << "JPE FOR_" << node->forid << "_END" << std::endl;
    }
    for (auto const &stmt : node->body)
    {
        writeStatementNode(stmt);
    }
    irFile << "FOR_" << node->forid << "_UPDATE:" << std::endl;
    if (node->update)
    {
        writeStatementNode(node->update);
    }
    irFile << "JMP FOR_" << node->forid << "_CONDITION" << std::endl;
    irFile << "FOR_" << node->forid << "_END:" << std::endl;
}

void IRFileWriter::writeSwitchNode(irgenerator::SwitchNode *node)
{
    for (auto const &expr : node->matchValue)
    {
        writeExpressionNode(expr);
    }
    irFile << "LSA" << std::endl;
    int caseCount = 0;
    for (auto const &val : node->caseValues)
    {
        irFile << "LDB " << val << std::endl;
        irFile << "CMP" << std::endl;
        irFile << "JPE SWITCH_" << node->switchId << "_CASE_" << caseCount++ << std::endl;
    }
    irFile << "JMP SWITCH_" << node->switchId << "_DEFAULT" << std::endl;

    for (auto const &stmt : node->body)
    {
        writeStatementNode(stmt);
    }
    irFile << "SWITCH_" << node->switchId << "_END:" << std::endl;
}

void IRFileWriter::writeCaseNode(irgenerator::CaseNode *node)
{
    irFile << "SWITCH_" << node->switchId << "_CASE_" << node->caseid << ":" << std::endl;
}

void IRFileWriter::writeDefaultCaseNode(irgenerator::DefaultCaseNode *node)
{
    irFile << "SWITCH_" << node->switchId << "_DEFAULT:" << std::endl;
}

void IRFileWriter::writeSwitchBreakStatementNode(irgenerator::SwitchBreakStatementNode *node)
{
    irFile << "JMP SWITCH_" << node->switchId << "_END" << std::endl;
}

void IRFileWriter::writeForLoopBreakStatementNode(irgenerator::ForLoopBreakStatementNode *node)
{
    irFile << "JMP FOR_" << node->forLoopId << "_END" << std::endl;
}

void IRFileWriter::writeWhileLoopBreakStatementNode(irgenerator::WhileLoopBreakStatementNode *node)
{
    irFile << "JMP WHILE_" << node->whileLoopId << "_END" << std::endl;
}

void IRFileWriter::writeDoWhileLoopBreakStatementNode(irgenerator::DoWhileLoopBreakStatementNode *node)
{
    irFile << "JMP DO_WHILE_" << node->doWhileLoopId << "_END" << std::endl;
}

void IRFileWriter::writeForLoopContinueStatementNode(irgenerator::ForLoopContinueStatementNode *node)
{
    irFile << "JMP FOR_" << node->forLoopId << "_UPDATE" << std::endl;
}

void IRFileWriter::writeWhileLoopContinueStatementNode(irgenerator::WhileLoopContinueStatementNode *node)
{
    irFile << "JMP WHILE_" << node->whileLoopId << "_START" << std::endl;
}

void IRFileWriter::writeDoWhileLoopContinueStatementNode(irgenerator::DoWhileLoopContinueStatementNode *node)
{
    irFile << "JMP DO_WHILE_" << node->doWhileLoopId << "_CONDITION" << std::endl;
}

void IRFileWriter::writeExpressionStatementNode(irgenerator::ExpressionStatementNode *node)
{
    for (auto const &expr : node->expressions)
    {
        writeExpressionNode(expr);
    }
    irFile << "STD" << std::endl; // Pop the result of the expression from the stack as it won't be used
}

void IRFileWriter::writeIntegerNode(irgenerator::IntegerNode *node)
{
    irFile << "LDA " << node->value << std::endl;
    irFile << "SSA" << std::endl;
}

void IRFileWriter::writeStringNode(irgenerator::StringNode *node)
{
    int currentAddress = node->baseAddress;
    for (auto const &ch : node->value)
    {
        irFile << "DATA-" << currentAddress << " " << static_cast<int>(ch) << std::endl;
        currentAddress++;
    }
    irFile << "DATA-" << currentAddress << " 0" << std::endl; // Null terminator
    irFile << "LDA DATA-" << node->baseAddress << std::endl;
    irFile << "SSA" << std::endl;
}

void IRFileWriter::writeGlobalVariableNode(irgenerator::GlobalVariableNode *node)
{
    if (node->dimensions.empty())
    {
        irFile << "LDB DATA-" << node->address << std::endl;
        irFile << "RBA" << std::endl;
        irFile << "SSA" << std::endl;
    }
    else
    {
        for (auto const &expr : node->dimensions)
        {
            writeExpressionNode(expr);
        }
        irFile << "LSA" << std::endl;
        irFile << "LDB DATA-" << node->address << std::endl;
        irFile << "SUM" << std::endl;
        irFile << "RCA" << std::endl;
        irFile << "SSA" << std::endl;
    }
}

void IRFileWriter::writeLocalVariableNode(irgenerator::LocalVariableNode *node)
{
    if (node->dimensions.empty())
    {
        irFile << "CSP" << std::endl;
        irFile << "LDB " << node->offset << std::endl;
        irFile << "SUM" << std::endl;
        irFile << "RCA" << std::endl;
        irFile << "SSA" << std::endl;
    }
    else
    {
        irFile << "CSP" << std::endl;
        irFile << "LDB " << node->offset << std::endl;
        irFile << "SUM" << std::endl;
        irFile << "SSC" << std::endl;
        for (auto const &expr : node->dimensions)
        {
            writeExpressionNode(expr);
        }
        irFile << "LSB" << std::endl;
        irFile << "LSA" << std::endl;
        irFile << "SUM" << std::endl;
        irFile << "RCA" << std::endl;
        irFile << "SSA" << std::endl;
    }
}

void IRFileWriter::writeLocalVariableAddressNode(irgenerator::LocalVariableAddressNode *node)
{
    irFile << "CSP" << std::endl;
    irFile << "LDB " << node->offset << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "SSC" << std::endl;
    if (!node->dimensions.empty())
    {
        for (auto const &expr : node->dimensions)
        {
            writeExpressionNode(expr);
        }
        irFile << "LSB" << std::endl;
        irFile << "LSA" << std::endl;
        irFile << "SUM" << std::endl;
        irFile << "SSC" << std::endl;
    }
}

void IRFileWriter::writeGlobalVariableAddressNode(irgenerator::GlobalVariableAddressNode *node)
{
    irFile << "LDA " << node->address << std::endl;
    irFile << "SSA" << std::endl;
    if (!node->dimensions.empty())
    {
        for (auto const &expr : node->dimensions)
        {
            writeExpressionNode(expr);
        }
        irFile << "LSB" << std::endl;
        irFile << "LSA" << std::endl;
        irFile << "SUM" << std::endl;
        irFile << "SSC" << std::endl;
    }
}

void IRFileWriter::writeFunctionCallNode(irgenerator::FunctionCallNode *node)
{
    irFile << "STU" << std::endl; // Empty space for return value
    for (auto const &args : node->arguments)
    {
        for (auto const &expr : args)
        {
            writeExpressionNode(expr);
        }
    }
    irFile << "CPC" << std::endl;
    irFile << "LDB " << 6 << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "SSC" << std::endl;
    irFile << "JMP FUNCTION_" << node->functionId << std::endl;
}

void IRFileWriter::writeArithmeticAdditionNode(irgenerator::ArithmeticAdditionNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeArithmeticSubtractionNode(irgenerator::ArithmeticSubtractionNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "SUB" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeArithmeticMultiplicationNode(irgenerator::ArithmeticMultiplicationNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "MUL" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeArithmeticDivisionNode(irgenerator::ArithmeticDivisionNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "DIV" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeArithmeticModuloNode(irgenerator::ArithmeticModuloNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "MOD" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeBitwiseAndNode(irgenerator::BitwiseAndNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "AND" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeBitwiseXorNode(irgenerator::BitwiseXorNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "XOR" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeBitwiseOrNode(irgenerator::BitwiseOrNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "OR" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeBitwiseNorNode(irgenerator::BitwiseNorNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "NOR" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeBitwiseNandNode(irgenerator::BitwiseNandNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "NND" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeBitwiseLeftShiftNode(irgenerator::BitwiseLeftShiftNode *node)
{
    irFile << "LSA" << std::endl;
    irFile << "MDA" << std::endl;
    irFile << "LEFT_SHIFT_" << node->leftShiftId << "_START:" << std::endl;
    irFile << "MAD" << std::endl;
    irFile << "LDB 1" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPL LEFT_SHIFT_" << node->leftShiftId << "_END" << std::endl;
    irFile << "SUB" << std::endl;
    irFile << "MDC" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "LDB 2" << std::endl;
    irFile << "MUL" << std::endl;
    irFile << "SSC" << std::endl;
    irFile << "JMP LEFT_SHIFT_" << node->leftShiftId << "_START" << std::endl;
    irFile << "LEFT_SHIFT_" << node->leftShiftId << "_END:" << std::endl;
}

void IRFileWriter::writeBitwiseRightShiftNode(irgenerator::BitwiseRightShiftNode *node)
{
    irFile << "LSA" << std::endl;
    irFile << "MDA" << std::endl;
    irFile << "RIGHT_SHIFT_" << node->rightShiftId << "_START:" << std::endl;
    irFile << "MAD" << std::endl;
    irFile << "LDB 1" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPL RIGHT_SHIFT_" << node->rightShiftId << "_END" << std::endl;
    irFile << "SUB" << std::endl;
    irFile << "MDC" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "LDB 2" << std::endl;
    irFile << "DIV" << std::endl;
    irFile << "SSC" << std::endl;
    irFile << "JMP RIGHT_SHIFT_" << node->rightShiftId << "_START" << std::endl;
    irFile << "RIGHT_SHIFT_" << node->rightShiftId << "_END:" << std::endl;
}

void IRFileWriter::writeRelationalLessThanNode(irgenerator::RelationalLessThanNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPL RELATIONAL_LESS_THAN_" << node->lessThanId << "_TRUE" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP RELATIONAL_LESS_THAN_" << node->lessThanId << "_END" << std::endl;
    irFile << "RELATIONAL_LESS_THAN_" << node->lessThanId << "_TRUE:" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "RELATIONAL_LESS_THAN_" << node->lessThanId << "_END:" << std::endl;
}

void IRFileWriter::writeRelationalLessThanEqualsNode(irgenerator::RelationalLessThanEqualsNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPG RELATIONAL_LESS_THAN_EQUALS_" << node->lessThanEqualsId << "_FALSE" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP RELATIONAL_LESS_THAN_EQUALS_" << node->lessThanEqualsId << "_END" << std::endl;
    irFile << "RELATIONAL_LESS_THAN_EQUALS_" << node->lessThanEqualsId << "_FALSE:" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "RELATIONAL_LESS_THAN_EQUALS_" << node->lessThanEqualsId << "_END:" << std::endl;
}

void IRFileWriter::writeRelationalGreaterThanNode(irgenerator::RelationalGreaterThanNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPG RELATIONAL_GREATER_THAN_" << node->greaterThanId << "_TRUE" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP RELATIONAL_GREATER_THAN_" << node->greaterThanId << "_END" << std::endl;
    irFile << "RELATIONAL_GREATER_THAN_" << node->greaterThanId << "_TRUE:" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "RELATIONAL_GREATER_THAN_" << node->greaterThanId << "_END:" << std::endl;
}

void IRFileWriter::writeRelationalGreaterThanEqualsNode(irgenerator::RelationalGreaterThanEqualsNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPL RELATIONAL_GREATER_THAN_EQUALS_" << node->greaterThanEqualsId << "_FALSE" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP RELATIONAL_GREATER_THAN_EQUALS_" << node->greaterThanEqualsId << "_END" << std::endl;
    irFile << "RELATIONAL_GREATER_THAN_EQUALS_" << node->greaterThanEqualsId << "_FALSE:" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "RELATIONAL_GREATER_THAN_EQUALS_" << node->greaterThanEqualsId << "_END:" << std::endl;
}

void IRFileWriter::writeRelationalEqualsNode(irgenerator::RelationalEqualsNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE RELATIONAL_EQUALS_" << node->equalsId << "_TRUE" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP RELATIONAL_EQUALS_" << node->equalsId << "_END" << std::endl;
    irFile << "RELATIONAL_EQUALS_" << node->equalsId << "_TRUE:" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "RELATIONAL_EQUALS_" << node->equalsId << "_END:" << std::endl;
}

void IRFileWriter::writeRelationalNotEqualsNode(irgenerator::RelationalNotEqualsNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE RELATIONAL_NOT_EQUALS_" << node->notEqualsId << "_FALSE" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP RELATIONAL_NOT_EQUALS_" << node->notEqualsId << "_END" << std::endl;
    irFile << "RELATIONAL_NOT_EQUALS_" << node->notEqualsId << "_FALSE:" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "RELATIONAL_NOT_EQUALS_" << node->notEqualsId << "_END:" << std::endl;
}

void IRFileWriter::writeLogicalAndNode(irgenerator::LogicalAndNode *node)
{
    irFile << "LSA" << std::endl;
    irFile << "LSD" << std::endl;
    irFile << "LSB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE LOGICAL_AND_" << node->logicalAndId << "_FALSE" << std::endl;
    irFile << "MAD" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE LOGICAL_AND_" << node->logicalAndId << "_FALSE" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP LOGICAL_AND_" << node->logicalAndId << "_END" << std::endl;
    irFile << "LOGICAL_AND_" << node->logicalAndId << "_FALSE:" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "LOGICAL_AND_" << node->logicalAndId << "_END:" << std::endl;
}

void IRFileWriter::writeLogicalOrNode(irgenerator::LogicalOrNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "ORR" << std::endl;
    irFile << "MAC" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE LOGICAL_OR_" << node->logicalOrId << "_FALSE" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP LOGICAL_OR_" << node->logicalOrId << "_END" << std::endl;
    irFile << "LOGICAL_OR_" << node->logicalOrId << "_FALSE:" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "LOGICAL_OR_" << node->logicalOrId << "_END:" << std::endl;
}

void IRFileWriter::writeUnaryPlusNode(irgenerator::UnaryPlusNode *node)
{
    return;
}

void IRFileWriter::writeUnaryNegationNode(irgenerator::UnaryNegationNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SUB" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeUnaryDereferenceNode(irgenerator::UnaryDereferenceNode *node)
{
    irFile << "LSB" << std::endl;
    irFile << "RBA" << std::endl;
    irFile << "SSA" << std::endl;
}

void IRFileWriter::writeUnaryBitwiseNotNode(irgenerator::UnaryBitwiseNotNode *node)
{
    irFile << "LSA" << std::endl;
    irFile << "NOT" << std::endl;
    irFile << "SSC" << std::endl;
}

void IRFileWriter::writeUnaryLogicalNotNode(irgenerator::UnaryLogicalNotNode *node)
{
    irFile << "LSA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE NOT_" << node->logicalNotId << "_SET" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "JMP NOT_" << node->logicalNotId << "_END" << std::endl;
    irFile << "NOT_" << node->logicalNotId << "_SET:" << std::endl;
    irFile << "LDA 1" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "NOT_" << node->logicalNotId << "_END:" << std::endl;
}

void IRFileWriter::writeAssignmentNode(irgenerator::AssignmentNode *node)
{
    irFile << "LSA" << std::endl;
    irFile << "LSB" << std::endl;
    irFile << "SBA" << std::endl;
    irFile << "SSA" << std::endl;
}

void IRFileWriter::writeBuiltInFunctions()
{
    irFile << "FUNCTION_32767:" << std::endl; // Print CHAR function
    irFile << "STD" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "MTA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "JPA" << std::endl;

    irFile << "FUNCTION_32766:" << std::endl; // Print INT function
    irFile << "STD" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LDB -1" << std::endl;
    irFile << "SSB" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPL PRINT_INT_NEGATIVE" << std::endl;
    irFile << "PRINT_INT_SECTION_1:" << std::endl;
    irFile << "LDB 10" << std::endl;
    irFile << "MOD" << std::endl;
    irFile << "SSC" << std::endl;
    irFile << "DIV" << std::endl;
    irFile << "MAC" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE PRINT_INT_SECTION_2" << std::endl;
    irFile << "JMP PRINT_INT_SECTION_1" << std::endl;
    irFile << "PRINT_INT_SECTION_2:" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "LDB -1" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE PRINT_INT_SECTION_3" << std::endl;
    irFile << "LDB 48" << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "MTC" << std::endl;
    irFile << "JMP PRINT_INT_SECTION_2" << std::endl;
    irFile << "PRINT_INT_SECTION_3:" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "JPA" << std::endl;
    irFile << "PRINT_INT_NEGATIVE:" << std::endl;
    irFile << "LDT 45" << std::endl;
    irFile << "JMP PRINT_INT_SECTION_1" << std::endl;

    irFile << "FUNCTION_32765:" << std::endl; // Print STRING function
    irFile << "STD" << std::endl;
    irFile << "LSD" << std::endl;
    irFile << "PRINT_STRING_SECTION_1:" << std::endl;
    irFile << "RDA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE PRINT_STRING_SECTION_2" << std::endl;
    irFile << "MTA" << std::endl;
    irFile << "MAD" << std::endl;
    irFile << "LDB 1" << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "MDC" << std::endl;
    irFile << "JMP PRINT_STRING_SECTION_1" << std::endl;
    irFile << "PRINT_STRING_SECTION_2:" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "JPA" << std::endl;

    irFile << "FUNCTION_32764:" << std::endl; // Read Char function
    irFile << "STD" << std::endl;
    irFile << "LSD" << std::endl;
    irFile << "READ_CHAR_SECTION_1:" << std::endl;
    irFile << "MAK" << std::endl;
    irFile << "MTA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_CHAR_SECTION_1" << std::endl;
    irFile << "SDA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "JPA" << std::endl;

    irFile << "FUNCTION_32763:" << std::endl; // Read Int function
    irFile << "READ_INT_SECTION_1:" << std::endl;
    irFile << "MAK" << std::endl;
    irFile << "MTA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_INT_SECTION_1" << std::endl;
    irFile << "LDB 45" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_INT_SECTION_5" << std::endl;
    irFile << "LDB 1" << std::endl;
    irFile << "SSB" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "SSB" << std::endl;
    irFile << "JMP READ_INT_SECTION_3" << std::endl;
    irFile << "READ_INT_SECTION_2:" << std::endl;
    irFile << "MAK" << std::endl;
    irFile << "MTA" << std::endl;
    irFile << "READ_INT_SECTION_3:" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_INT_SECTION_2" << std::endl;
    irFile << "LDB 10" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_INT_SECTION_4" << std::endl;
    irFile << "LDB 48" << std::endl;
    irFile << "SUB" << std::endl;
    irFile << "MDC" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "LDB 10" << std::endl;
    irFile << "MUL" << std::endl;
    irFile << "MAC" << std::endl;
    irFile << "MBD" << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "SSC" << std::endl;
    irFile << "JMP READ_INT_SECTION_2" << std::endl;
    irFile << "READ_INT_SECTION_4:" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "LSB" << std::endl;
    irFile << "MUL" << std::endl;
    irFile << "MAC" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "LSB" << std::endl;
    irFile << "SBA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "JPA" << std::endl;
    irFile << "READ_INT_SECTION_5:" << std::endl;
    irFile << "LDB -1" << std::endl;
    irFile << "SSB" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "SSB" << std::endl;
    irFile << "JMP READ_INT_SECTION_2" << std::endl;

    irFile << "FUNCTION_32762:" << std::endl; // Read String function
    irFile << "STD" << std::endl;
    irFile << "LSD" << std::endl;
    irFile << "READ_STRING_SECTION_1:" << std::endl;
    irFile << "MAK" << std::endl;
    irFile << "MTA" << std::endl;
    irFile << "LDB 0" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_STRING_SECTION_1" << std::endl;
    irFile << "LDB 10" << std::endl;
    irFile << "CMP" << std::endl;
    irFile << "JPE READ_STRING_SECTION_2" << std::endl;
    irFile << "SDA" << std::endl;
    irFile << "MAD" << std::endl;
    irFile << "LDB 1" << std::endl;
    irFile << "SUM" << std::endl;
    irFile << "MDC" << std::endl;
    irFile << "JMP READ_STRING_SECTION_1" << std::endl;
    irFile << "READ_STRING_SECTION_2:" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SDA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "LDA 0" << std::endl;
    irFile << "SSA" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "STU" << std::endl;
    irFile << "LSA" << std::endl;
    irFile << "STD" << std::endl;
    irFile << "JPA" << std::endl;
}

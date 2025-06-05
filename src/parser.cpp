#include "parser.hpp"

Parser::Parser(Lexer &l, ErrorHandler &e) : lexer(l), errorHandler(e) {}

Token *Parser ::peekNext()
{
    return lexer.getCurrentToken(false);
}

Token *Parser ::getNextToken()
{
    return lexer.getCurrentToken(true);
}

void Parser ::consumeToken()
{
    lexer.getCurrentToken(true);
}

Token *Parser ::peekBack()
{
    return lexer.getLastToken();
}

parser::IntegerNode *Parser ::parseInteger()
{
    Token *token = getNextToken();
    if (token && token->type == grammar::TYPE::NUMBER)
    {
        if (isalpha(token->value[0]))
        {
            return builder.createIntegerNode((int)token->value[0]);
        }
        if (token->value.length() > 2)
        {
            if (token->value.length() > 2 && token->value[0] == '0')
            {
                if (token->value[1] == 'x' || token->value[1] == 'X')
                {
                    return builder.createIntegerNode(std::stoi(token->value, nullptr, 16));
                }
                else if (token->value[1] == 'o' || token->value[1] == 'O')
                {
                    return builder.createIntegerNode(std::stoi(token->value, nullptr, 8));
                }
                else if (token->value[1] == 'b' || token->value[1] == 'B')
                {
                    return builder.createIntegerNode(std::stoi(token->value, nullptr, 2));
                }
                else
                {
                    return builder.createIntegerNode(std::stoi(token->value, nullptr, 10));
                }
            }
        }
        return builder.createIntegerNode(std::stoi(token->value));
    }
    return nullptr;
}

parser::ExpressionNode *Parser ::parseIdentifier()
{
    Token *token = getNextToken();
    Token *name = new Token();
    lexer.copyToken(name, token);
    token = peekNext();
    if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS))
    {
        return parseFunctionCall(name);
    }
    else if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
    {
        consumeToken();
        std::vector<parser::ExpressionNode *> dimensions;
        while (true)
        {
            parser::ExpressionNode *dim = parseBinaryExpression(0);
            dimensions.push_back(dim);
            token = peekNext();
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_SQUARE_BRACKET)))
                ;
            token = peekNext();
            if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
            {
                consumeToken();
            }
            else
            {
                break;
            }
        }
        return builder.createVariableNode(name, dimensions);
    }
    else
    {
        return builder.createVariableNode(name, std::vector<parser::ExpressionNode *>());
    }
}

parser::ExpressionNode *Parser::parsePrimaryExpression()
{
    Token *token = peekNext();
    if (token && token->type == grammar::TYPE::NUMBER)
    {
        return parseInteger();
    }
    else if (token && token->type == grammar::TYPE::IDENTIFIER)
    {
        return parseIdentifier();
    }
    else if (token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS))
    {
        consumeToken();
        parser::ExpressionNode *expr = parseBinaryExpression(0);
        Token *t2 = peekNext();
        if (t2 && t2->type == grammar::TYPE::SYMBOL && t2->value == grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS))
        {
            consumeToken();
            return expr;
        }
    }

    return nullptr;
}

parser::ExpressionNode *Parser::parseUnaryExpression()
{
    Token *token = peekNext();
    if (token && token->type == grammar::TYPE::SYMBOL && (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN) || token->value == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN) || token->value == grammar::SymbolAsString.at(grammar::SYMBOL::EXCLAMATION_MARK) || token->value == grammar::SymbolAsString.at(grammar::SYMBOL::TILDE) || token->value == grammar::SymbolAsString.at(grammar::SYMBOL::ASTERISK_SIGN) || token->value == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN)))
    {
        consumeToken();
        parser::ExpressionNode *expr = parseUnaryExpression();
        return builder.createUnaryOperationNode(token, expr);
    }
    return parsePrimaryExpression();
}

parser::ExpressionNode *Parser::parseBinaryExpression(int precedence)
{
    parser::ExpressionNode *left = parseUnaryExpression();
    if (!left)
        return nullptr;
    Token *token = peekNext();
    while (token && token->type == grammar::TYPE::SYMBOL && getPrecedence(token) >= precedence)
    {
        consumeToken();
        Token *op = new Token();
        lexer.copyToken(op, token);
        parser::ExpressionNode *right = parseBinaryExpression(getPrecedence(token) + ((token->value == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN)) ? 0 : 1));
        left = builder.createBinaryOperationNode(op, left, right);
        token = peekNext();
    }
    return left;
}

parser::BreakNode *Parser ::parseBreakStatement()
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    return builder.createBreakNode();
}

parser::ContinueNode *Parser ::parseContinueStatement()
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    return builder.createContinueNode();
}

parser::StatementNode *Parser::parseExpressionStatement()
{
    parser::ExpressionNode *expr = parseBinaryExpression(0);
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    if (!expr)
        return nullptr;
    return builder.createExpressionStatementNode(expr);
}

parser::StatementNode *Parser::parseStatement()
{
    Token *token = peekNext();
    if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::IF))
    {
        consumeToken();
        return parseIfElseStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::WHILE))
    {
        consumeToken();
        return parseWhileStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::FOR))
    {
        consumeToken();
        return parseForStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::DO))
    {
        consumeToken();
        return parseDoWhileStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::RETURN))
    {
        consumeToken();
        return parseReturnStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::BREAK))
    {
        consumeToken();
        return parseBreakStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::CONTINUE))
    {
        consumeToken();
        return parseContinueStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::SWITCH))
    {
        consumeToken();
        return parseSwitchStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::CASE))
    {
        consumeToken();
        return parseCaseStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::DEFAULT))
    {
        consumeToken();
        return parseDefaultCaseStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::RETURN))
    {
        consumeToken();
        return parseReturnStatement();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::VAR))
    {
        consumeToken();
        return parseVariableDeclaration();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::CONST))
    {
        consumeToken();
        return parseConstantDeclaration();
    }
    else if (token)
    {
        return parseExpressionStatement();
    }
    errorHandler.reportError(token->lineNumber, token->columnNumber, "Unexpected token: " + token->value);
    sync(grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON));
    return nullptr;
}

parser::Block *Parser::parseBlock()
{
    Token *token = peekNext();
    std::vector<parser::StatementNode *> statements;
    while (token && token->value != grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE))
    {
        parser::StatementNode *stmt = parseStatement();
        if (stmt)
        {
            statements.push_back(stmt);
        }
        token = peekNext();
    }
    return builder.createBlockNode(statements);
}

parser::ConstantDeclarationNode *Parser::parseConstantDeclaration()
{
    bool isArray = false;
    Token *token = getNextToken();
    if (!match(token, std::vector<grammar::TYPE>{grammar::TYPE::IDENTIFIER}))
    {
        return nullptr;
    }
    Token *name = new Token();
    lexer.copyToken(name, token);
    token = peekNext();
    std::vector<parser::ExpressionNode *> dimensions;
    if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
    {
        isArray = true;
        consumeToken();

        while (true)
        {
            parser::ExpressionNode *dim = parseBinaryExpression(0);
            dimensions.push_back(dim);
            token = peekNext();
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_SQUARE_BRACKET)))
                ;
            token = peekNext();
            if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
            {
                consumeToken();
            }
            else
            {
                break;
            }
        }
    }
    parser::VariableNode *var = builder.createVariableNode(name, dimensions);
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN)))
    {
        return builder.createConstantDeclarationNode(var, std::vector<parser::ExpressionNode *>());
    }
    std::vector<parser::ExpressionNode *> values;
    if (isArray)
    {

        token = peekNext();
        if (token && token->type == grammar::TYPE::STRING)
        {
            consumeToken();
            for (auto c : token->value)
            {
                values.push_back(builder.createIntegerNode((int)c));
            }
            values.push_back(builder.createIntegerNode(0));
        }
        else if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
        {
            consumeToken();
            while (true)
            {
                token = peekNext();
                if (token)
                {
                    values.push_back(parseBinaryExpression(0));
                }
                token = peekNext();
                if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::COMMA))
                {
                    consumeToken();
                }
                else
                {
                    break;
                }
            }
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
                ;
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
                ;
        }
    }
    else
    {
        values.push_back(parseBinaryExpression(0));
    }
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    return builder.createConstantDeclarationNode(var, values);
}

parser::VariableDeclarationNode *Parser::parseVariableDeclaration()
{
    bool isArray = false;
    Token *token = getNextToken();
    if (!match(token, std::vector<grammar::TYPE>{grammar::TYPE::IDENTIFIER}))
    {
        return nullptr;
    }
    Token *name = new Token();
    lexer.copyToken(name, token);
    token = peekNext();
    std::vector<parser::ExpressionNode *> dimensions;
    if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
    {
        isArray = true;
        consumeToken();

        while (true)
        {
            parser::ExpressionNode *dim = parseBinaryExpression(0);
            dimensions.push_back(dim);
            token = peekNext();
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_SQUARE_BRACKET)))
                ;
            token = peekNext();
            if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
            {
                consumeToken();
            }
            else
            {
                break;
            }
        }
    }
    parser::VariableNode *var = builder.createVariableNode(name, dimensions);
    std::vector<parser::ExpressionNode *> values;
    token = peekNext();
    if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN))
    {
        consumeToken();
        if (isArray)
        {

            token = peekNext();
            if (token && token->type == grammar::TYPE::STRING)
            {
                consumeToken();
                for (auto c : token->value)
                {
                    values.push_back(builder.createIntegerNode((int)c));
                }
                values.push_back(builder.createIntegerNode(0));
            }
            else if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
            {
                consumeToken();
                while (true)
                {
                    token = peekNext();
                    if (token)
                    {
                        values.push_back(parseBinaryExpression(0));
                    }
                    token = peekNext();
                    if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::COMMA))
                    {
                        consumeToken();
                    }
                    else
                    {
                        break;
                    }
                }
                if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
                    ;
            }
        }
        else
        {
            values.push_back(parseBinaryExpression(0));
        }
    }
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    return builder.createVariableDeclarationNode(var, values);
}

parser::IfElseNode *Parser::parseIfElseStatement()
{
    Token *token = peekNext();
    parser::StatementNode *trueBlock = nullptr;
    parser::StatementNode *falseBlock = nullptr;
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    parser::ExpressionNode *condition = parseBinaryExpression(0);
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
        ;
    token = peekNext();
    if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
    {
        consumeToken();
        trueBlock = parseBlock();
        token = peekNext();
        if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
            ;
    }
    else
    {
        trueBlock = parseStatement();
    }
    token = peekNext();
    if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::ELSE))
    {
        consumeToken();
        token = peekNext();
        if (token && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
        {
            consumeToken();
            falseBlock = parseBlock();
            token = peekNext();
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
                ;
        }
        else
        {
            falseBlock = parseStatement();
        }
    }
    return builder.createIfElseNode(condition, trueBlock, falseBlock);
}

parser::SwitchNode *Parser::parseSwitchStatement()
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    parser::ExpressionNode *matchValue = parseBinaryExpression(0);
    if (!matchValue)
        return nullptr;
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
        ;
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE)))
        ;
    parser::Block *body = parseBlock();
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
        ;
    return builder.createSwitchNode(matchValue, body);
}

parser::CaseNode *Parser::parseCaseStatement()
{
    Token *token = peekNext();
    if (token)
    {
        parser::ExpressionNode *caseValue = parseBinaryExpression(0);
        token = peekNext();
        if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::COLON)))
            ;
        return builder.createCaseNode(caseValue);
    }
    return nullptr;
}

parser::DefaultCaseNode *Parser::parseDefaultCaseStatement()
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::COLON)))
        ;
    return builder.createDefaultCaseNode();
}

parser::WhileLoopNode *Parser::parseWhileStatement()
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    parser::ExpressionNode *condition = parseBinaryExpression(0);
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
        ;
    token = peekNext();
    if (token && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
    {
        consumeToken();
        parser::Block *body = parseBlock();
        token = peekNext();
        if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
            ;
        return builder.createWhileLoopNode(condition, body);
    }
    else
    {
        parser::StatementNode *body = parseStatement();
        return builder.createWhileLoopNode(condition, body);
    }
}

parser::DoWhileLoopNode *Parser::parseDoWhileStatement()
{
    Token *token = peekNext();
    parser::StatementNode *body = new parser::StatementNode();
    if (token && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
    {
        consumeToken();
        body = parseBlock();
        token = peekNext();
        if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
            ;
    }
    else
    {
        body = parseStatement();
    }
    token = peekNext();
    if (!match(token, grammar::TYPE::KEYWORD, grammar::KeywordAsString.at(grammar::KEYWORD::WHILE)))
        ;
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    parser::ExpressionNode *condition = parseBinaryExpression(0);
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
        ;
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    return builder.createDoWhileLoopNode(body, condition);
}

parser::ForLoopNode *Parser::parseForStatement()
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    parser::StatementNode *init = new parser::StatementNode();
    token = peekNext();
    if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::VAR))
    {
        consumeToken();
        init = parseVariableDeclaration();
    }
    else if (token && token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::CONST))
    {
        consumeToken();
        init = parseConstantDeclaration();
    }
    else
    {
        init = parseExpressionStatement();
    }
    parser::ExpressionNode *condition = parseBinaryExpression(0);
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    parser::ExpressionNode *update = parseBinaryExpression(0);
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
        ;
    token = peekNext();
    if (token && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE))
    {
        consumeToken();
        parser::Block *body = parseBlock();
        token = peekNext();
        if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
            ;
        return builder.createForLoopNode(init, update, condition, body);
    }
    else
    {
        parser::StatementNode *body = parseStatement();
        return builder.createForLoopNode(init, update, condition, body);
    }
}

parser::FunctionDefinitionNode *Parser::parseFunctionDefinition()
{
    Token *functionName = new Token();
    std::vector<parser::VariableDeclarationNode *> parameters;
    Token *token = peekNext();
    if (token && token->type == grammar::TYPE::IDENTIFIER)
    {
        lexer.copyToken(functionName, token);
        consumeToken();
    }
    else
    {
        errorHandler.reportError(functionName->lineNumber, functionName->columnNumber, "Error: Expected function name.");
        sync(grammar::TYPE::IDENTIFIER, "");
        return nullptr;
    }
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    while (true)
    {
        bool isArray = false;
        Token *token = peekNext();
        if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS))
        {
            break;
        }
        consumeToken();
        Token *name = new Token();
        lexer.copyToken(name, token);
        token = peekNext();
        std::vector<parser::ExpressionNode *> dimensions;
        if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
        {
            isArray = true;
            consumeToken();

            while (true)
            {
                parser::ExpressionNode *dim = parseBinaryExpression(0);
                dimensions.push_back(dim);
                token = peekNext();
                if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_SQUARE_BRACKET)))
                    ;
                token = peekNext();
                if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_SQUARE_BRACKET))
                {
                    consumeToken();
                }
                else
                {
                    break;
                }
            }
        }
        parser::VariableNode *var = builder.createVariableNode(name, dimensions);
        std::vector<parser::ExpressionNode *> values;
        token = peekNext();
        parameters.push_back(builder.createVariableDeclarationNode(var, values));
        if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::COMMA))
        {

            consumeToken();
        }
        else
        {
            break;
        }
    }
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
        ;
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_CURLY_BRACE)))
        ;
    parser::Block *body = parseBlock();
    token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_CURLY_BRACE)))
        ;
    return builder.createFunctionDefinitionNode(functionName, parameters, body);
}

parser::FunctionCallNode *Parser::parseFunctionCall(Token *name)
{
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::LEFT_PARENTHESIS)))
        ;
    std::vector<parser::ExpressionNode *> arguments;
    while (true)
    {
        token = peekNext();
        if (token->type == grammar::TYPE::STRING)
        {
            consumeToken();
            arguments.push_back(builder.createStringNode(token->value));
        }
        else
        {
            parser::ExpressionNode *arg = parseBinaryExpression(0);
            if (arg)
            {
                arguments.push_back(arg);
            }
        }
        token = peekNext();
        if (token && token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::COMMA))
        {
            consumeToken();
        }
        else
        {
            if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::RIGHT_PARENTHESIS)))
                ;
            break;
        }
    }
    return builder.createFunctionCallNode(name, arguments);
}

parser::ReturnNode *Parser::parseReturnStatement()
{
    parser::ExpressionNode *returnValue = parseBinaryExpression(0);
    Token *token = peekNext();
    if (!match(token, grammar::TYPE::SYMBOL, grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON)))
        ;
    return builder.createReturnNode(returnValue);
}

int Parser ::getPrecedence(Token *t)
{
    if (t->type == grammar::TYPE::SYMBOL)
    {

        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::ASTERISK_SIGN))
            return 140;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::FORWARD_SLASH_SIGN))
            return 140;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::PERCENT_SIGN))
            return 140;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN))
            return 130;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN))
            return 130;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_LESS_THAN_SIGN))
            return 120;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_GREATER_THAN_SIGN))
            return 120;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN))
            return 110;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN))
            return 110;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_EQUALS_SIGN))
            return 110;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_EQUALS_SIGN))
            return 110;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_EQUALS_SIGN))
            return 100;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::NOT_EQUALS_SIGN))
            return 100;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN))
            return 90;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::CARET_SIGN))
            return 80;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::VERTICAL_BAR_SIGN))
            return 70;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_AMPERSAND_SIGN))
            return 60;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::DOUBLE_VERTICAL_BAR_SIGN))
            return 50;
        if (t->value == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN))
            return 40;
    }
    return -1;
}

parser::ParsedProgram *Parser::parse()
{
    std::vector<parser::VariableDeclarationNode *> globalVariables;
    std::vector<parser::ConstantDeclarationNode *> globalConstants;
    std::vector<parser::FunctionDefinitionNode *> functions;

    // Pushing built-in functions
    for (auto const &funcName : grammar::BuiltInFunctions)
    {
        functions.push_back(new parser::FunctionDefinitionNode(new Token(grammar::TYPE::IDENTIFIER, funcName, 0, 0), {}, nullptr));
    }
    while (true)
    {
        Token *token = peekNext();
        if (token == nullptr)
            break;

        if (token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::VAR))
        {
            consumeToken();
            parser::VariableDeclarationNode *globalVar = parseVariableDeclaration();
            if (globalVar)
            {
                globalVariables.push_back(globalVar);
            }
        }
        else if (token->type == grammar::TYPE::KEYWORD && token->value == grammar::KeywordAsString.at(grammar::KEYWORD::CONST))
        {
            consumeToken();
            parser::ConstantDeclarationNode *globalConst = parseConstantDeclaration();
            if (globalConst)
            {
                globalConstants.push_back(globalConst);
            }
        }
        else if (token->type == grammar::TYPE::IDENTIFIER)
        {
            parser::FunctionDefinitionNode *func = parseFunctionDefinition();
            if (func)
            {
                functions.push_back(func);
            }
        }
        else if (token->type == grammar::TYPE::SYMBOL && token->value == grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON))
        {
            continue;
        }
    }

    return new parser::ParsedProgram(globalVariables, globalConstants, functions);
}

void Parser::sync(grammar::TYPE expectedType, const std::string &expectedValue)
{
    Token *token = peekNext();
    if(!token)
    {
        errorHandler.reportError(lexer.getCurrentRow(), lexer.getCurrentCol(), "Error: Unexpected end of input.");
        return;
    }
    while (token->type != grammar::TYPE::SYMBOL && token->value != grammar::SymbolAsString.at(grammar::SYMBOL::SEMICOLON))
    {
        consumeToken();
        token = peekNext();
        if(!token)
        {
            errorHandler.reportError(lexer.getCurrentRow(), lexer.getCurrentCol(), "Error: Unexpected end of input.");
            return;
        }
    }
    return;
}

bool Parser::match(Token *token, grammar::TYPE expectedType, const std::string &expectedValue)
{
    if (!token)
    {
        errorHandler.reportError(lexer.getCurrentRow(), lexer.getCurrentCol(), "Error: Unexpected end of input.");
        return false;
    }
    if (token->type != expectedType || token->value != expectedValue)
    {
        errorHandler.reportError(token->lineNumber, token->columnNumber, "Error: Expected " + expectedValue + " of type " + grammar::typeNameOf.at(expectedType) + " but found " + token->value + " of type " + grammar::typeNameOf.at(token->type));
        sync(expectedType, expectedValue);
        return false;
    }
    else
        consumeToken();
    return true;
}

bool Parser::match(Token *token, std::vector<grammar::TYPE> expectedTypes)
{
    if (!token)
    {
        errorHandler.reportError(lexer.getCurrentRow(), lexer.getCurrentCol(), "Error: Unexpected end of input.");
        return false;
    }
    for (grammar::TYPE expectedType : expectedTypes)
    {
        if (token->type == expectedType)
        {
            return true;
        }
    }
    std::string errorMessage = "Expected one of the following types: ";
    for (grammar::TYPE expectedType : expectedTypes)
    {
        errorMessage += grammar::typeNameOf.at(expectedType) + ", ";
    }
    errorMessage += "but found " + token->value + " of type " + grammar::typeNameOf.at(token->type);
    errorHandler.reportError(token->lineNumber, token->columnNumber, errorMessage);
    sync(expectedTypes[0], "");
    return false;
}
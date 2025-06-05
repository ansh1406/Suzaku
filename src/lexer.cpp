#include "lexer.hpp"

Lexer::Lexer(std::ifstream &programFile, ErrorHandler &e) : sourceFile(programFile), errorHandler(e)
{
    colNumber = 0;
    rowNumber = 1;
    currentColNumber = 0;
    currentRowNumber = 0;
    lastToken = nullptr;
    currentToken = tokenize();
    nextToken = tokenize();
}

Token *Lexer::tokenize()
{
    currentColNumber = colNumber;
    currentRowNumber = rowNumber;
    std::string currentTokenValue;
    while (true)
    {
        char c = getNextChar();
        if (c == EOF)
            return nullptr;
        char nextC = sourceFile.peek();
        if (c == '#')
        {
            while (c != '\n' && c != EOF)
            {
                c = getNextChar();
            }
        }
        if (c == '"')
        {
            std::string buffer;
            c = getNextChar();
            while (c != EOF && c != '"' && c != '\n')
            {
                if (c == '\\')
                {
                    c = getNextChar();
                    if (c != EOF && grammar::ESCAPE_SEQUENCES.find(c) != grammar::ESCAPE_SEQUENCES.end())
                    {
                        switch (c)
                        {
                        case 'n':
                            buffer += '\n';
                            break;
                        case 't':
                            buffer += '\t';
                            break;
                        case 'r':
                            buffer += '\r';
                            break;
                        case 'b':
                            buffer += '\b';
                            break;
                        case 'f':
                            buffer += '\f';
                            break;
                        case 'v':
                            buffer += '\v';
                            break;
                        case 'a':
                            buffer += '\a';
                            break;
                        case '0':
                            buffer += '\0';
                            break;
                        case '\\':
                            buffer += '\\';
                            break;
                        case '"':
                            buffer += '"';
                            break;
                        case '\'':
                            buffer += '\'';
                            break;
                        }
                    }
                    else
                    {
                        errorHandler.reportError(rowNumber, colNumber, "Error: Invalid escape sequence in string literal");
                        return nullptr;
                    }
                }
                else
                {
                    buffer += c;
                }
                c = getNextChar();
            }
            if (c == EOF || c == '\n')
            {
                errorHandler.reportError(rowNumber, colNumber, "Error: Unmatched double quote in string literal");
            }
            Token *token = new Token();
            token->type = grammar::TYPE::STRING;
            token->value = buffer;
            token->lineNumber = currentRowNumber;
            token->columnNumber = currentColNumber;
            return token;
        }
        else if (c == '\'')
        {
            Token *token = new Token();
            token->type = grammar::TYPE::NUMBER;
            token->value = "";
            c = getNextChar();
            if (c == '\\')
            {
                c = getNextChar();
                if (c != EOF && grammar::ESCAPE_SEQUENCES.find(c) != grammar::ESCAPE_SEQUENCES.end())
                {
                    switch (c)
                    {
                    case 'n':
                        token->value += "10";
                        break;
                    case 't':
                        token->value += "9";
                        break;
                    case 'r':
                        token->value += "13";
                        break;
                    case 'b':
                        token->value += "8";
                        break;
                    case 'f':
                        token->value += "12";
                        break;
                    case 'v':
                        token->value += "11";
                        break;
                    case 'a':
                        token->value += "7";
                        break;
                    case '0':
                        token->value += "0";
                        break;
                    case '\\':
                        token->value += "92";
                        break;
                    case '"':
                        token->value += "34";
                        break;
                    case '\'':
                        token->value += "39";
                        break;
                    }
                }
                else
                {
                    errorHandler.reportError(rowNumber, colNumber, "Error: Invalid character.");
                    return nullptr;
                }
            }
            else if (c == '\'')
            {
                token->value += "0";
            }
            else
            {
                token->value += std::to_string(static_cast<int>(c));
            }
            c = getNextChar();
            if (c == EOF || c != '\'')
            {
                errorHandler.reportError(rowNumber, colNumber, "Error: Missing \" \' \" ");
            }
            token->lineNumber = currentRowNumber;
            token->columnNumber = currentColNumber;
            return token;
        }
        else if (grammar::SYMBOLS.find(c) != grammar::SYMBOLS.end())
        {
            if (!currentTokenValue.empty())
            {
                Token *token = new Token();
                token->value = currentTokenValue;
                if (grammar::KEYWORDS.find(currentTokenValue) != grammar::KEYWORDS.end())
                {
                    token->type = grammar::TYPE::KEYWORD;
                }
                else if (isdigit(currentTokenValue[0]))
                {
                    token->type = grammar::TYPE::NUMBER;
                }
                else
                {
                    token->type = grammar::TYPE::IDENTIFIER;
                }
                sourceFile.unget();
                token->lineNumber = currentRowNumber;
                token->columnNumber = currentColNumber;
                return token;
            }
            Token *token = new Token();
            token->type = grammar::TYPE::SYMBOL;
            token->value = "";
            token->value += c;
            if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN) &&
                nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::AMPERSAND_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::VERTICAL_BAR_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::VERTICAL_BAR_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::VERTICAL_BAR_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::EXCLAMATION_MARK) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::EQUALS_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::MINUS_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::PLUS_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::LESS_THAN_SIGN);
                getNextChar();
            }
            else if (token->value == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN) &&
                     nextC != EOF && nextC == grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN)[0])
            {
                token->value += grammar::SymbolAsString.at(grammar::SYMBOL::GREATER_THAN_SIGN);
                getNextChar();
            }
            token->lineNumber = currentRowNumber;
            token->columnNumber = currentColNumber;
            return token;
        }
        else if (isalnum(c) || c == '_')
        {
            currentTokenValue += c;
        }

        else if (isspace(c))
        {
            if (!currentTokenValue.empty())
            {
                Token *token = new Token();
                token->value = currentTokenValue;
                if (grammar::KEYWORDS.find(currentTokenValue) != grammar::KEYWORDS.end())
                {
                    token->type = grammar::TYPE::KEYWORD;
                }
                else
                {
                    if(isdigit(currentTokenValue[0]))
                    {
                        token->type = grammar::TYPE::NUMBER;
                    }
                    else
                    {
                        token->type = grammar::TYPE::IDENTIFIER;
                    }
                }
                currentTokenValue.clear();
                token->lineNumber = currentRowNumber;
                token->columnNumber = currentColNumber;
                return token;
            }
            currentColNumber = colNumber;
            currentRowNumber = rowNumber;
        }
    }
}

char Lexer::getNextChar()
{
    char c = sourceFile.get();
    if (c == EOF)
    {
        return EOF;
    }
    colNumber++;
    if (c == '\n')
    {
        rowNumber++;
        colNumber = 0;
    }
    return c;
}

Token *Lexer::getLastToken()
{
    return lastToken;
}

Token *Lexer::getNextToken()
{
    copyToken(lastToken, currentToken);
    copyToken(currentToken, nextToken);
    nextToken = tokenize();
    return currentToken;
}

Token *Lexer::getCurrentToken(bool doConsume)
{
    if (doConsume)
    {
        copyToken(lastToken, currentToken);
        copyToken(currentToken, nextToken);
        nextToken = tokenize();
    }
    Token *t = new Token();
    copyToken(t, (doConsume) ? lastToken : currentToken);
    return t;
}

void Lexer::copyToken(Token *&destination, Token *&source)
{
    if (source == nullptr)
    {
        destination = nullptr;
        return;
    }
    if (destination == nullptr)
    {
        destination = new Token();
    }
    destination->type = source->type;
    destination->value = source->value;
    destination->lineNumber = source->lineNumber;
    destination->columnNumber = source->columnNumber;
}

int Lexer::getCurrentCol()
{
    return currentColNumber;
}

int Lexer::getCurrentRow()
{
    return currentRowNumber;
}
#ifndef LAXER_H
#define LAXER_H

#include "errorHandler.hpp"
#include "grammar.hpp"
#include <string>
#include <vector>
#include <fstream>

struct Token
{
    grammar::TYPE type;
    std::string value;
    int lineNumber;
    int columnNumber;
    Token(grammar::TYPE t, const std::string &v, int line, int col) : type(t), value(v), lineNumber(line), columnNumber(col) {}
    Token() : type(grammar::TYPE::IDENTIFIER), value(""), lineNumber(0), columnNumber(0) {}
};

class Lexer
{
private:
    int rowNumber;
    int colNumber;
    int currentColNumber;
    int currentRowNumber;
    char getNextChar();
    Token *lastToken;
    Token *nextToken;
    Token *currentToken;
   
    ErrorHandler &errorHandler;
public:
    std::ifstream &sourceFile;
    Lexer(std::ifstream &programFile, ErrorHandler &e);
    Token *tokenize();
    Token *getLastToken();
    Token *getNextToken();
    Token *getCurrentToken(bool doConsume); 
    void copyToken(Token* &destination , Token* &source);
    int getCurrentRow();
    int getCurrentCol();
};

#endif
#ifndef KEYWORDS_H
#define KEYWORDS_H

#include <unordered_set>
#include <unordered_map>
#include <string>
#include <vector>

namespace grammar
{
    enum class TYPE
    {
        IDENTIFIER,
        NUMBER,
        STRING,
        OPERATOR,
        KEYWORD,
        SYMBOL,
    };

    const std::unordered_map<TYPE, std::string> typeNameOf = {
        {TYPE::IDENTIFIER, "Identifier"},
        {TYPE::NUMBER, "Number"},
        {TYPE::STRING, "String"},
        {TYPE::OPERATOR, "Operator"},
        {TYPE::KEYWORD, "Keyword"},
        {TYPE::SYMBOL, "Symbol"}};

    const std::unordered_set<std::string> KEYWORDS = {
        "var",
        "const",
        "if",
        "else",
        "for",
        "while",
        "do",
        "break",
        "continue",
        "switch",
        "case",
        "default",
        "return",

    };

    enum class KEYWORD
    {
        VAR,
        CONST,
        IF,
        ELSE,
        FOR,
        WHILE,
        DO,
        BREAK,
        CONTINUE,
        SWITCH,
        CASE,
        DEFAULT,
        RETURN,

    };

    const std::unordered_map<KEYWORD, std::string> KeywordAsString = {
        {KEYWORD::VAR, "var"},
        {KEYWORD::CONST, "const"},
        {KEYWORD::IF, "if"},
        {KEYWORD::ELSE, "else"},
        {KEYWORD::FOR, "for"},
        {KEYWORD::WHILE, "while"},
        {KEYWORD::DO, "do"},
        {KEYWORD::BREAK, "break"},
        {KEYWORD::CONTINUE, "continue"},
        {KEYWORD::SWITCH, "switch"},
        {KEYWORD::CASE, "case"},
        {KEYWORD::DEFAULT, "default"},
        {KEYWORD::RETURN, "return"}};

    const std::unordered_set<char> SYMBOLS = {
        '(', ')', '{', '}', '[', ']', ';', ':', ',', '.', '?', '!', '~', '@', '#', '$', '%', '^', '&', '*', '+', '-',
        '/', '<', '>', '=', '|', '\\'};

    enum class SYMBOL
    {
        LEFT_PARENTHESIS,
        RIGHT_PARENTHESIS,
        LEFT_CURLY_BRACE,
        RIGHT_CURLY_BRACE,
        LEFT_SQUARE_BRACKET,
        RIGHT_SQUARE_BRACKET,
        SEMICOLON,
        COLON,
        COMMA,
        DOT,
        QUESTION_MARK,
        EXCLAMATION_MARK,
        TILDE,
        AT_SIGN,
        HASH_SIGN,
        DOLLAR_SIGN,
        PERCENT_SIGN,
        CARET_SIGN,
        AMPERSAND_SIGN,
        DOUBLE_AMPERSAND_SIGN,
        ASTERISK_SIGN,
        PLUS_SIGN,
        MINUS_SIGN,
        FORWARD_SLASH_SIGN,
        LESS_THAN_SIGN,
        LESS_THAN_EQUALS_SIGN,
        DOUBLE_LESS_THAN_SIGN,
        GREATER_THAN_SIGN,
        DOUBLE_GREATER_THAN_SIGN,
        GREATER_THAN_EQUALS_SIGN,
        EQUALS_SIGN,
        DOUBLE_EQUALS_SIGN,
        NOT_EQUALS_SIGN,
        VERTICAL_BAR_SIGN,
        DOUBLE_VERTICAL_BAR_SIGN,
        BACKSLASH_SIGN
    };

    enum class BUILT_IN_FUNCTIONS
    {
        PRINT_INT,
        PRINT_CHAR,
        PRINT_STRING,
        READ_INT,
        READ_CHAR,
        READ_STRING
    };

    const std::unordered_map<SYMBOL, std::string> SymbolAsString = {
        {SYMBOL::LEFT_PARENTHESIS, "("},
        {SYMBOL::RIGHT_PARENTHESIS, ")"},
        {SYMBOL::LEFT_CURLY_BRACE, "{"},
        {SYMBOL::RIGHT_CURLY_BRACE, "}"},
        {SYMBOL::LEFT_SQUARE_BRACKET, "["},
        {SYMBOL::RIGHT_SQUARE_BRACKET, "]"},
        {SYMBOL::SEMICOLON, ";"},
        {SYMBOL::COLON, ":"},
        {SYMBOL::COMMA, ","},
        {SYMBOL::DOT, "."},
        {SYMBOL::QUESTION_MARK, "?"},
        {SYMBOL::EXCLAMATION_MARK, "!"},
        {SYMBOL::TILDE, "~"},
        {SYMBOL::AT_SIGN, "@"},
        {SYMBOL::HASH_SIGN, "#"},
        {SYMBOL::DOLLAR_SIGN, "$"},
        {SYMBOL::PERCENT_SIGN, "%"},
        {SYMBOL::CARET_SIGN, "^"},
        {SYMBOL::AMPERSAND_SIGN, "&"},
        {SYMBOL::DOUBLE_AMPERSAND_SIGN, "&&"},
        {SYMBOL::ASTERISK_SIGN, "*"},
        {SYMBOL::PLUS_SIGN, "+"},
        {SYMBOL::MINUS_SIGN, "-"},
        {SYMBOL::FORWARD_SLASH_SIGN, "/"},
        {SYMBOL::LESS_THAN_SIGN, "<"},
        {SYMBOL::LESS_THAN_EQUALS_SIGN, "<="},
        {SYMBOL::DOUBLE_LESS_THAN_SIGN, "<<"},
        {SYMBOL::GREATER_THAN_SIGN, ">"},
        {SYMBOL::DOUBLE_GREATER_THAN_SIGN, ">>"},
        {SYMBOL::GREATER_THAN_EQUALS_SIGN, ">="},
        {SYMBOL::EQUALS_SIGN, "="},
        {SYMBOL::DOUBLE_EQUALS_SIGN, "=="},
        {SYMBOL::NOT_EQUALS_SIGN, "!="},
        {SYMBOL::VERTICAL_BAR_SIGN, "|"},
        {SYMBOL::DOUBLE_VERTICAL_BAR_SIGN, "||"}};

    const std::unordered_set<char> ESCAPE_SEQUENCES = {
        'n', 't', 'r', 'b', 'f', 'v', 'a', '0', '\\', '\"', '\''};

    const std::vector<std::string> BuiltInFunctions = {
        "__printInt",
        "__printChar",
        "__printString",
        "__readInt",
        "__readChar",
        "__readString",
    };

    const std::unordered_map<BUILT_IN_FUNCTIONS, std::string> BuiltInFunctionAsString = {
        {BUILT_IN_FUNCTIONS::PRINT_INT, "__printInt"},
        {BUILT_IN_FUNCTIONS::PRINT_CHAR, "__printChar"},
        {BUILT_IN_FUNCTIONS::PRINT_STRING, "__printString"},
        {BUILT_IN_FUNCTIONS::READ_INT, "__readInt"},
        {BUILT_IN_FUNCTIONS::READ_CHAR, "__readChar"},
        {BUILT_IN_FUNCTIONS::READ_STRING, "__readString"}};

}

#endif
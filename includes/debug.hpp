#ifndef DEBUG_HPP
#define DEBUG_HPP

#include <iostream>
#include "grammar.hpp"
#include "lexer.hpp"
#include "parser.hpp"

void printExpression(parser::ExpressionNode *expr);
void printStatement(parser::StatementNode *statement);
void printFunction(parser::FunctionDefinitionNode *function);
void printProgram(parser::ParsedProgram *program);

#endif
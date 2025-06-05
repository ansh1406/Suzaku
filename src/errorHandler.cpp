#include "errorHandler.hpp"

#include<iostream>

void ErrorHandler::printError(const Error &error)
{
    std::cerr << "Error at line " << error.lineNumber << ", column " << error.columnNumber << ": " << error.message << std::endl;
};

void ErrorHandler::reportError(int line, int col, const std::string &msg)
{
    Error error(line, col, msg);
    errorCount++;
    printError(error);
}
#ifndef ERRORHANDLER_H
#define ERRORHANDLER_H

#include <string>
struct Error
{
    int lineNumber;
    int columnNumber;
    std::string message;
    Error(int line, int column, const std::string &msg) : lineNumber(line), columnNumber(column), message(msg) {}
};


class ErrorHandler
{
    public:
    void reportError(int line , int col , const std::string &msg);
    int getErrorCount() const { return errorCount; }
    private:
        void printError(const Error &error);
        int errorCount = 0;
};

#endif
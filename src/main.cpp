#include <iostream>
#include <vector>
#include <string>
#include <fstream>

#include "grammar.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "errorHandler.hpp"
#include "semanticChecker.hpp"
#include "IRGenerator.hpp"
#include "binaryGenerator.hpp"

#ifdef DEBUG
#include "debug.hpp"
#endif

int main(int argc, char *argv[])
{
    if(argc > 1) {
        if(std::string(argv[1]) == "--help"){
            std::cout << "Usage: " << argv[0] << " <input_file> <output_file> [-i]" << std::endl;
            std::cout << "  <input_file>   Source code file to compile." << std::endl;
            std::cout << "  <output_file>  Output binary file name (without extension)." << std::endl;
            std::cout << "  -i             (Optional) Keep intermediate representation (.lasm) file." << std::endl;
            std::cout << "  --help         Show this help message and exit." << std::endl;
            return 0;
        }
    }
    if (argc == 1)
    {
        std::cerr << "No input file provided. Please provide a file name." << std::endl;
        std::cerr << "Output file name is missing." << std::endl;
        std::cerr << "Execute with option --help to view usage instructions." << std::endl;
        return 1;
    }
    if (argc == 2)
    {
        std::cerr << "Output file name is missing." << std::endl;
        std::cerr << "Execute with option --help to view usage instructions." << std::endl;
        return 1;
    }
    bool keepIR = false;
    if (argc == 4)
    {
        if (std::string(argv[3]) == "-i")
        {
            keepIR = true;
        }
        else
        {
            std::cerr << "Invalid option." << std::endl;
            return 1;
        }
    }
    std::string inputFileName = argv[1];
    std::string outputFileName = argv[2];
    std::string irFileName = outputFileName + ".lasm";
    outputFileName += ".lm1";
    std::ifstream programFile(inputFileName);
    if (!programFile)
    {
        std::cerr << "Error opening file." << std::endl;
        return 1;
    }
    ErrorHandler errorHandler;
    Lexer lexer(programFile, errorHandler);
    Parser parser(lexer, errorHandler);
    parser::ParsedProgram *program = parser.parse();
    if (!program)
    {
        std::cerr << "Error parsing program." << std::endl;
        return 1;
    }
    std::cout << "Parsed program successfully." << std::endl;

    SemanticChecker semanticChecker(program, errorHandler);
    semanticChecker.check();
    if (errorHandler.getErrorCount() > 0)
    {
        std::cerr << "Errors found during semantic analysis. Exiting." << std::endl;
        return 1;
    }

    IRGenerator irGenerator(program);
    irgenerator::IntermediateRepresentation *ir = irGenerator.generateIR();
    std::cout << "Generated Intermediate Representation successfully." << std::endl;

    std::ofstream irFile(irFileName);
    if (!irFile)
    {
        std::cerr << "Error opening IR file." << std::endl;
        return 1;
    }
    IRFileWriter irFileWriter(irFile, ir);
    irFileWriter.write();
    std::cout << "Wrote Intermediate Representation to file successfully." << std::endl;
    irFile.close();
    std::ifstream irFileIn(irFileName);
    std::ofstream outputFile(outputFileName, std::ios::binary);
    BinaryGenerator binaryGenerator(irFileIn, outputFile);
    binaryGenerator.generateBinary();
    std::cout << "Generated binary file successfully." << std::endl;

    programFile.close();
    outputFile.close();
    irFileIn.close();
    if (!keepIR)
        std::remove(irFileName.c_str());
    return 0;
}


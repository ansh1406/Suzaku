#include <fstream>
#include <map>
#include <unordered_map>
#include <vector>
#include <string>
#include <sstream>
#include <iomanip>
#include <algorithm>

namespace binarygenerator
{
    const std::unordered_map<std::string, std::string> instructionSet = {
        {"LDA", "0100"},
        {"LDB", "0200"},
        {"LDC", "0300"},
        {"LDD", "0400"},
        {"LDT", "0500"},
        {"LRA", "0600"},
        {"LRB", "0700"},
        {"LRC", "0800"},
        {"LRD", "0900"},
        {"LRT", "0A00"},
        {"LSA", "0B00"},
        {"LSB", "0C00"},
        {"LSC", "0D00"},
        {"LSD", "0E00"},
        {"LST", "0F00"},
        {"SRA", "1000"},
        {"SRB", "1100"},
        {"SRC", "1200"},
        {"SRD", "1300"},
        {"SRK", "1400"},
        {"SSA", "1500"},
        {"SSB", "1600"},
        {"SSC", "1700"},
        {"SSD", "1800"},
        {"SSK", "1900"},
        {"MAB", "1A00"},
        {"MAC", "1B00"},
        {"MAD", "1C00"},
        {"MBA", "1D00"},
        {"MBC", "1E00"},
        {"MBD", "1F00"},
        {"MCA", "2000"},
        {"MCB", "2100"},
        {"MCD", "2200"},
        {"MDA", "2300"},
        {"MDB", "2400"},
        {"MDC", "2500"},
        {"MAK", "2600"},
        {"MBK", "2700"},
        {"MCK", "2800"},
        {"MDK", "2900"},
        {"MTA", "2A00"},
        {"MTB", "2B00"},
        {"MTC", "2C00"},
        {"MTD", "2D00"},
        {"SUM", "2E00"},
        {"SUB", "2F00"},
        {"MUL", "3000"},
        {"DIV", "3100"},
        {"MOD", "3200"},
        {"NOT", "3300"},
        {"AND", "3400"},
        {"ORR", "3500"},
        {"XOR", "3600"},
        {"NND", "3700"},
        {"NOR", "3800"},
        {"CMP", "3900"},
        {"SAV", "3A00"},
        {"RET", "3B00"},
        {"JMP", "3C00"},
        {"JPA", "3D00"},
        {"JPB", "3E00"},
        {"JPC", "3F00"},
        {"JPD", "4000"},
        {"CPC", "4100"},
        {"RBA", "4200"},
        {"RCA", "4300"},
        {"RDA", "4400"},
        {"STU", "4500"},
        {"STD", "4600"},
        {"KBP", "4700"},
        {"SBA", "4800"},
        {"SCA", "4900"},
        {"SDA", "4A00"},
        {"CSP", "4B00"},
        {"SUD", "4C00"},
        {"SDD", "4D00"},
        {"JPG", "FB00"},
        {"JPE", "FC00"},
        {"JPL", "FD00"},
        {"JPZ", "FE00"},
        {"HLT", "FF00"}};
}

class BinaryGenerator
{
public:
    void generateBinary();
    BinaryGenerator(std::ifstream &irFile, std::ofstream &binaryFile)
        : irFile(irFile), binaryFile(binaryFile) {}

private:
    std::ifstream &irFile;
    std::ofstream &binaryFile;
    std::string ir;
    std::unordered_map<std::string, int> labels;
    std::map<int, std::string> instructions;
    int currentRamHead = 0;
    int dataSegementBaseAddress;

    void handleLabels(std::string &line);
    void handleInstruction(std::string line);
    void handleDataEntries(std::string &line);
    void handleArguments();
    void printInstructions();
};
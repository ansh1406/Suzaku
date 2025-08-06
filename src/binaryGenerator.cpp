#include "binaryGenerator.hpp"
#include <cstdint>

void BinaryGenerator::generateBinary()
{
    std::string line;
    std::vector<std::string> code;
    while (std::getline(irFile, line))
    {
        line.erase(0, line.find_first_not_of(" \t\r\n"));
        line.erase(line.find_last_not_of(" \t\r\n") + 1);
        if (line.empty() || line[0] == '#')
        {
            continue;
        }
        code.push_back(line);
    }
    for (auto &line : code)
    {
        std::string word = line.substr(0, line.find(' '));
        if (binarygenerator::instructionSet.find(word) != binarygenerator::instructionSet.end())
        {
            handleInstruction(line);
        }
        else if (word.back() == ':')
        {
            handleLabels(line);
        }
    }
    dataSegementBaseAddress = currentRamHead;
    for (auto &line : code)
    {
        if (line.substr(0, 5) == "DATA-")
        {
            handleDataEntries(line);
        }
    }
    handleArguments();
    printInstructions();
}

void BinaryGenerator::handleInstruction(std::string line)
{
    std::vector<std::string> tokens;
    while (!line.empty())
    {
        size_t pos = line.find(' ');
        if (pos == std::string::npos)
        {
            tokens.push_back(line);
            break;
        }
        tokens.push_back(line.substr(0, pos));
        line.erase(0, pos + 1);
    }
    instructions[currentRamHead++] = binarygenerator::instructionSet.at(tokens[0]);
    if (tokens.size() > 1)
    {
        instructions[currentRamHead++] = "arg:" + tokens[1];
    }
}

void BinaryGenerator::handleArguments()
{
    for (auto &instruction : instructions)
    {
        if (instruction.second.find("arg:") == std::string::npos)
        {
            continue;
        }
        std::string arg = instruction.second.substr(4); // Remove "arg:"
        if (isdigit(arg[0]) || arg[0] == '-')
        {
            std::stringstream ss(arg);
            int value;
            ss >> value;
            uint16_t val = static_cast<uint16_t>(value);
            std::stringstream hexStream;
            hexStream << std::setw(4) << std::setfill('0') << std::hex << std::uppercase << val;
            instructions[instruction.first] = hexStream.str();
        }
        else if (arg[0] == '\'')
        {
            int value = static_cast<int>(arg[1]);
            std::stringstream hexStream;
            hexStream << std::setw(4) << std::setfill('0') << std::hex << std::uppercase << value;
            instructions[instruction.first] = hexStream.str();
        }
        else if (arg.substr(0, 4) == "DATA")
        {
            int address = dataSegementBaseAddress + stoi(arg.substr(5));
            std::stringstream hexStream;
            hexStream << std::setw(4) << std::setfill('0') << std::hex << std::uppercase << address;
            instructions[instruction.first] = hexStream.str();
        }
        else if (labels.find(arg) != labels.end())
        {
            int address = labels[arg];
            std::stringstream hexStream;
            hexStream << std::setw(4) << std::setfill('0') << std::hex << std::uppercase << address;
            instructions[instruction.first] = hexStream.str();
        }
        else
        {
            throw std::runtime_error("Unknown argument: " + arg);
        }
    }
}

void BinaryGenerator::handleLabels(std::string &line)
{
    std::string label = line.substr(0, line.find(':'));
    labels[label] = currentRamHead;
}

void BinaryGenerator::handleDataEntries(std::string &line)
{
    // DATA-<address> <value>
    int address = dataSegementBaseAddress + stoi(line.substr(line.find("DATA-") + 5, line.find(' ') - (line.find("DATA-") + 5)));
    std::string dataEntry = line.substr(line.find(' ') + 1);
    int value;
    std::stringstream ss(dataEntry);
    ss >> value;
    std::stringstream hexStream;
    hexStream << std::setw(4) << std::setfill('0') << std::hex << std::uppercase << value;
    instructions[address] = hexStream.str();
}

void BinaryGenerator::printInstructions()
{
    for (const auto &instruction : instructions)
    {
        std::string value = instruction.second;
        uint16_t num = 0;
        std::stringstream ss;
        ss << std::hex << value;
        ss >> num;
        binaryFile.write(reinterpret_cast<const char *>(&num), sizeof(num));
    }
}
#include <fstream>
#include <iostream>

#include "Lexer.hpp"
#include "Parser.hpp"
#include "PrettyPrinter.hpp"
#include "Token.hpp"
#include "WellFormedCheck.hpp"

int main() {

    drm::Lexer lexer;

    std::string input = "";
    //std::getline(std::cin, input);

    std::ifstream file("Test.drm");
    while (!file.eof())
        input += file.get();
    input.pop_back();
    
    std::cout << input << "\n\n";

    drm::TokenStream stream;

    try {
        stream = lexer.lex(input);
    } catch (drm::LexerError e) {
        std::cerr << "Error at " << e.index << " of length " << e.length << ": " << e.message << std::endl;
        std::cerr << "Error is at substring '" << input.substr(e.index, e.length) << "'" << std::endl;
        return 1;
    }

    for (std::size_t i = 0; i < stream.size(); i++)
        std::cout << static_cast<std::string>(stream.at(i)) << std::endl;
    std::cout << std::endl;
    
    drm::Parser parser;
    parser.load(stream);

    drm::PrettyPrinter printer;

    drm::Program prog;

    try {
        prog = parser.parse();
        std::cout << printer.visitProgram(prog) << std::endl;
    } catch (const drm::ParserError &e) {
        std::cerr << "Error of " << static_cast<std::string>(e.token) << ": " << e.message << std::endl;
        return 1;
    }

    drm::WellFormedCheck checker;
    try {
        checker.visitProgram(prog);
    } catch (const drm::BadlyFormedError &e) {
        std::cerr << "Error of " << static_cast<std::string>(e.token) << ": " << e.message << std::endl;
        return 1;
    }

    std::cout << "Program is well-formed" << std::endl;

    return 0;
}
#include <fstream>
#include <iostream>

#include "Lexer.hpp"
#include "Parser.hpp"
#include "PrettyPrinter.hpp"
#include "Token.hpp"
#include "TypeChecker.hpp"

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

    // for (std::size_t i = 0; i < stream.size(); i++)
    //     std::cout << static_cast<std::string>(stream.at(i)) << std::endl;
    // std::cout << std::endl;
    
    drm::Parser parser;
    parser.load(stream);

    drm::Program prog;

    try {
        prog = parser.parse();
    } catch (const drm::ParserError &e) {
        std::cerr << "Error of " << static_cast<std::string>(e.token) << ": " << e.message << std::endl;
        return 1;
    }

    drm::PrettyPrinter printer;
    std::cout << printer.visitGStmtProgram(prog) << std::endl;

    drm::TypeChecker type_checker;    
    std::string errors = "";

    try {
        type_checker.visitGStmtProgram(prog);
        errors = type_checker.errorMessages();
    } catch (std::exception &e) {
        std::cout << "Typechecking Exception: " << e.what() << std::endl;
        return 1;
    }

    if (errors.empty()) {
        std::cout << "Well-formed Program" << std::endl;
    } else {
        std::cout << "Program not well-formed:\n" << errors << std::endl;
        return 1;
    }

    return 0;
}
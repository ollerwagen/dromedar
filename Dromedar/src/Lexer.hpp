#pragma once

#include <map>
#include <regex>
#include <string>
#include <unordered_map>

#include "Token.hpp"

namespace drm {

    class Lexer {

    private:

        static const std::vector<Token::Type> DIRECT_TO_TOKEN;

        static const std::map<Token::Type, std::string> REGEXSTRINGS;
        static const std::map<Token::Type, std::regex>  REGEXES;

        static const std::unordered_map<std::string, Token::Type> KEYWORDS;

    public:

        std::string COMBINED_REGEXES;

    public:

        Lexer();

        TokenStream lex(std::string stream);

    }; // class Lexer
    

    struct LexerError {

        std::size_t index, length;
        std::string message;

        LexerError(std::size_t index, std::size_t length, std::string message) {
            this->index   = index;
            this->length  = length;
            this->message = message;
        }

    }; // struct LexerError

} // namespace drm
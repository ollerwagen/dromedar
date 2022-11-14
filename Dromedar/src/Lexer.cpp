#include <sstream>

// debug
#include <iostream>

#include "Lexer.hpp"

namespace drm {

    const std::vector<Token::Type> Lexer::DIRECT_TO_TOKEN = {
        Token::Type::DASH,     Token::Type::BANG,
        Token::Type::STARSTAR,
        Token::Type::STAR,     Token::Type::PLUS,
        Token::Type::LSHIFT,   Token::Type::ASHIFT, Token::Type::RSHIFT,
        Token::Type::BITAND,   Token::Type::XOR,    Token::Type::BITOR,
        Token::Type::ASSIGN,   Token::Type::COLON,  Token::Type::COMMA,
        Token::Type::ARROW,
        Token::Type::LPAREN,   Token::Type::RPAREN
    };

    // lexing order is important: every Flt token starts with a potential Int token
    const std::map<Token::Type, std::string> Lexer::REGEXSTRINGS = {
        { Token::Type::IDENTIFIER,   "[a-zA-Z][a-zA-Z0-9_]*"         },
        { Token::Type::LITERAL_FLT,  "[0-9]+\\.[0-9]+"               },
        { Token::Type::LITERAL_INT,  "[1-9][0-9]*"                   },
        { Token::Type::LITERAL_CHAR, "'([^'\\\\]|(\\\\[\\\\nrt']))'" },
        { Token::Type::DASH,         "-"                             },
        { Token::Type::BANG,         "!"                             },
        { Token::Type::STARSTAR,     "\\*\\*"                        },
        { Token::Type::STAR,         "\\*"                           },
        { Token::Type::PLUS,         "\\+"                           },
        { Token::Type::LSHIFT,       "<<"                            },
        { Token::Type::RSHIFT,       ">>"                            },
        { Token::Type::ASHIFT,       ">>>"                           },
        { Token::Type::BITAND,       "&"                             },
        { Token::Type::XOR,          "\\^"                           },
        { Token::Type::BITOR,        "\\|"                           },
        { Token::Type::ASSIGN,       ":="                            },
        { Token::Type::COLON,        ":"                             },
        { Token::Type::COMMA,        "\\,"                           },
        { Token::Type::ARROW,        "\\->"                          },
        { Token::Type::LPAREN,       "\\("                           },
        { Token::Type::RPAREN,       "\\)"                           },
    };

    #define TRANSLATE(NAME) { Token::Type::NAME, std::regex(REGEXSTRINGS.at(Token::Type::NAME)) }
    const std::map<Token::Type, std::regex> Lexer::REGEXES = {
        TRANSLATE(IDENTIFIER),
        TRANSLATE(LITERAL_FLT), TRANSLATE(LITERAL_INT),TRANSLATE(LITERAL_CHAR), TRANSLATE(DASH),
        TRANSLATE(BANG), TRANSLATE(STARSTAR), TRANSLATE(STAR), TRANSLATE(PLUS),
        TRANSLATE(LSHIFT), TRANSLATE(RSHIFT), TRANSLATE(ASHIFT),
        TRANSLATE(BITAND), TRANSLATE(XOR), TRANSLATE(BITOR),
        TRANSLATE(ASSIGN), TRANSLATE(COLON), TRANSLATE(COMMA),
        TRANSLATE(ARROW),
        TRANSLATE(LPAREN), TRANSLATE(RPAREN)
    };
    #undef TRANSLATE

    const std::unordered_map<std::string, Token::Type> Lexer::KEYWORDS = {
        { "false",  Token::Type::DIRECTTRANSLATE_FALSE },
        { "true",   Token::Type::DIRECTTRANSLATE_TRUE  },
        { "global", Token::Type::KEY_GLOBAL            },
        { "let",    Token::Type::KEY_LET               },
        { "mut",    Token::Type::KEY_MUT               },
        { "fn",     Token::Type::KEY_FN                },
        { "int",    Token::Type::KEY_INT               },
        { "flt",    Token::Type::KEY_FLT               },
        { "char",   Token::Type::KEY_CHAR              },
        { "bool",   Token::Type::KEY_BOOL              },
        { "void",   Token::Type::KEY_VOID              },
        { "if",     Token::Type::KEY_IF                },
        { "elif",   Token::Type::KEY_ELIF              },
        { "else",   Token::Type::KEY_ELSE              },
        { "while",  Token::Type::KEY_WHILE             },
        { "do",     Token::Type::KEY_DO                },
        { "return", Token::Type::KEY_RETURN            }
    };

    Lexer::Lexer() {
        static const std::vector<Token::Type> REGEX_ORDER = {
            Token::Type::IDENTIFIER,
            Token::Type::LITERAL_FLT, Token::Type::LITERAL_INT, Token::Type::LITERAL_CHAR,
            Token::Type::ARROW,
            Token::Type::DASH, Token::Type::DASH, Token::Type::STARSTAR, Token::Type::STAR, Token::Type::PLUS,
            Token::Type::LSHIFT, Token::Type::ASHIFT, Token::Type::RSHIFT,
            Token::Type::BITAND, Token::Type::XOR, Token::Type::BITOR,
            Token::Type::ASSIGN, Token::Type::COLON, Token::Type::COMMA,
            Token::Type::LPAREN, Token::Type::RPAREN
        };

        std::stringstream ss;
        for (const Token::Type t : REGEX_ORDER)
            ss << "(" << REGEXSTRINGS.at(t) << ")|";
        ss << "((\\s|(#[^\\n]*))+)";
        COMBINED_REGEXES = ss.str();
    }

    TokenStream Lexer::lex(std::string stream) {
        stream = '\n' + stream; // ensures that program starts with whitespace for correct indentation handling

        std::regex regex(COMBINED_REGEXES);

        auto begin = std::sregex_iterator(stream.begin(), stream.end(), regex);
        auto end   = std::sregex_iterator();

        TokenStream res;
        std::size_t index = 0;

        std::vector<std::string> whitespace_levels;
        bool has_found_first_line_indentation = false;

        for (auto it = begin; it != end; ++it) {
            std::smatch match = *it;
            std::string match_str = match.str();

            std::size_t token_start = match.position(), token_length = match.length();

            if (index != static_cast<std::size_t>(match.position())) {
                throw LexerError(token_start, token_length, "Unexpected Token");
            }

            index += match.length();

            if (std::regex_match(match_str, REGEXES.at(Token::Type::IDENTIFIER))) {
                if (KEYWORDS.find(match_str) != KEYWORDS.end()) {
                    Token::Type type = KEYWORDS.at(match_str);
                    switch (type) {
                        case Token::Type::DIRECTTRANSLATE_FALSE:
                            res.push_back(Token::BOOL_LITERAL(match_str, token_start, token_length, false));
                            break;
                        case Token::Type::DIRECTTRANSLATE_TRUE:
                            res.push_back(Token::BOOL_LITERAL(match_str, token_start, token_length, true));
                            break;
                        default: // normal keyword
                            res.push_back(Token::OF_TYPE(match_str, token_start, token_length, type));
                            break;
                    }
                } else { // identifier
                    res.push_back(Token::ID(match_str, token_start, token_length, match_str));
                }
            } else if (std::regex_match(match_str, REGEXES.at(Token::Type::LITERAL_FLT))) {
                Token::Flt num;
                try {
                    num = std::stod(match_str);
                } catch (std::exception &e) {
                    throw LexerError(token_start, token_length, "Cannot convert to 64-bit floating point number");
                }
                res.push_back(Token::FLT_LITERAL(match_str, token_start, token_length, num));
            } else if (std::regex_match(match_str, REGEXES.at(Token::Type::LITERAL_INT))) {
                Token::Int num;
                try {
                    num = std::stoll(match_str);
                } catch (std::exception &e) {
                    throw LexerError(token_start, token_length, "Cannot convert to 64-bit integer");
                }
                res.push_back(Token::INT_LITERAL(match_str, token_start, token_length, num));
            } else if (std::regex_match(match_str, REGEXES.at(Token::Type::LITERAL_CHAR))) {
                switch (match_str.length()) {
                    case 3: // simple 'c' character
                        res.push_back(Token::CHAR_LITERAL(match_str, token_start, token_length, match_str.at(1)));
                        break;
                    case 4: // escape character
                        if (match_str.at(1) != '\\')
                            throw LexerError(token_start, token_length, "Not a character");
                        switch (match_str.at(2)) {
                            case '\\': res.push_back(Token::CHAR_LITERAL(match_str, token_start, token_length, '\\')); break;
                            case '\'': res.push_back(Token::CHAR_LITERAL(match_str, token_start, token_length, '\'')); break;
                            case 'n' : res.push_back(Token::CHAR_LITERAL(match_str, token_start, token_length, '\n')); break;
                            case 't' : res.push_back(Token::CHAR_LITERAL(match_str, token_start, token_length, '\t')); break;
                            case 'r' : res.push_back(Token::CHAR_LITERAL(match_str, token_start, token_length, '\r')); break;
                            default  : throw LexerError(token_start, token_length, "Not an escape character");
                        }
                        break;
                    default:
                        throw LexerError(token_start, token_length, "Not a character");
                } 
            } else {
                for (const Token::Type t : DIRECT_TO_TOKEN) {
                    if (std::regex_match(match_str, REGEXES.at(t))) {
                        res.push_back(Token::OF_TYPE(match_str, token_start, token_length, t));
                        goto AFTER_WHITESPACE;
                    }
                }
                
                // whitespace handling
                {
                    std::size_t last_newline = match_str.find_last_of('\n');
                    std::string relevant_whitespace = match_str;

                    if (has_found_first_line_indentation) {
                        if (last_newline == match_str.npos)
                            goto AFTER_WHITESPACE;
                        relevant_whitespace = match_str.substr(last_newline + 1);
                    } else {
                        has_found_first_line_indentation = true;
                        relevant_whitespace = match_str.substr(last_newline + 1);
                        whitespace_levels = { relevant_whitespace };
                        res.push_back(Token::SPACE_LEVEL(match_str, token_start, token_length, 0));
                        goto AFTER_WHITESPACE;
                    }

                    // whitespace_levels.size() >= 1, even if global indentation level is ""
                    for (std::size_t i = 0; i < whitespace_levels.size(); i++) {
                        if (relevant_whitespace == whitespace_levels.at(i)) {
                            res.push_back(Token::SPACE_LEVEL(match_str.substr(last_newline + 1), token_start, token_length, i));
                            whitespace_levels.resize(i + 1);
                            goto AFTER_WHITESPACE;
                        } else if (relevant_whitespace.substr(0, whitespace_levels.at(i).length()) == whitespace_levels.at(i)) {
                            relevant_whitespace = relevant_whitespace.substr(whitespace_levels.at(i).length());
                        } else {
                            throw LexerError(token_start, token_length, "Significant indentation must match surroundings");
                        }
                    }
                    whitespace_levels.push_back(relevant_whitespace);
                    res.push_back(Token::SPACE_LEVEL(match_str.substr(last_newline + 1), token_start, token_length, whitespace_levels.size() - 1));
                }

            AFTER_WHITESPACE:;
            }
        }

        if (index < stream.length())
            throw LexerError(index, stream.length() - index, "Unexpected Token");
        
        while (!res.empty() && res.back().type == Token::Type::WHITESPACE)
            res.pop_back();

        res.push_back(Token::END_OF_FILE("<eof>", stream.length(), 1));
        return res;
    }

} // namespace drm 
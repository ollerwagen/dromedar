#pragma once

#include <boost/variant.hpp>
#include <cstdint>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace drm {

    struct Token {

        using Int  = std::int64_t;
        using Flt  = double;
        using Char = std::string::value_type;
        using Bool = bool;

        using Id   = std::string;

        enum Type {
            LITERAL_INT,
            LITERAL_FLT,
            LITERAL_CHAR,
            LITERAL_BOOL,

            IDENTIFIER,

            BANG,           // !
            STARSTAR,       // **
            STAR,           // *
            PLUS,           // +
            DASH,           // -
            LSHIFT,         // <<
            RSHIFT,         // >>
            ASHIFT,         // >>>
            BITAND,         // &
            XOR,            // ^
            BITOR,          // |

            ASSIGN,         // :=
            COLON,          // :
            COMMA,          // ,

            ARROW,          // ->

            LPAREN,         // (
            RPAREN,         // )
            
            $,              // <eof>

            WHITESPACE,     // level of whitespace (0-...)

            DIRECTTRANSLATE_TRUE,
            DIRECTTRANSLATE_FALSE,

            KEY_GLOBAL,
            KEY_FN,
            KEY_LET,
            KEY_MUT,

            KEY_INT,
            KEY_FLT,
            KEY_CHAR,
            KEY_BOOL,

            KEY_VOID,

            KEY_IF,
            KEY_ELIF,
            KEY_ELSE,

            KEY_WHILE,
            KEY_DO,

            KEY_RETURN
        } type;

        std::string lexeme;

        std::size_t start, length;

        using LiteralType =  boost::variant<Int, Flt, Char, Bool, Id>;
        LiteralType l;

        inline ~Token() {}

        static inline Token INT_LITERAL (std::string lexeme, std::size_t start, std::size_t length, Int i)  { return { LITERAL_INT,  lexeme, start, length, LiteralType(i) }; }
        static inline Token FLT_LITERAL (std::string lexeme, std::size_t start, std::size_t length, Flt f)  { return { LITERAL_FLT,  lexeme, start, length, LiteralType(f) }; }
        static inline Token CHAR_LITERAL(std::string lexeme, std::size_t start, std::size_t length, Char c) { return { LITERAL_CHAR, lexeme, start, length, LiteralType(c) }; }
        static inline Token BOOL_LITERAL(std::string lexeme, std::size_t start, std::size_t length, Bool b) { return { LITERAL_BOOL, lexeme, start, length, LiteralType(b) }; }
        static inline Token ID          (std::string lexeme, std::size_t start, std::size_t length, Id s)   { return { IDENTIFIER,   lexeme, start, length, LiteralType(s) }; }
        static inline Token END_OF_FILE (std::string lexeme, std::size_t start, std::size_t length)         { return { $,            lexeme, start, length, LiteralType(false) }; }
        static inline Token SPACE_LEVEL (std::string lexeme, std::size_t start, std::size_t length, Int l)  { return { WHITESPACE,   lexeme, start, length, LiteralType(l) }; }
        static inline Token OF_TYPE     (std::string lexeme, std::size_t start, std::size_t length, Type t) { return { t,            lexeme, start, length, LiteralType(false) }; }

        inline operator std::string() const {
            static const std::unordered_map<Type, std::string> TYPES = {
                { LITERAL_INT,  "LITERAL_INT"  }, { LITERAL_FLT,  "LITERAL_FLT"  },
                { LITERAL_CHAR, "LITERAL_CHAR" }, { LITERAL_BOOL, "LITERAL_BOOL" },
                { IDENTIFIER,   "IDENTIFIER"   },
                { DASH,         "DASH"         }, { BANG,         "BANG"         },
                { STARSTAR,     "STARSTAR"     },
                { STAR,         "STAR"         }, { PLUS,         "PLUS"         },
                { LSHIFT,       "LSHIFT"       }, { ASHIFT,       "ASHIFT"       }, { RSHIFT, "RSHIFT" },
                { BITAND,       "BITAND"       }, { XOR,          "XOR"          }, { BITOR,  "BITOR"  },
                { ASSIGN,       "ASSIGN"       }, { COLON,        "COLON"        }, { COMMA,  "COMMA"  },
                { ARROW,        "ARROW"        },
                { LPAREN,       "LPAREN"       }, { RPAREN,       "RPAREN"       },
                { $,            "EOF"          }, { WHITESPACE,   "INDENT"       },
                { KEY_GLOBAL,   "GLOBAL"       }, { KEY_FN,       "FN"           }, { KEY_LET,  "LET"  }, { KEY_MUT,  "MUT" },
                { KEY_INT,      "INT"          }, { KEY_FLT,      "FLT"          }, { KEY_CHAR, "CHAR" }, { KEY_BOOL, "BOOL"},
                { KEY_VOID,     "VOID"         },
                { KEY_IF,       "IF"           }, { KEY_ELIF,     "ELIF"         }, { KEY_ELSE,     "ELSE"         },
                { KEY_WHILE,    "WHILE"        }, { KEY_DO,       "DO"           },
                { KEY_RETURN,   "RETURN"       }
            };
            
            std::stringstream stream;
            stream << "[" << TYPES.at(type);
            switch (type) {
                case LITERAL_INT: case LITERAL_FLT: case LITERAL_CHAR: case LITERAL_BOOL:
                case IDENTIFIER:
                case WHITESPACE:
                    stream << " " << l; break;
                default:
                    break;
            }
            stream << "]";
            return stream.str();
        }

        class IntVisitor : public boost::static_visitor<Int> {
        public:
            Int operator()(Int  i) const { return i; }
 
            Int operator()(__attribute__((unused)) Flt  f) const { return 0; }
            Int operator()(__attribute__((unused)) Char c) const { return 0; }
            Int operator()(__attribute__((unused)) Bool b) const { return 0; }
            Int operator()(__attribute__((unused)) Id   i) const { return 0; }
        };

        Int getInt() const {
            return boost::apply_visitor(IntVisitor(), l);
        }

    }; // struct Token

    using TokenStream = std::vector<Token>;

} // namespace drm
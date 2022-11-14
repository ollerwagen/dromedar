#pragma once

#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

#include "AST.hpp"
#include "Token.hpp"

namespace drm {

    class Parser {
    
    private:

        using Type = Token::Type;

    private:

        static const std::vector<std::vector<Type>> PRECEDENCES;
        static const std::unordered_map<Type, bool> ASSOCIATIVITIES;

    private:

        TokenStream stream;
        std::size_t index;

        std::size_t indent;

    private:

        Type peek() const;
        Token next() const;
        Token advance();
        void reverse();
        bool match(Type t);
        bool match(const std::function<bool(Type)> &f);
        bool matchIndent();
        bool matchIndentAnd(Type t);
        bool matchStay(Type t);
        bool matchStay(const std::function<bool(Type)> &f);
        bool matchIndentStay();

        void expectIndent(void);
        Token expect(Type t, const std::string &message);
        Token expect(const std::function<bool(Type)> &f, const std::string &message);

        Program parseProgram(void);

        std::shared_ptr<GlobalStatement> parseGlobalStatement(void);

        std::shared_ptr<GlobalStatement> parseGlobalDeclaration(void);
        std::shared_ptr<GlobalStatement> parseGlobalFunctionDefinition(void);

        std::shared_ptr<Statement> parseStatement(void);

        std::shared_ptr<Statement> parseVarDeclaration(void);
        std::shared_ptr<Statement> parseAssignStatement(void);
        std::shared_ptr<Statement> parseIfStatement(Type t = Type::KEY_IF);
        std::shared_ptr<Statement> parseWhileStatement(void);
        std::shared_ptr<Statement> parseDoWhileStatement(void);
        std::shared_ptr<Statement> parseReturnStatement(void);

        std::shared_ptr<LHS> parseLHS(void);

        std::shared_ptr<TypeExpression> parseTypeExpression(bool return_type);
        std::shared_ptr<TypeExpression> parsePrimitiveTypeExpression(void);

        std::shared_ptr<Expression> parseExpression();

        std::shared_ptr<Expression> parseSimpleExpression();
        std::shared_ptr<Expression> parseUnaryExpression();
        std::shared_ptr<Expression> parseBinaryExpression(const std::size_t precindex);

        Block parseBlock(void);

    public:

        Parser();

        void load(const TokenStream &stream);

        Program parse(void) { return parseProgram(); }

    }; // class Parser

    struct ParserError {
        Token token;
        std::string message;

        ParserError(const Token &token, const std::string &message) {
            this->token = token;
            this->message = message;
        }
    };

} // namespace drm
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
        static const std::vector<Type> COMPARATORS;

    private:

        TokenStream stream;
        std::size_t index;

        std::size_t indent;

    private:

        std::size_t getIndex() const;
        std::size_t getLength(std::size_t start) const;
        Type peek() const;
        Token previous() const;
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

        GStmtPtr parseGlobalStatement(void);

        GStmtPtr parseGlobalDeclaration(void);
        GStmtPtr parseGlobalFunctionDefinition(void);

        StmtPtr parseStatement(void);

        StmtPtr parseVarDeclaration(void);
        StmtPtr parseAssignOrExpressionStatement(void);
        StmtPtr parseIfStatement(Type t = Type::KEY_IF);
        StmtPtr parseWhileStatement(void);
        StmtPtr parseDoWhileStatement(void);
        StmtPtr parseReturnStatement(void);

        TypePtr parseTypeExpression(bool return_type);
        TypePtr parsePrimitiveTypeExpression(void);

        ExprPtr parseExpression();

        ExprPtr parseSimpleExpression();
        ExprPtr parseApplicationExpression(const ExprPtr &lhs);
        ExprPtr parseUnaryExpression();
        ExprPtr parseBinaryExpression(const std::size_t precindex);

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
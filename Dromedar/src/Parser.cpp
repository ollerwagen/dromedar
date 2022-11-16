#include "Parser.hpp"

// debug
#include <iostream>

namespace drm {

    const std::vector<std::vector<Parser::Type>> Parser::PRECEDENCES = {
        { Type::LOGOR },
        { Type::LOGAND },
        { Type::EQUAL, Type::NOT_EQUAL, Type::GREATER, Type::LESS, Type::GREATER_EQ, Type::LESS_EQ },
        { Type::BITOR },
        { Type::XOR },
        { Type::BITAND },
        { Type::LSHIFT, Type::RSHIFT, Type::ASHIFT },
        { Type::PLUS, Type::DASH },
        { Type::STAR },
        { Type::STARSTAR }
    };

    const std::unordered_map<Parser::Type, bool> Parser::ASSOCIATIVITIES = {
        { Type::STARSTAR, false },
        { Type::STAR, true },
        { Type::PLUS, true }, { Type::DASH, true },
        { Type::LSHIFT, true }, { Type::RSHIFT, true }, { Type::ASHIFT, true },
        { Type::BITAND, true }, { Type::XOR, true }, { Type::BITOR, true },
        { Type::LOGAND, true }, { Type::LOGOR, true }
    };

    const std::vector<Parser::Type> Parser::COMPARATORS = {
        Type::EQUAL,   Type::NOT_EQUAL,
        Type::LESS,    Type::GREATER,
        Type::LESS_EQ, Type::GREATER_EQ
    };

    Parser::Parser() {}

    void Parser::load(const TokenStream &stream) {
        this->stream = stream;
        index = 0;
        indent = 0;
    }

    Program Parser::parseProgram() {
        Program res;
        while (!match(Type::$)) {
            res.push_back(parseGlobalStatement());
        }
        return res;
    }

    std::shared_ptr<GlobalStatement> Parser::parseGlobalStatement() {
        expectIndent();
        switch (peek()) {
            case Type::KEY_GLOBAL: return parseGlobalDeclaration();
            case Type::KEY_FN:     return parseGlobalFunctionDefinition();
            default:               throw ParserError(advance(), "expect a global declaration");
        }
    }

    std::shared_ptr<GlobalStatement> Parser::parseGlobalDeclaration() {
        expect(Type::KEY_GLOBAL, "global variable declaration must start with keyword 'global'");
        bool mut = match(Type::KEY_MUT);
        Token id = expect(Type::IDENTIFIER, "expect global variable name");
        if (match(Type::COLON)) {
            std::shared_ptr<TypeExpression> type = parseTypeExpression(false);
            expect(Type::ASSIGN, "global variable must be assigned a value");
            std::shared_ptr<Expression> expr = parseExpression();
            return GTypeVarDeclStatement::of(id, mut, type, expr);
        } else {
            expect(Type::ASSIGN, "global variable must be assigned a value");
            std::shared_ptr<Expression> expr = parseExpression();
            return GVarDeclStatement::of(id, mut, expr);
        }
    }

    std::shared_ptr<GlobalStatement> Parser::parseGlobalFunctionDefinition() {
        expect(Type::KEY_FN, "global function definition must start with keyword 'fn'");
        Token id = expect(Type::IDENTIFIER, "expect function name");
        std::vector<std::pair<Token, std::shared_ptr<TypeExpression>>> args;
        if (!match(Type::ARROW)) {
            expect(Type::COLON, "function argument list must start with a colon");
            do {
                Token name = expect(Type::IDENTIFIER, "function argument must have a name");
                expect(Type::COLON, "argument name and type are separated by a colon");
                std::shared_ptr<TypeExpression> type = parseTypeExpression(false);
                if (peek() != Type::ARROW)
                    expect(Type::COMMA, "commas must separate function arguments");
                args.push_back(std::make_pair(name, type));
            } while (!match(Type::ARROW));
        }
        std::shared_ptr<TypeExpression> rettype = parseTypeExpression(true);
        Block block = parseBlock();
        return GFDeclStatement::of(id, args, rettype, block);
    }

    Block Parser::parseBlock() {
        Block res;
        ++indent;
        while (matchIndentStay())
            res.push_back(parseStatement());
        --indent;
        return res;
    }

    std::shared_ptr<Statement> Parser::parseStatement() {
        expectIndent();
        switch (peek()) {
            case Type::KEY_LET:    return parseVarDeclaration();
            case Type::KEY_IF:     return parseIfStatement();
            case Type::KEY_WHILE:  return parseWhileStatement();
            case Type::KEY_DO:     return parseDoWhileStatement();
            case Type::KEY_RETURN: return parseReturnStatement();
            default:               return parseAssignStatement();
        }
    }

    std::shared_ptr<Statement> Parser::parseVarDeclaration() {
        expect(Type::KEY_LET, "local declaration must start with keyword 'let'");
        bool mut = match(Type::KEY_MUT);
        Token id = expect(Type::IDENTIFIER, "expect variable name");
        if (match(Type::COLON)) {
            std::shared_ptr<TypeExpression> type = parseTypeExpression(false);
            expect(Type::ASSIGN, "local variable must be assigned a value");
            std::shared_ptr<Expression> expr = parseExpression();
            return TypeVarDeclStatement::of(id, mut, type, expr);
        } else {
            expect(Type::ASSIGN, "local variable must be assigned a value");
            std::shared_ptr<Expression> expr = parseExpression();
            return VarDeclStatement::of(id, mut, expr);
        }
    }

    std::shared_ptr<Statement> Parser::parseAssignStatement() {
        std::shared_ptr<LHS> lhs = parseLHS();
        Token op = expect(Type::ASSIGN, "assignment must use ':=' operator");
        std::shared_ptr<Expression> expr = parseExpression();
        return AssignStatement::of(lhs, op, expr);
    }

    std::shared_ptr<Statement> Parser::parseIfStatement(Type t) {
        Token key = expect(t, "if statement must start with keyword 'if'");
        std::shared_ptr<Expression> condition = parseExpression();
        Block taken = parseBlock();
        if (matchIndentAnd(Type::KEY_ELIF)) {
            reverse();
            return IfStatement::of(key, condition, taken, { parseIfStatement(Type::KEY_ELIF) });
        } else if (matchIndentAnd(Type::KEY_ELSE)) {
            return IfStatement::of(key, condition, taken, parseBlock());
        } else {
            return IfStatement::of(key, condition, taken, {});
        }
    }

    std::shared_ptr<Statement> Parser::parseWhileStatement() {
        Token key = expect(Type::KEY_WHILE, "while statement must start with keyword 'while'");
        std::shared_ptr<Expression> condition = parseExpression();
        Block body = parseBlock();
        return WhileStatement::of(key, condition, body);
    }

    std::shared_ptr<Statement> Parser::parseDoWhileStatement() {
        expect(Type::KEY_DO, "do-while statement must start with keyword 'do'");
        Block body = parseBlock();
        if (!matchIndentAnd(Type::KEY_WHILE))
            throw ParserError(advance(), "do-while statement must end with <while %expr>");
        Token key = previous();
        std::shared_ptr<Expression> condition = parseExpression();
        return DoWhileStatement::of(condition, body, key);
    }

    std::shared_ptr<Statement> Parser::parseReturnStatement() {
        Token key = expect(Type::KEY_RETURN, "return statement must start with keyword 'start'");
        if (peek() == Type::$ || peek() == Type::WHITESPACE)
            return ReturnStatement::of(key);
        else
            return ReturnStatement::of(key, parseExpression());
    }

    std::shared_ptr<LHS> Parser::parseLHS() {
        return VariableLHS::of(expect(Type::IDENTIFIER, "left-hand side of assignment must be a name"));
    }

    std::shared_ptr<TypeExpression> Parser::parseTypeExpression(bool return_type) {
        if (return_type && matchStay(Type::KEY_VOID))
            return PrimitiveTypeExpression::ofTypename(advance());
        return parsePrimitiveTypeExpression();
    }

    std::shared_ptr<TypeExpression> Parser::parsePrimitiveTypeExpression() {
        return PrimitiveTypeExpression::ofTypename(expect([] (Type t) {
            return t == Token::Type::KEY_INT || t == Token::Type::KEY_FLT || t == Token::Type::KEY_CHAR || t == Token::Type::KEY_BOOL; },
            "expect a primitive type"));
    }

    std::shared_ptr<Expression> Parser::parseExpression() {
        return parseBinaryExpression(0);
    }

    std::shared_ptr<Expression> Parser::parseSimpleExpression() {
        if (match(Type::LPAREN)) {
            auto exp = parseExpression();
            expect(Type::RPAREN, "expected ')'");
            return exp;
        } else {
            Token t = expect([] (Type t) { return t == Type::LITERAL_INT  ||
                                                  t == Type::LITERAL_FLT  ||
                                                  t == Type::LITERAL_CHAR ||
                                                  t == Type::LITERAL_BOOL ||
                                                  t == Type::IDENTIFIER; },
                                                  "expected a literal or variable name");
            if (matchStay([] (Type t) { return t == Type::LPAREN; })) {
                return parseApplicationExpression(LiteralExpression::of(t));
            } else {
                return LiteralExpression::of(t);
            }
        }
    }

    std::shared_ptr<Expression> Parser::parseApplicationExpression(const std::shared_ptr<Expression> &lhs) {
        if (matchStay(Type::LPAREN)) {
            Token lparen = advance();
            std::vector<std::shared_ptr<Expression>> args;
            while (!match(Type::RPAREN))
                args.push_back(parseExpression());
            return FunctionExpression::of(lhs, lparen, args);
        } else {
            throw ParserError(advance(), "unexpected token in application expression");
        }
    }

    std::shared_ptr<Expression> Parser::parseUnaryExpression() {
        if (matchStay([] (Type t) { return t == Type::BANG || t == Type::DASH; })) {
            Token op = advance();
            auto exp = parseUnaryExpression();
            return UnaryExpression::of(op, exp);
        } else {
            return parseSimpleExpression();
        }
    }

    std::shared_ptr<Expression> Parser::parseBinaryExpression(const std::size_t precindex) {
        if (precindex >= PRECEDENCES.size()) {
            return parseUnaryExpression();
        } else {
            auto exp = parseBinaryExpression(precindex + 1);
            while (matchStay([&precindex] (Type t) {
                return std::find(PRECEDENCES.at(precindex).begin(), PRECEDENCES.at(precindex).end(), t) != PRECEDENCES.at(precindex).end();
            })) {
                Token op = advance();
                if (std::find(COMPARATORS.begin(), COMPARATORS.end(), op.type) != COMPARATORS.end()) {
                    reverse();
                    std::vector<std::pair<Token, std::shared_ptr<Expression>>> comparisons;
                    while (matchStay([&precindex] (Type t) {
                        return std::find(PRECEDENCES.at(precindex).begin(), PRECEDENCES.at(precindex).end(), t) != PRECEDENCES.at(precindex).end();
                    })) {
                        Token op = advance();
                        comparisons.push_back(std::make_pair(op, parseBinaryExpression(precindex + 1)));
                    }
                    return ComparisonList::of(exp, comparisons);
                } else {
                    if (ASSOCIATIVITIES.at(op.type)) { // left-associative
                        auto rexp = parseBinaryExpression(precindex + 1);
                        exp = BinaryExpression::of(exp, op, rexp);
                    } else {
                        auto rexp = parseBinaryExpression(precindex);
                        return BinaryExpression::of(exp, op, rexp);
                    }
                }
            }
            return exp;
        }
    }

    Parser::Type Parser::peek() const {
        return stream.at(index).type;
    }

    Token Parser::previous() const {
        return stream.at(index - 1);
    }

    Token Parser::next() const {
        return stream.at(index);
    }

    Token Parser::advance() {
        return stream.at(index++);
    }

    void Parser::reverse() {
        --index;
    }

    bool Parser::matchStay(Type t) {
        return peek() == t;
    }

    bool Parser::matchStay(const std::function<bool(Type)> &f) {
        return f(peek());
    }

    bool Parser::matchIndentStay() {
        return peek() == Type::WHITESPACE && static_cast<std::size_t>(next().getInt()) == indent;
    }

    bool Parser::match(Type t) {
        if (peek() == t) {
            advance();
            return true;
        }
        return false;
    }

    bool Parser::match(const std::function<bool(Type)> &f) {
        if (f(peek())) {
            advance();
            return true;
        }
        return false;
    }

    bool Parser::matchIndent() {
        return peek() == Type::WHITESPACE && static_cast<std::size_t>(advance().getInt()) == indent;
    }

    bool Parser::matchIndentAnd(Type t) {
        std::size_t i = index;
        if (matchIndent() && match(t))
            return true;
        index = i;
        return false;
    }

    void Parser::expectIndent() {
        Token next = advance();
        if (next.type != Type::WHITESPACE && static_cast<std::size_t>(boost::apply_visitor(Token::IntVisitor(), next.l)) != indent)
            throw ParserError(next, "Expected " + std::to_string(indent) + " indentation levels");
    }

    Token Parser::expect(Type t, const std::string &message) {
        Token next = advance();
        if (next.type != t)
            throw ParserError(next, message);
        return next;
    }

    Token Parser::expect(const std::function<bool(Type)> &f, const std::string &message) {
        Token next = advance();
        if (!f(next.type))
            throw ParserError(next, message);
        return next;
    }

} // namespace drm
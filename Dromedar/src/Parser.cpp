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

    GStmtPtr Parser::parseGlobalStatement() {
        expectIndent();
        switch (peek()) {
            case Type::KEY_GLOBAL: return parseGlobalDeclaration();
            case Type::KEY_FN:     return parseGlobalFunctionDefinition();
            default:               throw ParserError(advance(), "expect a global declaration");
        }
    }

    GStmtPtr Parser::parseGlobalDeclaration() {
        std::size_t start = getIndex();

        expect(Type::KEY_GLOBAL, "global variable declaration must start with keyword 'global'");
        Token id = expect(Type::IDENTIFIER, "expect global variable name");
        expect(Type::ASSIGN, "global variable must be assigned a value");
        ExprPtr expr = parseExpression();
        return gstmt::VDecl::of(start, getLength(start), id.lexeme, expr);
    }

    GStmtPtr Parser::parseGlobalFunctionDefinition() {
        std::size_t start = getIndex();

        expect(Type::KEY_FN, "global function definition must start with keyword 'fn'");
        Token id = expect(Type::IDENTIFIER, "expect function name");
        std::vector<std::pair<std::string, TypePtr>> args;
        if (!match(Type::ARROW)) {
            expect(Type::COLON, "function argument list must start with a colon");
            do {
                Token name = expect(Type::IDENTIFIER, "function argument must have a name");
                expect(Type::COLON, "argument name and type are separated by a colon");
                TypePtr type = parseTypeExpression(false);
                if (peek() != Type::ARROW)
                    expect(Type::COMMA, "commas must separate function arguments");
                args.push_back(std::make_pair(name.lexeme, type));
            } while (!match(Type::ARROW));
        }
        TypePtr rettype = parseTypeExpression(true);
        Block block = parseBlock();
        return gstmt::FDecl::of(start, getLength(start), id.lexeme, rettype, args, block);
    }

    Block Parser::parseBlock() {
        Block res;
        ++indent;
        while (matchIndentStay()) {
            res.push_back(parseStatement());
        }
        --indent;
        return res;
    }

    StmtPtr Parser::parseStatement() {
        expectIndent();
        switch (peek()) {
            case Type::KEY_LET:    return parseVarDeclaration();
            case Type::KEY_IF:     return parseIfStatement();
            case Type::KEY_WHILE:  return parseWhileStatement();
            case Type::KEY_DO:     return parseDoWhileStatement();
            case Type::KEY_RETURN: return parseReturnStatement();
            default:               return parseAssignOrExpressionStatement();
        }
    }

    StmtPtr Parser::parseVarDeclaration() {
        std::size_t start = getIndex();

        expect(Type::KEY_LET, "local declaration must start with keyword 'let'");
        Token id = expect(Type::IDENTIFIER, "expect variable name");
        expect(Type::ASSIGN, "local variable must be assigned a value");
        ExprPtr expr = parseExpression();
        return stmt::VDecl::of(start, getLength(start), id.lexeme, expr);
    }

    StmtPtr Parser::parseAssignOrExpressionStatement() {
        std::size_t start = getIndex();

        ExprPtr lhs = parseExpression(); // LHS is a subset of EXPR
        
        if (match(Type::ASSIGN)) {
            Token op = expect(Type::ASSIGN, "assignment must use ':=' operator");
            ExprPtr expr = parseExpression();
            return stmt::Assn::of(start, getLength(start), lhs, expr);
        } else {
            if (matchStay(Type::COLON)) { // special function application type
                Token colon = advance();
                std::vector<ExprPtr> args = { parseExpression() };
                while (match(Type::COMMA))
                    args.push_back(parseExpression());
                lhs = expr::Func::of(start, getLength(start), lhs, args);
            }
            return stmt::Expr::of(start, getLength(start), lhs);
        }
    }

    StmtPtr Parser::parseIfStatement(Type t) {
        std::size_t start = getIndex();

        Token key = expect(t, "if statement must start with keyword 'if'");
        ExprPtr condition = parseExpression();
        Block taken = parseBlock();
        if (matchIndentAnd(Type::KEY_ELIF)) {
            reverse();
            return stmt::If::of(start, getLength(start), condition, taken, { parseIfStatement(Type::KEY_ELIF) });
        } else if (matchIndentAnd(Type::KEY_ELSE)) {
            return stmt::If::of(start, getLength(start), condition, taken, parseBlock());
        } else {
            return stmt::If::of(start, getLength(start), condition, taken, {});
        }
    }

    StmtPtr Parser::parseWhileStatement() {
        std::size_t start = getIndex();

        Token key = expect(Type::KEY_WHILE, "while statement must start with keyword 'while'");
        ExprPtr condition = parseExpression();
        Block body = parseBlock();
        return stmt::While::of(start, getLength(start), condition, body);
    }

    StmtPtr Parser::parseDoWhileStatement() {
        std::size_t start = getIndex();

        expect(Type::KEY_DO, "do-while statement must start with keyword 'do'");
        Block body = parseBlock();
        if (!matchIndentAnd(Type::KEY_WHILE))
            throw ParserError(advance(), "do-while statement must end with <while %expr>");
        Token key = previous();
        ExprPtr condition = parseExpression();
        return stmt::DoWhile::of(start, getLength(start), condition, body);
    }

    StmtPtr Parser::parseReturnStatement() {
        std::size_t start = getIndex();

        Token key = expect(Type::KEY_RETURN, "return statement must start with keyword 'start'");
        if (peek() == Type::$ || peek() == Type::WHITESPACE)
            return stmt::Return::of(start, getLength(start));
        else
            return stmt::Return::of(start, getLength(start), parseExpression());
    }

    TypePtr Parser::parseTypeExpression(bool return_type) {
        std::size_t start = getIndex();

        if (return_type && matchStay(Type::KEY_VOID))
            return type::Prim::of(start, getLength(start), advance().type);
        return parsePrimitiveTypeExpression();
    }

    TypePtr Parser::parsePrimitiveTypeExpression() {
        std::size_t start = getIndex();

        return type::Prim::of(start, getLength(start),
            expect([] (Type t) {
                return t == Token::Type::KEY_INT || t == Token::Type::KEY_FLT || t == Token::Type::KEY_CHAR || t == Token::Type::KEY_BOOL; },
                "expect a primitive type").type);
    }

    ExprPtr Parser::parseExpression() {
        return parseBinaryExpression(0);
    }

    ExprPtr Parser::parseSimpleExpression() {
        std::size_t start = getIndex();

        ExprPtr expr;
        if (match(Type::LPAREN)) {
            expr = parseExpression();
            expect(Type::RPAREN, "expected ')'");
        } else {
            Token t = expect([] (Type t) { return t == Type::LITERAL_INT  ||
                                                  t == Type::LITERAL_FLT  ||
                                                  t == Type::LITERAL_CHAR ||
                                                  t == Type::LITERAL_BOOL ||
                                                  t == Type::IDENTIFIER; },
                                                  "expected a literal or variable name");
            if (t.type == Type::IDENTIFIER) {
                expr = expr::Id::of(start, getLength(start), t.getId());
            } else {
                expr = expr::Lit::of(start, getLength(start), t.l);
            }
        }

        if (matchStay([] (Type t) { return t == Type::LPAREN; }))
            return parseApplicationExpression(expr);
        else
            return expr;
      }

    ExprPtr Parser::parseApplicationExpression(const ExprPtr &lhs) {
        std::size_t start = getIndex();

        if (matchStay(Type::LPAREN)) {
            Token lparen = advance();
            std::vector<ExprPtr> args;
            while (!match(Type::RPAREN))
                args.push_back(parseExpression());
            return expr::Func::of(start, getLength(start), lhs, args);
        } else {
            throw ParserError(advance(), "unexpected token in application expression");
        }
    }

    ExprPtr Parser::parseUnaryExpression() {
        std::size_t start = getIndex();

        if (matchStay([] (Type t) { return t == Type::BANG || t == Type::DASH; })) {
            Token op = advance();
            ExprPtr exp = parseUnaryExpression();
            return expr::Uop::of(start, getLength(start), op.type, exp);
        } else {
            return parseSimpleExpression();
        }
    }

    ExprPtr Parser::parseBinaryExpression(const std::size_t precindex) {
        std::size_t start = getIndex();

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
                    std::vector<std::pair<Token::Type, ExprPtr>> cmps;
                    while (matchStay([&precindex] (Type t) {
                        return std::find(PRECEDENCES.at(precindex).begin(), PRECEDENCES.at(precindex).end(), t) != PRECEDENCES.at(precindex).end();
                    })) {
                        Token op = advance();
                        cmps.push_back(std::make_pair(op.type, parseBinaryExpression(precindex + 1)));
                    }
                    return expr::CmpList::of(start, getLength(start), exp, cmps);
                } else {
                    if (ASSOCIATIVITIES.at(op.type)) { // left-associative
                        auto rexp = parseBinaryExpression(precindex + 1);
                        exp = expr::Bop::of(start, getLength(start), op.type, exp, rexp);
                    } else {
                        auto rexp = parseBinaryExpression(precindex);
                        return expr::Bop::of(start, getLength(start), op.type, exp, rexp);
                    }
                }
            }
            return exp;
        }
    }

    std::size_t Parser::getIndex() const {
        return stream.at(index).start;
    }

    std::size_t Parser::getLength(std::size_t start) const {
        return stream.at(index).start + stream.at(index).length - start;
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
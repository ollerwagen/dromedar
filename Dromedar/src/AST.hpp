#pragma once

#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "Token.hpp"

// debug
#include <iostream>

namespace drm {

    struct PrimitiveTypeExpression;

    struct TypeExpression {
        
        virtual ~TypeExpression() {}

        enum Type {
            PRIMITIVE,
        };
        virtual Type getType() const = 0;

        template<typename T>
        class Visitor {
        public:
            virtual T visitTypeExpression(const TypeExpression *expr);
            virtual T visitPrimitiveTypeExpression(const PrimitiveTypeExpression *expr);
        };

        virtual bool equal(const std::shared_ptr<TypeExpression> &t) = 0;
    };

    struct PrimitiveTypeExpression : public TypeExpression {
        
        enum PType {
            INT, FLT, CHAR, BOOL,
            VOID
        } type;

        inline PrimitiveTypeExpression(PType t) { type = t; }
        ~PrimitiveTypeExpression() {}

        Type getType() const override { return PRIMITIVE; }

        bool equal(const std::shared_ptr<TypeExpression> &t) override {
            return t->getType() == getType() &&
                   type == std::static_pointer_cast<PrimitiveTypeExpression>(t)->type;
        }

        inline static std::shared_ptr<TypeExpression> of(PType t) {
            return std::static_pointer_cast<TypeExpression>(std::make_shared<PrimitiveTypeExpression>(t));
        }

        inline static std::shared_ptr<TypeExpression> ofTypename(Token::Type t) {   
            PType pt;
            switch (t) {
                case Token::Type::KEY_INT:  pt = INT;  break;
                case Token::Type::KEY_FLT:  pt = FLT;  break;
                case Token::Type::KEY_CHAR: pt = CHAR; break;
                case Token::Type::KEY_BOOL: pt = BOOL; break;
                case Token::Type::KEY_VOID: pt = VOID; break;
                default:                    throw std::runtime_error("not well-formed AST");
            }
            return of(pt);
        }

        inline static std::shared_ptr<TypeExpression> ofTypename(const Token &t) {
            return ofTypename(t.type);
        }

        inline static std::shared_ptr<TypeExpression> MAKE_INT()  { return ofTypename(Token::Type::KEY_INT);  }
        inline static std::shared_ptr<TypeExpression> MAKE_FLT()  { return ofTypename(Token::Type::KEY_FLT);  }
        inline static std::shared_ptr<TypeExpression> MAKE_CHAR() { return ofTypename(Token::Type::KEY_CHAR); }
        inline static std::shared_ptr<TypeExpression> MAKE_BOOL() { return ofTypename(Token::Type::KEY_BOOL); }
        inline static std::shared_ptr<TypeExpression> MAKE_VOID() { return ofTypename(Token::Type::KEY_VOID); }
    };

    struct VariableLHS;

    struct LHS {
        
        virtual ~LHS() {}

        enum Type {
            VARIABLE
        };
        virtual Type getType() const = 0;

        template<typename T>
        class Visitor {
        public:
            virtual T visitLHS(const LHS *lhs);
            virtual T visitVariableLHS(const VariableLHS *lhs);
        };
    };

    struct VariableLHS : public LHS {
        
        Token id;

        inline VariableLHS(const Token &t) { id = t; }
        ~VariableLHS() {}

        Type getType() const { return VARIABLE; }

        inline static std::shared_ptr<LHS> of(const Token &t) {
            return std::static_pointer_cast<LHS>(std::make_shared<VariableLHS>(t));
        }
    };

    struct LiteralExpression;
    struct UnaryExpression;
    struct BinaryExpression;
    struct ComparisonList;

    struct Expression {    
        using LiteralType = Token::LiteralType;

        virtual ~Expression() {}

        enum Type {
            LITERAL,
            UNARY,
            BINARY,
            COMPLIST
        };
        virtual Type getType() const = 0;

        template<typename T>
        class Visitor {
        public:
            virtual T visitExpression(const Expression *expr);
            virtual T visitLiteralExpression(const LiteralExpression *expr);
            virtual T visitUnaryExpression(const UnaryExpression *expr);
            virtual T visitBinaryExpression(const BinaryExpression *expr);
            virtual T visitComparisonList(const ComparisonList *expr);
        };
    };

    struct LiteralExpression : public Expression {

        Token lit; // of a literal type OR identifier
        
        inline LiteralExpression(const Token &t) { lit = t; }
        ~LiteralExpression() {}

        Type getType() const override { return LITERAL; }

        inline static std::shared_ptr<Expression> of(const Token &lit) {
            return std::static_pointer_cast<Expression>(std::make_shared<LiteralExpression>(lit));
        }
    };

    struct UnaryExpression : public Expression {

        Token op;
        std::shared_ptr<Expression> expr;

        inline UnaryExpression(const Token &op, const std::shared_ptr<Expression> &expr) { this->op = op; this->expr = expr; }
        ~UnaryExpression() {}

        Type getType() const override { return UNARY; }

        inline static std::shared_ptr<Expression> of(const Token &op, const std::shared_ptr<Expression> &expr) {
            return std::static_pointer_cast<Expression>(std::make_shared<UnaryExpression>(op, expr));
        }
    };

    struct BinaryExpression : public Expression {

        Token op;
        std::shared_ptr<Expression> lexp, rexp;

        inline BinaryExpression(const std::shared_ptr<Expression> &lexp, const Token &op, const std::shared_ptr<Expression> &rexp) { this->lexp = lexp; this->op = op; this->rexp = rexp; }
        ~BinaryExpression() {}

        Type getType() const override { return BINARY; }

        inline static std::shared_ptr<Expression> of(const std::shared_ptr<Expression> &lexp, const Token &op, const std::shared_ptr<Expression> &rexp) {
            return std::static_pointer_cast<Expression>(std::make_shared<BinaryExpression>(lexp, op, rexp));
        }
    };

    struct ComparisonList : public Expression {

        std::shared_ptr<Expression> first;
        std::vector<std::pair<Token, std::shared_ptr<Expression>>> comparisons;

        inline ComparisonList(const std::shared_ptr<Expression> &e, std::vector<std::pair<Token, std::shared_ptr<Expression>>> &l) { first = e; comparisons = l; }
        ~ComparisonList() {}

        Type getType() const override { return COMPLIST; }

        inline static std::shared_ptr<Expression> of(const std::shared_ptr<Expression> &e, std::vector<std::pair<Token, std::shared_ptr<Expression>>> &l) {
            return std::static_pointer_cast<Expression>(std::make_shared<ComparisonList>(e, l));
        }
    };

    struct VarDeclStatement;
    struct TypeVarDeclStatement;
    struct AssignStatement;
    struct IfStatement;
    struct WhileStatement;
    struct DoWhileStatement;
    struct ReturnStatement;

    struct Statement {
        using Block = std::vector<std::shared_ptr<Statement>>;

        virtual ~Statement() {}

        enum Type {
            VARDECL,
            TVARDECL,
            ASSIGN,
            IFSTMT,
            WHILESTMT,
            DOWHILESTMT,
            RETURN
        };
        virtual Type getType() const = 0;

        template<typename T>
        class Visitor {
        public:
            virtual T visitStatement(const Statement *stmt);
            virtual T visitVarDecl(const VarDeclStatement *stmt);
            virtual T visitTypeVarDecl(const TypeVarDeclStatement *stmt);
            virtual T visitAssignStatement(const AssignStatement *stmt);
            virtual T visitIfStatement(const IfStatement *stmt);
            virtual T visitWhileStatement(const WhileStatement *stmt);
            virtual T visitDoWhileStatement(const DoWhileStatement *stmt);
            virtual T visitReturnStatement(const ReturnStatement *stmt);

            virtual T visitBlock(const Block &block);
        };
    };
    using Block = Statement::Block;
    
    struct VarDeclStatement : public Statement {
        
        Token id;
        bool mut;
        std::shared_ptr<Expression> expr;

        inline VarDeclStatement(const Token &t, bool m, const std::shared_ptr<Expression> &e) { id = t; mut = m; expr = e; }
        ~VarDeclStatement() {}

        Type getType() const override { return VARDECL; }

        inline static std::shared_ptr<Statement> of(const Token &t, bool m, const std::shared_ptr<Expression> &e) {
            return std::static_pointer_cast<Statement>(std::make_shared<VarDeclStatement>(t, m, e));
        }
    };

    struct TypeVarDeclStatement : public Statement {

        Token id;
        bool mut;
        std::shared_ptr<TypeExpression> type;
        std::shared_ptr<Expression> expr;

        inline TypeVarDeclStatement(const Token &id, bool m, const std::shared_ptr<TypeExpression> &t, const std::shared_ptr<Expression> &e) { this->id = id; mut = m; type = t; expr = e; }
        ~TypeVarDeclStatement() {}

        Type getType() const override { return TVARDECL; }

        inline static std::shared_ptr<Statement> of(const Token &id, bool m, const std::shared_ptr<TypeExpression> &t, const std::shared_ptr<Expression> &e) {
            return std::static_pointer_cast<Statement>(std::make_shared<TypeVarDeclStatement>(id, m, t, e));
        }
    };

    struct AssignStatement : public Statement {

        std::shared_ptr<LHS> lhs;
        Token op;
        std::shared_ptr<Expression> expr;

        inline AssignStatement(const std::shared_ptr<LHS> &l, const Token &o, const std::shared_ptr<Expression> &e) { lhs = l; op = o; expr = e; }
        ~AssignStatement() {}

        Type getType() const override { return ASSIGN; }

        inline static std::shared_ptr<Statement> of(const std::shared_ptr<LHS> &l, const Token &o, const std::shared_ptr<Expression> &e) {
            return std::static_pointer_cast<Statement>(std::make_shared<AssignStatement>(l, o, e));
        }
    };

    struct IfStatement : public Statement {

        Token key;
        std::shared_ptr<Expression> cond;
        Block taken, not_taken;

        inline IfStatement(const Token &t, const std::shared_ptr<Expression> &e, const Block &a, const Block &b) { key = t; cond = e; taken = a; not_taken = b; }
        ~IfStatement() {}

        Type getType() const override { return IFSTMT; }

        inline static std::shared_ptr<Statement> of(const Token &t, const std::shared_ptr<Expression> &e, const Block &a, const Block &b) {
            return std::static_pointer_cast<Statement>(std::make_shared<IfStatement>(t, e, a, b));
        }
    };

    struct WhileStatement : public Statement {

        Token key;
        std::shared_ptr<Expression> cond;
        Block body;

        inline WhileStatement(const Token &t, const std::shared_ptr<Expression> &e, const Block &b) { key = t; cond = e; body = b; }
        ~WhileStatement() {}

        Type getType() const override { return WHILESTMT; }

        inline static std::shared_ptr<Statement> of(const Token &t, const std::shared_ptr<Expression> &e, const Block &b) {
            return std::static_pointer_cast<Statement>(std::make_shared<WhileStatement>(t, e, b));
        }
    };

    struct DoWhileStatement : public Statement {

        std::shared_ptr<Expression> cond;
        Block body;
        Token whilekey;

        inline DoWhileStatement(const std::shared_ptr<Expression> &e, const Block &b, const Token &t) { cond = e; body = b; whilekey = t; }
        ~DoWhileStatement() {}

        Type getType() const override { return DOWHILESTMT; }

        inline static std::shared_ptr<Statement> of(const std::shared_ptr<Expression> &e, const Block &b, const Token &t) {
            return std::static_pointer_cast<Statement>(std::make_shared<DoWhileStatement>(e, b, t));
        }
    };

    struct ReturnStatement : public Statement {

        Token key;
        bool withVal;
        std::shared_ptr<Expression> expr;

        inline ReturnStatement(const Token &t) : withVal(false) { key = t; }
        inline ReturnStatement(const Token &t, const std::shared_ptr<Expression> &e) : withVal(true) { key = t; expr = e; }
        ~ReturnStatement() {}

        Type getType() const override { return RETURN; }

        inline static std::shared_ptr<Statement> of(const Token &t) {
            return std::static_pointer_cast<Statement>(std::make_shared<ReturnStatement>(t));
        }
        inline static std::shared_ptr<Statement> of(const Token &t, const std::shared_ptr<Expression> &e) {
            return std::static_pointer_cast<Statement>(std::make_shared<ReturnStatement>(t, e));
        }
    };

    struct GVarDeclStatement;
    struct GTypeVarDeclStatement;
    struct GFDeclStatement;

    struct GlobalStatement {

        virtual ~GlobalStatement() {}
        
        enum Type {
            GVARDECL,
            GTYPEVARDECL,
            GFNDECL
        };
        virtual Type getType() const = 0;

        template<typename T>
        class Visitor {
        public:
            virtual T visitGlobalStatement(const GlobalStatement *stmt);
            virtual T visitGVarDecl(const GVarDeclStatement *stmt);
            virtual T visitGTypeVarDecl(const GTypeVarDeclStatement *stmt);
            virtual T visitGFDecl(const GFDeclStatement *stmt);
            
            virtual T visitProgram(const std::vector<std::shared_ptr<GlobalStatement>> &prog);
        };
    };

    struct GVarDeclStatement : public GlobalStatement {

        Token id;
        bool mut;
        std::shared_ptr<Expression> expr;

        inline GVarDeclStatement(const Token &t, bool m, const std::shared_ptr<Expression> &e) { id = t; mut = m; expr = e; }
        ~GVarDeclStatement() {}

        Type getType() const override { return GVARDECL; }

        inline static std::shared_ptr<GlobalStatement> of(const Token &t, bool m, const std::shared_ptr<Expression> &e) {
            return std::static_pointer_cast<GlobalStatement>(std::make_shared<GVarDeclStatement>(t, m, e));
        }
    };

    struct GTypeVarDeclStatement : public GlobalStatement {

        Token id;
        bool mut;
        std::shared_ptr<TypeExpression> type;
        std::shared_ptr<Expression> expr;

        inline GTypeVarDeclStatement(const Token &id, bool m, const std::shared_ptr<TypeExpression> &t, const std::shared_ptr<Expression> &e) { this->id = id; mut = m; type = t; expr = e; }
        ~GTypeVarDeclStatement() {}

        Type getType() const override { return GTYPEVARDECL; }

        inline static std::shared_ptr<GlobalStatement> of(const Token &id, bool m, const std::shared_ptr<TypeExpression> &t, const std::shared_ptr<Expression> &e) {
            return std::static_pointer_cast<GlobalStatement>(std::make_shared<GTypeVarDeclStatement>(id, m, t, e));
        }
    };

    struct GFDeclStatement : public GlobalStatement {

        Token id;
        std::vector<std::pair<Token, std::shared_ptr<TypeExpression>>> args;
        std::shared_ptr<TypeExpression> rettype;
        Block body;
        
        inline GFDeclStatement(const Token &t, const std::vector<std::pair<Token, std::shared_ptr<TypeExpression>>> &a, const std::shared_ptr<TypeExpression> &rt, const Block &b) { id = t; args = a; rettype = rt; body = b; }
        ~GFDeclStatement() {}

        Type getType() const override { return GFNDECL; }

        inline static std::shared_ptr<GlobalStatement> of(const Token &t, const std::vector<std::pair<Token, std::shared_ptr<TypeExpression>>> &a, const std::shared_ptr<TypeExpression> &rt, const Block &b) {
            return std::static_pointer_cast<GlobalStatement>(std::make_shared<GFDeclStatement>(t, a, rt, b));
        }
    };

    using Program = std::vector<std::shared_ptr<GlobalStatement>>;

} // namespace drm
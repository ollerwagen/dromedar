#pragma once

#include <cstdint>
#include <map>
#include <stack>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

#include <boost/variant.hpp>

#include "AST.hpp"

namespace drm {

    struct TypeError {
        std::size_t start, length;
        std::string message;

        TypeError(std::size_t start, std::size_t length, std::string message);
    
        std::string toString() const;
    };

    class TypeOfLit : public boost::static_visitor<Token::Type> {
    public:

        inline Token::Type operator() (__attribute__((unused)) Token::Int  i) const { return Token::KEY_INT;  }
        inline Token::Type operator() (__attribute__((unused)) Token::Flt  f) const { return Token::KEY_FLT;  }
        inline Token::Type operator() (__attribute__((unused)) Token::Char c) const { return Token::KEY_CHAR; }
        inline Token::Type operator() (__attribute__((unused)) Token::Bool b) const { return Token::KEY_BOOL; }
        inline Token::Type operator() (__attribute__((unused)) Token::Id   i) const { throw std::runtime_error("badly formed AST"); }
    };

    struct ExprRes {
        TypePtr type;
        bool write_to;
    };

    class TypeChecker :
        public Expr::Visitor<ExprRes>,
        public Type::Visitor<bool>, // true <=> can be assigned to a value (valid type - <void>)
        public Stmt::Visitor<bool>, // true <=> statement definitely returns
        public GStmt::Visitor<void> {

    private:

        static const std::map<Token::Type, std::vector<std::tuple<TypePtr, TypePtr>>> UOPS;
        static const std::map<Token::Type, std::vector<std::tuple<TypePtr, TypePtr, TypePtr>>> BOPS;

    private:

        std::vector<std::map<std::string, TypePtr>> bindings;
        std::stack<TypePtr> frtys;
        std::vector<TypeError> errors;

    private:

        bool hasBinding(const std::string &id);
        bool hasTopLevelBinding(const std::string &id);
        TypePtr getBinding(const std::string &id);
        void addBinding(const std::string &id, const TypePtr &type);

    public:

        TypeChecker();

        virtual ExprRes visitExprId(expr::Id *e) override;
        virtual ExprRes visitExprLit(expr::Lit *e) override;
        virtual ExprRes visitExprUop(expr::Uop *e) override;
        virtual ExprRes visitExprBop(expr::Bop *e) override;
        virtual ExprRes visitExprFunc(expr::Func *e) override;
        virtual ExprRes visitExprCmpList(expr::CmpList *e) override;

        virtual bool visitTypePrim(type::Prim *t) override;
        virtual bool visitTypeFType(type::FType *t) override;

        virtual bool visitStmtVDecl(stmt::VDecl *s) override;
        virtual bool visitStmtAssn(stmt::Assn *s) override;
        virtual bool visitStmtExpr(stmt::Expr *s) override;
        virtual bool visitStmtIf(stmt::If *s) override;
        virtual bool visitStmtWhile(stmt::While *s) override;
        virtual bool visitStmtDoWhile(stmt::DoWhile *s) override;
        virtual bool visitStmtReturn(stmt::Return *s) override;

        // exception logging for stmts delegated here
        virtual bool visitStmtBlock(const Block &block) override;

        virtual void visitGStmtFDecl(gstmt::FDecl *gs) override;
        virtual void visitGStmtVDecl(gstmt::VDecl *gs) override;
    
        // exception handling for badly formed ASTs delegated here
        virtual void visitGStmtProgram(const Program &prog) override;

        std::string errorMessages() const;
    };

} // namespace drm
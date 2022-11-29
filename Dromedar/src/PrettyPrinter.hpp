#pragma once

#include <string>

#include "AST.hpp"

namespace drm {

    class PrettyPrinter :
        public Expr::Visitor<std::string>,
        public Type::Visitor<std::string>,
        public Stmt::Visitor<std::string>,
        public GStmt::Visitor<std::string> {

    private:

        static const std::unordered_map<Token::Type, std::string> OPS;

        std::string indent;

    public:

        PrettyPrinter();

        virtual std::string visitExprId(expr::Id *e) override;
        virtual std::string visitExprLit(expr::Lit *e) override;
        virtual std::string visitExprUop(expr::Uop *e) override;
        virtual std::string visitExprBop(expr::Bop *e) override;
        virtual std::string visitExprFunc(expr::Func *e) override;
        virtual std::string visitExprCmpList(expr::CmpList *e) override;

        virtual std::string visitTypePrim(type::Prim *t) override;
        virtual std::string visitTypeFType(type::FType *t) override;

        virtual std::string visitStmtVDecl(stmt::VDecl *s) override;
        virtual std::string visitStmtAssn(stmt::Assn *s) override;
        virtual std::string visitStmtExpr(stmt::Expr *s) override;
        virtual std::string visitStmtIf(stmt::If *s) override;
        virtual std::string visitStmtWhile(stmt::While *s) override;
        virtual std::string visitStmtDoWhile(stmt::DoWhile *s) override;
        virtual std::string visitStmtReturn(stmt::Return *s) override;

        virtual std::string visitStmtBlock(const Block &block) override;

        virtual std::string visitGStmtFDecl(gstmt::FDecl *gs) override;
        virtual std::string visitGStmtVDecl(gstmt::VDecl *gs) override;
    
        virtual std::string visitGStmtProgram(const Program &prog) override;
    };
}
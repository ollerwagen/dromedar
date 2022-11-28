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

        std::string visitExprId(expr::Id *e) override;
        std::string visitExprLit(expr::Lit *e) override;
        std::string visitExprUop(expr::Uop *e) override;
        std::string visitExprBop(expr::Bop *e) override;
        std::string visitExprFunc(expr::Func *e) override;
        std::string visitExprCmpList(expr::CmpList *e) override;

        std::string visitTypePrim(type::Prim *t) override;
        std::string visitTypeFType(type::FType *t) override;

        std::string visitStmtVDecl(stmt::VDecl *s) override;
        std::string visitStmtAssn(stmt::Assn *s) override;
        std::string visitStmtExpr(stmt::Expr *s) override;
        std::string visitStmtIf(stmt::If *s) override;
        std::string visitStmtWhile(stmt::While *s) override;
        std::string visitStmtDoWhile(stmt::DoWhile *s) override;
        std::string visitStmtReturn(stmt::Return *s) override;

        std::string visitStmtBlock(const Block &block) override;

        std::string visitGStmtFDecl(gstmt::FDecl *gs) override;
        std::string visitGStmtVDecl(gstmt::VDecl *gs) override;
    
        std::string visitGStmtProgram(const Program &prog) override;
    };
}
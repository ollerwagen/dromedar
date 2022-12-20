#pragma once

#include <string>
#include <unordered_map>

#include "AST.hpp"

namespace drm {

    using Str = std::string;

    struct CmpExprRes { 
        Str stream;
        Str res;  // name of the variable that holds the return value
        TypePtr type; // type of the return variable
    };

    class Transpiler :
        private Expr::Visitor<CmpExprRes>,
        private Type::Visitor<Str>,
        private Stmt::Visitor<Str>,
        private GStmt::Visitor<Str> {

    private:

        static const std::unordered_map<Token::Type, std::string> OPS;

        std::unordered_map<std::string, std::pair<std::string, TypePtr>> variables;

    private:

        Str gensym(const std::string &base);

        virtual CmpExprRes visitExprId(expr::Id *e) override;
        virtual CmpExprRes visitExprLit(expr::Lit *e) override;
        virtual CmpExprRes visitExprUop(expr::Uop *e) override;
        virtual CmpExprRes visitExprBop(expr::Bop *e) override;
        virtual CmpExprRes visitExprFunc(expr::Func *e) override;
        virtual CmpExprRes visitExprCmpList(expr::CmpList *e) override;

        virtual Str visitTypePrim(type::Prim *t) override;
        virtual Str visitTypeFType(type::FType *t) override;

        virtual Str visitStmtVDecl(stmt::VDecl *s) override;
        virtual Str visitStmtAssn(stmt::Assn *s) override;
        virtual Str visitStmtExpr(stmt::Expr *s) override;
        virtual Str visitStmtIf(stmt::If *s) override; 
        virtual Str visitStmtWhile(stmt::While *s) override;
        virtual Str visitStmtDoWhile(stmt::DoWhile *s) override;
        virtual Str visitStmtReturn(stmt::Return *s) override;

        virtual Str visitStmtBlock(const Block &block) override;

        virtual Str visitGStmtFDecl(gstmt::FDecl *gs) override;
        virtual Str visitGStmtVDecl(gstmt::VDecl *gs) override;

        virtual Str visitGStmtProgram(const Program &prog) override;

    public:

        Str transpile(const Program &prog);

    };

} // namespace drm
#pragma once

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

// debugging purposes
#include <iostream>

#include "Token.hpp"

namespace drm {

    namespace expr {
        struct Id; struct Lit; struct Uop; struct Bop; struct Func; struct CmpList;
    }

    struct Expr {
        using ExprPtr = std::shared_ptr<Expr>;

        enum class T { ID, LIT, UOP, BOP, FUNC, CMPLIST };
        virtual T type() const = 0;

        std::size_t start, length;

        Expr(std::size_t start, std::size_t length);

        template<typename U>
        struct Visitor {
            virtual U visitExprId(expr::Id *e) = 0;
            virtual U visitExprLit(expr::Lit *e) = 0;
            virtual U visitExprUop(expr::Uop *e) = 0;
            virtual U visitExprBop(expr::Bop *e) = 0;
            virtual U visitExprFunc(expr::Func *e) = 0;
            virtual U visitExprCmpList(expr::CmpList *e) = 0;

            inline U visitExpr(Expr *e) {
                switch (e->type()) {
                    case T::ID:      return visitExprId((expr::Id*)e);
                    case T::LIT:     return visitExprLit((expr::Lit*)e);
                    case T::UOP:     return visitExprUop((expr::Uop*)e);
                    case T::BOP:     return visitExprBop((expr::Bop*)e);
                    case T::FUNC:    return visitExprFunc((expr::Func*)e);
                    case T::CMPLIST: return visitExprCmpList((expr::CmpList*)e);
                    default:         throw std::runtime_error("badly formed AST");
                }
            }
        };
    };

    using ExprPtr = Expr::ExprPtr;

    namespace expr {

        struct Id : public Expr {
            std::string id;

            T type() const final override { return T::ID; }

            Id(std::size_t start, std::size_t length, std::string id);
            static ExprPtr of(std::size_t start, std::size_t length, std::string id);
        };

        struct Lit : public Expr {
            Token::LiteralType lt;

            T type() const final override { return T::LIT; }

            Lit(std::size_t start, std::size_t length, const Token::LiteralType &lt);
            static ExprPtr of(std::size_t start, std::size_t length, const Token::LiteralType &lt);
        };

        struct Uop : public Expr {
            Token::Type op;
            ExprPtr exp;

            T type() const final override { return T::UOP; }

            Uop(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &exp);
            static ExprPtr of(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &exp);
        };

        struct Bop : public Expr {
            Token::Type op;
            ExprPtr lexp, rexp;

            T type() const final override { return T::BOP; }

            Bop(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &lexp, const ExprPtr &rexp);
            static ExprPtr of(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &lexp, const ExprPtr &rexp);
        };

        struct Func : public Expr {
            ExprPtr lhs;
            std::vector<ExprPtr> args;

            T type() const final override { return T::FUNC; }

            Func(std::size_t start, std::size_t length, const ExprPtr &lhs, const std::vector<ExprPtr> &args);
            static ExprPtr of(std::size_t start, std::size_t length, const ExprPtr &lhs, const std::vector<ExprPtr> &args);
        };

        struct CmpList : public Expr {
            ExprPtr first;
            std::vector<std::pair<Token::Type, ExprPtr>> cmps;

            T type() const final override { return T::CMPLIST; }

            CmpList(std::size_t start, std::size_t length, const ExprPtr &first, const decltype(cmps) &cmps);
            static ExprPtr of(std::size_t start, std::size_t length, const ExprPtr &first, const decltype(cmps) &cmps);
        };
    }

    namespace type {
        struct Prim; struct FType;
    }

    struct Type {
        using TypePtr = std::shared_ptr<Type>;

        enum class T { PRIM, FTYPE };
        virtual T type() const = 0;

        std::size_t start, length;

        Type(std::size_t start, std::size_t length);

        static bool equiv(const TypePtr &a, const TypePtr &b);

        template<typename U>
        struct Visitor {
            virtual U visitTypePrim(type::Prim *t) = 0;
            virtual U visitTypeFType(type::FType *t) = 0;

            inline U visitType(Type *t) {
                switch (t->type()) {
                    case T::PRIM:  return visitTypePrim((type::Prim*)t);
                    case T::FTYPE: return visitTypeFType((type::FType*)t);
                    default:       throw std::runtime_error("badly formed AST");
                }
            }
        };
    };

    using TypePtr = Type::TypePtr;

    namespace type {

        struct Prim : public Type {
            static const TypePtr INT, FLT, BOOL, CHAR, VOID;

            Token::Type t;

            T type() const final override { return T::PRIM; }

            static bool eq(Prim *a, Prim *b);

            Prim(std::size_t start, std::size_t length, Token::Type t);
            static TypePtr of(std::size_t start, std::size_t length, Token::Type t);
        };

        struct FType : public Type {
            TypePtr rty;
            std::vector<TypePtr> argts;

            T type() const final override { return T::FTYPE; }

            static bool eq(FType *a, FType *b);

            FType(std::size_t start, std::size_t length, const TypePtr &rty, const std::vector<TypePtr> &argts);
            static TypePtr of(std::size_t start, std::size_t length, const TypePtr &rty, const std::vector<TypePtr> &argts);
        };
    }

    namespace stmt {
        struct VDecl; struct Assn; struct Expr; struct If; struct While; struct DoWhile; struct Return;
    }

    struct Stmt {
        using StmtPtr = std::shared_ptr<Stmt>;
        using Block = std::vector<StmtPtr>;

        enum class T { VDECL, ASSN, EXPR, IF, WHILE, DOWHILE, RETURN };
        virtual T type() const = 0;

        std::size_t start, length;

        Stmt(std::size_t start, std::size_t length);

        template<typename U>
        struct Visitor {
            virtual U visitStmtVDecl(stmt::VDecl *s) = 0;
            virtual U visitStmtAssn(stmt::Assn *s) = 0;
            virtual U visitStmtExpr(stmt::Expr *s) = 0;
            virtual U visitStmtIf(stmt::If *s) = 0;
            virtual U visitStmtWhile(stmt::While *s) = 0;
            virtual U visitStmtDoWhile(stmt::DoWhile *s) = 0;
            virtual U visitStmtReturn(stmt::Return *s) = 0;

            inline U visitStmt(Stmt *s) {
                switch (s->type()) {
                    case T::VDECL:   return visitStmtVDecl((stmt::VDecl*)s);
                    case T::ASSN:    return visitStmtAssn((stmt::Assn*)s);
                    case T::EXPR:    return visitStmtExpr((stmt::Expr*)s);
                    case T::IF:      return visitStmtIf((stmt::If*)s);
                    case T::WHILE:   return visitStmtWhile((stmt::While*)s);
                    case T::DOWHILE: return visitStmtDoWhile((stmt::DoWhile*)s);
                    case T::RETURN:  return visitStmtReturn((stmt::Return*)s);
                    default:         throw std::runtime_error("badly formed AST");
                }
            }

            virtual U visitStmtBlock(const Block &block) = 0;
        };
    };

    using StmtPtr = Stmt::StmtPtr;
    using Block = Stmt::Block;

    namespace stmt {

        struct VDecl : public Stmt {
            std::string id;
            ExprPtr exp;

            T type() const final override { return T::VDECL; }

            VDecl(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp);
            static StmtPtr of(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp);
        };

        struct Assn : public Stmt {
            ExprPtr lhs, exp;

            T type() const final override { return T::ASSN; }

            Assn(std::size_t start, std::size_t length, const ExprPtr &lhs, const ExprPtr &exp);
            static StmtPtr of(std::size_t start, std::size_t length, const ExprPtr &lhs, const ExprPtr &exp);
        };

        struct Expr : public Stmt {
            ExprPtr exp;

            T type() const final override { return T::EXPR; }

            Expr(std::size_t start, std::size_t length, const ExprPtr &exp);
            static StmtPtr of(std::size_t start, std::size_t end, const ExprPtr &exp);
        };

        struct If : public Stmt {
            ExprPtr cond;
            Block taken, not_taken;

            T type() const final override { return T::IF; }

            If(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &taken, const Block &not_taken);
            static StmtPtr of(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &taken, const Block &not_taken);
        };

        struct While : public Stmt {
            ExprPtr cond;
            Block body;

            T type() const final override { return T::WHILE; }

            While(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body);
            static StmtPtr of(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body);
        };

        struct DoWhile : public Stmt {
            ExprPtr cond;
            Block body;

            T type() const final override { return T::DOWHILE; }

            DoWhile(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body);
            static StmtPtr of(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body);
        };

        struct Return : public Stmt {
            bool with_value;
            ExprPtr exp;

            T type() const final override { return T::RETURN; }

            Return(std::size_t start, std::size_t length);
            Return(std::size_t start, std::size_t length, const ExprPtr &exp);
            static StmtPtr of(std::size_t start, std::size_t length);
            static StmtPtr of(std::size_t start, std::size_t length, const ExprPtr &exp);
        };
    }

    namespace gstmt {
        struct FDecl; struct VDecl;
    }

    struct GStmt {
        using GStmtPtr = std::shared_ptr<GStmt>;
        using Program = std::vector<GStmtPtr>;

        enum class T { FDECL, VDECL };
        virtual T type() const = 0;

        std::size_t start, length;

        GStmt(std::size_t start, std::size_t length);

        template<typename U>
        struct Visitor {
            virtual U visitGStmtFDecl(gstmt::FDecl *gs) = 0;
            virtual U visitGStmtVDecl(gstmt::VDecl *gs) = 0;

            inline U visitGStmt(GStmt *gs) {
                switch (gs->type()) {
                    case T::FDECL: return visitGStmtFDecl((gstmt::FDecl*)gs);
                    case T::VDECL: return visitGStmtVDecl((gstmt::VDecl*)gs);
                    default:       throw std::runtime_error("badly formed AST");
                }
            }

            virtual U visitGStmtProgram(const Program &prog) = 0;
        };
    };

    using GStmtPtr = GStmt::GStmtPtr;
    using Program = GStmt::Program;

    namespace gstmt {

        struct FDecl : public GStmt {
            std::string id;
            TypePtr rty;
            std::vector<std::pair<std::string /*id*/, TypePtr /*type*/>> args;
            Block body;

            T type() const override final { return T::FDECL; }

            FDecl(std::size_t start, std::size_t length, const std::string &id, const TypePtr &rty, const decltype(args) &args, const Block &body);
            static GStmtPtr of(std::size_t start, std::size_t length, const std::string &id, const TypePtr &rty, const decltype(args) &args, const Block &body);
        };

        struct VDecl : public GStmt {
            std::string id;
            ExprPtr exp;

            T type() const override final { return T::VDECL; }

            VDecl(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp);
            static GStmtPtr of(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp);
        };

    } // namespace gstmt

} // namespace drm
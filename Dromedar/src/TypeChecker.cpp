#include <sstream>

// debugging purposes
#include <iostream>

#include "TypeChecker.hpp"

namespace drm {

    const std::map<Token::Type, std::vector<std::tuple<TypePtr, TypePtr>>> TypeChecker::UOPS = {
        { Token::BANG, {
            { type::Prim::BOOL, type::Prim::BOOL }
        } },
        { Token::DASH, {
            { type::Prim::INT,  type::Prim::INT },
            { type::Prim::FLT,  type::Prim::FLT }
        } }
    };

#define INT_FLT_BOP { type::Prim::INT, type::Prim::INT, type::Prim::INT }, { type::Prim::FLT, type::Prim::FLT, type::Prim::FLT }
#define INT_BOP     { type::Prim::INT, type::Prim::INT, type::Prim::INT }
#define BOOL_BOP    { type::Prim::BOOL, type::Prim::BOOL, type::Prim::BOOL }
#define CMP_OP      { type::Prim::INT, type::Prim::INT, type::Prim::BOOL }, { type::Prim::FLT, type::Prim::FLT, type::Prim::BOOL }, { type::Prim::CHAR, type::Prim::CHAR, type::Prim::BOOL }
    const std::map<Token::Type, std::vector<std::tuple<TypePtr, TypePtr, TypePtr>>> TypeChecker::BOPS = {
        { Token::STARSTAR, { INT_FLT_BOP } },
        { Token::STAR, { INT_FLT_BOP } },
        { Token::PLUS, {
            INT_FLT_BOP,
            { type::Prim::INT,  type::Prim::CHAR, type::Prim::CHAR },
            { type::Prim::CHAR, type::Prim::INT,  type::Prim::CHAR }
        } },
        { Token::DASH, {
            INT_FLT_BOP,
            { type::Prim::INT,  type::Prim::CHAR, type::Prim::CHAR },
            { type::Prim::CHAR, type::Prim::INT,  type::Prim::CHAR }
        } },
        { Token::LSHIFT, { INT_BOP } },
        { Token::RSHIFT, { INT_BOP } },
        { Token::ASHIFT, { INT_BOP } },
        { Token::BITAND, { INT_BOP } },
        { Token::XOR, { 
            INT_BOP,
            { type::Prim::BOOL, type::Prim::BOOL, type::Prim::BOOL }
         } },
        { Token::BITOR,  { INT_BOP  } },
        { Token::LOGAND, { BOOL_BOP } },
        { Token::LOGOR,  { BOOL_BOP } },
        { Token::EQUAL,      { CMP_OP } },
        { Token::NOT_EQUAL,  { CMP_OP } },
        { Token::LESS,       { CMP_OP } },
        { Token::GREATER,    { CMP_OP } },
        { Token::LESS_EQ,    { CMP_OP } },
        { Token::GREATER_EQ, { CMP_OP } }
    };
#undef CMP_OP
#undef BOOL_BOP
#undef INT_BOP
#undef INT_FLT_BOP

    TypeError::TypeError(std::size_t start, std::size_t length, std::string message) {
        this->start = start;
        this->length = length;
        this->message = message;
    }

    std::string TypeError::toString() const {
        std::stringstream stream;
        stream << "[" << start << ":" << length << "] -> " << message;
        return stream.str();
    }

    TypeChecker::TypeChecker() {}

    bool TypeChecker::hasBinding(const std::string &id) {
        for (const auto &u : bindings)
            if (u.find(id) != u.end())
                return true;
        return false;
    }

    bool TypeChecker::hasTopLevelBinding(const std::string &id) {
        return !bindings.empty() && bindings.back().find(id) != bindings.back().end();
    }

    TypePtr TypeChecker::getBinding(const std::string &id) {
        for (std::ptrdiff_t i = static_cast<std::ptrdiff_t>(bindings.size()) - 1; i >= 0; i--)
            if (bindings.at(i).find(id) != bindings.at(i).end())
                return bindings.at(i).at(id);
        throw std::runtime_error("binding not declared");
    }

    void TypeChecker::addBinding(const std::string &id, const TypePtr &type) {
        bindings.back().insert(std::make_pair(id, type));
    }

    ExprRes TypeChecker::visitExprId(expr::Id *e) {
        if (hasBinding(e->id)) {
            auto bnd = getBinding(e->id);
            return { getBinding(e->id), true };
        } else
            throw TypeError(e->start, e->length, "Undeclared Variable " + e->id);
    }

    ExprRes TypeChecker::visitExprLit(expr::Lit *e) {
        return { type::Prim::of(e->start, e->length, boost::apply_visitor(TypeOfLit(), e->lt)), false };
    }

    ExprRes TypeChecker::visitExprUop(expr::Uop *e) {
        const auto &v = UOPS.at(e->op);
        for (const auto &u : v) {
            if (Type::equiv(visitExpr(e->exp.get()).type, std::get<0>(u)))
                return { std::get<1>(u), false };
        }
        throw TypeError(e->start, e->length, "Unary operation: Undefined with given operand type");
    }

    ExprRes TypeChecker::visitExprBop(expr::Bop *e) {
        const auto &v = BOPS.at(e->op);
        for (const auto &u : v) {
            if (Type::equiv(visitExpr(e->lexp.get()).type, std::get<0>(u)) &&
                Type::equiv(visitExpr(e->rexp.get()).type, std::get<1>(u))) {
                return { std::get<2>(u), false };
            }
        }
        throw TypeError(e->start, e->length, "Binary operation: Undefined with given operand types");
    }

    ExprRes TypeChecker::visitExprFunc(expr::Func *e) {
        TypePtr ft = visitExpr(e->lhs.get()).type;
        if (ft->type() != Type::T::FTYPE)
            throw TypeError(e->start, e->length, "Function application: Function must be a function type");
        type::FType *ftype = std::static_pointer_cast<type::FType>(ft).get();
        if (ftype->argts.size() != e->args.size())
            throw TypeError(e->start, e->length, "Function application: Argument list length mismatch");
        for (std::size_t i = 0; i < ftype->argts.size(); i++)
            if (!Type::equiv(ftype->argts.at(i), visitExpr(e->args.at(i).get()).type))
                throw TypeError(e->args.at(i)->start, e->args.at(i)->length, "Function application: Type mismatch in argument");
        return { ftype->rty, false };
    }

    ExprRes TypeChecker::visitExprCmpList(expr::CmpList *e) {
        if (e->cmps.empty())
            throw std::runtime_error("badly formed AST");
        TypePtr cur_type = visitExpr(e->first.get()).type;
        TypePtr cur_res;
        for (std::size_t i = 0; i < e->cmps.size(); i++) {
            TypePtr rhs_type = visitExpr(e->cmps.at(i).second.get()).type;
            const auto &v = BOPS.at(e->cmps.at(i).first);
            bool success = false;
            for (const auto &u : v) {
                if (Type::equiv(cur_type, std::get<0>(u)) &&
                    Type::equiv(rhs_type, std::get<1>(u))) {

                    if (i == 0) {
                        cur_res = std::get<2>(u);
                        success = true; break;
                    } else if (Type::equiv(cur_res, std::get<2>(u))) {
                        cur_type = rhs_type;
                        success = true; break;
                    } else {
                        throw TypeError(e->start, e->length, "Comparison list: Return type mismatch for comparison operators");
                    }
                }
            }
            if (!success)
                throw TypeError(e->start, e->length, "Comparison list: Operator undefined with given input operand types");
        }
        return { cur_res, false };
    }

    bool TypeChecker::visitTypePrim(type::Prim *t) {
        switch (t->t) {
            case Token::KEY_INT: case Token::KEY_FLT: case Token::KEY_CHAR: case Token::KEY_BOOL:
                return true;
            case Token::KEY_VOID:
                return false;
            default:
                throw std::runtime_error("badly formed AST");
        }
    }

    bool TypeChecker::visitTypeFType(type::FType *t) {
        visitType(t->rty.get());
        for (const auto &u : t->argts)
            visitType(u.get());
        return false;
    }

    bool TypeChecker::visitStmtVDecl(stmt::VDecl *s) {
        TypePtr type = visitExpr(s->exp.get()).type;
        if (!visitType(type.get()))
            throw TypeError(s->exp->start, s->exp->length, "Type of expression is not assignable");
        if (hasTopLevelBinding(s->id))
            throw TypeError(s->start, s->length, "Variable already declared");
        addBinding(s->id, type);
        return false;
    }

    bool TypeChecker::visitStmtAssn(stmt::Assn *s) {
        ExprRes lhs = visitExpr(s->lhs.get());
        if (!lhs.write_to)
            throw TypeError(s->lhs->start, s->lhs->length, "Left-hand side expression cannot be written to");
        if (!Type::equiv(lhs.type, visitExpr(s->exp.get()).type))
            throw TypeError(s->exp->start, s->exp->length, "Assignment: type mismatch");
        return false;
    }

    bool TypeChecker::visitStmtExpr(stmt::Expr *s) {
        visitExpr(s->exp.get());
        return false;
    }

    bool TypeChecker::visitStmtIf(stmt::If *s) {
        if (!Type::equiv(visitExpr(s->cond.get()).type, type::Prim::BOOL))
            throw TypeError(s->cond->start, s->cond->length, "If condition must be of type bool");
        return visitStmtBlock(s->taken) && visitStmtBlock(s->not_taken);
    }

    bool TypeChecker::visitStmtWhile(stmt::While *s) {
        if (!Type::equiv(visitExpr(s->cond.get()).type, type::Prim::BOOL))
            throw TypeError(s->cond->start, s->cond->length, "While condition must be of type bool");
        visitStmtBlock(s->body);
        return false;
    }

    bool TypeChecker::visitStmtDoWhile(stmt::DoWhile *s) {
        if (!Type::equiv(visitExpr(s->cond.get()).type, type::Prim::BOOL))
            throw TypeError(s->cond->start, s->cond->length, "While condition must be of type bool");
        return visitStmtBlock(s->body);
    }

    bool TypeChecker::visitStmtReturn(stmt::Return *s) {
        if (s->with_value) {
            auto exp = visitExpr(s->exp.get()).type;
            if (!Type::equiv(frtys.top(), exp))
                throw TypeError(s->exp->start, s->exp->length, "Return expression type mismatch");
        } else {
            if (!Type::equiv(frtys.top(), type::Prim::VOID))
                throw TypeError(s->start, s->length, "Return statement should return a value in non-void function");
        }
        return true;
    }

    bool TypeChecker::visitStmtBlock(const Block &block) {
        bool res = false;

        bindings.push_back({});
        for (const auto &u : block) {
            try {
                if (res)
                    throw TypeError(u->start, u->length, "Unreachable Statement");
                res = visitStmt(u.get());
            } catch (const TypeError &e) {
                errors.push_back(e);
            }
        }
        bindings.pop_back();

        return res;
    }

    void TypeChecker::visitGStmtFDecl(gstmt::FDecl *gs) {
        bindings.push_back({});

        for (const auto &u : gs->args)
            addBinding(u.first, u.second);
        
        frtys.push(gs->rty);
        
        if (!visitStmtBlock(gs->body) && !Type::equiv(gs->rty, type::Prim::VOID)) {
            throw TypeError(gs->start, gs->length, "function body doesn't return");
        }

        frtys.pop();

        bindings.pop_back();
    }

    void TypeChecker::visitGStmtVDecl(gstmt::VDecl *gs) {
        TypePtr type = visitExpr(gs->exp.get()).type;
        if (!visitType(type.get()))
            throw TypeError(gs->exp->start, gs->exp->length, "Type of expression is not assignable");
        if (hasBinding(gs->id))
            throw TypeError(gs->start, gs->length, "Variable already declared");
        addBinding(gs->id, type);
    }

    void TypeChecker::visitGStmtProgram(const Program &prog) {

        // reset fields
        bindings = {{}};
        while (!frtys.empty())
            frtys.pop();
        errors.clear();

        // first populate bindings for global functions
        for (const auto &u : prog) {
            if (u->type() == GStmt::T::FDECL) {
                gstmt::FDecl *fd = (gstmt::FDecl*)u.get();
                if (hasBinding(fd->id))
                    throw TypeError(u->start, u->length, "Function already declared");

                std::vector<TypePtr> argts;
                for (const auto &v : fd->args)
                    argts.push_back(v.second);
                
                addBinding(fd->id, type::FType::of(u->start, u->length, fd->rty, argts));
            }
        }

        // typecheck program
        for (const auto &u : prog) {
            try {
                visitGStmt(u.get());
            } catch (TypeError &e) {
                errors.push_back(e);
            } catch (std::exception &e) { // handle badly formed ASTs
                throw e;
            }
        }
    }

    std::string TypeChecker::errorMessages() const {
        std::stringstream stream;
        for (const auto &u : errors)
            stream << u.toString() << '\n';
        return stream.str();
    }

} // namespace drm
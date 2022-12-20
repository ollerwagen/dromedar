#include <stdexcept>
#include <utility>

#include <boost/variant.hpp>

#include "Transpiler.hpp"
#include "TypeChecker.hpp"

namespace drm {

    const std::unordered_map<Token::Type, std::string> Transpiler::OPS = {
        { Token::BANG,       "!"  },
        { Token::DASH,       "-"  },
        { Token::STARSTAR,   "*"  }, // fix this!
        { Token::STAR,       "*"  },
        { Token::PLUS,       "+"  },
        { Token::BITAND,     "&"  },
        { Token::BITOR,      "|"  },
        { Token::XOR,        "^"  },
        { Token::EQUAL,      "==" },
        { Token::NOT_EQUAL,  "!=" },
        { Token::GREATER,    ">"  },
        { Token::LESS,       "<"  },
        { Token::GREATER_EQ, ">=" },
        { Token::LESS_EQ,    "<=" },
    };

    class ToStringVisitor : public boost::static_visitor<Str> {
        Str operator()(Token::Int  i) { return std::to_string(i); }
        Str operator()(Token::Flt  f) { std::string res = std::to_string(f); if (res.find('.') == res.npos) return res + ".0"; else return res; }
        Str operator()(Token::Char c) { return std::string("'") + c + '\''; }
        Str operator()(Token::Bool b) { return b ? "true" : "false"; }
        Str operator()(Token::Id   i) { throw std::runtime_error("badly formed AST"); }
    };

    class ToTypeVisitor : public boost::static_visitor<TypePtr> {
        TypePtr operator()(Token::Int  i) { return type::Prim::of(0UL, 0UL, Token::KEY_INT);  }
        TypePtr operator()(Token::Flt  f) { return type::Prim::of(0UL, 0UL, Token::KEY_FLT);  }
        TypePtr operator()(Token::Char c) { return type::Prim::of(0UL, 0UL, Token::KEY_CHAR); }
        TypePtr operator()(Token::Bool b) { return type::Prim::of(0UL, 0UL, Token::KEY_BOOL); }
        TypePtr operator()(Token::Id   i) { throw std::runtime_error("badly formed AST"); }
    };

    std::string Transpiler::gensym(const std::string &base) {
        static std::unordered_map<std::string, int> indices;

        if (indices.find(base) == indices.end()) {
            return base + std::to_string(++indices.at(base));
        } else {
            indices.insert(std::make_pair(base, 0));
            return gensym(base);
        }
    }

    CmpExprRes Transpiler::visitExprId(expr::Id *e) {
        const auto &bnd = variables.at(e->id);
        return { "", bnd.first, bnd.second };
    }

    CmpExprRes Transpiler::visitExprLit(expr::Lit *e) {
        return { "", boost::apply_visitor(ToStringVisitor(), e->lt), boost::apply_visitor(ToTypeVisitor(), e->lt) };
    }

    CmpExprRes Transpiler::visitExprUop(expr::Uop *e) {
        CmpExprRes cmp_exp = visitExpr(e->exp.get());
        return { cmp_exp.stream, OPS.at(e->op) + "(" + cmp_exp.res + ")",
            std::get<1>(*std::find_if(TypeChecker::UOPS.at(e->op).begin(), TypeChecker::UOPS.at(e->op).end(),
                [&cmp_exp] (const std::tuple<TypePtr, TypePtr> &t) -> bool { return Type::equiv(std::get<0>(t), cmp_exp.type); })) };
    }

    CmpExprRes Transpiler::visitExprBop(expr::Bop *e) {
        CmpExprRes cmp_lhs = visitExpr(e->lexp.get()), cmp_rhs = visitExpr(e->rexp.get());
        TypePtr rty = std::get<2>(*std::find_if(TypeChecker::BOPS.at(e->op).begin(), TypeChecker::BOPS.at(e->op).end(),
            [&cmp_lhs,&cmp_rhs] (const std::tuple<TypePtr, TypePtr, TypePtr> &t) -> bool { 
                return Type::equiv(std::get<0>(t), cmp_lhs.type) && Type::equiv(std::get<1>(t), cmp_rhs.type); 
            } ));
        return { cmp_lhs.stream + cmp_rhs.stream, "(" + visitType(rty.get()) + "(" + cmp_lhs.res + OPS.at(e->op) + cmp_rhs.res + "))", rty };
    }

    CmpExprRes Transpiler::visitExprFunc(expr::Func *e) {
        CmpExprRes fres = visitExpr(e->lhs.get());
        Str stream = fres.stream, fcall = "((" + fres.res + ")(";
        for (const auto &u : e->args) {
            CmpExprRes ares = visitExpr(u.get());
            stream += ares.stream;
            fcall += ares.res + ",";
        }
        if (fcall.back() == ',')
            fcall.pop_back();
        return { stream, fcall + "))", std::static_pointer_cast<type::FType>(fres.type)->rty };
    }

    CmpExprRes Transpiler::visitExprCmpList(expr::CmpList *e) {
        /*
        t x1 = exp1
        t x2 = exp2
        t' res = false
        if (x1 OP x2) {
            t x3 = exp3
            if (x2 OP x3) {
                t x4 = exp4
                ...
                if (x(n-1) OP xn)
                    res = true
            }
        }
        
        */

        CmpExprRes first_res = visitExpr(e->first.get());
        std::vector<CmpExprRes> list_res;
        for (const auto &u : e->cmps)
            list_res.push_back(visitExpr(u.second.get()));
        
        CmpExprRes res;
        res.stream = first_res.stream + list_res.front().stream;

        Str varname_lhs = gensym("x"), varname_rhs = gensym("x");
        Str assn_stmt = visitType(first_res.type.get()) + " " + varname_lhs + " = " + first_res.res + ";";

        res.stream += assn_stmt;

        assn_stmt = visitType(list_res.front().type.get()) + " " + varname_rhs + " = " + list_res.front().res + ";";
    }

} // namespace drm
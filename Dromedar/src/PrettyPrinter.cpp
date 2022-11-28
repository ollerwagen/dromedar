#include <sstream>

#include "PrettyPrinter.hpp"

namespace drm {

    const std::unordered_map<Token::Type, std::string> PrettyPrinter::OPS {
        { Token::Type::BANG,     "!"  },
        { Token::Type::STARSTAR, "**" },
        { Token::Type::STAR,     "*"  },
        { Token::Type::PLUS,     "+"  }, { Token::Type::DASH,       "-"  },
        { Token::Type::LSHIFT,   "<<" }, { Token::Type::RSHIFT,     ">>" }, { Token::Type::ASHIFT, ">>>" },
        { Token::Type::BITAND,   "&"  },
        { Token::Type::XOR,      "^"  },
        { Token::Type::BITOR,    "|"  },
        { Token::Type::LOGAND,   "&&" },
        { Token::Type::LOGOR,    "||" },
        { Token::Type::EQUAL,    "="  }, { Token::Type::NOT_EQUAL,  "!=" },
        { Token::Type::LESS,     "<"  }, { Token::Type::GREATER,    ">"  },
        { Token::Type::LESS_EQ,  "<=" }, { Token::Type::GREATER_EQ, ">=" }
    };

    PrettyPrinter::PrettyPrinter() {
        indent = "";
    }

    std::string PrettyPrinter::visitExprId(expr::Id *e) {
        return "(" + e->id + ")";
    }

    std::string PrettyPrinter::visitExprLit(expr::Lit *e) {
        std::stringstream stream;
        stream << e->lt;
        return stream.str();
    }

    std::string PrettyPrinter::visitExprUop(expr::Uop *e) {
        return "(" + OPS.at(e->op) + visitExpr(e->exp.get()) + ")";
    }

    std::string PrettyPrinter::visitExprBop(expr::Bop *e) {
        return "(" + visitExpr(e->lexp.get()) + OPS.at(e->op) + visitExpr(e->rexp.get()) + ")";
    }

    std::string PrettyPrinter::visitExprFunc(expr::Func *e) {
        std::stringstream stream;
        stream << "(" << visitExpr(e->lhs.get()) << "(";
        if (!e->args.empty()) {
            for (std::size_t i = 0; i < e->args.size() - 1; i++)
                stream << visitExpr(e->args.at(i).get()) << ",";
            stream << visitExpr(e->args.back().get());
        }
        stream << "))";
        return stream.str();
    }

    std::string PrettyPrinter::visitExprCmpList(expr::CmpList *e) {
        std::stringstream stream;
        stream << "(" << visitExpr(e->first.get());
        for (const auto &u : e->cmps)
            stream << OPS.at(u.first) << visitExpr(u.second.get());
        stream << ")";
        return stream.str();
    }

    std::string PrettyPrinter::visitTypePrim(type::Prim *t) {
        switch (t->t) {
            case Token::Type::KEY_VOID: return "void";
            case Token::Type::KEY_INT:  return "int";
            case Token::Type::KEY_FLT:  return "flt";
            case Token::Type::KEY_BOOL: return "bool";
            case Token::Type::KEY_CHAR: return "char";
            default:                    throw std::runtime_error("badly formed AST");
        }
    }

    std::string PrettyPrinter::visitTypeFType(type::FType *t) {
        std::stringstream stream;
        stream << "(";
        if (!t->argts.empty()) {
            for (std::size_t i = 0; i < t->argts.size() - 1; i++)
                stream << visitType(t->argts.at(i).get()) << ",";
            stream << visitType(t->argts.back().get());
        }
        stream << ") -> " << visitType(t->rty.get());
        return stream.str();
    }

    std::string PrettyPrinter::visitStmtVDecl(stmt::VDecl *s) {
        return indent + "let " + s->id + " := " + visitExpr(s->exp.get()) + "\n";
    }

    std::string PrettyPrinter::visitStmtAssn(stmt::Assn *s) {
        return indent + visitExpr(s->lhs.get()) + " := " + visitExpr(s->exp.get()) + "\n";
    }

    std::string PrettyPrinter::visitStmtExpr(stmt::Expr *s) {
        return indent + visitExpr(s->exp.get()) + "\n";
    }

    std::string PrettyPrinter::visitStmtIf(stmt::If *s) {
        return indent + "if " + visitExpr(s->cond.get()) + '\n'+ visitStmtBlock(s->taken) +
            indent + "else\n" + visitStmtBlock(s->not_taken);
    }

    std::string PrettyPrinter::visitStmtWhile(stmt::While *s) {
        return indent + "while " + visitExpr(s->cond.get()) + '\n'+ visitStmtBlock(s->body);
    }

    std::string PrettyPrinter::visitStmtDoWhile(stmt::DoWhile *s) {
        return indent + "do\n" + visitStmtBlock(s->body) + indent + "while " + visitExpr(s->cond.get()) + '\n';
    }

    std::string PrettyPrinter::visitStmtReturn(stmt::Return *s) {
        if (s->withValue)
            return indent + "return " + visitExpr(s->exp.get()) + "\n";
        else
            return indent + "return\n";
    }

    std::string PrettyPrinter::visitStmtBlock(const Block &block) {
        std::stringstream stream;
        indent += "  ";
        for (const auto &s : block) {
            stream << visitStmt(s.get());
        }
        indent.pop_back(), indent.pop_back();
        return stream.str();
    }

    std::string PrettyPrinter::visitGStmtFDecl(gstmt::FDecl *gs) {
        std::stringstream stream;
        stream << indent << "fn " << gs->id;
        if (!gs->args.empty()) {
            stream << " : ";
            for (std::size_t i = 0; i < gs->args.size() - 1; i++)
                stream << gs->args.at(i).first << ":" << visitType(gs->args.at(i).second.get()) << ", ";
            stream << gs->args.back().first << ":" << visitType(gs->args.back().second.get());
        }
        stream << " -> " << visitType(gs->rty.get()) << '\n' << visitStmtBlock(gs->body);
        return stream.str();
    }

    std::string PrettyPrinter::visitGStmtVDecl(gstmt::VDecl *gs) {
        return indent + "global " + gs->id + " := " + visitExpr(gs->exp.get()) + '\n';
    }

    std::string PrettyPrinter::visitGStmtProgram(const Program &prog) {
        std::stringstream stream;
        for (const auto &u : prog)
            stream << visitGStmt(u.get());
        return stream.str();
    }

} // namespace drm
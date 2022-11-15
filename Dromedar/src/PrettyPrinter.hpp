#pragma once

#include <sstream>
#include <string>

#include "AST.hpp"

namespace drm {

    class PrettyPrinter :
        public TypeExpression::Visitor<std::string>,
        public GlobalStatement::Visitor<std::string>,
        public Statement::Visitor<std::string>,
        public Expression::Visitor<std::string>,
        public LHS::Visitor<std::string> {

    private:

        std::string indent;

    public:

        PrettyPrinter() {
            indent = "";
        }

        ~PrettyPrinter() {}

        std::string visitProgram(const Program &prog) {
            std::stringstream stream;
            for (const auto &p : prog) {
                stream << visitGlobalStatement(p.get()) << std::endl;
            }
            return stream.str();
        }

        std::string visitGlobalStatement(const GlobalStatement *stmt) override {
            switch (stmt->getType()) {
                case GlobalStatement::Type::GVARDECL:     return visitGVarDecl(static_cast<const GVarDeclStatement*>(stmt));
                case GlobalStatement::Type::GTYPEVARDECL: return visitGTypeVarDecl(static_cast<const GTypeVarDeclStatement*>(stmt));
                case GlobalStatement::Type::GFNDECL:      return visitGFDecl(static_cast<const GFDeclStatement*>(stmt));
                default:                                  return "<match fail>"; // never reached if all types are matched
            }
        }

        std::string visitGVarDecl(const GVarDeclStatement *stmt) override {
            return std::string("global ") + (stmt->mut ? "mut " : "") + stmt->id.lexeme + " := " + visitExpression(stmt->expr.get());
        }

        std::string visitGTypeVarDecl(const GTypeVarDeclStatement *stmt) override {
            return std::string("global ") + (stmt->mut ? "mut " : "") + stmt->id.lexeme + " : " + visitTypeExpression(stmt->type.get()) + " := " + visitExpression(stmt->expr.get());
        }

        std::string visitGFDecl(const GFDeclStatement *stmt) override {
            std::stringstream stream;
            stream << "fn " << stmt->id.lexeme;
            if (!stmt->args.empty()) {
                stream << " : ";
                for (std::size_t i = 0; i < stmt->args.size() - 1; i++) {
                    stream << stmt->args.at(i).first.lexeme << ":" << visitTypeExpression(stmt->args.at(i).second.get()) << ", ";
                }
                stream << stmt->args.back().first.lexeme << ":" << visitTypeExpression(stmt->args.back().second.get());
            }
            stream << " -> " << visitTypeExpression(stmt->rettype.get()) << '\n' << visitBlock(stmt->body) << '\n';
            return stream.str();
        }

        std::string visitTypeExpression(const TypeExpression *expr) override {
            switch (expr->getType()) {
                case TypeExpression::Type::PRIMITIVE: return visitPrimitiveTypeExpression(static_cast<const PrimitiveTypeExpression*>(expr));
                default:                              return "<match fail>"; // never reached if all types are matched
            }
        }

        std::string visitPrimitiveTypeExpression(const PrimitiveTypeExpression *expr) override {
            switch (expr->type) {
                case PrimitiveTypeExpression::PType::INT:  return "int";
                case PrimitiveTypeExpression::PType::FLT:  return "flt";
                case PrimitiveTypeExpression::PType::CHAR: return "char";
                case PrimitiveTypeExpression::PType::BOOL: return "bool";
                case PrimitiveTypeExpression::PType::VOID: return "void";
                default:                    return "<not well-formed>"; // never reached in well-formed AST
            }
        }

        std::string visitBlock(const Block &block) override {
            std::stringstream stream;

            indent.push_back('\t');
            for (const auto &stmt : block) {
                stream << indent << visitStatement(stmt.get()) << std::endl;
            }
            indent.pop_back();

            return stream.str();
        }

        std::string visitStatement(const Statement *stmt) override {
            switch (stmt->getType()) {
                case Statement::Type::VARDECL:     return visitVarDecl(static_cast<const VarDeclStatement*>(stmt));
                case Statement::Type::TVARDECL:    return visitTypeVarDecl(static_cast<const TypeVarDeclStatement*>(stmt));
                case Statement::Type::ASSIGN:      return visitAssignStatement(static_cast<const AssignStatement*>(stmt));
                case Statement::Type::IFSTMT:      return visitIfStatement(static_cast<const IfStatement*>(stmt));
                case Statement::Type::WHILESTMT:   return visitWhileStatement(static_cast<const WhileStatement*>(stmt));
                case Statement::Type::DOWHILESTMT: return visitDoWhileStatement(static_cast<const DoWhileStatement*>(stmt));
                case Statement::Type::RETURN:      return visitReturnStatement(static_cast<const ReturnStatement*>(stmt));
                default:                           return "<match fail>"; // never reached in well-formed AST
            }
        }

        std::string visitVarDecl(const VarDeclStatement *stmt) override {
            return std::string("let ") + (stmt->mut ? "mut " : "") + stmt->id.lexeme + " := " + visitExpression(stmt->expr.get());
        }

        std::string visitTypeVarDecl(const TypeVarDeclStatement *stmt) override {
            return std::string("let ") + (stmt->mut ? "mut " : "") + stmt->id.lexeme + " : " + visitTypeExpression(stmt->type.get()) + " := " + visitExpression(stmt->expr.get());
        }

        std::string visitAssignStatement(const AssignStatement *stmt) override {
            return visitLHS(stmt->lhs.get()) + " := " + visitExpression(stmt->expr.get());
        }

        std::string visitIfStatement(const IfStatement *stmt) override {
            std::stringstream stream;
            stream << "if " << visitExpression(stmt->cond.get()) << '\n';
            if (!stmt->taken.empty()) stream << visitBlock(stmt->taken);
            if (!stmt->not_taken.empty())
                stream << indent << "else" << '\n' << visitBlock(stmt->not_taken);
            return stream.str();
        }

        std::string visitWhileStatement(const WhileStatement *stmt) override {
            std::stringstream stream;
            stream << "while " << visitExpression(stmt->cond.get()) << '\n';
            if (!stmt->body.empty()) stream << visitBlock(stmt->body);
            return stream.str();
        }

        std::string visitDoWhileStatement(const DoWhileStatement *stmt) override {
            std::stringstream stream;
            stream << "do\n";
            if (!stmt->body.empty()) stream << visitBlock(stmt->body);
            stream << indent << "while " << visitExpression(stmt->cond.get());
            return stream.str();
        }

        std::string visitReturnStatement(const ReturnStatement *stmt) override {
            return "return " + (stmt->withVal ? visitExpression(stmt->expr.get()) : std::string(""));
        }

        std::string visitLHS(const LHS *lhs) override {
            switch (lhs->getType()) {
                case LHS::Type::VARIABLE: return visitVariableLHS(static_cast<const VariableLHS*>(lhs));
                default:                  return "<match fail>";
            }
        }

        std::string visitVariableLHS(const VariableLHS *lhs) override {
            return lhs->id.lexeme;
        }

        std::string visitExpression(const Expression *expr) override {
            switch (expr->getType()) {
                case Expression::Type::LITERAL:  return visitLiteralExpression(static_cast<const LiteralExpression*>(expr));
                case Expression::Type::UNARY:    return visitUnaryExpression(static_cast<const UnaryExpression*>(expr));
                case Expression::Type::BINARY:   return visitBinaryExpression(static_cast<const BinaryExpression*>(expr));
                case Expression::Type::COMPLIST: return visitComparisonList(static_cast<const ComparisonList*>(expr));
                default:                         return "<match fail>";
            }
        }

        std::string visitLiteralExpression(const LiteralExpression *expr) override {
            return "(" + expr->lit.lexeme + ")";
        }

        std::string visitUnaryExpression(const UnaryExpression *expr) override {
            return "(" + expr->op.lexeme + visitExpression(expr->expr.get()) + ")";
        }

        std::string visitBinaryExpression(const BinaryExpression *expr) override {
            return "(" + visitExpression(expr->lexp.get()) + expr->op.lexeme + visitExpression(expr->rexp.get()) + ")";
        }

        std::string visitComparisonList(const ComparisonList *expr) override {
            std::stringstream stream;
            stream << "(" << visitExpression(expr->first.get());
            for (const auto &u : expr->comparisons)
                stream << u.first.lexeme << visitExpression(u.second.get());
            stream << ")";
            return stream.str();
        }

    }; // class PrettyPrinter

} // namespace drm
#pragma once

#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "AST.hpp"
#include "PrettyPrinter.hpp"
#include "Token.hpp"

namespace drm {

    struct BadlyFormedError {

        Token token;
        std::string message;

        BadlyFormedError(const Token &token, const std::string &message) {
            this->token = token;
            this->message = message;
        }
    };

    class WellFormedCheck :
        public TypeExpression::Visitor<void>,
        public LHS::Visitor<std::shared_ptr<TypeExpression>>,
        public Expression::Visitor<std::shared_ptr<TypeExpression>>,
        public Statement::Visitor<void>,
        public GlobalStatement::Visitor<void> {

    private:

        const std::unordered_map<Token::Type, std::vector<std::pair<std::shared_ptr<TypeExpression>, std::shared_ptr<TypeExpression>>>> UNARIES = {
            { Token::Type::DASH, {
                { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() },
                { PrimitiveTypeExpression::MAKE_FLT(), PrimitiveTypeExpression::MAKE_FLT() }
            } },
            { Token::Type::BANG, {
                { PrimitiveTypeExpression::MAKE_BOOL(), PrimitiveTypeExpression::MAKE_BOOL() }
            } }
        };

        const std::unordered_map<Token::Type, std::vector<std::pair<std::pair<std::shared_ptr<TypeExpression>, std::shared_ptr<TypeExpression>>, std::shared_ptr<TypeExpression>>>> BINARIES = {
            { Token::Type::STARSTAR, {
                { { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() }, PrimitiveTypeExpression::MAKE_INT() },
                { { PrimitiveTypeExpression::MAKE_FLT(), PrimitiveTypeExpression::MAKE_FLT() }, PrimitiveTypeExpression::MAKE_FLT() }
            } },
            { Token::Type::STAR, {
                { { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() }, PrimitiveTypeExpression::MAKE_INT() },
                { { PrimitiveTypeExpression::MAKE_FLT(), PrimitiveTypeExpression::MAKE_FLT() }, PrimitiveTypeExpression::MAKE_FLT() }
            } },
            { Token::Type::PLUS, {
                { { PrimitiveTypeExpression::MAKE_INT(),  PrimitiveTypeExpression::MAKE_INT()  }, PrimitiveTypeExpression::MAKE_INT() },
                { { PrimitiveTypeExpression::MAKE_FLT(),  PrimitiveTypeExpression::MAKE_FLT()  }, PrimitiveTypeExpression::MAKE_FLT() },
                { { PrimitiveTypeExpression::MAKE_INT(),  PrimitiveTypeExpression::MAKE_CHAR() }, PrimitiveTypeExpression::MAKE_CHAR() },
                { { PrimitiveTypeExpression::MAKE_CHAR(), PrimitiveTypeExpression::MAKE_INT()  }, PrimitiveTypeExpression::MAKE_CHAR() }
            } },
            { Token::Type::DASH, {
                { { PrimitiveTypeExpression::MAKE_INT(),  PrimitiveTypeExpression::MAKE_INT()  }, PrimitiveTypeExpression::MAKE_INT() },
                { { PrimitiveTypeExpression::MAKE_FLT(),  PrimitiveTypeExpression::MAKE_FLT()  }, PrimitiveTypeExpression::MAKE_FLT() },
                { { PrimitiveTypeExpression::MAKE_INT(),  PrimitiveTypeExpression::MAKE_CHAR() }, PrimitiveTypeExpression::MAKE_CHAR() },
                { { PrimitiveTypeExpression::MAKE_CHAR(), PrimitiveTypeExpression::MAKE_INT()  }, PrimitiveTypeExpression::MAKE_CHAR() }
            } },
            { Token::Type::BITAND, {
                { { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() }, PrimitiveTypeExpression::MAKE_INT() }
            } },
            { Token::Type::XOR, {
                { { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() }, PrimitiveTypeExpression::MAKE_INT() }
            } },
            { Token::Type::BITOR, {
                { { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() }, PrimitiveTypeExpression::MAKE_INT() }
            } },
            { Token::Type::LOGAND, {
                { { PrimitiveTypeExpression::MAKE_BOOL(), PrimitiveTypeExpression::MAKE_BOOL() }, PrimitiveTypeExpression::MAKE_BOOL() }
            } },
            { Token::Type::LOGOR, {
                { { PrimitiveTypeExpression::MAKE_BOOL(), PrimitiveTypeExpression::MAKE_BOOL() }, PrimitiveTypeExpression::MAKE_BOOL() }
            } }
        };

        const std::vector<std::pair<std::pair<std::shared_ptr<TypeExpression>, std::shared_ptr<TypeExpression>>, std::shared_ptr<TypeExpression>>> COMPARATORS = {
            { { PrimitiveTypeExpression::MAKE_INT(), PrimitiveTypeExpression::MAKE_INT() }, PrimitiveTypeExpression::MAKE_BOOL() },
            { { PrimitiveTypeExpression::MAKE_FLT(), PrimitiveTypeExpression::MAKE_FLT() }, PrimitiveTypeExpression::MAKE_BOOL() },
        };

        inline std::pair<bool, std::shared_ptr<TypeExpression>> checkUnary(Token::Type op, const std::shared_ptr<TypeExpression> &type) {
            if (UNARIES.find(op) == UNARIES.end())
                throw std::runtime_error("not well-formed AST");
            for (const auto &u : UNARIES.at(op)) {
                if (u.first->equal(type))
                    return { true, u.second };
            }
            return { false, std::shared_ptr<TypeExpression>() };
        }

        inline std::pair<bool, std::shared_ptr<TypeExpression>> checkBinary(Token::Type op, const std::shared_ptr<TypeExpression> &lhs, const std::shared_ptr<TypeExpression> &rhs) {
            if (BINARIES.find(op) == BINARIES.end())
                throw std::runtime_error("not well-formed AST");
            for (const auto &u : BINARIES.at(op)) {
                if (u.first.first->equal(lhs) && u.first.second->equal(rhs))
                    return { true, u.second };
            }
            return { false, std::shared_ptr<TypeExpression>() };
        }

    private:

        class Context {
        private:

            std::vector<std::unordered_map<std::string, std::pair<bool, std::shared_ptr<TypeExpression>>>> data;
        
        public:

            inline void push() { data.push_back({}); }
            inline void pop() { data.pop_back(); }

            inline bool contains(const std::string &name) {
                for (const auto &u : data) {
                    if (u.find(name) != u.end())
                        return true;
                }
                return false;
            }

            inline bool containsAtCurrentLevel(const std::string &name) {
                return data.back().find(name) != data.back().end();
            }

            inline std::pair<bool, std::shared_ptr<TypeExpression>> getVariable(const std::string &name) {
                for (auto ri = data.rbegin(); ri != data.rend(); ++ri) {
                    if (ri->find(name) != ri->end())
                        return ri->at(name);
                }
                throw std::runtime_error("variable name not bound in context");
            }

            inline void declare(const std::string &name, bool mut, const std::shared_ptr<TypeExpression> &t) {
                data.back()[name] = { mut, t };
            }
        };

        Context context;

        std::shared_ptr<FunctionTypeExpression> current_function;
        std::string current_function_name;
        bool know_full_function_type;

        PrettyPrinter printer;

    public:

        void visitTypeExpression(const TypeExpression *expr) override {
            switch (expr->getType()) {
                case TypeExpression::Type::PRIMITIVE: visitPrimitiveTypeExpression(static_cast<const PrimitiveTypeExpression*>(expr)); break;
                case TypeExpression::Type::FUNCTION:  visitFunctionTypeExpression(static_cast<const FunctionTypeExpression*>(expr));   break;
                default:                              throw std::runtime_error("not well-formed AST");
            }
        }

        void visitPrimitiveTypeExpression(__attribute__((unused)) const PrimitiveTypeExpression *expr) override {}
        void visitFunctionTypeExpression(__attribute__((unused)) const FunctionTypeExpression *expr) override {}

        std::shared_ptr<TypeExpression> visitLHS(const LHS *lhs) override {
            switch (lhs->getType()) {
                case LHS::Type::VARIABLE: return visitVariableLHS(static_cast<const VariableLHS*>(lhs));
                default:                  throw std::runtime_error("not well-formed AST");
            }
        }

        std::shared_ptr<TypeExpression> visitVariableLHS(const VariableLHS *lhs) override {
            if (!context.contains(lhs->id.lexeme))
                throw BadlyFormedError(lhs->id, "cannot access an undeclared name");
            auto res = context.getVariable(lhs->id.lexeme);
            if (!res.first)
                throw BadlyFormedError(lhs->id, "constant cannot be a left-hand side expression");
            return res.second;
        }

        std::shared_ptr<TypeExpression> visitExpression(const Expression *expr) override {
            switch (expr->getType()) {
                case Expression::Type::LITERAL:  return visitLiteralExpression(static_cast<const LiteralExpression*>(expr));
                case Expression::Type::FUNCTION: return visitFunctionExpression(static_cast<const FunctionExpression*>(expr));
                case Expression::Type::UNARY:    return visitUnaryExpression(static_cast<const UnaryExpression*>(expr));
                case Expression::Type::BINARY:   return visitBinaryExpression(static_cast<const BinaryExpression*>(expr));
                case Expression::Type::COMPLIST: return visitComparisonList(static_cast<const ComparisonList*>(expr));
                default:                         throw std::runtime_error("not well-formed AST");
            }
        }

        std::shared_ptr<TypeExpression> visitLiteralExpression(const LiteralExpression *expr) override {
            switch (expr->lit.type) {
                case Token::Type::LITERAL_INT:  return PrimitiveTypeExpression::ofTypename(Token::Type::KEY_INT);
                case Token::Type::LITERAL_FLT:  return PrimitiveTypeExpression::ofTypename(Token::Type::KEY_FLT);
                case Token::Type::LITERAL_CHAR: return PrimitiveTypeExpression::ofTypename(Token::Type::KEY_CHAR);
                case Token::Type::LITERAL_BOOL: return PrimitiveTypeExpression::ofTypename(Token::Type::KEY_BOOL);
                case Token::Type::IDENTIFIER:
                    if (!context.contains(expr->lit.lexeme))
                        throw BadlyFormedError(expr->lit, "variable not declared");
                    return context.getVariable(expr->lit.lexeme).second;
                default:
                    throw std::runtime_error("not well-formed AST");
            }
        }

        std::shared_ptr<TypeExpression> visitFunctionExpression(const FunctionExpression *expr) override {
            std::shared_ptr<TypeExpression> f = visitExpression(expr->function.get());
            if (f->getType() != TypeExpression::Type::FUNCTION)
                throw BadlyFormedError(expr->lparen, "can only apply to a function type");
            std::shared_ptr<FunctionTypeExpression> ft = std::static_pointer_cast<FunctionTypeExpression>(f);
            if (ft->args.size() != expr->arguments.size())
                throw BadlyFormedError(expr->lparen, "function argument list must match definition list in length and types");
            for (std::size_t i = 0; i < ft->args.size(); i++) {
                if (!ft->args.at(i)->equal(visitExpression(expr->arguments.at(i).get())))
                    throw BadlyFormedError(expr->lparen, "function argument list must match definition list in length and types");
            }
            return ft->rettype;
        }

        std::shared_ptr<TypeExpression> visitUnaryExpression(const UnaryExpression *expr) override {
            std::shared_ptr<TypeExpression> etype = visitExpression(expr->expr.get());
            auto check = checkUnary(expr->op.type, etype);
            if (!check.first)
                throw BadlyFormedError(expr->op, "unary operator doesn't accept this input type");
            else
                return check.second;
        }

        std::shared_ptr<TypeExpression> visitBinaryExpression(const BinaryExpression *expr) override {
            std::shared_ptr<TypeExpression> ltype = visitExpression(expr->lexp.get()), rtype = visitExpression(expr->rexp.get());
            auto check = checkBinary(expr->op.type, ltype, rtype);
            if (!check.first)
                throw BadlyFormedError(expr->op, "binary operator doesn't accept these input types");
            else
                return check.second;
        }

        std::shared_ptr<TypeExpression> visitComparisonList(const ComparisonList *expr) override {
            if (expr->comparisons.empty())
                throw std::runtime_error("not well-formed AST");
            std::shared_ptr<TypeExpression> first_type = visitExpression(expr->first.get());
            auto comp_type = std::find_if(COMPARATORS.begin(), COMPARATORS.end(), [&first_type]
                (const std::pair<std::pair<std::shared_ptr<TypeExpression>, std::shared_ptr<TypeExpression>>, std::shared_ptr<TypeExpression>> &p) -> bool {
                    return p.first.first->equal(first_type);
                });
            if (comp_type == COMPARATORS.end())
                throw BadlyFormedError(expr->comparisons.front().first, "types in comparison do not match with comparison operator");
            for (const auto &u : expr->comparisons) {
                if (!visitExpression(u.second.get())->equal(comp_type->first.second))
                    throw BadlyFormedError(u.first, "types in comparison do not match with comparison operator");
            }
            return comp_type->second;
        }

        void visitStatement(const Statement *stmt) override {
            switch (stmt->getType()) {
                case Statement::Type::VARDECL:     visitVarDecl(static_cast<const VarDeclStatement*>(stmt));          break;
                case Statement::Type::TVARDECL:    visitTypeVarDecl(static_cast<const TypeVarDeclStatement*>(stmt));  break;
                case Statement::Type::ASSIGN:      visitAssignStatement(static_cast<const AssignStatement*>(stmt));   break;
                case Statement::Type::IFSTMT:      visitIfStatement(static_cast<const IfStatement*>(stmt));           break;
                case Statement::Type::WHILESTMT:   visitWhileStatement(static_cast<const WhileStatement*>(stmt));     break;
                case Statement::Type::DOWHILESTMT: visitDoWhileStatement(static_cast<const DoWhileStatement*>(stmt)); break;
                case Statement::Type::RETURN:      visitReturnStatement(static_cast<const ReturnStatement*>(stmt));   break;
                default:                           throw std::runtime_error("not well-formed AST");
            }
        }

        void visitVarDecl(const VarDeclStatement *stmt) override {
            if (context.containsAtCurrentLevel(stmt->id.lexeme))
                throw BadlyFormedError(stmt->id, "cannot overwrite a declaration in the same block");
            context.declare(stmt->id.lexeme, stmt->mut, visitExpression(stmt->expr.get()));
        }

        void visitTypeVarDecl(const TypeVarDeclStatement *stmt) override {
            if (context.containsAtCurrentLevel(stmt->id.lexeme))
                throw BadlyFormedError(stmt->id, "cannot overwrite a declaration in the same block");
            std::shared_ptr<TypeExpression> type = visitExpression(stmt->expr.get());
            if (!stmt->type->equal(type)) {
                throw BadlyFormedError(stmt->id, printer.visitTypeExpression(stmt->type.get()) + " vs " +
                                                 printer.visitTypeExpression(type.get()) +
                                                 ": declared type and assigned type do not match");
            }
            context.declare(stmt->id.lexeme, stmt->mut, stmt->type);
        }

        void visitAssignStatement(const AssignStatement *stmt) override {
            std::shared_ptr<TypeExpression> type = visitLHS(stmt->lhs.get());
            std::shared_ptr<TypeExpression> expr = visitExpression(stmt->expr.get());
            if (!type->equal(expr))
                throw BadlyFormedError(stmt->op, "assignee's and assigner's types do not match");
        }

        void visitIfStatement(const IfStatement *stmt) override {
            if (!visitExpression(stmt->cond.get())->equal(PrimitiveTypeExpression::MAKE_BOOL()))
                throw BadlyFormedError(stmt->key, "if/elif condition must be of type 'bool'");
            visitBlock(stmt->taken);
            visitBlock(stmt->not_taken);
        }

        void visitWhileStatement(const WhileStatement *stmt) override {
            if (!visitExpression(stmt->cond.get())->equal(PrimitiveTypeExpression::MAKE_BOOL()))
                throw BadlyFormedError(stmt->key, "while condition must be of type 'bool'");
            visitBlock(stmt->body);
        }

        void visitDoWhileStatement(const DoWhileStatement *stmt) override {
            if (!visitExpression(stmt->cond.get())->equal(PrimitiveTypeExpression::MAKE_BOOL()))
                throw BadlyFormedError(stmt->whilekey, "do-while condition must be of type 'bool'");
            visitBlock(stmt->body);
        }

        void visitReturnStatement(const ReturnStatement *stmt) override {
            if (know_full_function_type) {
                if (stmt->withVal) {
                    if (!visitExpression(stmt->expr.get())->equal(current_function->rettype))
                        throw BadlyFormedError(stmt->key, "return expression doesn't match function return type");
                } else {
                    if (!current_function->rettype->equal(PrimitiveTypeExpression::MAKE_VOID()))
                        throw BadlyFormedError(stmt->key, "have to return an expression in non-void function");
                }
            } else {
                know_full_function_type = true;
                if (stmt->withVal) {
                    current_function->rettype = visitExpression(stmt->expr.get());
                } else {
                    current_function->rettype = PrimitiveTypeExpression::MAKE_VOID();
                }
            }
        }

        void visitBlock(const Block &block) override {
            context.push();
            for (const auto &instr : block) {
                visitStatement(instr.get());
            }
            context.pop();
        }

        void visitGlobalStatement(const GlobalStatement *stmt) override {
            switch (stmt->getType()) {
                case GlobalStatement::Type::GVARDECL:     visitGVarDecl(static_cast<const GVarDeclStatement*>(stmt));         break;
                case GlobalStatement::Type::GTYPEVARDECL: visitGTypeVarDecl(static_cast<const GTypeVarDeclStatement*>(stmt)); break;
                case GlobalStatement::Type::GFNDECL:      visitGFDecl(static_cast<const GFDeclStatement*>(stmt));             break;
                default:                                  throw std::runtime_error("not well-formed AST");
            }
        }

        void visitGVarDecl(const GVarDeclStatement *stmt) override {
            // have to make sure it's a constant expression
            if (context.containsAtCurrentLevel(stmt->id.lexeme))
                throw BadlyFormedError(stmt->id, "cannot overwrite a declaration in the same block");
            context.declare(stmt->id.lexeme, stmt->mut, visitExpression(stmt->expr.get()));
        }

        void visitGTypeVarDecl(const GTypeVarDeclStatement *stmt) override {
            if (!stmt->type->equal(visitExpression(stmt->expr.get())))
                throw BadlyFormedError(stmt->id, "declared type and assigned type do not match");
            context.declare(stmt->id.lexeme, stmt->mut, stmt->type);
        }

        void visitGFDecl(const GFDeclStatement *stmt) override {
            current_function_name = stmt->id.lexeme;
            know_full_function_type = true;

            current_function = std::make_shared<FunctionTypeExpression>(std::vector<std::shared_ptr<TypeExpression>>(), stmt->rettype);

            for (const auto &u : stmt->args)
                current_function->args.push_back(u.second);
            context.declare(current_function_name, false, std::static_pointer_cast<TypeExpression>(current_function));

            context.push();
            for (const auto &u : stmt->args)
                context.declare(u.first.lexeme, true, u.second);
            visitBlock(stmt->body);
            context.pop();
        }

        void visitProgram(const Program &prog) override {
            context.push();
            for (const auto &u : prog) {
                visitGlobalStatement(u.get());
            }
        }

    };

} // namespace drm
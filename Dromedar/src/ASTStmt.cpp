#include "AST.hpp"

#define as std::static_pointer_cast<Stmt>
#define make std::make_shared

using namespace drm;
using namespace drm::stmt;

Stmt::Stmt(std::size_t start, std::size_t length) {
    this->start = start;
    this->length = length;
}

VDecl::VDecl(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp) : Stmt(start, length) {
    this->id = id;
    this->exp = exp;
}

StmtPtr VDecl::of(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp) {
    return as(make<VDecl>(start, length, id, exp));
}

Assn::Assn(std::size_t start, std::size_t length, const ExprPtr &lhs, const ExprPtr &exp) : Stmt(start, length) {
    this->lhs = lhs;
    this->exp = exp;
}

StmtPtr Assn::of(std::size_t start, std::size_t length, const ExprPtr &lhs, const ExprPtr &exp) {
    return as(make<Assn>(start, length, lhs, exp));
}

stmt::Expr::Expr(std::size_t start, std::size_t length, const ExprPtr &exp) : Stmt(start, length) {
    this->exp = exp;
}

StmtPtr stmt::Expr::of(std::size_t start, std::size_t length, const ExprPtr &exp) {
    return as(make<Expr>(start, length, exp));
}

If::If(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &taken, const Block &not_taken) : Stmt(start, length) {
    this->cond = cond;
    this->taken = taken;
    this->not_taken = not_taken;
}

StmtPtr If::of(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &taken, const Block &not_taken) {
    return as(make<If>(start, length, cond, taken, not_taken));
}

While::While(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body) : Stmt(start, length) {
    this->cond = cond;
    this->body = body;
}

StmtPtr While::of(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body) {
    return as(make<While>(start, length, cond, body));
}

DoWhile::DoWhile(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body) : Stmt(start, length) {
    this->cond = cond;
    this->body = body;
}

StmtPtr DoWhile::of(std::size_t start, std::size_t length, const ExprPtr &cond, const Block &body) {
    return as(make<While>(start, length, cond, body));
}

Return::Return(std::size_t start, std::size_t length) : Stmt(start, length) {}

Return::Return(std::size_t start, std::size_t length, const ExprPtr &exp) : Stmt(start, length) {
    this->exp = exp;
}

StmtPtr Return::of(std::size_t start, std::size_t length) {
    return as(make<Return>(start, length));
}

StmtPtr Return::of(std::size_t start, std::size_t length, const ExprPtr &exp) {
    return as(make<Return>(start, length, exp));
}
#include "AST.hpp"

#define as std::static_pointer_cast<Expr>
#define make std::make_shared

using namespace drm;
using namespace drm::expr;

Expr::Expr(std::size_t start, std::size_t length) {
    this->start = start;
    this->length = length;
}

Id::Id(std::size_t start, std::size_t length, std::string id) : Expr(start, length) {
    this->id = id;
}

ExprPtr Id::of(std::size_t start, std::size_t length, std::string id) {
    return as(make<Id>(start, length, id));
}

Lit::Lit(std::size_t start, std::size_t length, const Token::LiteralType &lt) : Expr(start, length) {
    this->lt = lt;
}

ExprPtr Lit::of(std::size_t start, std::size_t length, const Token::LiteralType &lt) {
    return as(make<Lit>(start, length, lt));
}

Uop::Uop(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &exp) : Expr(start, length) {
    this->op = op;
    this->exp = exp;
}

ExprPtr Uop::of(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &exp) {
    return as(make<Uop>(start, length, op, exp));
}

Bop::Bop(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &lexp, const ExprPtr &rexp) : Expr(start, length) {
    this->op = op;
    this->lexp = lexp;
    this->rexp = rexp;
}

ExprPtr Bop::of(std::size_t start, std::size_t length, Token::Type op, const ExprPtr &lexp, const ExprPtr &rexp) {
    return as(make<Bop>(start, length, op, lexp, rexp));
}

Func::Func(std::size_t start, std::size_t length, const ExprPtr &lhs, const std::vector<ExprPtr> &args) : Expr(start, length) {
    this->lhs = lhs;
    this->args = args;
}

ExprPtr Func::of(std::size_t start, std::size_t length, const ExprPtr &lhs, const std::vector<ExprPtr> &args) {
    return as(make<Func>(start, length, lhs, args));
}

CmpList::CmpList(std::size_t start, std::size_t length, const ExprPtr &first, const decltype(cmps) &cmps) : Expr(start, length) {
    this->first = first;
    this->cmps = cmps;
}

ExprPtr CmpList::of(std::size_t start, std::size_t length, const ExprPtr &first, const decltype(cmps) &cmps) {
    return as(make<CmpList>(start, length, first, cmps));
}
#include "AST.hpp"

#define as std::static_pointer_cast<GStmt>
#define make std::make_shared

using namespace drm;
using namespace drm::gstmt;

GStmt::GStmt(std::size_t start, std::size_t length) {
    this->start = start;
    this->length = length;
}

FDecl::FDecl(std::size_t start, std::size_t length, const std::string &id, const TypePtr &rty, const decltype(args) &args, const Block &body) : GStmt(start, length) {
    this->id = id;
    this->rty = rty;
    this->args = args;
    this->body = body;
}

GStmtPtr FDecl::of(std::size_t start, std::size_t length, const std::string &id, const TypePtr &rty, const decltype(args) &args, const Block &body) {
    return as(make<FDecl>(start, length, id, rty, args, body));
}

VDecl::VDecl(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp) : GStmt(start, length) {
    this->id = id;
    this->exp = exp;
}

GStmtPtr VDecl::of(std::size_t start, std::size_t length, const std::string &id, const ExprPtr &exp) {
    return as(make<VDecl>(start, length, id, exp));
}
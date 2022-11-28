#include "AST.hpp"

#define as std::static_pointer_cast<Type>
#define make std::make_shared

using namespace drm;
using namespace drm::type;

Type::Type(std::size_t start, std::size_t length) {
    this->start = start;
    this->length = length;
}

Prim::Prim(std::size_t start, std::size_t length, Token::Type t) : Type(start, length) {
    this->t = t;
}

TypePtr Prim::of(std::size_t start, std::size_t length, Token::Type t) {
    return as(make<Prim>(start, length, t));
}

FType::FType(std::size_t start, std::size_t length, const TypePtr &rty, const std::vector<TypePtr> &argts) : Type(start, length) {
    this->rty = rty;
    this->argts = argts;
}

TypePtr FType::of(std::size_t start, std::size_t length, const TypePtr &rty, const std::vector<TypePtr> &argts) {
    return as(make<FType>(start, length, rty, argts));
}
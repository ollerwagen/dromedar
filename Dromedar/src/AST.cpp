#include <stdexcept>

// debugging purposes
#include <iostream>

#include "AST.hpp"

using namespace drm;
using namespace drm::type;

const TypePtr Prim::INT  = Prim::of(0UL, 0UL, Token::KEY_INT);
const TypePtr Prim::FLT  = Prim::of(0UL, 0UL, Token::KEY_FLT);
const TypePtr Prim::BOOL = Prim::of(0UL, 0UL, Token::KEY_BOOL);
const TypePtr Prim::CHAR = Prim::of(0UL, 0UL, Token::KEY_CHAR);
const TypePtr Prim::VOID = Prim::of(0UL, 0UL, Token::KEY_VOID);

bool Type::equiv(const TypePtr &a, const TypePtr &b) {
    if (a->type() != b->type())
        return false;
    switch (a->type()) {
        case Type::T::PRIM:  return Prim::eq((Prim*)a.get(), (Prim*)b.get());
        case Type::T::FTYPE: return FType::eq((FType*)a.get(), (FType*)b.get());
        default:             throw std::runtime_error("badly formed AST");
    }
}

bool Prim::eq(Prim *a, Prim *b) {
    return a->t == b->t;
}

bool FType::eq(FType *a, FType *b) {
    if (a->argts.size() != b->argts.size())
        return false;
    for (std::size_t i = 0; i < a->argts.size(); i++)
        if (!Type::equiv(a->argts.at(i), b->argts.at(i)))
            return false;
    return Type::equiv(a->rty, b->rty);
}
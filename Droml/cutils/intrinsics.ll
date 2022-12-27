; integer exponentiation, used in builtin '**' operator
declare i64 @_pow_ii(i64,i64)

; floating point exponentiation, used in builtin '**' operator
declare double @_pow_ff(double,double)


declare void @_memcpy(i8*,i8*,i64)

declare {i64,[0 x i8]*}* @_strconcat({i64,[0 x i8]*}*, {i64,[0 x i8]*}*)
declare {i64,[0 x i8]*}* @_strmul_1({i64,[0 x i8]*}*, i64)
declare {i64,[0 x i8]*}* @_strmul_2(i64, {i64,[0 x i8]*}*)

declare i64 @_strcmp({i64,[0 x i8]*}*, {i64,[0 x i8]*}*)

declare {i64,[0 x i8]*}* @_arrconcat({i64,[0 x i8]*}*, {i64,[0 x i8]*}*, i64, i1)


; sprintf helper functions
declare {i64,[0 x i8]*}* @_sprintf_int(i64)
declare {i64,[0 x i8]*}* @_sprintf_flt(double)
declare {i64,[0 x i8]*}* @_sprintf_char(i8)
declare {i64,[0 x i8]*}* @_sprintf_bool(i1)
declare {i64,[0 x i8]*}* @_sprintf_cat(i64, ...)

;
; Garbage Collection Functions
;

; returns an address to a memory block of given size
declare i8* @_allocate(i64)

; removes a reference to an object in memory
declare void @_removeref(i8*)

; adds a program reference to an object in memory
declare void @_addref(i8*)

; adds a child to an address
declare void @_addchild(i8*, i8*)
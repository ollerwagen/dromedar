declare void @_abort(i64)

; create argument vector for main function
declare {i64,[0 x {i64,[0 x i8]*}*]*}* @_makestrvec(i64,i8**)

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

; range list
declare {i64,[0 x i64]*}* @_makerangelist(i64,i64,i1,i1)


; sprintf helper functions
declare {i64,[0 x i8]*}* @_sprintf_int(i64)
declare {i64,[0 x i8]*}* @_sprintf_flt(i64)
declare {i64,[0 x i8]*}* @_sprintf_char(i64)
declare {i64,[0 x i8]*}* @_sprintf_bool(i64)
declare {i64,[0 x i8]*}* @_sprintf_str(i64)
declare {i64,[0 x i8]*}* @_sprintf_array(i64,i64,i64,{i64,[0 x i8]*}*(i64)*)
declare {i64,[0 x i8]*}* @_sprintf_cat(i64, ...)

; list comprehension helper functions
declare i8* @_make_vector()
declare void @_addelem(i8*, i64)
declare {i64,[0 x i8]*}* @_genlist(i8*,i64,i1)

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

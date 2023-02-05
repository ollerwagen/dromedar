declare void @_abort(i64)
declare void @_checknull(i8*)

; create argument vector for main function
declare {i64,i64,[0 x {i64,i64,[0 x i8]*}*]*}* @_makestrvec(i64,i8**)

; integer exponentiation, used in builtin '**' operator
declare i64 @_pow_ii(i64,i64)

; floating point exponentiation, used in builtin '**' operator
declare double @_pow_ff(double,double)


declare void @_memcpy(i8*,i8*,i64)

declare {i64,i64,[0 x i8]*}* @_strconcat({i64,i64,[0 x i8]*}*, {i64,i64,[0 x i8]*}*)
declare {i64,i64,[0 x i8]*}* @_strmul_1({i64,i64,[0 x i8]*}*, i64)
declare {i64,i64,[0 x i8]*}* @_strmul_2(i64, {i64,i64,[0 x i8]*}*)

declare i64 @_strcmp({i64,i64,[0 x i8]*}*, {i64,i64,[0 x i8]*}*)

declare {i64,i64,[0 x i8]*}* @_arrconcat({i64,i64,[0 x i8]*}*, {i64,i64,[0 x i8]*}*, i64, i1)
declare {i64,i64,[0 x i8]*}* @_arrmul({i64,i64,[0 x i8]*}*, i64, i64, i1)

; range list
declare {i64,i64,[0 x i64]*}* @_makerangeintlist(i64,i64,i1,i1)
declare {i64,i64,[0 x i8]*}*  @_makerangecharlist(i8,i8,i1,i1)


; sprintf helper functions
declare {i64,i64,[0 x i8]*}* @_sprintf_int(i64)
declare {i64,i64,[0 x i8]*}* @_sprintf_flt(i64)
declare {i64,i64,[0 x i8]*}* @_sprintf_char(i64)
declare {i64,i64,[0 x i8]*}* @_sprintf_bool(i64)
declare {i64,i64,[0 x i8]*}* @_sprintf_str(i64)
declare {i64,i64,[0 x i8]*}* @_sprintf_array(i64,i64,i64,{i64,i64,[0 x i8]*}*(i64)*)
declare {i64,i64,[0 x i8]*}* @_sprintf_cat(i64, ...)

; list comprehension helper functions
declare i8* @_make_vector()
declare void @_addelem(i8*, i64)
declare {i64,i64,[0 x i8]*}* @_genlist(i8*,i64,i1)

;
; Garbage Collection Functions
;

; returns an address to a memory block of given size
declare i8* @_allocate(i64)

; removes a reference to an object in memory
declare void @_removeref(i8*)

; adds a program reference to an object in memory
declare void @_addref(i8*)

; swaps two children of a parent
declare void @_swapchild(i8*,i8*,i8*)

; adds a child to an address
declare void @_addchild(i8*, i8*)

; helper function for printf
declare void @_print_string({i64,i64,[0 x i8]*}*)

; creates arrays
declare {i64,i64,[0 x i8]*}*  @_allocate_string(i64)
declare {i64,i64,[0 x i8]*}*  @_allocate_string_of(i8*)
declare {i64,i64,[0 x i64]*}* @_allocate_intarr(i64)
declare {i64,i64,[0 x i8]*}* @_allocate_blindarr(i64,i64)

; generalized array member functions
declare {i64,i64,[0 x i8]*}* @_array_push       (i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)
declare {i64,i64,[0 x i8]*}* @_array_pop        (i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*)
declare {i64,i64,[0 x i8]*}* @_array_insert     (i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)
declare {i64,i64,[0 x i8]*}* @_array_insert_all (i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,{i64,i64,[0 x i8]*}*)
declare {i64,i64,[0 x i8]*}* @_array_erase      (i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)
declare {i64,i64,[0 x i8]*}* @_array_sub        (i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)

@_Array$push$c       = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)*} {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)* @_array_push}
@_Array$push         = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)*}* @_Array$push$c

@_Array$pop$c        = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*)*} {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*)* @_array_pop}
@_Array$pop          = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*)*}* @_Array$pop$c

@_Array$insert$c     = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)*} {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)* @_array_insert}
@_Array$insert       = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)*}* @_Array$insert$c

@_Array$insert_all$c = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,{i64,i64,[0 x i8]*}*)*} {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,{i64,i64,[0 x i8]*}*)* @_array_insert_all}
@_Array$insert_all   = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,{i64,i64,[0 x i8]*}*)*}* @_Array$insert_all$c

@_Array$erase$c      = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)*} {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)* @_array_erase}
@_Array$erase        = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64)*}* @_Array$erase$c

@_Array$sub$c        = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)*} {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)* @_array_sub}
@_Array$sub          = global {{i64,i64,[0 x i8]*}*(i64*,i64,i64,i1,{i64,i64,[0 x i8]*}*,i64,i64)*}* @_Array$sub$c

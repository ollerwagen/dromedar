; integer exponentiation, used in builtin '**' operator
declare i64 @_pow_ii(i64,i64)

; floating point exponentiation, used in builtin '**' operator
declare double @_pow_ff(double,double)


declare void @_memcpy(i8*,i8*,i64)

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
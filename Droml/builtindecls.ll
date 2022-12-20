
; integer exponentiation, used in builtin '**' operator
declare i64 @pow_ii(i64,i64)

; floating point exponentiation, used in builtin '**' operator
declare double @pow_ff(double,double)

; print integers
declare void @print_int(i64)

; print floating point numbers
declare void @print_flt(double)
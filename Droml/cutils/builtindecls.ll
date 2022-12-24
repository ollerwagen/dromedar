
; print integers
declare void @print_int(i64)

; print floating point numbers
declare void @print_flt(double)

; print strings
declare void @print_str({i64, [0 x i8]*}*)
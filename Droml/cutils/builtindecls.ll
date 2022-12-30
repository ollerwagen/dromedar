
; print integers
declare void @print_int(i64)

; print floating point numbers
declare void @print_flt(double)

; print strings
declare void @print_str({i64, [0 x i8]*}*)

; print characters
declare void @print_char(i8)

; print newline character
declare void @println()

declare {i64, [0 x i8]*}* @int_to_str(i64)
declare {i64, [0 x i8]*}* @flt_to_str(double)
declare {i64, [0 x i8]*}* @char_to_str(i8)
declare {i64, [0 x i8]*}* @bool_to_str(i1)

declare { i64 , [0 x i8]* }* @_Str$of_int(i64)
declare { i64 , [0 x i8]* }* @_Str$of_flt(double)

declare i64 @_IO$print_int1(i64*, i64)
@_IO$print_int2 = global { i64(i64*, i64)* } { i64(i64*, i64)* @_IO$print_int1 }
@_IO$print_int = global { i64(i64*, i64)* }* @_IO$print_int2

declare void @_IO$print_flt(double)
declare void @_IO$print_str({ i64 , [0 x i8]* }*)
declare void @_IO$print_char(i8)
declare void @_IO$print_bool(i1)

declare { i64, [0 x { i64 , [0 x i8]* }*]* }* @_File$readall({ i64 , [0 x i8]* }*)

declare double @_Math$sin(double)
declare double @_Math$cos(double)
declare double @_Math$tan(double)

@_Math$e = external global double
@_Math$pi = external global double

declare i8* @_Regex$compile({ i64 , [0 x i8]* }*)
declare i1  @_Regex$matches(i8*, { i64 , [0 x i8]* }*)
declare i8* @_Regex$compile_number()
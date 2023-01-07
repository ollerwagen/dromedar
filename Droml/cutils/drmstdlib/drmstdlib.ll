declare {i64, [0 x i8]*}* @__Str$of_int(i64*, i64)
@__Str$of_int$ = global { {i64, [0 x i8]*}*(i64*, i64)* } { {i64, [0 x i8]*}*(i64*, i64)* @__Str$of_int }
@_Str$of_int = global { {i64, [0 x i8]*}*(i64*, i64)* }* @__Str$of_int$

declare {i64, [0 x i8]*}* @__Str$of_flt(i64*, double)
@__Str$of_flt$ = global { {i64, [0 x i8]*}*(i64*, double)* } { {i64, [0 x i8]*}*(i64*, double)* @__Str$of_flt }
@_Str$of_flt = global { {i64, [0 x i8]*}*(i64*, double)* }* @__Str$of_flt$

declare void @__IO$print_int(i64*, i64)
@__IO$print_int$ = global { void(i64*, i64)* } { void(i64*, i64)* @__IO$print_int }
@_IO$print_int = global { void(i64*, i64)* }* @__IO$print_int$

declare void @__IO$print_flt(i64*, double)
@__IO$print_flt$ = global { void(i64*, double)* } { void(i64*, double)* @__IO$print_flt }
@_IO$print_flt = global { void(i64*, double)* }* @__IO$print_flt$

declare void @__IO$print_str(i64*, {i64, [0 x i8]*}*)
@__IO$print_str$ = global { void(i64*, {i64, [0 x i8]*}*)* } { void(i64*, {i64, [0 x i8]*}*)* @__IO$print_str }
@_IO$print_str = global { void(i64*, {i64, [0 x i8]*}*)* }* @__IO$print_str$

declare void @__IO$print_char(i64*, i8)
@__IO$print_char$ = global { void(i64*, i8)* } { void(i64*, i8)* @__IO$print_char }
@_IO$print_char = global { void(i64*, i8)* }* @__IO$print_char$

declare void @__IO$print_bool(i64*, i1)
@__IO$print_bool$ = global { void(i64*, i1)* } { void(i64*, i1)* @__IO$print_bool }
@_IO$print_bool = global { void(i64*, i1)* }* @__IO$print_bool$

declare {i64, [0 x {i64, [0 x i8]*}*]*}* @__File$readall(i64*, {i64, [0 x i8]*}*)
@__File$readall$ = global { {i64, [0 x {i64, [0 x i8]*}*]*}*(i64*, {i64, [0 x i8]*}*)* } { {i64, [0 x {i64, [0 x i8]*}*]*}*(i64*, {i64, [0 x i8]*}*)* @__File$readall }
@_File$readall = global { {i64, [0 x {i64, [0 x i8]*}*]*}*(i64*, {i64, [0 x i8]*}*)* }* @__File$readall$

declare double @__Math$sin(i64*, double)
@__Math$sin$ = global { double(i64*, double)* } { double(i64*, double)* @__Math$sin }
@_Math$sin = global { double(i64*, double)* }* @__Math$sin$

declare double @__Math$cos(i64*, double)
@__Math$cos$ = global { double(i64*, double)* } { double(i64*, double)* @__Math$cos }
@_Math$cos = global { double(i64*, double)* }* @__Math$cos$

declare double @__Math$tan(i64*, double)
@__Math$tan$ = global { double(i64*, double)* } { double(i64*, double)* @__Math$tan }
@_Math$tan = global { double(i64*, double)* }* @__Math$tan$

@_Math$e = external global double

@_Math$pi = external global double

declare i8* @__Regex$compile(i64*, {i64, [0 x i8]*}*)
@__Regex$compile$ = global { i8*(i64*, {i64, [0 x i8]*}*)* } { i8*(i64*, {i64, [0 x i8]*}*)* @__Regex$compile }
@_Regex$compile = global { i8*(i64*, {i64, [0 x i8]*}*)* }* @__Regex$compile$

declare i1 @__Regex$matches(i64*, i8*, {i64, [0 x i8]*}*)
@__Regex$matches$ = global { i1(i64*, i8*, {i64, [0 x i8]*}*)* } { i1(i64*, i8*, {i64, [0 x i8]*}*)* @__Regex$matches }
@_Regex$matches = global { i1(i64*, i8*, {i64, [0 x i8]*}*)* }* @__Regex$matches$


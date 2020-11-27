%%
true "TRUE"
false "FALSE"
while "WHILE"
if "IF"
else "ELSE"
return "RETURN"

int "INT"
string "STR"
boolean "BOOL"
void "VOID"

; ";"
, ","

\+\+ "++"
-- "--"

<= "<="
>= ">="
< "<"
> ">"
== "=="
!= "!="
! "!"

= "="
\+ "+"
\* "*"
- "-"
/ "/"
% "%"

\{ "{"
\} "}"
\( "("
\) ")"

[a-zA-z][a-zA-Z0-9]* "IDENT"
0|([1-9][0-9]*) "INTEGER"
"[^"]*" "STRING"

[\s]+ ;
#.*?\n ;
//.*?\n ;
/[*].*?[*]/ ;

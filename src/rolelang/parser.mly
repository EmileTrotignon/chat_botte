%{

open Parser_aux

%}

%token RPar
%token LPar
%token <Ast.id> TId
%token TOr
%token TAnd
%token TNot
%token EOM

%left TOr TAnd TNot

%start <Ast.t> ast
%%

ast: e=expr EOM { e }

expr :
| role = TId { Ast.Id role }
| LPar e=expr RPar { e }
| TNot e=expr { Ast.Not e }
| e1=expr TOr e2=expr { Ast.Or (e1, e2) }
| e1=expr TAnd e2=expr { Ast.And (e1, e2) }
| error { raise (SyntaxError ($startpos, $endpos)) }

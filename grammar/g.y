%token ID
%token IF
%token WHILE
%token ELIF
%token ELSE
%token FLOAT
%token INTEGER
%token VOID
%token U8
%token I8
%token I16
%token U16
%token I32
%token U32
%token I64
%token U64
%token F32
%token F64

%start prog

%%

prog                  : /* empty */
                      | prog struct
                      | prog function
;

//---------------------------------------
// STRUCT
//---------------------------------------

struct                : struct_opt '}'
;

struct_opt            : ID '{' var
                      | struct_opt var
;

//---------------------------------------
// FUNCTION
//---------------------------------------

function              : ID function_type statement_lst

/* -- FUNCTION_TYPE -- */

function_type         : function_type_param ':' type
                      | function_type_param
;

function_type_param   : function_type_opt ')'
                      | '(' ')'
;

function_type_opt     : '(' var
                      | function_type_opt ',' var
;

//---------------------------------------
// VARIABLE
//---------------------------------------

var                   : ID ':' type
;

//---------------------------------------
// TYPE
//---------------------------------------

type                  : '*' type
                      | U8
                      | I8
                      | U16
                      | I16
                      | U32
                      | I32
                      | U64
                      | I64
                      | F32
                      | F64
                      | VOID
;

//---------------------------------------
// STATEMENT
//---------------------------------------

statement_lst         : statement_lst_opt '}'
;

statement_lst_opt     : '{'
                      | statement_lst_opt statement
;

statement             : var_stm
                      | conditional_stm
                      | loop_stm
                      | exp
;

var_stm               : var '=' exp
                      | var
;

conditional_stm       : conditional_stm_opt ELSE exp statement_lst
                      | conditional_stm_opt

conditional_stm_opt   : IF exp statement_lst 
                      | conditional_stm_opt ELIF exp statement_lst
;

loop_stm              : WHILE exp statement_lst
;

//---------------------------------------
// EXPRESSION
//---------------------------------------

exp                   : INTEGER
                      | FLOAT
                      | ID '.' exp
                      | ID
                      | '&' exp
                      | '*' exp
;

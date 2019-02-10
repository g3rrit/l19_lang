%token ID
%token IF
%token BRK
%token CON
%token RET
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
                      | ID
;

//---------------------------------------
// STATEMENT
//---------------------------------------

statement_lst         : statement_lst_opt '}'
;

statement_lst_opt     : '{'
                      | statement_lst_opt statement
;

statement             : ret_stm
                      | brk_stm
                      | con_stm
                      | var_stm
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

ret_stm               : RET '(' exp ')'
                      | RET
;

brk_stm               : BRK
;

con_stm               : CON
;

//---------------------------------------
// EXPRESSION
//---------------------------------------

exp                   : INTEGER
                      | FLOAT
                      | access_exp
                      | call_exp
                      | ID
                      | '&' exp
                      | '*' exp
                      | cast_exp
;

access_exp            : ID '.' ID
                      | ID '-' '>' ID
                      | access_exp '.' ID
                      | access_exp '-' '>' ID
;

cast_exp              : '<' type '>' exp
;

call_exp              : call_exp_opt ')'
;

call_exp_opt          : ID '('
                      | call_exp_opt exp 
;

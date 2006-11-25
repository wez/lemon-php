/* This is a lemon grammar for the C language,
 * with some Objective C extensions */
%name ObjC
%token_prefix TK_

/* this defines a symbol for the lexer */
%nonassoc PRAGMA.

/* the precedence of IF/ELSE solves the dangling else conflict */
%nonassoc IF.
%nonassoc ELSE.

start ::= translation_unit.

primary_expression ::= IDENTIFIER.
primary_expression ::= CONSTANT.
primary_expression ::= STRING_LITERAL.
primary_expression ::= LPAREN expression RPAREN.
primary_expression ::= LPAREN error RPAREN.

postfix_expression ::= primary_expression.
postfix_expression ::= postfix_expression LSQUARE expression RSQUARE.
postfix_expression ::= postfix_expression LPAREN RPAREN.
postfix_expression ::= postfix_expression LPAREN argument_expression_list RPAREN.
postfix_expression ::= postfix_expression PERIOD IDENTIFIER.
postfix_expression ::= postfix_expression PTR_OP IDENTIFIER.
postfix_expression ::= postfix_expression INC_OP.
postfix_expression ::= postfix_expression DEC_OP.

argument_expression_list ::= assignment_expression.
argument_expression_list ::= argument_expression_list COMMA assignment_expression.

unary_expression ::= postfix_expression.
unary_expression ::= INC_OP unary_expression.
unary_expression ::= DEC_OP unary_expression.
unary_expression ::= unary_operator cast_expression.
unary_expression ::= SIZEOF unary_expression.
unary_expression ::= SIZEOF LPAREN type_name RPAREN.

unary_operator ::= AMP.
unary_operator ::= STAR.
unary_operator ::= PLUS.
unary_operator ::= MINUS.
unary_operator ::= TILDE.
unary_operator ::= EXCLAM.

cast_expression ::= unary_expression.
cast_expression ::= LPAREN type_name RPAREN cast_expression.

multiplicative_expression ::= cast_expression.
multiplicative_expression ::= multiplicative_expression STAR cast_expression.
multiplicative_expression ::= multiplicative_expression SLASH cast_expression.
multiplicative_expression ::= multiplicative_expression PERCENT cast_expression.

additive_expression ::= multiplicative_expression.
additive_expression ::= additive_expression PLUS multiplicative_expression.
additive_expression ::= additive_expression MINUS multiplicative_expression.

shift_expression ::= additive_expression.
shift_expression ::= shift_expression LEFT_OP additive_expression.
shift_expression ::= shift_expression RIGHT_OP additive_expression.

relational_expression ::= shift_expression.
relational_expression ::= relational_expression LANGLE shift_expression.
relational_expression ::= relational_expression RANGLE shift_expression.
relational_expression ::= relational_expression LE_OP shift_expression.
relational_expression ::= relational_expression GE_OP shift_expression.

equality_expression ::= relational_expression.
equality_expression ::= equality_expression EQ_OP relational_expression.
equality_expression ::= equality_expression NE_OP relational_expression.

and_expression ::= equality_expression.
and_expression ::= and_expression AMP equality_expression.

exclusive_or_expression ::= and_expression.
exclusive_or_expression ::= exclusive_or_expression CARET and_expression.

inclusive_or_expression ::= exclusive_or_expression.
inclusive_or_expression ::= inclusive_or_expression PIPE exclusive_or_expression.

logical_and_expression ::= inclusive_or_expression.
logical_and_expression ::= logical_and_expression AND_OP inclusive_or_expression.

logical_or_expression ::= logical_and_expression.
logical_or_expression ::= logical_or_expression OR_OP logical_and_expression.

conditional_expression ::= logical_or_expression.
conditional_expression ::= logical_or_expression QUESTION expression COLON conditional_expression.

assignment_expression ::= conditional_expression.
assignment_expression ::= unary_expression assignment_operator assignment_expression.

assignment_operator ::= EQUALS.
assignment_operator ::= MUL_ASSIGN.
assignment_operator ::= DIV_ASSIGN.
assignment_operator ::= MOD_ASSIGN.
assignment_operator ::= ADD_ASSIGN.
assignment_operator ::= SUB_ASSIGN.
assignment_operator ::= LEFT_ASSIGN.
assignment_operator ::= RIGHT_ASSIGN.
assignment_operator ::= AND_ASSIGN.
assignment_operator ::= XOR_ASSIGN.
assignment_operator ::= OR_ASSIGN.

expression ::= assignment_expression.
expression ::= expression COMMA assignment_expression.

constant_expression ::= conditional_expression.

declaration ::= declaration_specifiers SEMIC.
declaration ::= declaration_specifiers init_declarator_list SEMIC.
declaration ::= error SEMIC.
declaration ::= error RCURLY.

declaration_specifiers ::= storage_class_specifier.
declaration_specifiers ::= storage_class_specifier declaration_specifiers.
declaration_specifiers ::= type_specifier.
declaration_specifiers ::= type_specifier declaration_specifiers.
declaration_specifiers ::= type_qualifier.
declaration_specifiers ::= type_qualifier declaration_specifiers.

init_declarator_list ::= init_declarator.
init_declarator_list ::= init_declarator_list COMMA init_declarator.

init_declarator ::= declarator.
init_declarator ::= declarator EQUALS initializer.

storage_class_specifier ::= TYPEDEF.
storage_class_specifier ::= EXTERN.
storage_class_specifier ::= STATIC.
storage_class_specifier ::= AUTO.
storage_class_specifier ::= REGISTER.

type_specifier ::= VOID.
type_specifier ::= CHAR.
type_specifier ::= SHORT.
type_specifier ::= INT.
type_specifier ::= LONG.
type_specifier ::= FLOAT.
type_specifier ::= DOUBLE.
type_specifier ::= SIGNED.
type_specifier ::= UNSIGNED.
type_specifier ::= struct_or_union_specifier.
type_specifier ::= enum_specifier.
type_specifier ::= TYPE_NAME.

struct_or_union_specifier ::= struct_or_union IDENTIFIER LCURLY struct_declaration_list RCURLY.
struct_or_union_specifier ::= struct_or_union LCURLY struct_declaration_list RCURLY.
struct_or_union_specifier ::= struct_or_union IDENTIFIER.

struct_or_union ::= STRUCT.
struct_or_union ::= UNION.

struct_declaration_list ::= struct_declaration.
struct_declaration_list ::= struct_declaration_list struct_declaration.

struct_declaration ::= specifier_qualifier_list struct_declarator_list SEMIC.

specifier_qualifier_list ::= type_specifier specifier_qualifier_list.
specifier_qualifier_list ::= type_specifier.
specifier_qualifier_list ::= type_qualifier specifier_qualifier_list.
specifier_qualifier_list ::= type_qualifier.

struct_declarator_list ::= struct_declarator.
struct_declarator_list ::= struct_declarator_list COMMA struct_declarator.

struct_declarator ::= declarator.
struct_declarator ::= COLON constant_expression.
struct_declarator ::= declarator COLON constant_expression.

enum_specifier ::= ENUM LCURLY enumerator_list RCURLY.
enum_specifier ::= ENUM IDENTIFIER LCURLY enumerator_list RCURLY.
enum_specifier ::= ENUM IDENTIFIER.

enumerator_list ::= enumerator.
enumerator_list ::= enumerator_list COMMA enumerator.

enumerator ::= IDENTIFIER.
enumerator ::= IDENTIFIER EQUALS constant_expression.

type_qualifier ::= CONST.
type_qualifier ::= VOLATILE.

declarator ::= pointer direct_declarator.
declarator ::= direct_declarator.

direct_declarator ::= IDENTIFIER.
direct_declarator ::= LPAREN declarator RPAREN.
direct_declarator ::= direct_declarator LSQUARE constant_expression RSQUARE.
direct_declarator ::= direct_declarator LSQUARE RSQUARE.
direct_declarator ::= direct_declarator LPAREN parameter_type_list RPAREN.
direct_declarator ::= direct_declarator LPAREN identifier_list RPAREN.
direct_declarator ::= direct_declarator LPAREN RPAREN.

pointer ::= STAR.
pointer ::= STAR type_qualifier_list.
pointer ::= STAR pointer.
pointer ::= STAR type_qualifier_list pointer.

type_qualifier_list ::= type_qualifier.
type_qualifier_list ::= type_qualifier_list type_qualifier.

parameter_type_list ::= parameter_list.
parameter_type_list ::= parameter_list COMMA ELLIPSIS.

parameter_list ::= parameter_declaration.
parameter_list ::= parameter_list COMMA parameter_declaration.
 
parameter_declaration ::= declaration_specifiers declarator.
parameter_declaration ::= declaration_specifiers abstract_declarator.
parameter_declaration ::= declaration_specifiers.

identifier_list ::= IDENTIFIER.
identifier_list ::= identifier_list COMMA IDENTIFIER.

type_name ::= specifier_qualifier_list.
type_name ::= specifier_qualifier_list abstract_declarator.

abstract_declarator ::= pointer.
abstract_declarator ::= direct_abstract_declarator.
abstract_declarator ::= pointer direct_abstract_declarator.

direct_abstract_declarator ::= LPAREN abstract_declarator RPAREN.
direct_abstract_declarator ::= LSQUARE RSQUARE.
direct_abstract_declarator ::= LSQUARE constant_expression RSQUARE.
direct_abstract_declarator ::= direct_abstract_declarator LSQUARE RSQUARE.
direct_abstract_declarator ::= direct_abstract_declarator LSQUARE constant_expression RSQUARE.
direct_abstract_declarator ::= LPAREN RPAREN.
direct_abstract_declarator ::= LPAREN parameter_type_list RPAREN.
direct_abstract_declarator ::= direct_abstract_declarator LPAREN RPAREN.
direct_abstract_declarator ::= direct_abstract_declarator LPAREN parameter_type_list RPAREN.

initializer ::= assignment_expression.
initializer ::= LCURLY initializer_list RCURLY.
initializer ::= LCURLY initializer_list COMMA RCURLY.

initializer_list ::= initializer.
initializer_list ::= initializer_list COMMA initializer.

statement ::= labeled_statement.
statement ::= compound_statement.
statement ::= expression_statement.
statement ::= selection_statement.
statement ::= iteration_statement.
statement ::= jump_statement.

labeled_statement ::= IDENTIFIER COLON statement.
labeled_statement ::= CASE constant_expression COLON statement.
labeled_statement ::= DEFAULT COLON statement.

compound_statement ::= LCURLY RCURLY.
compound_statement ::= LCURLY statement_list RCURLY.
compound_statement ::= LCURLY declaration_list RCURLY.
compound_statement ::= LCURLY declaration_list statement_list RCURLY.

declaration_list ::= declaration.
declaration_list ::= declaration_list declaration.

statement_list ::= statement.
statement_list ::= statement_list statement.

expression_statement ::= SEMIC.
expression_statement ::= expression SEMIC.

selection_statement ::= IF LPAREN expression RPAREN statement.
selection_statement ::= IF LPAREN expression RPAREN statement ELSE statement.
selection_statement ::= SWITCH LPAREN expression RPAREN statement.

iteration_statement ::= WHILE LPAREN expression RPAREN statement.
iteration_statement ::= DO statement WHILE LPAREN expression RPAREN SEMIC.
iteration_statement ::= FOR LPAREN expression_statement expression_statement RPAREN statement.
iteration_statement ::= FOR LPAREN expression_statement expression_statement expression RPAREN statement.

jump_statement ::= GOTO IDENTIFIER SEMIC.
jump_statement ::= CONTINUE SEMIC.
jump_statement ::= BREAK SEMIC.
jump_statement ::= RETURN SEMIC.
jump_statement ::= RETURN expression SEMIC.

translation_unit ::= external_declaration.
translation_unit ::= translation_unit external_declaration.

external_declaration ::= function_definition.
external_declaration ::= declaration.
external_declaration ::= objc_declaration.

function_definition ::= declaration_specifiers declarator declaration_list compound_statement.
function_definition ::= declaration_specifiers declarator compound_statement.
function_definition ::= declarator declaration_list compound_statement.
function_definition ::= declarator compound_statement.

objc_declaration ::= objc_class_def.
objc_declaration ::= objc_class_decl.
objc_declaration ::= objc_alias_decl.
objc_declaration ::= objc_protocol_def.
objc_declaration ::= objc_method_def.
objc_declaration ::= AT_END.

objc_alias_decl ::= AT_ALIAS IDENTIFIER IDENTIFIER SEMIC.
objc_class_decl ::= AT_CLASS identifier_list SEMIC.

objc_class_def ::= AT_INTERFACE IDENTIFIER
					objc_protocol_refs 
					LCURLY objc_impl_var_decl_list RCURLY
					objc_method_proto_list
					AT_END.
objc_class_def ::= AT_INTERFACE IDENTIFIER 
					objc_protocol_refs
					objc_method_proto_list
					AT_END.
objc_class_def ::= AT_INTERFACE IDENTIFIER COLON IDENTIFIER
					objc_protocol_refs
					LCURLY objc_impl_var_decl_list RCURLY
					objc_method_proto_list
					AT_END.
objc_class_def ::= AT_INTERFACE IDENTIFIER COLON IDENTIFIER
					objc_protocol_refs
					objc_method_proto_list
					AT_END.
objc_class_def ::= AT_INTERFACE IDENTIFIER LPAREN IDENTIFIER RPAREN
					objc_protocol_refs
					objc_method_proto_list
					AT_END.

objc_class_def ::= AT_IMPLEMENTATION IDENTIFIER.
objc_class_def ::= AT_IMPLEMENTATION IDENTIFIER COLON IDENTIFIER
					LCURLY objc_impl_var_decl_list RCURLY.
objc_class_def ::= AT_IMPLEMENTATION IDENTIFIER COLON IDENTIFIER.
objc_class_def ::= AT_IMPLEMENTATION IDENTIFIER LPAREN IDENTIFIER RPAREN.

objc_protocol_def ::= AT_PROTOCOL IDENTIFIER objc_protocol_refs
					objc_method_proto_list
					AT_END.

objc_protocol_refs ::= .
objc_protocol_refs ::= LANGLE identifier_list RANGLE.

objc_impl_var_decl_list ::= objc_impl_var_decl_list objc_visibility_spec
							objc_impl_var_decls.
objc_impl_var_decl_list ::= objc_impl_var_decls.

objc_visibility_spec ::= AT_PRIVATE.
objc_visibility_spec ::= AT_PROTECTED.
objc_visibility_spec ::= AT_PUBLIC.

objc_impl_var_decls ::= .
objc_impl_var_decls ::= objc_impl_var_decls objc_impl_var_decl SEMIC.
objc_impl_var_decls ::= objc_impl_var_decls SEMIC. /* stray ; */

objc_impl_var_decl ::= struct_declarator.

objc_method_def ::= PLUS objc_method_decl objc_opt_arg_list compound_statement.
objc_method_def ::= MINUS objc_method_decl objc_opt_arg_list compound_statement.


objc_method_decl ::= LPAREN type_name RPAREN objc_unary_selector.
objc_method_decl ::= objc_unary_selector.
objc_method_decl ::= LPAREN type_name RPAREN objc_keyword_selector objc_opt_param_list.
objc_method_decl ::= objc_keyword_selector objc_opt_param_list.

objc_opt_param_list ::= .
objc_opt_param_list ::= COMMA ELLIPSIS.
objc_opt_param_list ::= COMMA.
objc_opt_param_list ::= FIXME. /* FIXME */

objc_unary_selector ::= objc_selector.
objc_keyword_selector ::= objc_keyword_decl.
objc_keyword_selector ::= objc_keyword_selector objc_keyword_decl.

objc_selector ::= IDENTIFIER.
objc_selector ::= TYPE_NAME.
objc_selector ::= OBJECTNAME. /* what's this? */
objc_selector ::= objc_reserved_words.

objc_reserved_words ::= ENUM.
objc_reserved_words ::= STRUCT.
objc_reserved_words ::= UNION.
objc_reserved_words ::= IF.
objc_reserved_words ::= ELSE.
objc_reserved_words ::= WHILE.
objc_reserved_words ::= DO.
objc_reserved_words ::= FOR.
objc_reserved_words ::= SWITCH.
objc_reserved_words ::= CASE.
objc_reserved_words ::= DEFAULT.
objc_reserved_words ::= BREAK.
objc_reserved_words ::= CONTINUE.
objc_reserved_words ::= RETURN.
objc_reserved_words ::= GOTO.
objc_reserved_words ::= SIZEOF.
objc_reserved_words ::= TYPEOF.

objc_keyword_decl ::= objc_selector COLON LPAREN type_name RPAREN IDENTIFIER.
objc_keyword_decl ::= objc_selector COLON IDENTIFIER.
objc_keyword_decl ::= COLON LPAREN type_name RPAREN IDENTIFIER.
objc_keyword_decl ::= COLON IDENTIFIER.

objc_method_proto_list ::= .
objc_method_proto_list ::= objc_method_proto_list2.

objc_method_proto_list2 ::= objc_method_proto.
objc_method_proto_list2 ::= objc_method_proto_list2 objc_method_proto.
objc_method_proto_list2 ::= declaration.
objc_method_proto_list2 ::= objc_method_proto_list2 declaration.

objc_method_proto ::= PLUS objc_method_decl SEMIC.
objc_method_proto ::= MINUS objc_method_decl SEMIC.

objc_opt_arg_list ::= .
objc_opt_arg_list ::= SEMIC objc_arg_list.

objc_arg_list ::= objc_arg.
objc_arg_list ::= objc_arg_list objc_arg.

objc_arg ::= parameter_declaration SEMIC.


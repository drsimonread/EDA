grammar VHDL;

// TODO Get this to put the parser in a package other than default.

// This is unfortunate.  The package command line option cannot be set in ide4antlr
@header{
	package edu.smcm.mathcs.vhdl;
}

// We don't use this in case we can re-use the grammar with different target languages
// @lexer::members {
//	public static final int WHITESPACE = 1;
//	public static final int COMMENTS = 2;
// }

// This grammar follows the IEEE 1076-1993 standard as closely as possible.
// The grammar will be updated to IEEE 1076-2014 when it becomes available.

// There is a well known problem with the name production.  This contains an ambiguity that
// can only be resolved by knowing the type of identifiers.  Rather than resolve this in the
// parser, we accept all possibilities and leave it to the user application to determine the
// valid choice.

// Section 13.9 Reserved Words

ABS: [Aa][Bb][Ss] ;
ACCESS: [Aa][Cc][Cc][Ee][Ss][Ss] ;
AFTER: [Aa][Ff][Tt][Ee][Rr] ;
ALIAS: [Aa][Ll][Ii][Aa][Ss] ;
ALL: [Aa][Ll][Ll] ;
AND: [Aa][Nn][Dd] ;
ARCHITECTURE: [Aa][Rr][Cc][Hh][Ii][Tt][Ee][Cc][Tt][Uu][Rr][Ee] ;
ARRAY: [Aa][Rr][Rr][Aa][Yy] ;
ASSERT: [Aa][Ss][Ss][Ee][Rr][Tt] ;
ATTRIBUTE: [Aa][Tt][Tt][Rr][Ii][Bb][Uu][Tt][Ee] ;
BEGIN: [Bb][Ee][Gg][Ii][Nn] ;
BLOCK: [Bb][Ll][Oo][Cc][Kk] ;
BODY: [Bb][Oo][Dd][Yy] ;
BUFFER: [Bb][Uu][Ff][Ff][Ee][Rr] ;
BUS: [Bb][Uu][Ss] ;
CASE: [Cc][Aa][Ss][Ee] ;
COMPONENT: [Cc][Oo][Mm][Pp][Oo][Nn][Ee][Nn][Tt] ;
CONFIGURATION: [Cc][Oo][Nn][Ff][Ii][Gg][Uu][Rr][Aa][Tt][Ii][Oo][Nn] ;
CONSTANT: [Cc][Oo][Nn][Ss][Tt][Aa][Nn][Tt] ;
DISCONNECT: [Dd][Ii][Ss][Cc][Oo][Nn][Nn][Ee][Cc][Tt] ;
DOWNTO: [Dd][Oo][Ww][Nn][Tt][Oo] ;
ELSE: [Ee][Ll][Ss][Ee] ;
ELSIF: [Ee][Ll][Ss][Ii][Ff] ;
END: [Ee][Nn][Dd] ;
ENTITY: [Ee][Nn][Tt][Ii][Tt][Yy] ;
EXIT: [Ee][Xx][Ii][Tt] ;
FILE: [Ff][Ii][Ll][Ee] ;
FOR: [Ff][Oo][Rr] ;
FUNCTION: [Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn] ;
GENERATE: [Gg][Ee][Nn][Ee][Rr][Aa][Tt][Ee] ;
GENERIC: [Gg][Ee][Nn][Ee][Rr][Ii][Cc] ;
GROUP: [Gg][Rr][Oo][Uu][Pp] ;
GUARDED: [Gg][Uu][Aa][Rr][Dd][Ee][Dd] ;
IF: [Ii][Ff] ;
IMPURE: [Ii][Mm][Pp][Uu][Rr][Ee] ;
IN: [Ii][Nn] ;
INERTIAL: [Ii][Nn][Ee][Rr][Tt][Ii][Aa][Ll] ;
INOUT: [Ii][Nn][Oo][Uu][Tt] ;
IS: [Ii][Ss] ;
LABEL: [Ll][Aa][Bb][Ee][Ll] ;
LIBRARY: [Ll][Ii][Bb][Rr][Aa][Rr][Yy] ;
LINKAGE: [Ll][Ii][Nn][Kk][Aa][Gg][Ee] ;
LITERAL: [Ll][Ii][Tt][Ee][Rr][Aa][Ll] ;
LOOP: [Ll][Oo][Oo][Pp] ;
MAP: [Mm][Aa][Pp] ;
MOD: [Mm][Oo][Dd] ;
NAND: [Nn][Aa][Nn][Dd] ;
NEW: [Nn][Ee][Ww] ;
NEXT: [Nn][Ee][Xx][Tt] ;
NOR: [Nn][Oo][Rr] ;
NOT: [Nn][Oo][Tt] ;
NULL: [Nn][Uu][Ll][Ll] ;
OF: [Oo][Ff] ;
ON: [Oo][Nn] ;
OPEN: [Oo][Pp][Ee][Nn] ;
OR: [Oo][Rr] ;
OTHERS: [Oo][Tt][Hh][Ee][Rr][Ss] ;
OUT: [Oo][Uu][Tt] ;
PACKAGE: [Pp][Aa][Cc][Kk][Aa][Gg][Ee] ;
PORT: [Pp][Oo][Rr][Tt] ;
POSTPONED: [Pp][Oo][Ss][Tt][Pp][Oo][Nn][Ee][Dd] ;
PROCEDURE: [Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee] ;
PROCESS: [Pp][Rr][Oo][Cc][Ee][Ss][Ss] ;
PURE: [Pp][Uu][Rr][Ee] ;
RANGE: [Rr][Aa][Nn][Gg][Ee] ;
RECORD: [Rr][Ee][Cc][Oo][Rr][Dd] ;
REGISTER: [Rr][Ee][Gg][Ii][Ss][Tt][Ee][Rr] ;
REJECT: [Rr][Ee][Jj][Ee][Cc][Tt] ;
REM: [Rr][Ee][Mm] ;
REPORT: [Rr][Ee][Pp][Oo][Rr][Tt] ;
RETURN: [Rr][Ee][Tt][Uu][Rr][Nn] ;
ROL: [Rr][Oo][Ll] ;
ROR: [Rr][Oo][Rr] ;
SELECT: [Ss][Ee][Ll][Ee][Cc][Tt] ;
SEVERITY: [Ss][Ee][Vv][Ee][Rr][Ii][Tt][Yy] ;
SHARED: [Ss][Hh][Aa][Rr][Ee][Dd] ;
SIGNAL: [Ss][Ii][Gg][Nn][Aa][Ll] ;
SLA: [Ss][Ll][Aa] ;
SLL: [Ss][Ll][Ll] ;
SRA: [Ss][Rr][Aa] ;
SRL: [Ss][Rr][Ll] ;
SUBTYPE: [Ss][Uu][Bb][Tt][Yy][Pp][Ee] ;
THEN: [Tt][Hh][Ee][Nn] ;
TO: [Tt][Oo] ;
TRANSPORT: [Tt][Rr][Aa][Nn][Ss][Pp][Oo][Rr][Tt] ;
TYPE: [Tt][Yy][Pp][Ee] ;
UNAFFECTED: [Uu][Nn][Aa][Ff][Ff][Ee][Cc][Tt][Ee][Dd] ;
UNITS: [Uu][Nn][Ii][Tt][Ss] ;
UNTIL: [Uu][Nn][Tt][Ii][Ll] ;
USE: [Uu][Ss][Ee] ;
VARIABLE: [Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee] ;
WAIT: [Ww][Aa][Ii][Tt] ;
WHEN: [Ww][Hh][Ee][Nn] ;
WHILE: [Ww][Hh][Ii][Ll][Ee] ;
WITH: [Ww][Ii][Tt][Hh] ;
XNOR: [Xx][Nn][Oo][Rr] ;
XOR: [Xx][Oo][Rr] ;

// Section 13.1 Character set

fragment BASIC_GRAPHIC_CHARACTER :
  UPPER_CASE_LETTER | DIGIT | SPECIAL_CHARACTER | ' ' ;
 
fragment GRAPHIC_CHARACTER :
  BASIC_GRAPHIC_CHARACTER | LOWER_CASE_LETTER | OTHER_SPECIAL_CHARACTER ;
  
fragment UPPER_CASE_LETTER :
  [A-Z\u00C0-\u00D6\u00D8-\u00DF] ;
  
fragment DIGIT :
  [0-9] ;

// NOTE: The ' character throws off the formatting in antlr4ide
fragment SPECIAL_CHARACTER :
  ["#&\u0027\(\)*+,\-./:;<=>\[\]_|] ;
  
fragment LOWER_CASE_LETTER :
  [a-z\u00E0-\u00F6\u00F8-\u00FF] ;
  
fragment OTHER_SPECIAL_CHARACTER :
  [!$%@?\005C^`{}\~\u00A1-\u00BF\u00D7\u00F7] ;

// Section 13.2 Lexical elements, separators, and delimiters

WS: [ \t\r\n\u000B\u000C\u00A0]+ -> channel(1) ;

// Section 13.3 Identifiers

// Section 13.3.1 Basic Identifiers

BASIC_IDENTIFIER :
	LETTER ( ('_')? LETTER_OR_DIGIT )*
	;
	
fragment LETTER_OR_DIGIT :
	LETTER | DIGIT 
	;
	
fragment LETTER :
	UPPER_CASE_LETTER | LOWER_CASE_LETTER ;
	
// Section 13.3.2 Extended identifiers

// NOTE: This differs significantly from the rule in the LRM because the rule doesn't
// account for the use of \\ in extended identifiers to represent \.

// LRM: The LRM doesn't specifically disallow the empty identifier (\\) but we're
// assuming that this is not permitted.
EXTENDED_IDENTIFIER :
	('\\' (GRAPHIC_CHARACTER)*? '\\')*
	'\\' GRAPHIC_CHARACTER ( GRAPHIC_CHARACTER )*? '\\' 
	('\\' (GRAPHIC_CHARACTER)*? '\\')* |
	'\\\\';

// Section 13.8 Comments

PRAGMA: '--' ([ \t])* [Pp][Rr][Aa][Gg][Mm][Aa] .*? [\r\n] -> channel(3);
COMMENT: '--' .*? [\r\n] -> channel(2) ;

// Section 13.4 Abstract literals

// Section 13.4.1 Decimal literals

DECIMAL_LITERAL :
	INTEGER ('.' INTEGER )? ( EXPONENT )? 
	;

fragment INTEGER :
	DIGIT ( ('_')? DIGIT )*
	;

fragment EXPONENT :
	'E' ('+'|'-')? INTEGER
	;

// Section 13.4.2 Based literals
// NOTE: See also: Section 13.10 Allowable replacements of characters 

BASED_LITERAL :
	(BASE '#' BASED_INTEGER ( '.' BASED_INTEGER )? '#' ( EXPONENT )? ) |
	(BASE ':' BASED_INTEGER ( '.' BASED_INTEGER )? ':' ( EXPONENT )? )
	;
	
fragment BASE :
	INTEGER
	;
	
fragment BASED_INTEGER :
	EXTENDED_DIGIT ( ( '_' )? EXTENDED_DIGIT )*
	;
	
fragment EXTENDED_DIGIT :
	DIGIT | LETTER
	;

// Section 13.5 Character literals

// TODO Alternate production shouldn't be necessary
CHARACTER_LITERAL :
	'\'' GRAPHIC_CHARACTER '\'' | '\'\'\''
	;
	
// Section 13.6 String literals
// See also: Section 13.10 Allowable replacements of characters

// NOTE: This differs significantly from the rule in the LRM because the rule doesn't
// account for the use of "" in string literals to represent ".

// LRM: Section 13.10 doesn't specify how to treat special cases of % enclosed 
// string_literals.

// NOTE: We don't check to see if the " character is absent in % enclosed
// string_literals.  This will need to be done by an analyzer; doing so in the
// lexer is overly complicated.

STRING_LITERAL :
	( '\"' GRAPHIC_CHARACTER*? '\"' ('\"' GRAPHIC_CHARACTER*? '\"')* ) |
	('%' GRAPHIC_CHARACTER*? '%')
	;

// Section 13.7 Bit string literals
// NOTE: See also: Section 13.10 Allowable replacements of characters

BIT_STRING_LITERAL :
	( BASE_SPECIFIER '\"' ( BIT_VALUE )? '\"' ) |
	( BASE_SPECIFIER '%' ( BIT_VALUE )? '%')
	;

fragment BIT_VALUE :
	EXTENDED_DIGIT ( ( '_' )? EXTENDED_DIGIT )*
	;

// LRM: The standard doesn't mention lower-case base_specifiers	
fragment BASE_SPECIFIER :
	[BbOoXx]
	;
	
// Annex A
// Syntax Summary

abstract_literal :
	DECIMAL_LITERAL	| BASED_LITERAL
	;

access_type_definition : 
	ACCESS subtype_indication
	;

actual_designator :
	expression
	| name
	| OPEN
	;

actual_parameter_part :
	association_list
	;

actual_part :
	actual_designator
	| name '(' actual_designator ')'
	| type_mark '(' actual_designator ')'
	;

adding_operator :
	'+' | '-' | '&'
	;

aggregate :
	'(' element_association ( ',' element_association )* ')'
	;

alias_declaration :
	ALIAS alias_designator ( ':' subtype_indication )? IS name signature? ';'
	;
	
alias_designator : 
	identifier | CHARACTER_LITERAL | operator_symbol
	;

allocator :
	NEW subtype_indication
	| NEW qualified_expression
	;

architecture_body :
	ARCHITECTURE identifier OF name IS
		architecture_declarative_part
	BEGIN
		architecture_statement_part
	END ARCHITECTURE? simple_name? ';'
	;

architecture_declarative_part :
	block_declarative_item*
	;

architecture_statement_part :
	concurrent_statement*
	;

array_type_definition :
	unconstrained_array_definition
	| constrained_array_definition
	;
 
assertion :
	ASSERT condition
		( REPORT expression )?
		( SEVERITY expression )?
		;

assertion_statement :
	 ( label ':' )? assertion ';'
	 ;

association_element :
	( formal_part '=>' )? actual_part
	;

association_list :
	association_element ( ',' association_element )*
	;
	
attribute_declaration :
	ATTRIBUTE identifier ':' type_mark ';'
	;

attribute_designator : 
	simple_name
	;

// NOTE: See name.  Part of indirect left-recursion.
// attribute_name :
//	prefix signature? '\'' attribute_designator ( '(' expression ')' )?
//	;

attribute_specification :
	ATTRIBUTE attribute_designator OF entity_specification IS expression ';'
	;

base_unit_declaration : 
	identifier ';'
	;

binding_indication :
	( USE entity_aspect )?
	generic_map_aspect?
	port_map_aspect?
	;

block_configuration :
	FOR block_specification
		( use_clause )*
		( configuration_item )*
	END FOR ';'
	;

block_declarative_item :
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| component_declaration
	| attribute_declaration
	| attribute_specification
	| configuration_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration
	;

block_declarative_part :
	( block_declarative_item )*
	;

block_header :
	( generic_clause
	( generic_map_aspect ';' )? )?
	( port_clause
	( port_map_aspect ';' )? )?
	;

block_specification :
	name
	| label ( '(' index_specification ')' )?
	;

block_statement :
	label ':'
		BLOCK ( '(' expression ')' )? IS?
			block_header
			block_declarative_part
		BEGIN
			block_statement_part
		END BLOCK label? ';'
		;

block_statement_part :
	( concurrent_statement )*
	;

case_statement :
	( label ':' )?
		CASE expression IS
			case_statement_alternative
			( case_statement_alternative )*
		END CASE label? ';'
	;

case_statement_alternative :
	WHEN choices '=>'
		sequence_of_statements
	;

choice :
	simple_expression
	| discrete_range
	| simple_name
	| OTHERS
	;

// NOTE: See also: 13.10 Allowable replacements of characters
choices : 
	choice ( '|' choice )* |
	choice ( '!' choice )*
	;

component_configuration :
	FOR component_specification
		( binding_indication ';' )?
		block_configuration?
	END FOR ';'
	;

component_declaration :
	COMPONENT identifier IS?
		generic_clause?
		port_clause?
	END COMPONENT simple_name? ';'
	;

component_instantiation_statement :
	label ':'
		instantiated_unit
			generic_map_aspect?
			port_map_aspect? ';'
	;

component_specification :
	instantiation_list ':' name
	;

composite_type_definition :
	array_type_definition
	| record_type_definition
	;

concurrent_assertion_statement :
	( label ':' )? POSTPONED? assertion ';'
	;

concurrent_procedure_call_statement :
	( label ':' )? POSTPONED? procedure_call ';'
	;

concurrent_signal_assignment_statement :
	  ( label ':' )? POSTPONED? conditional_signal_assignment
	| ( label ':' )? POSTPONED? selected_signal_assignment
	;

concurrent_statement :
	block_statement
	| process_statement
	| concurrent_procedure_call_statement
	| concurrent_assertion_statement
	| concurrent_signal_assignment_statement
	| component_instantiation_statement
	| generate_statement
	;

condition :
	expression
	;

condition_clause :
	UNTIL condition
	;

conditional_signal_assignment :
	target '<=' vhdl_options conditional_waveforms ';'
	;

conditional_waveforms :
	( waveform WHEN condition ELSE )*
	waveform ( WHEN condition )?
	;

configuration_declaration :
	CONFIGURATION identifier OF name IS
		configuration_declarative_part
		block_configuration
	END CONFIGURATION? simple_name? ';'
	;

configuration_declarative_item :
	use_clause
	| attribute_specification
	| group_declaration
	;

configuration_declarative_part :
	( configuration_declarative_item )*
	;

configuration_item :
	block_configuration
	| component_configuration
	;

configuration_specification :
	FOR component_specification binding_indication ';'
	;

constant_declaration :
	CONSTANT identifier_list ':' subtype_indication ( ':=' expression )? ';'
	;

constrained_array_definition :
	ARRAY index_constraint OF subtype_indication
	;

constraint :
	range_constraint
	| index_constraint
	;

context_clause : 
	( context_item )*
	;

context_item :
	library_clause
	| use_clause
	;

declaration :
	type_declaration
	| subtype_declaration
	| object_declaration
	| interface_declaration
	| alias_declaration
	| attribute_declaration
	| component_declaration
	| group_template_declaration
	| group_declaration
	| entity_declaration
	| configuration_declaration
	| subprogram_declaration
	| package_declaration
	;

delay_mechanism :
	TRANSPORT
	| ( REJECT expression )? INERTIAL
	;

design_file : 
	design_unit ( design_unit )*
	;

design_unit : 
	context_clause library_unit
	;

designator : 
	identifier | operator_symbol
	;

direction :
	TO 
	| DOWNTO
	;

disconnection_specification :
	DISCONNECT guarded_signal_specification AFTER expression ';'
	;

discrete_range : 
	subtype_indication | range
	;

element_association :
	( choices '=>' )? expression
	;

element_declaration :
	identifier_list ':' element_subtype_definition ';'
	;

element_subtype_definition : 
	subtype_indication
	;

entity_aspect :
	  ENTITY name ( '(' identifier ')' )?
	| CONFIGURATION name
	| OPEN
	;

entity_class :
	ENTITY      | ARCHITECTURE | CONFIGURATION
	| PROCEDURE	| FUNCTION     | PACKAGE
	| TYPE      | SUBTYPE      | CONSTANT
	| SIGNAL    | VARIABLE     | COMPONENT
	| LABEL     | LITERAL      | UNITS
	| GROUP     | FILE
	;

entity_class_entry :
	entity_class ( '<>' )?
	;

entity_class_entry_list :
	entity_class_entry ( ',' entity_class_entry )*
	;

entity_declaration :
	ENTITY identifier IS
		entity_header
		entity_declarative_part
    ( BEGIN
		entity_statement_part )?
	END ENTITY? simple_name? ';'
	;

entity_declarative_item :
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration
	;

entity_declarative_part :
	entity_declarative_item*
	;

entity_designator : 
	entity_tag ( signature )?
	;

entity_header :
	generic_clause?
	port_clause?
	;

entity_name_list :
	entity_designator ( ',' entity_designator )*
	| OTHERS
	| ALL
	;

entity_specification :
	entity_name_list ':' entity_class
	;

entity_statement :
	concurrent_assertion_statement
	| concurrent_procedure_call_statement
	| process_statement
	;

entity_statement_part :
	( entity_statement )*
	;

entity_tag :
	simple_name	| CHARACTER_LITERAL | operator_symbol
	;

enumeration_literal : 
	identifier | CHARACTER_LITERAL
	;

enumeration_type_definition :
	'(' enumeration_literal ( ',' enumeration_literal )* ')'
	;

exit_statement :
	( label ':' )? EXIT label? ( WHEN condition )? ';'
	;

expression :
  	relation ( AND relation )*
	| relation ( OR relation )*
	| relation ( XOR relation )*
	| relation ( NAND relation )?
	| relation ( NOR relation )?
	| relation ( XNOR relation )*
	;

factor :
	primary ( '**' primary )?
	| ABS primary
	| NOT primary
	;

file_declaration :
	FILE identifier_list ':' subtype_indication file_open_information? ';'
	;

file_logical_name : 
	expression
	;

file_open_information :
	( OPEN expression )? IS file_logical_name
	;

file_type_definition :
	FILE OF type_mark
	;

floating_type_definition : 
	range_constraint
	;

formal_designator :
	name
	;

formal_parameter_list : 
	interface_list
	;

formal_part :
	formal_designator
	| name '(' formal_designator ')'
	| type_mark '(' formal_designator ')'
	;

full_type_declaration :
	TYPE identifier IS type_definition ';'
	;

// NOTE: See name.  Part of indirect left-recursion.
// function_call :
//	name ( '(' actual_parameter_part ')' )?
//	;

generate_statement :
	label ':'
		generation_scheme GENERATE
			( block_declarative_item*
		BEGIN )?
			concurrent_statement*
		END GENERATE label? ';'
	;

generation_scheme :
	FOR parameter_specification
	| IF condition
	;

generic_clause :
	GENERIC '(' generic_list ')' ';'
	;

generic_list :
	interface_list
	;

generic_map_aspect :
	GENERIC MAP '(' association_list ')'
	;

group_constituent : 
	name 
	| CHARACTER_LITERAL
	;

group_constituent_list : 
	group_constituent ( ',' group_constituent )*
	;

group_declaration :
	GROUP identifier ':' name '(' group_constituent_list ')' ';'
	;

group_template_declaration :
	GROUP identifier IS '(' entity_class_entry_list ')' ';'
	;

guarded_signal_specification :
	signal_list ':' type_mark
	;

identifier : BASIC_IDENTIFIER | EXTENDED_IDENTIFIER
	;

identifier_list :
	identifier ( ',' identifier )*
	;

if_statement :
	( label ':' )?
		IF condition THEN
			sequence_of_statements
		( ELSIF condition THEN
			sequence_of_statements )?
		( ELSE
			sequence_of_statements )?
		END IF label? ';'
		;

incomplete_type_declaration :
	 TYPE identifier ';'
	 ;

index_constraint : 
	'(' discrete_range ( ',' discrete_range )* ')'
	;

index_specification :
	discrete_range
	| expression
	;

index_subtype_definition : 
	type_mark range '<>'
	;

// NOTE: See name. Part of indirect left-recursion
// indexed_name : 
//	prefix '(' expression ( ',' expression )* ')'
//	;

instantiated_unit :
	COMPONENT? name
	| ENTITY name ( '(' identifier ')' )?
	| CONFIGURATION name
	;	

instantiation_list :
	label ( ',' label )*
	| OTHERS
	| ALL
	;

integer_type_definition : 
	range_constraint
	;
	
interface_constant_declaration :
	CONSTANT? identifier_list ':' IN? subtype_indication ( ':=' expression )?
	;

interface_declaration :
	interface_constant_declaration
	| interface_signal_declaration
	| interface_variable_declaration
	| interface_file_declaration
	;

interface_element : 
	interface_declaration
	;
	
interface_file_declaration :
	FILE identifier_list ':' subtype_indication
	;

interface_list :
	interface_element ( ';' interface_element )*
	;

interface_signal_declaration :
	SIGNAL? identifier_list ':'  vhdl_mode? subtype_indication BUS? ( ':=' expression )?
	;
	
interface_variable_declaration :
	VARIABLE? identifier_list ':' vhdl_mode? subtype_indication ( ':=' expression )?
	;
	
iteration_scheme :
	WHILE condition
	| FOR parameter_specification
	;

label : 
	identifier
	;

library_clause : 
	LIBRARY logical_name_list ';'
	;

library_unit :
	primary_unit
	| secondary_unit
	;
	
literal :
	numeric_literal
	| enumeration_literal
	| STRING_LITERAL
	| BIT_STRING_LITERAL
	| NULL
	;

logical_name : 
	identifier
	;
	
logical_name_list : 
	logical_name ( ',' logical_name )*
	;

logical_operator : 
	AND | OR | NAND | NOR | XOR | XNOR
	;

loop_statement :
	( label ':' )?
		iteration_scheme? LOOP
			sequence_of_statements
		END LOOP label? ';'
	;

miscellaneous_operator : 
	'**' | ABS | NOT
	;

// NOTE: mode is an ANTLR keyword
vhdl_mode :
	IN 
	| OUT 
	| INOUT 
	| BUFFER 
	| LINKAGE
	;

multiplying_operator : 
	'*' | '/' | MOD	| REM
	;

// NOTE: Altered to overcome indirect left recursion through prefix
// NOTE: Altered to overcome ambiguity between indexed_name slice_name and
// function_call
// NOTE: A function call needn't have a parameter list.
// NOTE: There is an ambiguity between indexed_name and function_call that must be 
// resolved during elaboration.

// LRM: The standard doesn't mention if spaces are allowed around the period in a 
// selected_name.

 name :
	simple_name # name_simple_name
	| operator_symbol # name_operator_symbol
//	| selected_name
	| name '.' suffix # selected_name
//	| indexed_name
//	| name '(' expression ( ',' expression )* ')' # indexed_name
	| name '(' actual_parameter_part ')' # function_or_indexed
//	| slice_name
	| name '(' discrete_range ')' # slice_name
//	| attribute_name
	| name signature? '\'' attribute_designator ( '(' expression ')' )? # attribute_name
	;

next_statement :
	( label ':' )? NEXT label? ( WHEN condition )? ';'
	;

null_statement : 
	( label ':' )? NULL ';'
	;

numeric_literal :
	abstract_literal
	| physical_literal
	;

object_declaration :
	constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	;

operator_symbol : 
	STRING_LITERAL
	;

// NOTE: options is an ANTLR keyword
vhdl_options :
	GUARDED? delay_mechanism?
	;
	
package_body :
	PACKAGE BODY simple_name IS
		package_body_declarative_part
	END ( PACKAGE BODY )? simple_name? ';'
	;
	
package_body_declarative_item :
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| use_clause
	| group_template_declaration
	| group_declaration
	;

package_body_declarative_part :
	( package_body_declarative_item )*
	;

package_declaration :
	PACKAGE identifier IS
		package_declarative_part
	END PACKAGE? simple_name? ';'
	;

package_declarative_item :
	subprogram_declaration
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| signal_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| component_declaration
	| attribute_declaration
	| attribute_specification
	| disconnection_specification
	| use_clause
	| group_template_declaration
	| group_declaration
	;

package_declarative_part :
	( package_declarative_item )*
	;

parameter_specification :
	identifier IN discrete_range
	;

physical_literal : 
	abstract_literal? name
	;

physical_type_definition :
	range_constraint
		UNITS
			base_unit_declaration
			secondary_unit_declaration*
		END UNITS simple_name?
	;

port_clause :
	PORT '(' port_list ')' ';'
	;

port_list : 
	interface_list
	;

port_map_aspect :
	PORT MAP '(' association_list ')'
	;

// NOTE: See name. Part of indirect left-recursion.
// prefix :
//	name
//	| function_call
//	;
	
// NOTE: See name. Part of indirect left-recursion.
primary :
	name
	| literal
	| aggregate
//	| function_call
	| qualified_expression
	| type_conversion
	| allocator
	| '(' expression ')'
	;

primary_unit :
	entity_declaration
	| configuration_declaration
	| package_declaration
	;

procedure_call : 
	name ( '(' actual_parameter_part ')' )?
	;

procedure_call_statement :
	( label ':' )? procedure_call ';'
	;

process_declarative_item :
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| use_clause
	| group_template_declaration
	| group_declaration
	;

process_declarative_part :
	process_declarative_item*
	;

process_statement :
	( label ':' )?
	 	POSTPONED? PROCESS ( '(' sensitivity_list ')' )? IS?
			process_declarative_part
		BEGIN
			process_statement_part
		END POSTPONED? PROCESS label? ';'
		;

process_statement_part :
	sequential_statement*
	;

qualified_expression :
	type_mark '\'' '(' expression ')'
	| type_mark '\'' aggregate
	;

// NOTE: attribute_name no longer exists
// NOTE: An analyzer should check that name is the name of an attribute
range :
	name
	| simple_expression direction simple_expression
	;

range_constraint : 
	RANGE range
	;

record_type_definition :
	RECORD
		element_declaration
		element_declaration*
	END RECORD simple_name?
	;

relation :
	shift_expression ( relational_operator shift_expression )?
	;

relational_operator :
	'='  
	| '/='  
	| '<'
	| '<='
	| '>'  
	| '>='
	;

report_statement :
	( label ':' )?
		REPORT expression
			( SEVERITY expression )? ';'
	;

return_statement :
	( label ':' )? RETURN expression? ';'
	;

scalar_type_definition :
	enumeration_type_definition
	| integer_type_definition
	| floating_type_definition
	| physical_type_definition
	;

secondary_unit :
	architecture_body
	| package_body
	;

secondary_unit_declaration :
	identifier '=' physical_literal ';'
	;

// NOTE: See name. Part of indirect left-recursion
// selected_name :
//	prefix '.' suffix
//	;

selected_signal_assignment :
	WITH expression SELECT
		target	'<=' vhdl_options selected_waveforms ';'
	;

selected_waveforms :
	( waveform WHEN choices ',' )*
	waveform WHEN choices
	;

sensitivity_clause :
	ON sensitivity_list
	;

sensitivity_list : 
	name ( ',' name )*
	;

sequence_of_statements :
	( sequential_statement )*
	;

sequential_statement :
	wait_statement
	| assertion_statement
	| report_statement
	| signal_assignment_statement
	| variable_assignment_statement
	| procedure_call_statement
	| if_statement
	| case_statement
	| loop_statement
	| next_statement
	| exit_statement
	| return_statement
	| null_statement
	;

shift_expression :
	simple_expression ( shift_operator simple_expression )?
	;

shift_operator : 
	SLL
	| SRL 
	| SLA 
	| SRA 
	| ROL 
	| ROR
	;

sign :
	'+'
	| '-'
	;

signal_assignment_statement :
	( label ':' )? target '<=' delay_mechanism? waveform ';'
	;

signal_declaration :
	SIGNAL identifier_list ':' subtype_indication signal_kind? ( ':=' expression )? ';'
	;

signal_kind :
	REGISTER
	|  BUS
	;

signal_list :
	name ( ',' name )*
	| OTHERS
	| ALL
	;

signature : 
	'[' ( type_mark ( ',' type_mark )* )? ( RETURN type_mark )? ']'
	;

simple_expression :
	sign? term ( adding_operator term )*
	;

simple_name :	 
	identifier
	;

// NOTE: See name. Part of indirect left-recursion
// slice_name :
//	prefix '(' discrete_range ')'
//	;

subprogram_body :
	subprogram_specification IS
		subprogram_declarative_part
	BEGIN
		subprogram_statement_part
	END subprogram_kind? designator? ';'
	;

subprogram_declaration :
	subprogram_specification ';'
	;

subprogram_declarative_item :
	subprogram_declaration
	| subprogram_body
	| type_declaration
	| subtype_declaration
	| constant_declaration
	| variable_declaration
	| file_declaration
	| alias_declaration
	| attribute_declaration
	| attribute_specification
	| use_clause
	| group_template_declaration
	| group_declaration
	;

subprogram_declarative_part :
	( subprogram_declarative_item )*
	;

subprogram_kind : 
	PROCEDURE 
	| FUNCTION
	;

subprogram_specification :
	PROCEDURE designator ( '(' formal_parameter_list ')' )?
	| ( PURE | IMPURE )?  FUNCTION designator ( '(' formal_parameter_list ')' )?
		RETURN type_mark
	;

subprogram_statement_part :
	( sequential_statement )*
	;

subtype_declaration :
	SUBTYPE identifier IS subtype_indication ';'
	;

subtype_indication :
	name? type_mark constraint?
	;

suffix :
	simple_name
	| CHARACTER_LITERAL
	| operator_symbol
	| ALL
	;

target :
	name
	| aggregate
	;

term :
	factor ( multiplying_operator factor )*
	;

timeout_clause : 
	FOR expression
	;

type_conversion : 
	type_mark '(' expression ')'
	;

type_declaration :
	full_type_declaration
	| incomplete_type_declaration
	;

type_definition :
	scalar_type_definition
	| composite_type_definition
	| access_type_definition
	| file_type_definition
	;

type_mark :
	name
	;

unconstrained_array_definition :
	ARRAY '(' index_subtype_definition ( ',' index_subtype_definition )* ')'
		OF subtype_indication
	;

// NOTE: See name.  selected_name no longer exists.
use_clause :
//	USE selected_name ( ',' selected_name )* ';'
	USE name ( ',' name )* ';'
	;

variable_assignment_statement :
	( label ':' )? target ':=' expression ';'
	;

variable_declaration :
	SHARED? VARIABLE identifier_list ':' subtype_indication ( ':=' expression )? ';'
	;

wait_statement :
	( label ':' )? WAIT sensitivity_clause? condition_clause? timeout_clause? ';'
	;

waveform :
	waveform_element ( ',' waveform_element )*
	| UNAFFECTED
	;

waveform_element :
	expression ( AFTER expression )?
	| NULL ( AFTER expression )?
	;

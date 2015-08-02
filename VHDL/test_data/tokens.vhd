-- This file is just intended to validate the lexical analyser
-- NOTE: This is an ASCII file so it can't contain Latin-1 characters.
--       This limits out tests.

--
-- 1994: 13.1 Character Set
-- NOTE: Cannot be tested directly

--
-- 1994: 13.2 Lexical elements
-- Separators are sent to "whitespace" (with the exception of the end of line for a comment) 
-- first second
-- first	second
-- NOTE: Cannot test vertical tab
-- NOTE: Cannot test non-breaking space

-- Special character separators
first&second
first'second
first(second
first)second
first*second
first+second
first+second
first,second
first-second
first.second
first/second
first:second
first;second
first<second
first=second
first>second
first|second
first[second
first]second

-- Compound delimiters
first=>second -- arrow
first**second -- double star, exponentiate
first:=second -- variable assignment
first/=second -- inequality (pronounced "not equal")
first>=second -- greater than or equal
first<=second -- less than or equal; signal assignment
first<>second -- box

--
-- 1994:13.3 Identifiers
-- 1994:13.3.1 Basic identifiers
-- LRM Start
COUNT	X	c_out		FFT		Decoder
VHSIC	X1	PageCount	STORE_NEXT_ITEM
-- LRM End

n identifier A IDENTIFIER N AB
IDENTI_FIER
I_D_E_N_T_I_F_I_E_R

-- 1994:13.3.2 Extended Identifiers
-- NOTE: We cannot test non-ASCII characters

-- LRM Start
\BUS\	\bus\
\a\\b\
VHDL \VHDL\ \vhdl\
-- LRM End

\ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"#&'()*+,-./:;<=>[]_|!$%@?\\^`{}~\

-- 1994:13.4 Abstract literals

-- 1994:13.4.1 Decimal literals

-- LRM
12			0		1E6			123_456
12.0		0.0		0.456		3.14159_26
1.34E-12	1.0E+6	6.023E+24

-- 1994:13.4.2 Based literals

-- LRM Start
B"1111_1111_1111"
X"FFFF"
O"777"
X"777"
-- LRM End

-- 1994:13.5 Character literals

-- LRM Start
'A'	'*'	'''	' '
-- LRM End

-- 1994:13.6 String literals

-- LRM Start
"Setup time is too short"
'"'
" "	"A"	""""
-- LRM End

"This string contains an *"

-- This comment contains an *

-- 1994:13.7 Bit string literals

-- LRM Start
B"1111_1111_1111"
X"FFFF"
O"777"
X"777"
-- LRM End

-- 1994:13.8 Comments

-- LRM Start
-- The last sentence above paraphrases the Algol 68 report.

end;		-- Processing of LINE is complete

-- A long comment may be split onto
-- two or more consecutive lines.

-- LRM End

------------------ The first two hyphens start the comment.

-- Horizontal	tabulation
-- pragma This is a pragma.

-- 1994:13.9 Reserved words

-- LRM Start
abs				file		nand	select
access			for			new		severity
after			function	next	signal
alias						nor		shared
all				generate	not		sla
and				generic		null	sll
architecture	group				sra
array			guarded		of		srl
assert						on		subtype
attribute		if			open	
				impure		or		then
begin			in			others	to
block			inertial	out		transport
body			inout				type
buffer			is			package	
bus							port	unaffected
				label		postponed	units
case			library		procedure	until
component		linkage		process		use
configuration	literal		pure
constant		loop					variable
							range
disconnect		map			record		wait
downto			mod			register	when
							reject		while
else						rem			with
elsif						report		
end							return		xnor
entity						rol			xor
exit						ror			
-- LRM End

-- 1994:13.10 Allowable replacement of characters

%This is a string!%

! -- This is a separator

2:1111_1111:	16:FF:					016:0FF:
16:E:E1			2:1110_0000:
16:F.FF:E+2		2:1.1111_1111_111:E11



B%1111_1111_1111%
X%FFFF%
O%777%
X%777%


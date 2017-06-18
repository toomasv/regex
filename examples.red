regex/parse "[a-c]+" "abcaaabbbcaab"
;== true
regex/parse/spec "[a-c]+" "abcaaabbbcaab"
; [(cs_1: charset [#"a" - #"c"]) to [some cs_1] copy br_0 thru [some cs_1] (br_/0: br_0) to end]

ip-v4: "\A(25[0-5]|2[0-4]\d|1\d\d|[1-9]\d|\d)(?:\.(?1)){3}\z"
regex/parse/spec ip-v4 "127.0.0.25"
; [(cs_1: charset [#"0" - #"5"] cs_2: charset [#"0" - #"4"] cs_3: charset [#"1" - #"9"] 
;	_1: [#"2" #"5" cs_1 | #"2" cs_2 digit | #"1" digit digit | cs_3 digit | digit]) 
;	copy br_0 thru [copy br_1 _1 (put br_ 1 br_1) 3 [#"." _1]] (br_/0: br_0) end]
regex/parse ip-v4 "255.255.255.256"
;== false

email: "^^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9.-]+$"
regex/spec/parse email "n00b@lost.island.org"
; [(cs_1: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"_" #"." #"+" #"-"] 
;	cs_2: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"-"] 
;	cs_3: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"." #"-"]) 
;	copy br_0 thru [some cs_1 #"@" some cs_2 #"." some cs_3] (br_/0: br_0) opt #"^/" end]

parse "very02.common@email.address.com" regex email
;== true

; named groups, captured strings 
regex/parse/spec "<h1[^^>]*>\n?(?P<title>[^^<\n]*)\n?</h1>" read http://www.red-lang.org/
; [(cs_1: charset [not [#">"]] cs_2: charset [not [#"<" #"^/"]] _title: _1: [any cs_2]) 
;	to [#"<" #"h" #"1" any cs_1 #">" opt #"^/" _1 opt #"^/" #"<" #"/" #"h" #"1" #">"] 
;	copy br_0 thru [#"<" #"h" #"1" any cs_1 #">" opt #"^/" copy br_1 _1 
;		(put br_ 1 br_1 title: br_1 put br_ 'title br_1) opt #"^/" #"<" #"/" #"h" #"1" #">"] 
;	(br_/0: br_0) to end]
;== true
&  ; was br_
;== #(
;	0 "<h1 class='title'>^/Red Programming Language^/</h1>"
;	1 "Red Programming Language"
;	title: "Red Programming Language"
;)
print title
; Red Programming Language

; absolute, relative and named subroutines in Perl syntax
regex/spec/parse "(?+1)(?'name'[abc])(?1)(?-1)(?&name)" "abcab"
; [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) 
; to [_1 _1 _1 _1 _name] 
; copy br_0 thru [_1 copy br_1 _1 (put br_ 1 br_1 name: br_1 put br_ 'name br_1) _1 _1 _name] 
; (br_/0: br_0) to end] 

; same in Ruby syntax
regex/spec/parse "\g'+1'(?'name'[abc])\g'1'\g'-1'\g'name'" "bbccc"
; [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) 
; to [_1 _1 _1 _1 _name] 
; copy br_0 thru [_1 copy br_1 _1 (put br_ 1 br_1 name: br_1 put br_ 'name br_1) _1 _1 _name] 
; (br_/0: br_0) to end]

; PCRE
regex/spec/parse "(?P<name>[abc])(?1)(?P>name)" "bca"
; [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) 
; to [_1 _1 _name] 
; copy br_0 thru [copy br_1 _1 (put br_ 1 br_1 name: br_1 put br_ 'name br_1) _1 _name] 
; (br_/0: br_0) to end]

; comments
regex/spec "abc(?# Simple re )"
; [to [#"a" #"b" #"c"] copy br_0 thru [#"a" #"b" #"c"] (br_/0: br_0) to end]

; freespace demonstration + named group + wordboundary + backreference
; without wordboundary invalid IP addresses like eg 192.186.1.265 will be 
; reckognized as 192.186.1.26
ip-v4: {
(   						# We are capturing the whole address
	?P< ipaddr >				# Let's give it a name
	(					# Begin definition of quad number
		\b (?:					# Quad starts (wordboundary + noncapturing group)
				25[0-5]				# Highest quads			250-255
			|	2[0-4]\d			# Second highest quads 		200-249
			|	1\d\d				# Quads in second hundred 	100-199
			|	[1-9]\d				# Quads 			 10-99
			|	\d				# Quads 			  0-9
		) \b					# Quad ends (nc-group ends + wordboundary)
	)					# End definition of quad number,
						# and check first quad
	(?: 					# Begin non-capturing group
		\.					# check for dot
		(? 2 ) 					# and call quad definition subroutine
	)    					# End group
	{ 3 }					# Iterate last group 3 times 
)
}
regex/parse/freespace ip-v4 "some text 192.168.1.65 around the address"
;== true
ipaddr
;== "192.168.1.65"
regex/parse/freespace ip-v4 "some text 192.168.1.265 around the address"
;== false

; global mode with nested groups
regex/parse/g "(\w(\w{1,2}))\W(\w+)" "per aspera ad astra"
;== true
&  ; was br_
;== #(
;	0 ["per aspera" "ad astra"]
;	2 ["er" "d"]
;	1 ["per" "ad"]
;	3 ["aspera" "astra"]
;)

; global + simplex (non-capturing) modes 
regex/parse/g/n "(\w(\w{1,2}))\W(\w+)" "per aspera ad astra"
;== true
&  ; was br_
;== #(
;	0 ["per aspera" "ad astra"]
;)

; as previous + named groups
regex/parse/g/n "(?<pre>\w(\w{1,2}))\W(?<nom>\w+)" "per aspera ad astra"
;== true
&  ; was br_
;== #(
;	0 ["per aspera" "ad astra"]
;	1 ["per" "ad"]
;	pre: ["per" "ad"]
;	2 ["aspera" "astra"]
;	nom: ["aspera" "astra"]
;)

; different groups with same name
regex/parse/g/n "(?<pre>\w(\w{1,2}))\W(?<pre>\w+)" "per aspera ad astra"
;== true
br_ 
;== #(
;	0 ["per aspera" "ad astra"]
;	1 ["per" "ad"]
;	pre: ["per" "aspera" "ad" "astra"]
;	2 ["aspera" "astra"]
;)

; changing match-map's symbol
re-ctx/symbol: '¤
;== ¤
regex/parse "a(b)c" "xabcx"
¤0
;=="abc"
¤1
;== "b"
¤
;== #(
;	0 "abc"
;	1 "b"
;)
re-ctx/symbol: 'backreference
;== backreference
regex/parse "a(b)c" "xabcx" backreference
;== #(
;	0 "abc"
;	1 "b"
;)
parse "xabcx" regex "a(b)c" backreference
;== #(
;	0 "abc"
;	1 "b"
;)

; replacement of overall match
regex/parse/replace "like" x: "I like Red" "love" head x
;== "I love Red"
; replacement of outer word
var: "trick" regex/parse/replace "abc" x: "xabcx" "\'var'" head x
;== "xtrickx"
var: "track" parse x: "xabcx" regex/replace "abc" "\'var'" head x
;== "xtrackx"

; global replacement of overall match
regex/parse/replace/g "(\w+) (\w+)( ?)" x: "first second third fourth" "\2 \1\3" head x
;== "second first fourth third"

; global replacement2
regex/parse/replace/g "(\w\w)\w+" x: "first second third fourth" "\1" head x
;== "fi se th fo"

; global replacement with block
a: "first" regex/parse/g/replace "\w+" x: "1 second 2" [a "\0" "third"] head x
;== "first second third"

; soft quantifiers
regex/parse/g "\w*s" "boys and girls" &/0
;== ["boys" "girls"]
; compare possessive
regex/parse/g "\w*+s" "boys and girls" &/0
;== []
; greedy
regex/parse ".*s" "boys and girls" &/0
;== "boys and girls"
; lazy
regex/parse ".*?s" "boys and girls" &/0
;== "boys"




; lookahead:
; negative
regex/parse "q(?!u)" "Iraqis"
;== true
regex/parse "q(?!u)" "Iroquois"
;== false

; positive
regex/parse "q(?=u)" "Iraqis"
;== false
regex/parse "q(?=u)" "Iroquois"
;== true

; lookbehind:
; positive
regex/parse "(?<=d)i.*" "tiandi" &/0
;== "i"
regex/parse "(?<=t)i.*" "tiandi" &/0
;== "iandi"

; negative
regex/parse "(?<!d)i.*" "tiandi" &/0
;== "iandi"
regex/parse "(?<!t)i.*" "tiandi" &/0
;== "i"

; charclass subtraction (find consonants in string)
regex/parse/g "[[:alpha:]-[aeiou]]" "bereshit bara elohim et hashamayim ve'et ha'arets" rejoin unique sort &/0
;== "bhlmrstvy"

; charclass intersection
regex/parse/g "[1234&&[3456]]" "0123456789" &/0
;== ["3" "4"]
regex/parse/g "[1234&&[^^3456]]" "0123456789" &/0
;== ["1" "2"]
regex/parse/g "[^^1234&&[3456]]" "0123456789" &/0
;== ["5" "6"]
; NB! Instead of e.g.
regex/parse/g "[\d&&[^^3456]]" "0123456789" &/0
;== ["0" "1" "2" "7"] <-- not what you expect
; use rather
regex/parse/g "[\d-[3456]]" "0123456789" &/0
;== ["0" "1" "2" "7" "8" "9"]

Red [
	Author: 	"Toomas Vooglaid"
	file: 		"%regex~.red"
	Purpose: 	{Perl-like regex for Red}
	History:	{
		Started: 	2017-05-30 from %regex.red
		2017-05-31	Improved pattern. Now simple matches with no modes can be done wo delimiters.
				eg. "xyzabc" ~ "abc"
	}
]

re-ctx: make reactor! [
	_spec: 			clear []
	spec: 			off
	starting: 		'loose
	ending: 		'loose
	nest-level:		0
	sl: ml: 		off
	nocase: 		off
	freespace: 		off
	glob: 			off
	simp:			off
	debugging: 		off
	longout: 		clear []
	shortout:		clear []
	rpt: seq: 		clear []
	levelgrp: 		clear []
	levelgr2:		clear []
	levelcap: 		clear []
	levelsym: 		clear []
	levelnam: 		clear []
	capturing: 		off
	to-short: 		off
	symbol:			'&
	brsymb:			is [to-string symbol]
	full-match:		is [to-word append copy brsymb 0]
	sbrsymb1:		is [append copy "_" brsymb]
	sbrsymb2:		is [append copy "_'" brsymb]
	defs: 			clear []
	sym:			none
	sbrdef1:		none
	sbrdef2:		none
	bckref: 		none
	assignments: 		clear []
	replace:		off
	replacement:		none
	br-num: 		0
	
	next-br: does [br-num: br-num + 1]
	
	cs-num: 0 
	empty-cs?: false
	cs-open?: false
	next-cs: does [to-word append copy "cs_" cs-num: cs-num + 1] 									; charset-number-word generator
		
	make-charset: func [s /local c e s1 s2 negated cs rpt cb][									; cb -- charset definition; e -- continuation string
		c: copy [] rpt: none													; c -- whole charset expression; rpt -- quantifier
		cs-open?: true
		if negated: to-logic find/match s #"^^"	[s: next s]									; if charset is negated, jump over ^
		s1: index? s														; register position after possible ^
		system/words/parse s [													; let's form the charset
			collect set cb some [												; collect charset definitional elements
				 #"]" s2: [												; register current position after ]
					if (s1 < ((index? s2) - 1))	[								; if ] is not in the beginning of charclass
						any #"]" opt [collect set rpt repeater]							; we are in the end of charset, check for quantifiers, register, exit
						(cs-open?: false) e: break								; register closing of charset definition
					] | [ 
						if (not empty-cs?) [keep (#"]")] 							; if empty charsets are not allowed, then keep ] as part of charset definition
						| (cs-open?: false) e: break								; otherwise, register closing, position and exit
					]
				]  											
				| "-]" keep (#"-") 											; if - occurs before closing ]
					   any #"]" opt [collect set rpt repeater] 				        		; collect quantifier, if any
					   (cs-open?: false) e: break							                ; declare charclass closed, register position, go back
				| "^^" keep (#"^^")											; we can keep ^, because its negation meaning is taken care of
				| ["\n" keep (#"^/") | "\r" keep (#"^M")]								; keep linebreakers
				| #"\" [
					ahead 	[clmetaset | metaset] 									; metachars may be escaped 
					keep 	[clmetaset | metaset] 							
					| (cause-error 'user 'message ["Unescaped \ in char-class!"])					; only one, which sould necessarily be escaped
				]					
				| #"-" s2: [if (s1 = ((index? s2) - 1)) keep (#"-")							; we have just a dash
					| keep ('-) ]											; we have a range
				| keep clliteral 											; keep more or less anything
				| (cause-error 'user 'message ["Malformed charset?"])							; just for checking
			]
		]
		cs: next-cs														; new word to bind charset to
		if (cs-open? and empty? e) [
			cause-error 'user 'message ["Character class unclosed!"]
		]
		if to-logic negated [cb: compose/deep [not [(cb)]]]									; if charset is negated, make negation
		append defs copy compose/deep [ 
			(to-set-word cs) charset [(cb)]
		] 															; escape parse to actually make a charset
		if rpt [append c either block? rpt/1 [rpt/1][rpt]]									; did we have quantifiers? Append them here
		append c cs 														; and finally a word referring to created charset
		compose/deep [[(c)] (e)]												; send proudly back charset def and remaining string after charset
	]
	group: 	[if (not cs-open?) [
		#"(" 															; named group
		[	"?P<" 	copy gname to #">" 
		| 	"?'" 	copy gname to #"'" 
		| 	"?<" 	copy gname to #">"
		] skip
		(
			insert levelcap capturing
			capturing: on
			if 1 < length? levelcap [to-short: on]
			insert levelnam copy gname
			insert levelsym next-br
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout
			if debugging [probe compose ["lgrp:" (levelgrp)]]
			longout:	clear []
			shortout: 	clear []
		)
	|	[ #"("  "?+" 	copy n number  #")"											; relative subroutine (forward)
		| "\g"  
			[	"<+" 	copy n number  #">"
			| 	"'+" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 br-num + to-integer n)
	|	[ #"("  "?-" 	copy n number  #")"											; relative subroutine (backward)
		| "\g"  
			[	"<-" 	copy n number  #">"
			| 	"'-" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 br-num + 1 - to-integer n)
	|	[ #"("  #"?" 	 copy n number  #")" 											; absolute subroutine reference
		| "\g"  
			[	#"<" 	 copy n number  #">"
			| 	#"'" 	 copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 n)
	|	[ #"("  														; named subroutine reference
			[	"?&" 	copy gname to #")" 
			| 	"?P>" 	copy gname to  #")"]
		| "\g"  
			[	#"<" 	copy gname to  #">"
			| 	#"'" 	copy gname to  #"'"]
		] skip 
		keep (to-word append copy "_" gname)
	|	"(?:" (															; non-capturing group
			insert levelcap capturing 
			capturing: off
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout 
			longout: 	clear []
			shortout: 	clear []
		) 
	|	#"(" ( 															; capturing group
			insert levelcap capturing
			either simp [													; unless simplex
				capturing: off
			][
				capturing: on
				if 1 < length? levelcap [to-short: on]
				insert levelsym next-br 
				insert levelnam none 
			]
			insert/only levelgrp copy longout
			insert/only levelgr2 copy shortout			
			if debugging [probe compose ["lgrp:" (levelgrp)]]
			longout: 	clear []
			shortout: 	clear []
		)
			| 	#")"  opt collect set rpt repeater 									; end of group, check for quantifier
		(
			either capturing [												; are we capturing?
				all [
					nam: take levelnam 
					append defs to-set-word copy append copy "_" nam
					nam: to-word copy nam
				] 
				sym: take levelsym 
				sbrdef1: to-word append copy sbrsymb1 sym
				sbrdef2: to-word append either to-short [copy sbrsymb2][copy sbrsymb1] sym
				bckref: to-word append copy brsymb sym
				append/only append defs to-set-word sbrdef1 copy longout 
				all [
					starting = 'loose 
					to-short
					append/only append defs to-set-word sbrdef2 copy shortout 
				]
				if debugging [probe compose ["defs:" (defs)]]
				if block? first rpt [rpt: first rpt]
				longout: append append take levelgrp rpt compose [copy (bckref) (sbrdef1)]
				shortout: append append take levelgr2 rpt sbrdef2
				assignments: make block! 5
				either glob [
					unless block? select get symbol sym compose [extend (symbol) reduce [sym make block! 5]]
					append assignments compose [append select (symbol) (sym) (bckref)]
					if nam [
						unless block? select get symbol nam compose [extend (symbol) reduce [nam make block! 5]]
						append assignments compose [
							(to-set-word nam) (bckref) 
							append select (symbol) (to-lit-word nam) (bckref)
						]
					]
				][
					append assignments compose [put (symbol) (sym) (bckref)]
					if nam [
						append assignments compose [
							(to-set-word nam) (bckref) 
							put (symbol) (to-lit-word nam) (bckref)
						]
					]
				]
				append/only longout to-paren assignments
				if debugging [probe compose ["lout:" (longout)]]
			][														; we are not capturing
				longout: 	append/only append take levelgrp rpt copy longout
				shortout: 	append/only append take levelgr2 rpt copy shortout
			]
			capturing: take levelcap
		) 
	| 	keep literal
	]]
	class: [
		"[:" copy cl to ":]" 2 skip keep (to-word cl)										; this is in wrong place?
	| 	#"[" s: 
		(
			set [c e] make-charset s 											; send string to the charset-factory
			append longout c												; put the charset into place
			append shortout c
			e: any [find/last s e tail s]
		)
		:e															; continue after the charclass
	]
	special: [
		  "\d" 		keep ('digit)
		| "\D"		keep ('nondigit)
		| "\w" 		keep ('word)
		| "\W"		keep ('nonword)
		| "\s" 		keep ('wspace)
		| "\S"		keep ('nonwspace)
		| "\t"		keep (#"^-")
		| "\n"		keep (#"^/")
		| "\N"		keep ('nonlinebreak)
		| "\r"		keep (#"^M")
		| "\f"		keep (#"^L")
		| "\v"		keep (#"^K")
	]
	backref: [
		[ #"("	"?P=" 	copy gname to #")" 									              	; reference to named captured string
		| "\k"  
			[	#"'" 	copy gname to #"'" 
			| 	#"<" 	copy gname to #">" 
			| 	#"{" 	copy gname to #"}"]
		] skip 
		keep (to-word gname)
	|	#"\" copy n number 
		keep (to-word append copy brsymb n)
	]
	replref: [#"\" [
		[	#"'" 	copy n number #"'" 
		|	#"<" 	copy n number #">" 
		|	#"^{" 	copy n number #"^}" 
		] 	keep (to-word append copy brsymb n)
	|	[	"'" 	copy gname to #"'" 
		|	#"<" 	copy gname to #">"
		|	#"^{"	copy gname to #"^}"
		]]
		skip 
		keep (to-word gname)
	]
	linestart: 		is [either ml [[#"^^" keep (#"^/")]][[#"^^" (starting: 'strict)]]]
	lineend:		is [either ml [[#"$" keep ([ahead [opt #"^/" end | #"^/"]])]][[#"$" keep ([opt #"^/" end])]]] 
	wordboundary:	["\b" keep (												                ; does not react on changing word / nonword values
		[s: [
			if ((1 = index? s) or find nonword first back s) 	[ahead word] 
			| if (find word first back s) 				[ahead [nonword | end]]
		]]
	)]
	anchor:			[linestart | lineend | wordboundary]
	paren: 			charset "()"
	square: 		charset "[]"
	lower: 			charset [#"a" - #"z" #"ß" - #"ö" #"ø" - #"ÿ"]
	upper: 			charset [#"A" - #"Z" #"À" - #"Ö" #"Ø" - #"Þ"]
	alpha: 			is [union lower upper]
	digit: 			charset "0123456789"
	nondigit:		complement digit
	number:			[some digit]
	alnum:			is [union alpha digit]
	word:			is [union alnum charset "_"]
	nonword:		is [complement word]
	blank:			charset [#" " #"^-"]
	nonblank:		complement blank
	linebreak:		charset [#"^M" #"^/" #"^L"]
	nonlinebreak:		is [complement linebreak]
	wspace: 		is [union blank linebreak] 
	nonwspace:		is [complement wspace]
	punct:			charset ",;!^"':-" ;"
	nonpunct: 		is [complement punct]
	meta: 			[#"\" #"^^" #"$"  #"." #"|" #"?" #"*" #"+" #"(" #")" #"[" #"^{"] 
	metaset: 		charset meta
	literal: 		is [complement metaset]
	clmeta:			[#"\" #"^^" #"-" #"]"]
	clmetas:		[#"\" | #"^^" | #"-" | #"]"]
	clmetaset:		charset clmeta
	clliteral:		is [complement clmetaset]
	closing-paren:		charset [not #")"]
	not-closing-paren: 	complement closing-paren
	
	comm: 			["(?#" thru #")"]
	anychar: is [either sl [
		union metaset literal
	][
		exclude union metaset literal charset [#"^/" #"^M"]]
	]
	escaped: [
		#"\" keep metaset 
	| 	"$$" keep #"$" 
	]
	altern: 		[#"|" keep ('|)]
	char: 			[keep literal]
	exception: [
		  	#"\" 	(cause-error 'user 'message ["Unescaped \!"])
		| 	#")" 	(cause-error 'user 'message ["Invalid use of closing parentheses!"])
	]
	rmfree: [any [
			remove [any wspace #"#" thru linebreak]							              ; comments
		| 	change "\ " " "
		|	"\[" | "\]"
		|	#"[" (cs-open?: yes)
		|	#"]" (cs-open?: no)
		|	if (not cs-open?) remove some wspace
		| 	skip
	]]
	sequence: [
			escaped
		| 	anchor
		| 	altern
		| 	comm
		| 	backref
		| 	group
		| 	class
		| 	special
		| 	#"." 	keep ('anychar)  
		| 	char 
		| 	exception
	]
	repeater: [
			#"^{"  copy n1 number  #","  copy n2 number  #"^}" 
										keep (reduce [to-integer n1 to-integer n2])
		| 	#"^{"  #","  copy n2 number  #"^}" 			keep (reduce [0  to-integer n2])
		| 	#"^{"  copy n1 number  #","  "^}"			keep (reduce [to-integer n1 10000])
		| 	#"^{"  copy n1 number  #"^}"				keep (to-integer n1)
		| 	#"?" 							keep ('opt)
		| 	#"+" 							keep ('some)
		| 	#"*" 							keep ('any) 
	]
	build: func [inner /local s e c t r n mp][				                    ; main workhorse
		longout: 	clear []
		shortout: 	clear []
		system/words/parse/case inner [
			any [
				collect set seq sequence  
				opt collect set rpt repeater
				(
					if block? first rpt [rpt: first rpt] 
					if debugging [probe compose ["seq rpt:" (seq) (rpt)]]
					append longout 	compose [(rpt) (seq)] 
					append shortout compose [(rpt) (seq)]
					if debugging [probe compose/deep ["short long:" [(shortout)] [(longout)]]]
				) 											 
			]
		]
		compose/deep [[(shortout)] [(longout)]]
	]

	finish: func [inner /local short long repl][
		set [short long] build copy inner 
		append _spec switch starting [
			strict [compose/deep [
				copy (full-match) [(long)] 
				(to-paren compose [put (symbol) 0 (full-match)])
			]] 
			loose [either glob [
				either replace [
					system/words/parse replacement [collect set repl any [replref | backref | char]]
					if debugging [probe compose/deep ["repl:" [(repl)]]]
					compose/deep [
						some [
							to [(short)] s: copy (full-match) thru [(long)] 
							(to-paren compose [append select (symbol) 0 (full-match)])
							:s change [(short)] (to-paren append/only copy [rejoin] compose [(repl)])
						]
					]
				][
					compose/deep [
						some [to [(short)] copy (full-match) thru [(long)] 
						(to-paren compose [append select (symbol) 0 (full-match)])]
					]
				]
			][
				either replace [
					system/words/parse replacement [collect set repl any [replref | backref | char]]
					if debugging [probe compose/deep ["repl:" [(repl)]]]
					compose/deep [
						to [(short)] s: copy (full-match) thru [(long)] 
						(to-paren compose [put (symbol) 0 (full-match)]) 
						:s change [(short)] (to-paren append/only copy [rejoin] compose [(repl)])
					]
				][
					compose/deep [
						to [(short)] copy (full-match) thru [(long)] 
						(to-paren compose [put (symbol) 0 (full-match)])
					]
				]
			]]
		]
		append _spec switch ending [
			strict 			[[opt #"^/" end]] 
			strictissima 	['end]
			loose 			[[to end]]
		]
		if debugging [probe compose/deep ["_spec:" [(_spec)]]]
	]
	
	regex: func [
		"Perl-like regex for Red"
		str 	[string!]		 						"string to parse"
		rex 	[string!]  								"regular expression"
		/local 	inner basemode delim re modes mode
	][  
		cs-num:   			0
		br-num:				0
		_spec:				make block! 100
		inner: 				clear ""
		self/replace:			any [replace off]
		self/replacement:		any [replacement none]
		empty-cs?: 			false
		cs-open?: 			false
		levelgrp: 			make block! 5
		levelgr2:			make block! 5
		levelcap: 			make block! 5
		levelsym: 			make block! 5
		levelnam: 			make block! 5
		capturing: 			off
		to-short: 			off
		defs: 				make block! 5
		
		mode: charset "imsxgn"
		parse rex [
			[
				copy basemode opt [#"m" | #"s"]
				copy delim skip
				copy re to delim skip
				[if (basemode = "s") copy replacement to delim skip | none]
				copy modes any mode end
			] 
			| 	copy re to end
		]
		
		nocase:		any [find modes "i" off]
		self/ml: 	any [find modes "m" off]
		self/sl: 	any [find modes "s" off]
		freesp: 	any [find modes "x" off]
		glob:		any [find modes "g" off]
		simp:		any [find modes "n" off]
		
		set symbol either glob [
			make map! reduce [0 make block! 10]
		][
			make map! reduce [0 make string! 20]
		]
				
		if replacement 		[
			self/replace: 		on 
			self/replacement: 	replacement
		]
		
		if freesp [system/words/parse re rmfree cs-open?: no]
		
		system/words/parse re [
			[
				  ["\A" | "\`" | if (not ml) ["^^"]]					(starting: 'strict)
				| 									(starting: 'loose)
			]
			copy inner [
				to [
					#"\" copy s skip 
					if (find [39 90] to-integer to-char s) end 			;#"Z" #"'"
				| 	if (not ml) #"$" end
				]									(ending: 'strict) 
			| 	to [
					#"\" copy s skip 
					if (122 = to-integer to-char s) end
				]									(ending: 'strictissima) ;#"z" 
			| 	to end									(ending: 'loose)
			] 
			(finish copy inner)
		]
		_spec: either empty? defs [_spec][head insert/only _spec to-paren defs]
		bind _spec: load mold _spec re-ctx							; rebinding corrects some strange behavior
		if spec [print mold _spec]
		return either nocase [system/words/parse str _spec][system/words/parse/case str _spec]
	]
	set '~ make op! :regex
]
examples: :comment 
examples [
"abcaaabbbcaab" ~ "/[a-c]+/" 

ip-v4: "/\A(25[0-5]|2[0-4]\d|1\d\d|[1-9]\d|\d)(?:\.(?1)){3}\z/"
"127.0.0.25" ~ ip-v4 

"255.255.255.256" ~ ip-v4
;== false

email: "|^^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9.-]+$|"
"n00b@lost.island.org" ~ email

; named groups, captured strings 
(read http://www.red-lang.org/) ~ "|<h1[^^>]*>\n?(?P<title>[^^<\n]*)\n?</h1>|"
&
;== #(
;	0 "<h1 class='title'>^/Red Programming Language^/</h1>"
;	1 "Red Programming Language"
;	title: "Red Programming Language"
;)
print title
;Red Programming Language

; absolute, relative and named subroutines in Perl syntax
"abcab" ~ "/(?+1)(?'name'[abc])(?1)(?-1)(?&name)/"

; same in Ruby syntax
"bbccc" ~ "/\g'+1'(?'name'[abc])\g'1'\g'-1'\g'name'/"

; PCRE
"bca" ~ "|(?P<name>[abc])(?1)(?P>name)|"

; comments
"abc" ~ ":abc(?# Simple re ):"

; freespace demonstration + named group + wordboundary + backreference
; without wordboundary invalid IP addresses like eg 192.186.1.265 will be 
; reckognized as 192.186.1.26
ip-v4: {@
(   					# We are capturing the whole address
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
@x}						{# Put the freespace mode switch in the end}
"some text 192.168.1.65 around the address" ~ ip-v4
ipaddr
;== "192.168.1.65"
regex/parse/freespace ip-v4 "some text 192.168.1.265 around the address"
;== false

; global mode with nested groups
"per aspera ad astra" ~ "/(\w(\w{1,2}))\W(\w+)/g"
&
;== #(
;	0 ["per aspera" "ad astra"]
;	2 ["er" "d"]
;	1 ["per" "ad"]
;	3 ["aspera" "astra"]
;)

; global + simplex (non-capturing) modes 
"per aspera ad astra" ~ "/(\w(\w{1,2}))\W(\w+)/gn"
== true
>> br_
== #(
	0 ["per aspera" "ad astra"]
)

; as previous + named groups
>> regex/parse/g/n "(?<pre>\w(\w{1,2}))\W(?<nom>\w+)" "per aspera ad astra"
&
;== #(
;	0 ["per aspera" "ad astra"]
;	1 ["per" "ad"]
;	pre: ["per" "ad"]
;	2 ["aspera" "astra"]
;	nom: ["aspera" "astra"]
;)

; different groups with same name
"per aspera ad astra" ~ "/(?<pre>\w(\w{1,2}))\W(?<pre>\w+)/gn"
&
;== #(
;	0 ["per aspera" "ad astra"]
;	1 ["per" "ad"]
;	pre: ["per" "aspera" "ad" "astra"]
;	2 ["aspera" "astra"]
;)

; replacement of overall match
(x: "I like Red") ~ "s/ik/ov/" head x
;== "I love Red"

; replacement of outer word
var: "trick" (x: "xabcx") ~ "s/abc/\'var'/" head x
== "xtrickx"

;global replacement of overall match
(x: "first second third fourth") ~ "s|(\w+) (\w+)( ?)|\2 \1\3|g" head x
;== "second first fourth third"
]

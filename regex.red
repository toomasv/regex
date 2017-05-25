Red [
	Author: 	"Toomas Vooglaid"
	file: 		"%regex.red"
	Purpose: 	{Simple regex to parse converter}
	History:	{Started: 2017-05-09
				v.0.1: 2017-05-12 -- first working version with: 
					start `^^` and end `$` anchors, 
					non-capturing groups (one level) with alterns, 
					quantifiers (possessive), 
					some character-classes.
				v.0.2: 2017-05-15 -- 
					hack to avoid recursive calls, got nested groups,
					added some wordclasses
				v.0.3: 2017-05-16 --
					made re-ctx reactive, 
					implemented singleline and multiline refinements,
					wordboundaries,
					added some special characters
				2017-05-17 --
					added negative and positive on-spot charclasses, without subtraction
				v.0.4: 2017-05-18 --
					noncapturing groups (just correct syntax)
					added capturing groups with backreferences to captured strings (\1, \2...)
				2017-05-19 -- restructured the output: by default `regex` now just takes `re` and returns the spec block. 
					With refinement `/string str` string to be parsed by regex may be provided. In this 
					case spec block is created and provided string parsed with this spec. 
					With refinement `/withspec` spec block is also printed out. 
					`/case` refinement still there, as well as 
					`/singleline` and `/multiline`.
					Also added primitive `/debug` refinement, which shows collected quantifiers and sequences on each step, 
					and preliminary `/try flavor` refinement, which will be used to try out major flavors of `re`s.
				2017-05-20 -- changed `/string str` refinement into `/parse str` 
					and `/withspec` refinement into `spec`
				2017-05-22 -- corrected groups nesting and backreferences, compacted code
				v.0.5: 2017-05-23 --
					added soubroutines with several syntactic flavors (PCRE, Perl, Ruby):
					a) named groups declaration
						(?P<name>regex), (?'name'regex), (?<name>regex)
					b) referring subroutines
						absolute reference
						(?1), \g'1', \g<1> - number referring to the capturing group (see example with ip-v4)
						relative reference
						(?+1), (?-2), \g'+3', \g<-4>
						named reference
						(?&name), (?P>name), \g'name', \g<name>
					added backreferences with several syntactic flavors:
					a) numbered backreferences
						\1, \2, ...
					b) named backreferences to captured strings
						(?P=name), \k'name', \k<name>, \k{name} 
					added some examples
				2017-05-23 -- made regex case-sensitive by default, 
					changed `/case` refinement into `/icase` to turn it case-insensitive
					added inline comments syntax - (?# Comment )
				2017-05-24 -- added `/freespace` refinement - see corresponding example
					
			}
	TBD: 		{atomic groups, "soft" quantifiers, character-class subtraction, switches, substitution, look-around etc
				inline created words are leaking into global environment}
]

re-ctx: make reactor! [
	_spec: 		clear []
	starting: 	'loose
	ending: 	'loose
	nest-level: 0
	sl: ml: off
	bebugging: off
	out: clear []
	rpt: seq: clear []
	levelgrp: clear []
	levelcap: clear []
	levelsym: clear []
	levelnam: clear []
	capturing: off
	defs: copy []
	freespace: off
	br-num: 0
	next-br: does [compose 									; backreference-identifier generator
		[(to-word append copy "_" br-num: br-num + 1) 
		 (to-word append copy "br_" br-num)]
	] 								
	cs-num: 0 
	empty-cs?: false
	cs-open?: false
	next-cs: does [to-word append copy "cs_" cs-num: cs-num + 1] 				; charset-number-word generator
		
	make-charset: func [s /local c e s1 s2 negated cs rpt g cb][				; cb -- charset definition; e -- continuation string
		c: copy [] rpt: none								; c -- whole charset expression; rpt -- quantifier
		cs-open?: true
		if negated: to-logic find/match s #"^^"	[s: next s]				; if charset is negated, jump over ^
		s1: index? s														; register position after possible ^
		system/words/parse s [								; let's form the charset
			collect set cb some [							; collect charset definitional elements
				 #"]" s2: [													; register current position after ]
					if (s1 < ((index? s2) - 1))	[			; if ] is not in the beginning of charclass
						any #"]" opt [collect set rpt repeater]		; we are in the end of charset, check for quantifiers, register, exit
						(cs-open?: false) e: break			; register closing of charset definition
					] | [ 
						if (not empty-cs?) [keep (#"]")] 		; if empty charsets are not allowed, then keep ] as part of charset definition
						| (cs-open?: false) e: break			; otherwise, register closing, position and exit
					]
				]  											
				| "-]" keep (#"-") 											; if - occurs before closing ]
					   any #"]" opt [collect set rpt repeater] 		; collect quantifier, if any
					   (cs-open?: false) e: break				; declare charclass closed, register position, go back
				| "^^" keep (#"^^")											; we can keep ^, because its negation meaning is taken care of
				| ["\n" keep (#"^/") | "\r" keep (#"^M")]			; keep linebreakers
				| #"\" [
					ahead 	[clmetaset | metaset] 				; metachars may be escaped 
					keep 	[clmetaset | metaset] 							
					| (cause-error 'user 'message ["Unescaped \!"])		; only one, which sould necessarily be escaped
				]					
				| #"-" s2: [if (s1 = ((index? s2) - 1)) keep (#"-")		; we have just a dash
					| keep ('-) ]											; we have a range
				| keep clliteral ;(print "ha")					; keep more or less anything
				| (cause-error 'user 'message ["Malformed charset?"])		; just for checking
			]
		]
		cs: next-cs															; new word to bind charset to
		if (cs-open? and empty? e) [
			cause-error 'user 'message ["Character class unclosed!"]
		]
		if to-logic negated [cb: compose/deep [not [(cb)]]]				; if charset is negated, make negation
		append defs copy compose/deep [ 
			(to-set-word cs) charset [(cb)]
		] 																	; escape parse to actually make a charset
		if rpt [append c either block? rpt/1 [rpt/1][rpt]]				; did we have quantifiers? Append them here
		append c cs 														; and finally a word referring to created charset
		compose/deep [[(c)] (e)]							; send proudly back charset def and remaining string after charset
	]
	frsp: 	[if (freesp) [some wspace] | ]
	group: 	[if (not cs-open?) [
		#"(" frsp
		[	"?P<" 	copy gname to #">" 
		| 	"?'" 	copy gname to #"'" 
		| 	"?<" 	copy gname to #">"
		] skip
		(
			insert levelcap capturing
			capturing: on
			insert levelnam copy gname
			insert/only levelsym next-br
			insert/only levelgrp copy out 
			(if debugging [probe compose ["lgrp:" (levelgrp)]])
			out: clear []
		)
	|	[ #"(" frsp #"?" frsp #"+" 	copy n number frsp #")"
		| "\g" frsp 
			[	#"<" frsp #"+" 	copy n number frsp #">"
			| 	#"'" frsp #"+" 	copy n number frsp #"'"]
		] keep (to-word append copy "_" br-num + to-integer n)
	|	[ #"(" frsp #"?" frsp #"-" 	copy n number frsp #")"
		| "\g" frsp 
			[	#"<" frsp #"-" 	copy n number frsp #">"
			| 	#"'" frsp #"-" 	copy n number frsp #"'"]
		] keep (to-word append copy "_" br-num + 1 - to-integer n)
	|	[ #"(" frsp "?" 	frsp copy n number frsp #")" 
		| "\g" frsp 
			[	"<" 	frsp copy n number frsp #">"
			| 	"'" 	frsp copy n number frsp #"'"]
		] keep (to-word append copy "_" n)
	|	[ #"(" frsp 
			[	"?&" 	copy gname to #")" 
			| 	"?P>" 	copy gname to  #")"]
		| "\g" frsp 
			[	"<" 	copy gname to  #">"
			| 	"'" 	copy gname to  #"'"]
		] skip 
		keep (to-word append copy "_" gname)
	|	[ #"(" frsp "?P=" 	copy gname to #")" 
		| "\k" frsp 
			[	"'" 	copy gname to #"'" 
			| 	"<" 	copy gname to #">" 
			| 	"{" 	copy gname to #"}"]
		] skip 
		keep (to-word gname)
	|	#"(" frsp "?:" (
			insert levelcap capturing 
			capturing: off
			insert/only levelgrp copy out 
			out: clear []
		) 
	|	#"(" (
			insert levelcap capturing
			capturing: on
			insert levelnam none
			insert/only levelsym next-br
			insert/only levelgrp copy out 
			(if debugging [probe compose ["lgrp:" (levelgrp)]])
			out: clear []
		)
	| 	#")" opt free opt collect set rpt repeater 
		(
			either capturing [
				if nam: take levelnam [nam: to-word copy nam]
				sym: take levelsym 
				if nam [append defs to-set-word copy append copy "_" to-lit-word nam]
				append/only append defs to-set-word sym/1 copy out 
				if debugging [probe compose ["defs:" (defs)]]
				if block? first rpt [rpt: first rpt]
				out: append append take levelgrp rpt compose [copy (sym/2) (sym/1)] 
				if nam [append/only out to-paren compose [(to-set-word nam) (sym/2)]]
				if debugging [probe compose ["out:" (out)]]
			][
				out: append/only append take levelgrp rpt copy out
			]
			capturing: take levelcap
		) 
	| 	keep literal
	]]
	class: [
		"[:" copy cl to ":]" 2 skip keep (to-word cl)										; this is on wrong place?
	| 	#"[" s: 
		(
			set [c e] make-charset s 														; send string to the charset-factory
			append out c								; put the charset into place
			e: any [find/last s e tail s]
		)
		:e										; continue after the charclass
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
		#"\" copy n number keep (to-word append copy "br_" n)
	]
	linestart: 		is [either ml [[#"^^" keep (#"^/")]][[#"^^" (starting: 'strict)]]]
	lineend:		is [either ml [[#"$" keep ([ahead [opt #"^/" end | #"^/"]])]][[#"$" keep ([opt #"^/" end])]]] 
	wordboundary:	["\b" keep (										; does not react on changing word / nonword values
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
	nonlinebreak:	is [complement linebreak]
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
	closing-paren:	charset [not #")"]
	not-closing-paren: complement closing-paren
	
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
	exception: 		[
		  #"\" 		(cause-error 'user 'message ["Unescaped \!"])
		| #")" 		(print "Warning! Invalid use of closing parentheses.")
	]
	free: [if (freesp) [
			any wspace #"#" thru linebreak				; comments
		|	#"(" any wspace s: [
				#"?" some wspace [#":" | #">" | #"<" | #"'" | #"&" | #"P"] 
			| 	"?P" some wspace [#"<" | #">" | #"="]
			] 
			(cause-error 'user 'message to-block mold rejoin [
				"Illegal free space in group reference: " mold copy/part s 10
			])
		| 	"\ " keep (#" ")
		|	if (not cs-open?) [some wspace]
	]]
	
	sequence: [
		escaped
		| free
		| anchor
		| altern
		| comm
		| group
		| class
		| special
		| backref
		| #"." 		keep ('anychar)
		| char 
		| exception
	]
	repeater: [frsp [
		  #"^{" frsp copy n1 number frsp #"," frsp copy n2 number frsp #"^}" 
									keep (reduce [to-integer n1 to-integer n2])
		| #"^{" frsp #"," frsp copy n2 number frsp #"^}" 	keep (reduce [0  to-integer n2])
		| #"^{" frsp copy n1 number frsp #"," frsp "^}"		keep (reduce [to-integer n1 10000])
		| #"^{" frsp copy n1 number frsp #"^}"			keep (to-integer n1)
		| #"?" 							keep ('opt)
		| #"+" 							keep ('some)
		| #"*" 							keep ('any) 
	]]
	build: func [inner /local s e c t r n mp][				; main workhorse
		out: clear []
		system/words/parse/case inner [
			any [
				collect set seq sequence 
				opt collect set rpt repeater
				(if block? first rpt [rpt: first rpt] 
				append out compose [(rpt) (seq)]) 
			]
		]
		out
	]

	finish: func [inner /local middle][
		middle: build copy inner
		append _spec switch starting [
			strict 			[middle] 
			loose 			[compose/deep [thru [(middle)]]]
		]
		append _spec switch ending [
			strict 			[[opt #"^/" end]] 
			strictissima 	['end]
			loose 			[[to end]]
		]
	]
	flavors: [JS [empty-cs?: true]]								; just a probe so far
	rm-wspace: [(print "hi")
		any [remove wspace]
	]
	
	set 'regex func [
		"Regex to parse converter"
		re [string!]  
		/parse str [string!] 								"string to parse"
		/icase 										"turns on case-insensitivity"
		/debug										"turns on debugging"
		/spec 										"prints out generated spec"
		/multiline 									"lets ^^ and $ match beginning and end of line also"
		/singleline 									"lets dot match linebreaks also"
		/try "try specific flavor of regexp" flavor [lit-word!] "flavor to try"
		/freespace
		/local inner
	][  
		debugging: 	any [debug off]
		lazy-num:	0 lazy: off
		cs-num:   	0
		br-num:		0
		_spec:		clear []
		inner: 		clear ""
		self/ml: 	any [multiline off]
		self/sl: 	any [singleline off]
		empty-cs?: 	false
		cs-open?: 	false
		levelgrp: 	clear []
		levelcap: 	clear []
		levelsym: 	clear []
		levelnam: 	clear []
		capturing: 	off
		defs: 		copy []
		freesp: 	any [freespace off]

		if try [unless do select flavors flavor [print append to-string flavor " is not supported :("]]
		
		system/words/parse re [
			[frsp [
				  ["\A" | "\`" | if (not ml) ["^^"]]				(starting: 'strict)
				| 								(starting: 'loose)
			]]
			copy inner [
				  to [#"\" copy s skip 
					if (find [39 90] to-integer to-char s) [opt free end] 	;#"Z" #"'"
					| if (not ml) [#"$" opt free end]]			(ending: 'strict) 
				| to [#"\" copy s skip 
					if (122 = to-integer to-char s) [opt free end]]		(ending: 'strictissima) ;#"z"
				| to [opt free end]						(ending: 'loose)
			] 
			(finish copy inner)
		]
		_spec: either empty? defs [_spec][head insert/only _spec to-paren defs]
		bind _spec: load mold _spec re-ctx						; hack to get nesting groups to work 
												; rebinding removes some strange references
		if spec [print mold _spec]
		either parse [
			return either icase [system/words/parse str _spec][system/words/parse/case str _spec]
		][
			return _spec
		]
	]
]
examples: :comment 
examples [
	regex/parse "[a-c]+" "abcaaabbbcaab"
	regex/parse/spec "[a-c]+" "abcaaabbbcaab"
	=> [thru [(cs_1: charset [#"a" - #"c"]) some cs_1] to end]
	
	ip-v4: "\A(25[0-5]|2[0-4]\d|1\d\d|[1-9]\d|\d)(?:\.(?1)){3}\z"
	regex/parse/spec ip-v4 "127.0.0.25"
	=> [(_1: [#"2" #"5" [(cs_1: charset [#"0" - #"5"]) cs_1] 
			| #"2" [(cs_2: charset [#"0" - #"4"]) cs_2] digit 
			| #"1" digit digit 
			| [(cs_3: charset [#"1" - #"9"]) cs_3] digit 
			| digit]) copy br_1 _1 3 [#"." _1] end]
	regex/parse ip-v4 "255.255.255.255"

	email: "^^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9.-]+$"
	regex/spec/parse email "n00b@lost.island.org"
	=> [[(cs_1: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"_" #"." #"+" #"-"]) some cs_1] 
		#"@" [(cs_2: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"-"]) some cs_2] 
		#"." [(cs_3: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"." #"-"]) some cs_3] 
		opt #"^/" end]
	
	parse "very02.common@email.address.com" regex email
	
	; named groups
	>> regex/parse/spec "<h1[^^>]*>(?P<title>[^^<]*)</h1>" read http://www.red-lang.org/
	[(cs_1: charset [not [#">"]] cs_2: charset [not [#"<"]] _title: _1: [any cs_2]) thru [#"<" #"h" #"1" any cs_1 #">" copy br_1 _1 (title: br_1) #"<" #"/" #"h" #"1" #">"] to end]
	== true
	>> print title
	
	Red Programming Language
	
	>>
	
	; absolute, relative and named subroutines in Perl syntax
	regex/spec/parse "(?+1)(?'name'[abc])(?1)(?-1)(?&name)" "abcab"
	=> [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) thru [_1 copy br_1 _1 (name: br_1) _1 _1 _name] to end]
	; same in Ruby syntax
	regex/spec/parse "\g'+1'(?'name'[abc])\g'1'\g'-1'\g'name'" "bbccc"
	=> [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) thru [_1 copy br_1 _1 (name: br_1) _1 _1 _name] to end]
	; PCRE
	regex/spec/parse "(?P<name>[abc])(?1)(?P>name)" "bca"
	=> [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) thru [copy br_1 _1 (name: br_1) _1 _name] to end]
	
	; comments
	regex/spec "abc(?# Simple re )"
	=> [thru [#"a" #"b" #"c"] to end]
	
	; freespace demonstration
	ip-v4: {
	\A						# Check start of the string
	(						# Begin definition of quad number
			25[0-5]					# Highest quads			250-255
		|	2[0-4]\d				# Second highest quads 		200-249
		|	1\d\d					# Quads in second hundred 	100-199
		|	[1-9]\d					# Quads				10-99
		|	\d					# Quads				0-9
	)						# End definition of quad number,
							# and  first number is checked
	(?: 						# Begin non-capturing group 
			\.					# check for dot
			(? 1 ) 					# and call quad definition subroutine
	)    						# End group
	{ 3 }						# Iterate last group 3 times  
	\z						# Check end of the string
}
	regex/spec/parse/freespace ip-v4 "192.168.1.65"

]

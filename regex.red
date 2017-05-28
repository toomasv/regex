Red [
	Author: 	"Toomas Vooglaid"
	file: 		"%regex.red"
	Purpose: 	{Regex to parse converter}
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
				2017-05-25 -- added short refinements `/i`, `/s`, `/m` and `/x`, with same meaning as 
					`/icase`, `/singleline`, `/multiline` and `/freespace` correspondingly.
					Also added `/modes options` refinement, where shortcoded switches can be set in one string.
					Made freespace more permissive and made an enhanced example for it.
				v.0.6:	2017-05-27 -- 
					normalized freespace handling
					added capturing of overall match into `br_0`
					added global mode refinements `/global` and `/g`
					and a map `br_`, which holds captured strings on numbered or named keys:
						if in global mode -- in blocks, otherwise in strings (see examples)
				2017-05-28 -- added refinements `/simplex` with short alias `/n` to turn on non-capturing mode, 
					where only named groups are captured (and numbered)
			}
	TBD: 		{atomic groups, "soft" quantifiers, character-class subtraction, switches, substitution, look-around etc
				inline created words are leaking into global environment}
]

re-ctx: make reactor! [
	_spec: 		clear []
	starting: 	'loose
	ending: 	'loose
	nest-level: 0
	sl: ml: 	off
	nocase: 	off
	freespace: 	off
	glob: 		off
	simp:		off
	debugging: 	off
	longout: 	clear []
	shortout:	clear []
	rpt: seq: 	clear []
	levelgrp: 	clear []
	levelgr2:	clear []
	levelcap: 	clear []
	levelsym: 	clear []
	levelnam: 	clear []
	capturing: 	off
	to-short: 	off
	set 'br_	make map! 5
	defs: 		clear []
	sym:		none
	sbrdef1:	none
	sbrdef2:	none
	bckref: 	none
	assignments: clear []
	br-num: 	0
	
	next-br: does [br-num: br-num + 1]
	
	cs-num: 0 
	empty-cs?: false
	cs-open?: false
	next-cs: does [to-word append copy "cs_" cs-num: cs-num + 1] 						; charset-number-word generator
		
	make-charset: func [s /local c e s1 s2 negated cs rpt cb][						; cb -- charset definition; e -- continuation string
		c: copy [] rpt: none										; c -- whole charset expression; rpt -- quantifier
		cs-open?: true
		if negated: to-logic find/match s #"^^"	[s: next s]						; if charset is negated, jump over ^
		s1: index? s											; register position after possible ^
		system/words/parse s [										; let's form the charset
			collect set cb some [									; collect charset definitional elements
				 #"]" s2: [									; register current position after ]
					if (s1 < ((index? s2) - 1))	[					; if ] is not in the beginning of charclass
						any #"]" opt [collect set rpt repeater]				; we are in the end of charset, check for quantifiers, register, exit
						(cs-open?: false) e: break					; register closing of charset definition
					] | [ 
						if (not empty-cs?) [keep (#"]")] 				; if empty charsets are not allowed, then keep ] as part of charset definition
						| (cs-open?: false) e: break					; otherwise, register closing, position and exit
					]
				]  											
				| "-]" keep (#"-") 								; if - occurs before closing ]
					   any #"]" opt [collect set rpt repeater] 				; collect quantifier, if any
					   (cs-open?: false) e: break						; declare charclass closed, register position, go back
				| "^^" keep (#"^^")								; we can keep ^, because its negation meaning is taken care of
				| ["\n" keep (#"^/") | "\r" keep (#"^M")]					; keep linebreakers
				| #"\" [
					ahead 	[clmetaset | metaset] 						; metachars may be escaped 
					keep 	[clmetaset | metaset] 							
					| (cause-error 'user 'message ["Unescaped \ in char-class!"])		; only one, which sould necessarily be escaped
				]					
				| #"-" s2: [if (s1 = ((index? s2) - 1)) keep (#"-")				; we have just a dash
					| keep ('-) ]								; we have a range
				| keep clliteral 								; keep more or less anything
				| (cause-error 'user 'message ["Malformed charset?"])				; just for checking
			]
		]
		cs: next-cs											; new word to bind charset to
		if (cs-open? and empty? e) [
			cause-error 'user 'message ["Character class unclosed!"]
		]
		if to-logic negated [cb: compose/deep [not [(cb)]]]						; if charset is negated, make negation
		append defs copy compose/deep [ 
			(to-set-word cs) charset [(cb)]
		] 												; escape parse to actually make a charset
		if rpt [append c either block? rpt/1 [rpt/1][rpt]]						; did we have quantifiers? Append them here
		append c cs 											; and finally a word referring to created charset
		compose/deep [[(c)] (e)]									; send proudly back charset def and remaining string after charset
	]
	group: 	[if (not cs-open?) [
		#"(" 												; named group
		[	"?P<" 	copy gname to #">" 
		| 	"?'" 	copy gname to #"'" 
		| 	"?<" 	copy gname to #">"
		] skip
		(
			insert levelcap capturing
			capturing: on
			if 1 < length? levelcap [to-short: on]
			insert levelnam copy gname
			unless simp [insert levelsym next-br]
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout
			if debugging [probe compose ["lgrp:" (levelgrp)]]
			longout:	clear []
			shortout: 	clear []
		)
	|	[ #"("  #"?"  #"+" 	copy n number  #")"							; relative subroutine (forward)
		| "\g"  
			[	#"<"  #"+" 	copy n number  #">"
			| 	#"'"  #"+" 	copy n number  #"'"]
		] keep (to-word append copy "_" br-num + to-integer n)
	|	[ #"("  #"?"  #"-" 	copy n number  #")"							; relative subroutine (backward)
		| "\g"  
			[	#"<"  #"-" 	copy n number  #">"
			| 	#"'"  #"-" 	copy n number  #"'"]
		] keep (to-word append copy "_" br-num + 1 - to-integer n)
	|	[ #"("  "?" 	 copy n number  #")" 								; absolute subroutine reference
		| "\g"  
			[	"<" 	 copy n number  #">"
			| 	"'" 	 copy n number  #"'"]
		] keep (to-word append copy "_" n)
	|	[ #"("  											; named subroutine reference
			[	"?&" 	copy gname to #")" 
			| 	"?P>" 	copy gname to  #")"]
		| "\g"  
			[	"<" 	copy gname to  #">"
			| 	"'" 	copy gname to  #"'"]
		] skip 
		keep (to-word append copy "_" gname)
	|	[ #"("  "?P=" 	copy gname to #")" 								; reference to named captured string
		| "\k"  
			[	"'" 	copy gname to #"'" 
			| 	"<" 	copy gname to #">" 
			| 	"{" 	copy gname to #"}"]
		] skip 
		keep (to-word gname)
	|	#"("  "?:" (											; non-capturing group
			insert levelcap capturing 
			capturing: off
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout 
			longout: 	clear []
			shortout: 	clear []
		) 
	|	#"(" ( 												; capturing group
			insert levelcap capturing
			either simp [										; unless simplex
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
	| 	#")"  opt collect set rpt repeater 								; end of group, check for quantifier
		(
			either capturing [									; are we capturing?
				all [
					nam: take levelnam 
					append defs to-set-word copy append copy "_" nam
					nam: to-word copy nam
				] 
				sym: take levelsym 
				sbrdef1: to-word append copy "_" sym
				sbrdef2: to-word append either to-short [copy "_'"][copy "_"] sym
				bckref: to-word append copy "br_" sym
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
					unless block? br_/(sym) [extend br_ reduce [sym make block! 5]]
					append assignments compose [append select br_ (sym) (bckref)]
					if nam [
						unless block? br_/(nam) [extend br_ reduce [nam make block! 5]]
						append assignments compose [
							(to-set-word nam) (bckref) 
							append select br_ (to-lit-word nam) (bckref)
						]
					]
				][
					append assignments compose [put br_ (sym) (bckref)]
					if nam [
						append assignments compose [
							(to-set-word nam) (bckref) 
							put br_ (to-lit-word nam) (bckref)
						]
					]
				]
				append/only longout to-paren assignments
				if debugging [probe compose ["lout:" (longout)]]
			][											; we are not capturing
				longout: 	append/only append take levelgrp rpt copy longout
				shortout: 	append/only append take levelgr2 rpt copy shortout
			]
			capturing: take levelcap
		) 
	| 	keep literal
	]]
	class: [
		"[:" copy cl to ":]" 2 skip keep (to-word cl)							; this is in wrong place?
	| 	#"[" s: 
		(
			set [c e] make-charset s 								; send string to the charset-factory
			append longout c									; put the charset into place
			append shortout c
			e: any [find/last s e tail s]
		)
		:e												; continue after the charclass
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
	wordboundary:	["\b" keep (							; does not react on changing word / nonword values
		[s: [
			if ((1 = index? s) or find nonword first back s) 		[ahead word] 
			| if (find word first back s) 					[ahead [nonword | end]]
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
		| #")" 		(cause-error 'user 'message ["Invalid use of closing parentheses!"])
	]
	rmfree: [any [
			remove [any wspace #"#" thru linebreak]				; comments
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
		| 	group
		| 	class
		| 	special
		| 	backref
		| 	#"." 	keep ('anychar)  
		| 	char 
		| 	exception
	]
	repeater: [
		  #"^{"  copy n1 number  #","  copy n2 number  #"^}" 
											keep (reduce [to-integer n1 to-integer n2])
		| #"^{"  #","  copy n2 number  #"^}" 					keep (reduce [0  to-integer n2])
		| #"^{"  copy n1 number  #","  "^}"					keep (reduce [to-integer n1 10000])
		| #"^{"  copy n1 number  #"^}"						keep (to-integer n1)
		| #"?" 									keep ('opt)
		| #"+" 									keep ('some)
		| #"*" 									keep ('any) 
	]
	build: func [inner /local s e c t r n mp][					; main workhorse
		longout: 	clear []
		shortout: 	clear []
		system/words/parse/case inner [
			any [
				collect set seq sequence  
				opt collect set rpt repeater
				(
					if block? first rpt [rpt: first rpt] 
					append longout 	compose [(rpt) (seq)] 
					append shortout compose [(rpt) (seq)]
				) 											 
			]
		]
		compose/deep [[(shortout)] [(longout)]]
	]

	finish: func [inner /local short long][
		set [short long] build copy inner 
		append _spec switch starting [
			strict [compose/deep [copy br_0 thru [(long)] (to-paren [br_/0: br_0])]] 
			loose [either glob [
				compose/deep [some [to [(short)] copy br_0 thru [(long)] (to-paren [append br_/0 br_0])]]
			][
				compose/deep [to [(short)] copy br_0 thru [(long)] (to-paren [br_/0: br_0])]
			]]
		]
		append _spec switch ending [
			strict 			[[opt #"^/" end]] 
			strictissima 	['end]
			loose 			[[to end]]
		]
	]
	flavors: [JS [empty-cs?: true]]							; just a probe so far
	
	set 'regex func [
		"Regex to parse converter"
		re [string!]  
		/parse str [string!] 							"string to parse"
		/debug									"turns on debugging"
		/spec 									"prints out generated spec"
		/modes "passes all the modes in one string"  optstr [string!] 		"shortcoded modes"
		/icase		"see next"	/i					"turns on case-insensitivity"
		/multiline	"see next"	/m					"lets ^^ and $ match beginning and end of line also"
		/singleline	"see next"	/s					"lets dot match linebreaks also"
		/freespace	"see next"	/x					"lets you use whitespace, to make re more readable"
		/global		"see next"	/g					"global mode, puts captured strings into block"
		/simplex	"see next"	/n					"non-numbering mode, only named groups are captured (and numbered)"
		/try	"try specific flavor of regexp"	flavor	[word!] 		"flavor to try"
		/local inner
	][  
		debugging: 	any [debug off]
		cs-num:   	0
		br-num:		0
		_spec:		make block! 100
		inner: 		clear ""
		nocase:		any [icase 		i all [modes find optstr "i"] off]
		self/ml: 	any [multiline  m all [modes find optstr "m"] off]
		self/sl: 	any [singleline s all [modes find optstr "s"] off]
		freesp: 	any [freespace 	x all [modes find optstr "x"] off]
		glob:		any [global 	g all [modes find optstr "g"] off]
		simp:		any [simplex 	n all [modes find optstr "n"] off]
		empty-cs?: 	false
		cs-open?: 	false
		levelgrp: 	make block! 5
		levelgr2:	make block! 5
		levelcap: 	make block! 5
		levelsym: 	make block! 5
		levelnam: 	make block! 5
		capturing: 	off
		to-short: 	off
		defs: 		make block! 5
		br_:		either glob [make map! reduce [0 make block! 10]][make map! reduce [0 make string! 20]]
		
		if freesp [system/words/parse re rmfree cs-open?: no probe re]
		
		if try [unless do select flavors flavor [print append to-string flavor " is not supported :("]]
		
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
		either parse [
			return either nocase [system/words/parse str _spec][system/words/parse/case str _spec]
		][
			return _spec
		]
	]
]
examples: :comment 
examples [
	regex/parse "[a-c]+" "abcaaabbbcaab"
	regex/parse/spec "[a-c]+" "abcaaabbbcaab"
	=> [(cs_1: charset [#"a" - #"c"]) to [some cs_1] copy br_0 thru [some cs_1] (br_/0: br_0) to end]
	
	ip-v4: "\A(25[0-5]|2[0-4]\d|1\d\d|[1-9]\d|\d)(?:\.(?1)){3}\z"
	regex/parse/spec ip-v4 "127.0.0.25"
	=> [(cs_1: charset [#"0" - #"5"] cs_2: charset [#"0" - #"4"] cs_3: charset [#"1" - #"9"] 
		_1: [#"2" #"5" cs_1 | #"2" cs_2 digit | #"1" digit digit | cs_3 digit | digit]) 
		copy br_0 thru [copy br_1 _1 (put br_ 1 br_1) 3 [#"." _1]] (br_/0: br_0) end]
	>> regex/parse ip-v4 "255.255.255.256"
	== false

	email: "^^[a-zA-Z0-9_.+-]*@[a-zA-Z0-9-]+\.[a-zA-Z0-9.-]+$"
	regex/spec/parse email "n00b@lost.island.org"
	=> [(cs_1: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"_" #"." #"+" #"-"] 
		cs_2: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"-"] 
		cs_3: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"." #"-"]) 
		copy br_0 thru [some cs_1 #"@" some cs_2 #"." some cs_3] (br_/0: br_0) opt #"^/" end]
	
	parse "very02.common@email.address.com" regex email
	
	; named groups, captured strings 
	>> regex/parse/spec "<h1[^^>]*>\n?(?P<title>[^^<\n]*)\n?</h1>" read http://www.red-lang.org/
	[(cs_1: charset [not [#">"]] cs_2: charset [not [#"<" #"^/"]] _title: _1: [any cs_2]) 
		to [#"<" #"h" #"1" any cs_1 #">" opt #"^/" _1 opt #"^/" #"<" #"/" #"h" #"1" #">"] 
		copy br_0 thru [#"<" #"h" #"1" any cs_1 #">" opt #"^/" copy br_1 _1 
			(put br_ 1 br_1 title: br_1 put br_ 'title br_1) opt #"^/" #"<" #"/" #"h" #"1" #">"] 
		(br_/0: br_0) to end]
	== true
	>> br_
	== #(
		0 "<h1 class='title'>^/Red Programming Language^/</h1>"
		1 "Red Programming Language"
		title: "Red Programming Language"
	)
	>> print title
	Red Programming Language
	
	; absolute, relative and named subroutines in Perl syntax
	regex/spec/parse "(?+1)(?'name'[abc])(?1)(?-1)(?&name)" "abcab"
	=> [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) 
	to [_1 _1 _1 _1 _name] 
	copy br_0 thru [_1 copy br_1 _1 (put br_ 1 br_1 name: br_1 put br_ 'name br_1) _1 _1 _name] 
	(br_/0: br_0) to end] 
	; same in Ruby syntax
	regex/spec/parse "\g'+1'(?'name'[abc])\g'1'\g'-1'\g'name'" "bbccc"
	=> [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) 
	to [_1 _1 _1 _1 _name] 
	copy br_0 thru [_1 copy br_1 _1 (put br_ 1 br_1 name: br_1 put br_ 'name br_1) _1 _1 _name] 
	(br_/0: br_0) to end]
	; PCRE
	regex/spec/parse "(?P<name>[abc])(?1)(?P>name)" "bca"
	=> [(cs_1: charset [#"a" #"b" #"c"] _name: _1: [cs_1]) 
	to [_1 _1 _name] 
	copy br_0 thru [copy br_1 _1 (put br_ 1 br_1 name: br_1 put br_ 'name br_1) _1 _name] 
	(br_/0: br_0) to end]
	
	; comments
	regex/spec "abc(?# Simple re )"
	=> [to [#"a" #"b" #"c"] copy br_0 thru [#"a" #"b" #"c"] (br_/0: br_0) to end]
	
	; freespace demonstration + named group + wordboundary + backreference
	; without wordboundary invalid IP addresses like eg 192.186.1.265 will be 
	; reckognized as 192.186.1.26
	ip-v4: {
	(   						# We are capturing the whole address
		?P< ipaddr >				# Let's give it a name
		(						# Begin definition of quad number
			\b (?:					# Quad starts (wordboundary + noncapturing group)
					25[0-5]				# Highest quads			250-255
				|	2[0-4]\d			# Second highest quads 		200-249
				|	1\d\d				# Quads in second hundred 	100-199
				|	[1-9]\d				# Quads 			 10-99
				|	\d				# Quads 			  0-9
			) \b					# Quad ends (nc-group ends + wordboundary)
		)						# End definition of quad number,
								# and check first quad
		(?: 					# Begin non-capturing group
			\.					# check for dot
			(? 2 ) 					# and call quad definition subroutine
		)    					# End group
		{ 3 }					# Iterate last group 3 times 
	)
}
	>> regex/parse/freespace ip-v4 "some text 192.168.1.65 around the address"
	== true
	>> ipaddr
	== "192.168.1.65"
	>> regex/parse/freespace ip-v4 "some text 192.168.1.265 around the address"
	== false

	; global mode with nested groups
	>> regex/parse/g "(\w(\w{1,2}))\W(\w+)" "per aspera ad astra"
	== true
	>> br_
	== #(
		0 ["per aspera" "ad astra"]
		2 ["er" "d"]
		1 ["per" "ad"]
		3 ["aspera" "astra"]
	)
	
	; global + simplex (non-capturing) modes 
	>> regex/parse/g/n "(\w(\w{1,2}))\W(\w+)" "per aspera ad astra"
	== true
	>> br_
	== #(
		0 ["per aspera" "ad astra"]
	)

	; as previous + named groups
	>> regex/parse/g/n "(?<pre>\w(\w{1,2}))\W(?<nom>\w+)" "per aspera ad astra"
	== true
	>> br_
	== #(
		0 ["per aspera" "ad astra"]
		1 ["per" "ad"]
		pre: ["per" "ad"]
		2 ["aspera" "astra"]
		nom: ["aspera" "astra"]
	)
	
	; different groups with same name
	>> regex/parse/g/n "(?<pre>\w(\w{1,2}))\W(?<pre>\w+)" "per aspera ad astra"
	== true
	>> br_
	== #(
		0 ["per aspera" "ad astra"]
		1 ["per" "ad"]
		pre: ["per" "aspera" "ad" "astra"]
		2 ["aspera" "astra"]
	)

]

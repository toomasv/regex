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
					added capturing of overall match into `br_0` (changed later into `&0`)
					added global mode refinements `/global` and `/g`
					and a map `br_`, which holds captured strings on numbered or named keys:
						if in global mode -- in blocks, otherwise in strings (see examples)
				2017-05-28 -- added refinements `/simplex` with short alias `/n` to turn on non-capturing mode, 
					where only named groups are captured (and numbered)
				2017-05-29 -- changed `br_` into tcl-like `&` and made it changeable. It can be changed
					as eg. `re-ctx/symbol: '¤`. See examples. Earlier examples presume symbol `br_`.
				v.0.7:	2017-05-30 --
					added first bit of `/replace replacement` refinement. `replacement` is string (so far), 
					which may include backreferences to captured strings in the same form as in matching, 
					or as `\'1'` or `\'name'`. `name` may be assigned a value in matching, but also outside 
					of the `regex`. The overall match is replaced by evaluated replacement. 
					Also in `global` mode. Examples.
			}
	TBD: 	{atomic groups, "soft" quantifiers, character-class subtraction, switches, substitution, look-around etc
				inline created words are leaking into global environment}
]

re-ctx: make reactor! [
	_spec: 			clear []
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
	next-cs: does [to-word append copy "cs_" cs-num: cs-num + 1] 							; charset-number-word generator
		
	make-charset: func [s /local c e s1 s2 negated cs rpt cb][							; cb -- charset definition; e -- continuation string
		c: copy [] rpt: none											; c -- whole charset expression; rpt -- quantifier
		cs-open?: true
		if negated: to-logic find/match s #"^^"	[s: next s]							; if charset is negated, jump over ^
		s1: index? s												; register position after possible ^
		system/words/parse s [											; let's form the charset
			collect set cb some [										; collect charset definitional elements
				 #"]" s2: [										; register current position after ]
					if (s1 < ((index? s2) - 1))	[						; if ] is not in the beginning of charclass
						any #"]" opt [collect set rpt repeater]					; we are in the end of charset, check for quantifiers, register, exit
						(cs-open?: false) e: break						; register closing of charset definition
					] | [ 
						if (not empty-cs?) [keep (#"]")] 					; if empty charsets are not allowed, then keep ] as part of charset definition
						| (cs-open?: false) e: break						; otherwise, register closing, position and exit
					]
				]  											
				| "-]" keep (#"-") 									; if - occurs before closing ]
					   any #"]" opt [collect set rpt repeater] 					; collect quantifier, if any
					   (cs-open?: false) e: break							; declare charclass closed, register position, go back
				| "^^" keep (#"^^")									; we can keep ^, because its negation meaning is taken care of
				| ["\n" keep (#"^/") | "\r" keep (#"^M")]						; keep linebreakers
				| #"\" [
					ahead 	[clmetaset | metaset] 							; metachars may be escaped 
					keep 	[clmetaset | metaset] 							
					| (cause-error 'user 'message ["Unescaped \ in char-class!"])			; only one, which sould necessarily be escaped
				]					
				| #"-" s2: [if (s1 = ((index? s2) - 1)) keep (#"-")					; we have just a dash
					| keep ('-) ]									; we have a range
				| keep clliteral 									; keep more or less anything
				| (cause-error 'user 'message ["Malformed charset?"])					; just for checking
			]
		]
		cs: next-cs												; new word to bind charset to
		if (cs-open? and empty? e) [
			cause-error 'user 'message ["Character class unclosed!"]
		]
		if to-logic negated [cb: compose/deep [not [(cb)]]]							; if charset is negated, make negation
		append defs copy compose/deep [ 
			(to-set-word cs) charset [(cb)]
		] 													; escape parse to actually make a charset
		if rpt [append c either block? rpt/1 [rpt/1][rpt]]							; did we have quantifiers? Append them here
		append c cs 												; and finally a word referring to created charset
		compose/deep [[(c)] (e)]										; send proudly back charset def and remaining string after charset
	]
	group: 	[if (not cs-open?) [
		#"(" 													; named group
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
	|	[ #"("  "?+" 	copy n number  #")"									; relative subroutine (forward)
		| "\g"  
			[	"<+" 	copy n number  #">"
			| 	"'+" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 br-num + to-integer n)
	|	[ #"("  "?-" 	copy n number  #")"									; relative subroutine (backward)
		| "\g"  
			[	"<-" 	copy n number  #">"
			| 	"'-" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 br-num + 1 - to-integer n)
	|	[ #"("  #"?" 	 copy n number  #")" 									; absolute subroutine reference
		| "\g"  
			[	#"<" 	 copy n number  #">"
			| 	#"'" 	 copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 n)
	|	[ #"("  												; named subroutine reference
			[	"?&" 	copy gname to #")" 
			| 	"?P>" 	copy gname to  #")"]
		| "\g"  
			[	#"<" 	copy gname to  #">"
			| 	#"'" 	copy gname to  #"'"]
		] skip 
		keep (to-word append copy "_" gname)
	|	"(?:" (													; non-capturing group
			insert levelcap capturing 
			capturing: off
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout 
			longout: 	clear []
			shortout: 	clear []
		) 
	|	#"(" ( 													; capturing group
			insert levelcap capturing
			either simp [											; unless simplex
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
			either capturing [										; are we capturing?
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
			][												; we are not capturing
				longout: 	append/only append take levelgrp rpt copy longout
				shortout: 	append/only append take levelgr2 rpt copy shortout
			]
			capturing: take levelcap
		) 
	| 	keep literal
	]]
	class: [
		"[:" copy cl to ":]" 2 skip keep (to-word cl)								; this is in wrong place?
	| 	#"[" s: 
		(
			set [c e] make-charset s 									; send string to the charset-factory
			append longout c										; put the charset into place
			append shortout c
			e: any [find/last s e tail s]
		)
		:e													; continue after the charclass
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
		[ #"("	"?P=" 	copy gname to #")" 									; reference to named captured string
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
		[	#"'" copy n number #"'" 
		|	#"<" copy n number #">" 
		|	#"^{" copy n number #"^}" 
		] 	keep (to-word append copy brsymb n)
	|	[	"'" copy gname to #"'" 
		|	#"<" copy gname to #">"
		|	#"^{" (print "hi") copy gname to #"^}"
		]]
		skip 
		keep (to-word gname)
	]

	linestart: 		is [either ml [[#"^^" keep (#"^/")]][[#"^^" (starting: 'strict)]]]
	lineend:		is [either ml [[#"$" keep ([ahead [opt #"^/" end | #"^/"]])]][[#"$" keep ([opt #"^/" end])]]] 
	wordboundary:	["\b" keep (											; does not react on changing word / nonword values
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
	exception: 		[
		  #"\" 		(cause-error 'user 'message ["Unescaped \!"])
		| #")" 		(cause-error 'user 'message ["Invalid use of closing parentheses!"])
	]
	rmfree: [any [
			remove [any wspace #"#" thru linebreak]							; comments
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
		| 	#"^{"  #","  copy n2 number  #"^}" 		keep (reduce [0  to-integer n2])
		| 	#"^{"  copy n1 number  #","  "^}"		keep (reduce [to-integer n1 10000])
		| 	#"^{"  copy n1 number  #"^}"			keep (to-integer n1)
		| 	#"?" 						keep ('opt)
		| 	#"+" 						keep ('some)
		| 	#"*" 						keep ('any) 
	]
	build: func [inner /local s e c t r n mp][				; main workhorse
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
				copy (full-match) thru [(long)] 
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
			strict 		[[opt #"^/" end]] 
			strictissima 	['end]
			loose 		[[to end]]
		]
		if debugging [probe compose/deep ["_spec:" [(_spec)]]]
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
		/simplex	"see next"	/n					"non-numbering mode, only named groups are captured"
		/replace 	"Captured matches are used in replacements"
			replacement [string!] {String replaces any overall matches, 
				block replaces global overall matches in order 
				and map specifies numbered and named groups to use in ereplacement}
		/try		"try specific flavor of regexp"	flavor	[word!] "flavor to try"
		/local inner
	][  
		debugging: 		any [debug off]
		cs-num:   		0
		br-num:			0
		_spec:			make block! 100
		inner: 			clear ""
		nocase:			any [icase 	i all [modes find optstr "i"] off]
		self/ml: 		any [multiline  m all [modes find optstr "m"] off]
		self/sl: 		any [singleline s all [modes find optstr "s"] off]
		freesp: 		any [freespace 	x all [modes find optstr "x"] off]
		glob:			any [global 	g all [modes find optstr "g"] off]
		simp:			any [simplex 	n all [modes find optstr "n"] off]
		self/replace:		any [replace off]
		self/replacement:	any [replacement none]
		empty-cs?: 		false
		cs-open?: 		false
		levelgrp: 		make block! 5
		levelgr2:		make block! 5
		levelcap: 		make block! 5
		levelsym: 		make block! 5
		levelnam: 		make block! 5
		capturing: 		off
		to-short: 		off
		defs: 			make block! 5
		set symbol		either glob [make map! reduce [0 make block! 10]][make map! reduce [0 make string! 20]]
		
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
comment {}

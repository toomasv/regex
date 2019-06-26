Red [
	Author: 	"Toomas Vooglaid"
	file: 		"%regex.red"
	Purpose: 	{Regex to parse converter}
	Licence: "MIT"
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
				2017-05-31 -- corrected some mistakes. Reorganised replacement code. 
					Added block! as replacement. In case of global parsing replaces matches in the block order,
					eg. `a: "first" regex/parse/g/replace "\w+" x: "1 second 3" [a "\0" "third"] head x` would yield
					`"first second third"`. Only as many matches will be changed as there are elements in the block, 
					superfluous elements in the block will be disregarded. 
					Some stylistic change of code in `build` function.
				2017-06-01 -- Allowed any-string! as `/parse string` argument, 
					so that emails, urls, tags and filenames could be parsed directly.
					corrected treatment of quantified parentheses, eg. (\w)* . Previously the pattern was quantified, 
						so it captured strings of different letters (if any), 
						now the captured letter is quantified, so it captures strings of same letter.
				2017-06-03 -- Added basic support for lookaround, both negative and positive lookahead and lookbehind.
				2017-06-08 -- Added refinement `/possessive`, short `/+`, which switches on possessive mode, so that 
					soft quantifiers lose their meaning.
					Made symbols that leak into global environment cleanable. They are unset by calling `re-ctx/clean`,
						on every calling of `regex` and also on every loading of %regex.red
				2017-06-13 -- Got soft quantifiers to work correctly while not in/on groups or charsets.
				2017-06-17 -- Charclasses can now include `shortclasses`, e.g. `\d` or `\w`, which means that these 
						charclasses are connected through `union`. Also `[:alpha:]` kind of classes may be included.
					* subtraction with syntax `[class1-[class2]]`, which is interpreted as `exclude class1 class2`;
					* intersection with syntax `[class1&&[class2]]`, interpreted as `intersect class1 class2`;
					* negated charclasses may give unexpected results with intersect due to the `red` mechanism
						of allocating bits to bitset. E.g "[\d&&[^^1234]]" (intersect charset [#"0" - #"9"] complement charset "1234")
						should result in "056789", but results actually in "0567". To avoid this, use exclusion instead: 
						"[\d-[1234]]" (exclude charset [#"0" - #"9"] charset "1234"). TBD: do this automatically
					* soft quantifiers now work also on charclasses (but not in and on groups).
				2017-06-18 -- Corrected negative lookbehind behavior. It is still not correct if there are more than one lookaround.
				2017-06-22 -- Added conditionals with syntax `(?(if)then|else)`. `if` may be a number referencing captured group 
					e.g "(pretty )?(?(1)girls|boys)", which matches "pretty girls" and "boys",	or it may be lookaround 
					(currently only lookahead), e.g. "b(?(?=i)inary|unny)", which matches "binary" and "bunny". 
					`then` and `else` may be any re-s (currently excluding soft quantifiers).
						
			}
	TBD: 	{"soft" quantifiers in/on groups/lookarounds, substitution with maps and functions, multiple look-arounds, conditionals}
]

re-ctx: make reactor! [
	attempt [while [s: take re-symbols][unset s] unset 're-symbols]
	_spec: 			clear []
	starting: 		'loose
	ending: 		'loose
	;nest-level:	0
	sl: ml: 		off
	nocase: 		off
	freespace: 		off
	glob: 			off
	simp:			off
	debugging: 		off
	poss:			off
	longout: 		clear []
	shortout:		clear []
	rpt: seq: 		clear []
	rpt1: seq1:		clear []
	rptlimit:		10000
	levelgrp: 		clear []
	levelgr2:		clear []
	levelcap: 		clear []
	levelsym: 		clear []
	levelnam: 		clear []
	capturing: 		off
	to-short: 		off
	symbols:		clear []										; container for public symbols created in spec block
	clean: 			function [][while [s: take symbols][unset s]]
	symbol:			'&												; base symbol for capturing codes and captured strings
	brsymb:			is [to-string symbol]
	full-match: 	is [to-word append copy brsymb 0]
	sbrsymb1:		is [append copy "_" brsymb]
	sbrsymb2:		is [append copy "_'" brsymb]
	defs: 			clear []
	sym: nam:		none
	sbrdef1:		none
	sbrdef2:		none
	bckref: 		none
	assignments: 	clear []
	replace:		off
	replacement:	none
	lookaround:		clear []
	lookbehind: 	clear []
	look: 			none
	sbrnum:			none
	condition:		none
	condition-open?: no
	longsoftgrp:	clear []
	shortsoftgrp: 	clear []
	longsoft: 		clear []
	shortsoft: 		clear []
	soft-prepare:	clear []
	softvars:		clear []
	soft: 			off
	softsbrsymb:	none
	softsymb:		none
	soft-num: 		0
	br-num: 		0
	cs-num: 		0 
	lb-num:			0
	next-soft: 		does [soft-num: soft-num + 1]
	next-br: 		does [br-num: br-num + 1]
	next-cs: 		does [getword "cs_" cs-num: cs-num + 1] 		; charset-number-word generator
	next-lb:		does [getword "lb_" lb-num: lb-num + 1]			; lookbehind identifier generator
	ccgrp: 			clear []										; charclass holder
	neg?: 			no												; is charclass negated?
	char-class: 	clear []
	char-set:		charset ""
	empty-cs?: 		false											; is empty charset allowed?
	cs-open?: 		false											; is charset still open when collecting?
	;do %../helpers.red
	getword: func [pref suf][										; while generating symbols used in spec, insert these into container
		insert symbols to-word append copy pref suf first symbols
	]
	map: function [series [series!] fn [any-function!] /only][
		out: make type? series []
		foreach i series [
			either only [
				append/only out fn i
			][
				append out fn i
			]
		]
	]
	make-char-set: func [c-class neg? /local c-set c-cl][
		;do %../helpers.red
		c-cl: clear []
		;probe compose [(c-class)]
		c-set: charset ""
		foreach c c-class [
			switch type?/word c [
				word! [c-set: union c-set re-ctx/:c]
				char! [append c-cl c]
			] 
		]
		;forall c-class [
		;	probe reduce [length? c-class c-class type?/word first c-class]
		;	if word? first c-class [c-set: union c-set get take c-class]
			;print ["cs1" bitset-to-string c-set]
		;]
		unless empty? c-cl [c-set: union c-set charset c-cl]
		;print ["cs2:" bitset-to-string c-set "c-cl:" c-cl]
		compose/deep either neg? [(append copy [complement] to-block mold c-set)][[(c-set)]]
	]
	subroutine: [if (not cs-open?) [
		[ #"("  "?+" 	copy n number  #")"									; relative subroutine (forward)
		| "\g"  
			[	"<+" 	copy n number  #">"
			| 	"'+" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 br-num + to-integer n)
	|	[ #"("  "?-" 	copy n number  #")"									; relative subroutine (backward)
		| "\g"  
			[	"<-" 	copy n number  #">"
			| 	"'-" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 br-num + 1 - to-integer n)
	|	[ #"("  #"?" 	 copy n number  #")" 								; absolute subroutine reference
		| "\g"  
			[	#"<" 	copy n number  #">"
			| 	#"'" 	copy n number  #"'"]
		] keep (to-word append copy sbrsymb1 n)
	|	[ #"("  															; named subroutine reference
			[	"?&" 	copy gname to  #")" 
			| 	"?P>" 	copy gname to  #")"]
		| "\g"  
			[	#"<" 	copy gname to  #">"
			| 	#"'" 	copy gname to  #"'"]
		] skip 
		keep (to-word append copy gname getword "_" gname)
	]]
	lookaround1: [
		#"=" (look: [ahead])												; postive lookahead
	|	#"!" (look: [ahead not])											; negative lookahead
	|	"<=" (look: [behind []])											; positive lookbehind
	|	"<!" (look: [behind not])											; negative lookbehind
	]
	conditional: [if (not cs-open?) [
		"(?(" (insert/only lookaround look) [
			#"?" lookaround1
		|	copy sbrnum to #")" skip (look: none)
		] (
			insert levelcap capturing 
			capturing: off
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout 
			if debugging [probe compose/deep ["sgrp lgrp:" [(levelgr2)] [(levelgrp)]]]
			longout: 	clear []
			shortout: 	clear []
			condition-open?: true
		)
	]]
	group: 	[if (not cs-open?) [
		#"(" (																; group
			insert levelcap capturing										; generally for groups
			insert/only levelgrp copy longout 
			insert/only levelgr2 copy shortout
			if debugging [probe compose/deep ["sgrp lgrp:" [(levelgr2)] [(levelgrp)]]]
			longout:	clear []
			shortout: 	clear []
			insert/only lookaround look
			look: none
		)[
		#"?" [lookaround1 | #":"] (capturing: off)							; non-capturing
		|	[	"?P<" 	copy gname to #">"  								; named group
			| 	"?'" 	copy gname to #"'" 
			| 	"?<" 	copy gname to #">"
			] 	skip (	
				capturing: on
				if 1 < length? levelcap [to-short: on]
				insert symbols 'gname
				insert levelnam copy gname
				insert levelsym next-br
			)
		|	(																; capturing group
			either simp [													; unless simplex
				capturing: off
			][
				capturing: on
				if 1 < length? levelcap [to-short: on]
				insert levelsym next-br 
				insert levelnam none 
			]
		)]
	| 	#")"  opt collect set rptblk repeater rest:							; end of group, check for quantifier
		( 
			set [rpt mode] either empty? rptblk [[[] none]][first rptblk] ;probe rptblk
			either capturing [												; are we capturing?
				all [
					nam: take levelnam 										; if this is named group
					append defs to-set-word getword copy "_" nam			; set the name in defs
					insert symbols nam: to-word copy nam					; and make the copied name into word
				] 
				sym: take levelsym 											; take the group's number
				sbrdef1: getword sbrsymb1 sym								; reference for subroutine
				sbrdef2: getword either to-short [sbrsymb2][sbrsymb1] sym	; reference for subroutine in shortout - when there is more than 1 level
				bckref:  getword brsymb sym									; reference to the string captured
				append/only append defs to-set-word sbrdef1 copy longout 	; append to definitions reference symbol and subroutine def
				all [														; if
					starting = 'loose 										; 	we are not bound in the beginning
					to-short												; 	we have deeper than 1 level groups
					append/only append defs to-set-word sbrdef2 copy shortout 	; then 	let's add shorter version into defs, to be used in `to` part
				]
				if debugging [probe compose ["defs:" (defs)]]
				shortout: append take levelgr2 compose/deep [				; we have to capture the string in the `to` part
					ahead [copy (bckref) (rpt) (sbrdef2)] (rpt) (bckref)
				]
				longout: append take levelgrp compose [(rpt) (bckref)] 		; when going `thru` we need to quantify captured string only
				assignments: make block! 5									; let's build up the map for captured strings
				either glob [												; in global mode 
					unless block? select get symbol sym compose [			; captures will be held in blocks
						extend (symbol) reduce [sym make block! 5]
					]
					append assignments compose [							; append latest captured string
						append select (symbol) (sym) (bckref)
					]
					if nam [												; for named captures
						unless block? select get symbol nam compose [
							extend (symbol) reduce [nam make block! 5]
						]
						append assignments compose [
							(to-set-word nam) (bckref) 
							append select (symbol) (to-lit-word nam) (bckref)
						]
					]
				][															; if not global, values will be strings
					append assignments compose [
						put (symbol) (sym) (bckref)
					]
					if nam [
						append assignments compose [
							(to-set-word nam) (bckref) 
							put (symbol) (to-lit-word nam) (bckref)
						]
					]
				]
				append/only shortout to-paren assignments 					; keep assignments in parse spec so that map will be built up when parsing
				if debugging [probe compose/deep [
					"sout lout:" [(shortout)] [(longout)]]
				]
			][																; we are not capturing
				either look [
					if debugging [probe compose/deep ["look:" [(look)]]]
					set [direction lookmode] look 
					switch direction [
						ahead [
							shortout: either condition-open? [
								condition: append/only copy look shortout 
								look: none
								clear [] 
							][
								append/only append take levelgr2 look copy compose [(shortout)]
								longout: 	take levelgrp
							]
						]
						behind [
							insert/only lookbehind longout
							shortout: 	take levelgr2 
							longout: 	take levelgrp 
						]
					]
				][
					either condition-open? [
						shortout: append/only append take levelgr2 compose [
							copy (getword brsymb "cond") thru
						] append copy compose/deep [(
							either condition [
								compose [(condition)] 
							][
								compose/deep [
									if (to-paren compose [not empty? (getword brsymb sbrnum)])
								]
							]
						)] copy compose [(shortout)]
						longout: append take levelgrp to-word rejoin [brsymb "cond"]
						condition-open?: no
					][
						shortout: 	append/only append take levelgr2 rpt copy shortout		; append the (shorter?) block to the output for the `to` part
						longout: 	append/only append take levelgrp rpt copy longout		; append the block to the output for the `thru` part (or start-bound)
					]
				]
			]
			capturing: take levelcap										; what was the capturing status for the previous level?
			look: take lookaround
		) 
	;| 	keep literal														; if building char class, keep literal value (?)
	]]
	charclass: [collect into char-class some [
		ahead ["-[" | "&&" | #"]"] break
	|	ahead "-]" keep skip 												; if - occurs before closing ]
	| 	#"^^" keep (#"^^")													; we can keep ^, because its negation meaning is taken care of
	|	"[:" copy cl to ":]" 2 skip keep (to-word cl)
	|	shortclass
	| 	"\n" keep (#"^/") 
	| 	"\r" keep (#"^M")													; keep linebreakers
	| 	#"\" [
			ahead 	[clmetaset | metaset] 									; metachars may be escaped 
			keep 	[clmetaset | metaset] 							
		| 	(cause-error 'user 'message ["Unescaped \ in char-class!"])		; only one, which sould necessarily be escaped
		]				
	| 	#"-" keep ('-) 														; we have a range
	| 	keep clliteral 														; keep more or less anything
	| 	(cause-error 'user 'message ["Malformed charset?"])					; just for checking
	]]
	class1: [#"[" (cs-open?: true)
		[#"^^" (neg?: yes) | (neg?: no)]
		collect set char-class [
			[#"-" keep (#"-") | ahead #"]" if (not empty-cs?) keep skip | none]
		]
		charclass opt [
			#"-" (
				char-set: make-char-set char-class neg?
				append ccgrp copy compose [exclude (char-set)]
			) class1 														; subtraction
		|	opt #"]" "&&" (
				char-set: make-char-set char-class neg?
				append ccgrp copy compose [intersect (char-set)]
			) [																; intersection
				class1 
			| 	charclass (neg?: no)
			]
		]
	]
	class: [
		"[:" copy cl to ":]" 2 skip keep (to-word cl)
	|	class1
	|	some #"]" 
		(
			char-set: append ccgrp copy make-char-set char-class neg?
			cs: next-cs														; new word to bind charset to
			append defs copy compose/deep [ 
				(to-set-word cs) (char-set)
			]
			cs-open?: false
		)
		;(probe reduce [cs char-set type? first char-set]) 
		keep (cs)
	]
	shortclass: [
			"\d" 		keep ('digit)
		| 	"\D"		keep ('nondigit)
		| 	"\w" 		keep ('word)
		| 	"\W"		keep ('nonword)
		| 	"\s" 		keep ('wspace)
		| 	"\S"		keep ('nonwspace)
		| 	"\N"		keep ('nonlinebreak)
	]
	special: [
			"\t"		keep (#"^-")
		| 	"\n"		keep (#"^/")
		| 	"\r"		keep (#"^M")
		| 	"\f"		keep (#"^L")
		| 	"\v"		keep (#"^K")
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
		[	#"'"	copy n number #"'" 
		| 	#"<"	copy n number #">" 
		| 	#"^{"	copy n number #"^}" 
		] 	keep (to-word append copy brsymb n)
	| 	[	#"'"	copy gname to #"'" 
		| 	#"<"	copy gname to #">"	
		| 	#"^{"	copy gname to #"^}"
		] 	skip keep (to-word gname)
	]]
comment {		]
		|
		[if (glob) 
			#"#" copy n number
			#"/" copy m number
			keep (
				to-paren compose [pick select (symbol) (to-integer n) (to-integer m)]
			)
		]
}
	linestart: is [either ml [
		[#"^^" keep (#"^/")]
	][
		[#"^^" (starting: 'strict)]]
	]
	lineend: is [either ml [
		[#"$" keep ([ahead [opt #"^/" end | #"^/"]])]
	][
		[#"$" keep ([opt #"^/" end])]]
	] 
	wordboundary: ["\b" keep (												; does not react on changing word / nonword values
		[s: [
			if ((1 = index? s) or find nonword first back s) 	[ahead word] 
			| if (find word first back s) 						[ahead [nonword | end]]
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
	punct:			charset {.,;:!?"'-}
	nonpunct: 		is [complement punct]
	meta: 			[#"\" #"^^" #"$"  #"." #"|" #"?" #"*" #"+" #"(" #")" #"[" #"^{"] 
	metaset: 		charset meta
	literal: 		is [complement metaset]
	clmeta:			[#"\" #"^^" #"-" #"]"]
	clmetas:		[#"\" | #"^^" | #"-" | #"]"]
	clmetaset:		charset clmeta
	clliteral:		is [complement clmetaset]
	;closing-paren:		charset [#")"]
	;not-closing-paren: 	complement closing-paren
	
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
		|	subroutine
		|	conditional
		| 	lookaround1
		| 	group
		| 	class
		|	shortclass
		| 	special
		| 	#"." 	keep ('anychar)  
		| 	char 
		| 	exception
	]
	repeater: [
		#"^{" [
			copy n1 number  #","  copy n2 number
		|	#","  copy n2 number (n1: 0)
		|	copy n1 number  #"," (n2: rptlimit)
		]
		#"}" 
		(mode: 'greedy) opt [#"?" (mode: 'lazy) | #"+" (mode: 'possessive)]
		keep (compose/deep [[(reduce [to-integer n1 to-integer n2])] (mode)])
	| 	#"^{" copy n1 number #"^}" 
		opt [#"?" | #"+"]
		keep (compose [(reduce [to-integer n1]) possessive])
	|	[#"?" (rpt: 'opt) | #"+" (rpt: 'some) | #"*" (rpt: 'any)] 
		(mode: 'greedy)	opt [#"?" (mode: 'lazy) | #"+" (mode: 'possessive)]
		keep (reduce [rpt mode])
;	|	keep (clear [])
	]

	build: func [inner /local s e c t r n out][								; main workhorse
		insert symbols [n1 n2 rest rest1]
		longout: 	clear []
		shortout: 	clear []
		longsoft: 	clear []
		shortsoft: 	clear []
		parse/case inner [
			any [ 
				collect set seq sequence  ;(probe seq)
				opt collect set rptblk repeater rest: 
				(
					if debugging [probe compose/deep ["rptblk:" [(rptblk)]]]
					set [rpt mode] either empty? rptblk [[[] none]][first rptblk] 
					if debugging [probe reduce ["rpt mode seq rest" rpt mode seq rest]]
					unless any [attempt [empty? rpt] poss mode = 'possessive empty? rest] [ 
						soft: on ;r: copy rest
						insert/only softvars reduce [seq rpt mode] 
						insert/only shortsoftgrp copy shortout
						insert/only longsoftgrp  copy longout
						if debugging [
							probe compose/deep [
								"svars:" 	[(softvars)] 
								"ssoftgrp:" [(shortsoftgrp)] 
								"lsoftgrp:" [(longsoftgrp)]
							]
						]
						shortout: clear []
						longout: clear []
					]
				)
				[	if (soft) (soft: off) :rest 
				|
				(
					if debugging [probe compose/deep ["rpt seq mode:" [(rpt)] [(seq)] [(mode)]]]
					append shortout compose [(rpt) (seq)]
					append longout 	compose [(rpt) (seq)]
					if debugging [probe compose/deep ["shortout longout:" [(shortout)] [(longout)]]]
					if all [tail? rest not empty? softvars] [
						while [
							set [seq rpt mode] take softvars
						][
							softrpt: switch/default rpt [
								opt [[0 1]] 
								any [compose [0 (rptlimit)]] 
								some [compose [1 (rptlimit)]]
							][rpt]
							softsymb: getword rejoin [brsymb "_"] next-soft
							softsbrsymb: getword "_" softsymb
							append/only append defs to-set-word softsbrsymb copy shortout 
							if debugging [probe compose/deep ["soft-defs:" [(defs)]]]

							append/only shortout: take shortsoftgrp switch mode [
								greedy [
									compose/deep [
										s:
										copy (softsymb) (rpt) (seq) (softsbrsymb)
									|
										;(to-paren compose/deep [probe reduce [1 r s (softsymb)]])
										(
											to-paren compose/deep [
												unless value? (to-lit-word softsymb) [
													(to-set-word softsymb) clear ""
												] n: length? (softsymb)
											]
										)
										while not [
											if (to-paren compose/deep [any [not n n < (softrpt/1)]]) reject
										|	(softsymb) (softsbrsymb) break
										|	(to-paren compose/deep [(to-set-word softsymb) take/part (softsymb) n: n - 1]) fail
										] 
										if (to-paren compose/deep [any [not n n < (softrpt/1)]]) fail
									|	(softsymb) (softsbrsymb)
									]
								]
								lazy [
									compose/deep [
										s: 
										copy (softsymb) [
											thru [
												if (to-paren compose [(softrpt/1) > 0]) (softrpt/1) (seq) 
											| 	none
											] 	to (softsbrsymb)
										] 
										;(to-paren compose/deep [probe reduce [2 r s (softsymb)]])
										(to-paren compose/deep [n: length? (softsymb)])
										while not [
											if (to-paren compose/deep [n > (softrpt/2)]) reject
										|	if (to-paren compose/deep [parse (softsymb) [(rpt) (seq) end]]) break
										|	reject
										]
										if (to-paren compose/deep [n > (softrpt/2)]) reject
									|	if (
											to-paren compose [
												not (to-paren compose/deep [parse (softsymb) [(rpt) (seq) end]])
											]
										) reject
									|	(softsymb) (softsbrsymb) 
									]
								]
							]
							append longout: take longsoftgrp compose/deep [
								[(softsymb) (softsbrsymb)]
							]
						]
					]
				)]
			]
		] 
		if (cs-open?) [
			cause-error 'user 'message ["Character class unclosed!"]
		]
		unless ending = 'loose [
			foreach out [shortout longout][
				append get out switch ending [
					strict 		[[ahead [opt #"^/" end]]] 
					strictissima 	[[ahead end]]
				]
			]
		]
	]

	finish: func [inner /local repl][
		insert symbols [s e]
		build copy inner 

		append _spec either replace [
			switch type?/word replacement [
				string! [
					parse replacement [collect set repl any [replref | backref | char]]
					if debugging [probe compose/deep ["repl:" [(repl)]]]
					compose/deep [
						s: copy (full-match) (either starting = 'loose ['thru][]) [(longout)] 
						(
							to-paren append copy either glob [
								[append select]
							][
								[put]
							] compose [(symbol) 0 (full-match)]
						)
						:s change [(shortout)] (to-paren append/only copy [rejoin] compose [(repl)])
					]
				]
				block! [
					replacement: either empty? replacement [clear []][
						map/only replacement func [rep][
							parse reduce rep [collect any [replref | backref | char]]
						]
					]
					compose/deep [
						s: copy (full-match) (either starting = 'loose ['thru][]) [(longout)] 
						(
							to-paren append copy either glob [
								[append select]
							][
								[put]
							] compose [(symbol) 0 (full-match)]
						)
						:s change [(shortout)] 
						(to-paren append/only copy [rejoin] 
							compose [
								(to-paren compose/deep [
									either reduce pick replacement length? select (symbol) 0 [
										reduce pick replacement length? select (symbol) 0
									][(full-match)]
								])
							]
						)
					]					
				]
				map! [
					
				]
				function! [
					
				]
			]
		][
			compose/deep [
				copy (full-match) (either starting = 'loose ['thru][]) [(longout)] 
				(
					to-paren append copy either glob [
						[append select]
					][
						[put]
					] compose [(symbol) 0 (full-match)]
				)
			]
		]
comment {}
		all [
			starting = 'loose 
			either empty? lookbehind [
				insert _spec compose/deep [to [(shortout)]] 
			][
				insert _spec compose/deep [(
					either 'not = lookmode [
						lbsymb: next-lb compose/deep [
							while [
								s: copy (lbsymb) to [(shortout)] e: ;(to-paren [print [s e]])
								if (
									to-paren compose/deep [
										parse (lbsymb) [thru [(copy take lookbehind)] end]
									]
								) skip 
							] thru (lbsymb)
						]
					][
						compose/deep [
							to [(copy first lookbehind) (shortout)]
							thru [(copy take lookbehind)]
						]
					]
				)]
			]
			ending = 'loose
			glob 
			;_spec: append/only copy [some] append copy compose [(_spec)] [ | skip]
			_spec: append/only copy [some] compose [(_spec)]
		]
		append _spec switch ending [
			strict 		[[opt #"^/" end]] 
			strictissima 	['end]
			loose 		[[to end]]
		]
		if debugging [probe compose/deep ["_spec:" [(_spec)]]]
	]
	flavors: [JS [empty-cs?: true]]											; just a probe so far
	
	set 'regex func [
		"Regex to parse converter"
		re [string!]  
		/parse 
			str [any-string!] 							"string to parse"
		/debug										"turns on debugging"
		/spec 										"prints out generated spec"
		/modes 										"passes all the modes in one string"  
			optstr [string!] 							"shortcoded modes"
		/icase		"see next"	/i					"turns on case-insensitivity"
		/multiline	"see next"	/m					"lets ^^ and $ match beginning and end of line also"
		/singleline	"see next"	/s					"lets dot match linebreaks also"
		/freespace	"see next"	/x					"lets you use whitespace, to make re more readable"
		/global		"see next"	/g					"global mode, puts captured strings into block"
		/simplex	"see next"	/n					"non-numbering mode, only named groups are captured"
		/possessive	"see next"	/+					"possessive mode, no soft quantifiers"
		/replace 									"Captured matches are used in replacements"
			replacement [string! block!] 				{String replaces any overall matches, 
														block replaces global overall matches in order} 
														;and map specifies numbered and named groups to use in replacement}
		/try										"try specific flavor of regexp"	
			flavor	[word!] 							"flavor to try"
		/local inner
	][  
		self/clean
		insert symbols full-match
		debugging: 			any [debug off]
		cs-num:   			0
		br-num:				0
		soft-num:			0
		lb-num:				0
		_spec:				make block! 100
		inner: 				clear ""
		rpt: seq: 			clear []
		rpt1: seq1:			clear []
		nocase:				any [icase 	i all [modes find optstr "i"] off]
		self/ml: 			any [multiline  m all [modes find optstr "m"] off]
		self/sl: 			any [singleline s all [modes find optstr "s"] off]
		freesp: 			any [freespace 	x all [modes find optstr "x"] off]
		glob:				any [global 	g all [modes find optstr "g"] off]
		simp:				any [simplex 	n all [modes find optstr "n"] off]
		poss:				any [possessive + all [modes find optstr "+"] off]
		self/replace:		any [replace off]
		self/replacement:	any [replacement none]
		ccgrp: 				clear []
		char-class: 		clear []
		char-set:			charset ""
		empty-cs?: 			false
		cs-open?: 			false
		neg?:				false
		levelgrp: 			make block! 5
		levelgr2:			make block! 5
		levelcap: 			make block! 5
		levelsym: 			make block! 5
		levelnam: 			make block! 5
		capturing: 			off
		to-short: 			off
		longsoftgrp:		clear []
		shortsoftgrp: 		clear []
		longsoft: 			clear []
		shortsoft: 			clear []
		soft-prepare:		clear []
		softvars:			clear []
		soft: 				off
		softsbrsymb:		none
		softsymb:			none
		defs: 				make block! 5
		sbrdef1:			none
		sbrdef2:			none
		lookaround: 		clear []
		lookbehind: 		clear []
		look:				none

		set symbol either glob [
			make map! reduce [0 make block! 10]
		][
			make map! reduce [0 make string! 20]
		]
		
		if freesp [system/words/parse re rmfree cs-open?: no]
		
		if try [
			unless do select flavors flavor [
				print append to-string flavor " is not supported :("
			]
		]
		
		system/words/parse re [
			[
				  ["\A" | "\`" | if (not ml) ["^^"]]	(starting: 'strict)
				| 										(starting: 'loose)
			]
			copy inner [
				to [
					#"\" copy s skip 
					if (find [39 90] to-integer to-char s) end 					;#"Z" #"'"
				| 	if (not ml) #"$" end
				]										(ending: 'strict) 
			| 	to [
					#"\" copy s skip 
					if (122 = to-integer to-char s) end
				]										(ending: 'strictissima) ;#"z" 
			| 	to end									(ending: 'loose)
			] 
			(finish copy inner)
		]
		_spec: either empty? defs [_spec][head insert/only _spec to-paren defs]
		set 're-symbols symbols
		bind _spec: load mold _spec re-ctx										; rebinding corrects some strange behavior
		if spec [print mold _spec]
		either parse [
			return either nocase [
				system/words/parse str _spec
			][
				system/words/parse/case str _spec
			]
		][
			return _spec
		]
	]
]
comment {}

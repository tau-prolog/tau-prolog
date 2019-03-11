var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// append/2
			"append/2": [
				new pl.type.Rule(new pl.type.Term("append", [new pl.type.Var("X"),new pl.type.Var("L")]), new pl.type.Term("foldl", [new pl.type.Term("append", []),new pl.type.Var("X"),new pl.type.Term("[]", []),new pl.type.Var("L")]))
			],

			// append/3
			"append/3": [
				new pl.type.Rule(new pl.type.Term("append", [new pl.type.Term("[]", []),new pl.type.Var("X"),new pl.type.Var("X")]), null),
				new pl.type.Rule(new pl.type.Term("append", [new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("S")])]), new pl.type.Term("append", [new pl.type.Var("T"),new pl.type.Var("X"),new pl.type.Var("S")]))
			],
			
			// member/2
			"member/2": [
				new pl.type.Rule(new pl.type.Term("member", [new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("_")])]), null),
				new pl.type.Rule(new pl.type.Term("member", [new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("_"),new pl.type.Var("Xs")])]), new pl.type.Term("member", [new pl.type.Var("X"),new pl.type.Var("Xs")]))
			],
			
			// permutation/2
			"permutation/2": [
				new pl.type.Rule(new pl.type.Term("permutation", [new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("permutation", [new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("S")]), new pl.type.Term(",", [new pl.type.Term("permutation", [new pl.type.Var("T"),new pl.type.Var("P")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("P")]),new pl.type.Term("append", [new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("Y")]),new pl.type.Var("S")])])]))
			],
			
			// maplist/2
			"maplist/2": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("X")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("Xs")])]))
			],
			
			// maplist/3
			"maplist/3": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("A"),new pl.type.Var("As")]),new pl.type.Term(".", [new pl.type.Var("B"),new pl.type.Var("Bs")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("A"),new pl.type.Var("B")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("As"),new pl.type.Var("Bs")])]))
			],
			
			// maplist/4
			"maplist/4": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("A"),new pl.type.Var("As")]),new pl.type.Term(".", [new pl.type.Var("B"),new pl.type.Var("Bs")]),new pl.type.Term(".", [new pl.type.Var("C"),new pl.type.Var("Cs")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("A"),new pl.type.Var("B"),new pl.type.Var("C")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("As"),new pl.type.Var("Bs"),new pl.type.Var("Cs")])]))
			],
			
			// maplist/5
			"maplist/5": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("A"),new pl.type.Var("As")]),new pl.type.Term(".", [new pl.type.Var("B"),new pl.type.Var("Bs")]),new pl.type.Term(".", [new pl.type.Var("C"),new pl.type.Var("Cs")]),new pl.type.Term(".", [new pl.type.Var("D"),new pl.type.Var("Ds")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("A"),new pl.type.Var("B"),new pl.type.Var("C"),new pl.type.Var("D")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("As"),new pl.type.Var("Bs"),new pl.type.Var("Cs"),new pl.type.Var("Ds")])]))
			],
			
			// maplist/6
			"maplist/6": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("A"),new pl.type.Var("As")]),new pl.type.Term(".", [new pl.type.Var("B"),new pl.type.Var("Bs")]),new pl.type.Term(".", [new pl.type.Var("C"),new pl.type.Var("Cs")]),new pl.type.Term(".", [new pl.type.Var("D"),new pl.type.Var("Ds")]),new pl.type.Term(".", [new pl.type.Var("E"),new pl.type.Var("Es")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("A"),new pl.type.Var("B"),new pl.type.Var("C"),new pl.type.Var("D"),new pl.type.Var("E")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("As"),new pl.type.Var("Bs"),new pl.type.Var("Cs"),new pl.type.Var("Ds"),new pl.type.Var("Es")])]))
			],
			
			// maplist/7
			"maplist/7": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("A"),new pl.type.Var("As")]),new pl.type.Term(".", [new pl.type.Var("B"),new pl.type.Var("Bs")]),new pl.type.Term(".", [new pl.type.Var("C"),new pl.type.Var("Cs")]),new pl.type.Term(".", [new pl.type.Var("D"),new pl.type.Var("Ds")]),new pl.type.Term(".", [new pl.type.Var("E"),new pl.type.Var("Es")]),new pl.type.Term(".", [new pl.type.Var("F"),new pl.type.Var("Fs")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("A"),new pl.type.Var("B"),new pl.type.Var("C"),new pl.type.Var("D"),new pl.type.Var("E"),new pl.type.Var("F")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("As"),new pl.type.Var("Bs"),new pl.type.Var("Cs"),new pl.type.Var("Ds"),new pl.type.Var("Es"),new pl.type.Var("Fs")])]))
			],
			
			// maplist/8
			"maplist/8": [
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("A"),new pl.type.Var("As")]),new pl.type.Term(".", [new pl.type.Var("B"),new pl.type.Var("Bs")]),new pl.type.Term(".", [new pl.type.Var("C"),new pl.type.Var("Cs")]),new pl.type.Term(".", [new pl.type.Var("D"),new pl.type.Var("Ds")]),new pl.type.Term(".", [new pl.type.Var("E"),new pl.type.Var("Es")]),new pl.type.Term(".", [new pl.type.Var("F"),new pl.type.Var("Fs")]),new pl.type.Term(".", [new pl.type.Var("G"),new pl.type.Var("Gs")])]), new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P"),new pl.type.Var("A"),new pl.type.Var("B"),new pl.type.Var("C"),new pl.type.Var("D"),new pl.type.Var("E"),new pl.type.Var("F"),new pl.type.Var("G")]),new pl.type.Term("maplist", [new pl.type.Var("P"),new pl.type.Var("As"),new pl.type.Var("Bs"),new pl.type.Var("Cs"),new pl.type.Var("Ds"),new pl.type.Var("Es"),new pl.type.Var("Fs"),new pl.type.Var("Gs")])]))
			],
			
			// include/3
			"include/3": [
				new pl.type.Rule(new pl.type.Term("include", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("include", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("L")]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P"),new pl.type.Var("A")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("A"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term("[]", [])]),new pl.type.Var("B")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("F"),new pl.type.Var("B")]),new pl.type.Term(",", [new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("F")]),new pl.type.Term(",", [new pl.type.Term("=", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("S")])]),new pl.type.Term("!", [])])]),new pl.type.Term("=", [new pl.type.Var("L"),new pl.type.Var("S")])]),new pl.type.Term("include", [new pl.type.Var("P"),new pl.type.Var("T"),new pl.type.Var("S")])])])])]))
			],
			
			// exclude/3
			"exclude/3": [
				new pl.type.Rule(new pl.type.Term("exclude", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
				new pl.type.Rule(new pl.type.Term("exclude", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("S")]), new pl.type.Term(",", [new pl.type.Term("exclude", [new pl.type.Var("P"),new pl.type.Var("T"),new pl.type.Var("E")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P"),new pl.type.Var("L")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term("[]", [])]),new pl.type.Var("Q")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("R"),new pl.type.Var("Q")]),new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("R")]),new pl.type.Term(",", [new pl.type.Term("!", []),new pl.type.Term("=", [new pl.type.Var("S"),new pl.type.Var("E")])])]),new pl.type.Term("=", [new pl.type.Var("S"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("E")])])])])])])]))
			],
			
			// foldl/4
			"foldl/4": [
				new pl.type.Rule(new pl.type.Term("foldl", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Var("I"),new pl.type.Var("I")]), null),
				new pl.type.Rule(new pl.type.Term("foldl", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("I"),new pl.type.Var("R")]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P"),new pl.type.Var("L")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("I"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])])])]),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P2"),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P2")]),new pl.type.Term("foldl", [new pl.type.Var("P"),new pl.type.Var("T"),new pl.type.Var("X"),new pl.type.Var("R")])])])])]))
			],
			
			// select/3
			"select/3": [
				new pl.type.Rule(new pl.type.Term("select", [new pl.type.Var("E"),new pl.type.Term(".", [new pl.type.Var("E"),new pl.type.Var("Xs")]),new pl.type.Var("Xs")]), null),
				new pl.type.Rule(new pl.type.Term("select", [new pl.type.Var("E"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Ys")])]), new pl.type.Term("select", [new pl.type.Var("E"),new pl.type.Var("Xs"),new pl.type.Var("Ys")]))
			],
			
			// sum_list/2
			"sum_list/2": [
				new pl.type.Rule(new pl.type.Term("sum_list", [new pl.type.Term("[]", []),new pl.type.Num(0, false)]), null),
				new pl.type.Rule(new pl.type.Term("sum_list", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Var("S")]), new pl.type.Term(",", [new pl.type.Term("sum_list", [new pl.type.Var("Xs"),new pl.type.Var("Y")]),new pl.type.Term("is", [new pl.type.Var("S"),new pl.type.Term("+", [new pl.type.Var("X"),new pl.type.Var("Y")])])]))
			],
			
			// max_list/2
			"max_list/2": [
				new pl.type.Rule(new pl.type.Term("max_list", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])]),new pl.type.Var("X")]), null),
				new pl.type.Rule(new pl.type.Term("max_list", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Var("S")]), new pl.type.Term(",", [new pl.type.Term("max_list", [new pl.type.Var("Xs"),new pl.type.Var("Y")]),new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term(">=", [new pl.type.Var("X"),new pl.type.Var("Y")]),new pl.type.Term(",", [new pl.type.Term("=", [new pl.type.Var("S"),new pl.type.Var("X")]),new pl.type.Term("!", [])])]),new pl.type.Term("=", [new pl.type.Var("S"),new pl.type.Var("Y")])])]))
			],
			
			// min_list/2
			"min_list/2": [
				new pl.type.Rule(new pl.type.Term("min_list", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])]),new pl.type.Var("X")]), null),
				new pl.type.Rule(new pl.type.Term("min_list", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Var("S")]), new pl.type.Term(",", [new pl.type.Term("min_list", [new pl.type.Var("Xs"),new pl.type.Var("Y")]),new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term("=<", [new pl.type.Var("X"),new pl.type.Var("Y")]),new pl.type.Term(",", [new pl.type.Term("=", [new pl.type.Var("S"),new pl.type.Var("X")]),new pl.type.Term("!", [])])]),new pl.type.Term("=", [new pl.type.Var("S"),new pl.type.Var("Y")])])]))
			],
			
			// prod_list/2
			"prod_list/2": [
				new pl.type.Rule(new pl.type.Term("prod_list", [new pl.type.Term("[]", []),new pl.type.Num(1, false)]), null),
				new pl.type.Rule(new pl.type.Term("prod_list", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Var("S")]), new pl.type.Term(",", [new pl.type.Term("prod_list", [new pl.type.Var("Xs"),new pl.type.Var("Y")]),new pl.type.Term("is", [new pl.type.Var("S"),new pl.type.Term("*", [new pl.type.Var("X"),new pl.type.Var("Y")])])]))
			],
			
			// last/2
			"last/2": [
				new pl.type.Rule(new pl.type.Term("last", [new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])]),new pl.type.Var("X")]), null),
				new pl.type.Rule(new pl.type.Term("last", [new pl.type.Term(".", [new pl.type.Var("_"),new pl.type.Var("Xs")]),new pl.type.Var("X")]), new pl.type.Term("last", [new pl.type.Var("Xs"),new pl.type.Var("X")]))
			],
			
			// prefix/2
			"prefix/2": [
				new pl.type.Rule(new pl.type.Term("prefix", [new pl.type.Var("Part"),new pl.type.Var("Whole")]), new pl.type.Term("append", [new pl.type.Var("Part"),new pl.type.Var("_"),new pl.type.Var("Whole")]))
			],
			
			// nth0/3
			"nth0/3": [
				new pl.type.Rule(new pl.type.Term("nth0", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")]), new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("var", [new pl.type.Var("X")]),new pl.type.Term("nth", [new pl.type.Num(0, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("_")])]),new pl.type.Term(",", [new pl.type.Term(">=", [new pl.type.Var("X"),new pl.type.Num(0, false)]),new pl.type.Term(",", [new pl.type.Term("nth", [new pl.type.Num(0, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("_")]),new pl.type.Term("!", [])])])]))
			],
			
			// nth1/3
			"nth1/3": [
				new pl.type.Rule(new pl.type.Term("nth1", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")]), new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("var", [new pl.type.Var("X")]),new pl.type.Term("nth", [new pl.type.Num(1, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("_")])]),new pl.type.Term(",", [new pl.type.Term(">", [new pl.type.Var("X"),new pl.type.Num(0, false)]),new pl.type.Term(",", [new pl.type.Term("nth", [new pl.type.Num(1, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("_")]),new pl.type.Term("!", [])])])]))
			],
			
			// nth0/4
			"nth0/4": [
				new pl.type.Rule(new pl.type.Term("nth0", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("W")]), new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("var", [new pl.type.Var("X")]),new pl.type.Term("nth", [new pl.type.Num(0, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("W")])]),new pl.type.Term(",", [new pl.type.Term(">=", [new pl.type.Var("X"),new pl.type.Num(0, false)]),new pl.type.Term(",", [new pl.type.Term("nth", [new pl.type.Num(0, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("W")]),new pl.type.Term("!", [])])])]))
			],
			
			// nth1/4
			"nth1/4": [
				new pl.type.Rule(new pl.type.Term("nth1", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("W")]), new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("var", [new pl.type.Var("X")]),new pl.type.Term("nth", [new pl.type.Num(1, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("W")])]),new pl.type.Term(",", [new pl.type.Term(">", [new pl.type.Var("X"),new pl.type.Num(0, false)]),new pl.type.Term(",", [new pl.type.Term("nth", [new pl.type.Num(1, false),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z"),new pl.type.Var("W")]),new pl.type.Term("!", [])])])]))
			],
			
			// nth/5
			// DO NOT EXPORT
			"nth/5": [
				new pl.type.Rule(new pl.type.Term("nth", [new pl.type.Var("N"),new pl.type.Var("N"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Var("X"),new pl.type.Var("Xs")]), null),
				new pl.type.Rule(new pl.type.Term("nth", [new pl.type.Var("N"),new pl.type.Var("O"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Xs")]),new pl.type.Var("Y"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("Ys")])]), new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("M"),new pl.type.Term("+", [new pl.type.Var("N"),new pl.type.Num(1, false)])]),new pl.type.Term("nth", [new pl.type.Var("M"),new pl.type.Var("O"),new pl.type.Var("Xs"),new pl.type.Var("Y"),new pl.type.Var("Ys")])]))
			],
			
			// length/2
			"length/2": function( thread, point, atom ) {
				var list = atom.args[0], length = atom.args[1];
				if( !pl.type.is_variable( length ) && !pl.type.is_integer( length ) ) {
					thread.throw_error( pl.error.type( "integer", length, atom.indicator ) );
				} else if( pl.type.is_integer( length ) && length.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", length, atom.indicator ) );
				} else {
					var newgoal = new pl.type.Term("length", [list, new pl.type.Num(0, false), length]);
					if( pl.type.is_integer( length ) )
						newgoal = new pl.type.Term( ",", [newgoal, new pl.type.Term( "!", [] )] );
					thread.prepend( [new pl.type.State(point.goal.replace(newgoal), point.substitution, point)] );
				}
			},
			
			// length/3
			// DO NOT EXPORT
			"length/3": [
				new pl.type.Rule(new pl.type.Term("length", [new pl.type.Term("[]", []),new pl.type.Var("N"),new pl.type.Var("N")]), null),
				new pl.type.Rule(new pl.type.Term("length", [new pl.type.Term(".", [new pl.type.Var("_"),new pl.type.Var("X")]),new pl.type.Var("A"),new pl.type.Var("N")]), new pl.type.Term(",", [new pl.type.Term("succ", [new pl.type.Var("A"),new pl.type.Var("B")]),new pl.type.Term("length", [new pl.type.Var("X"),new pl.type.Var("B"),new pl.type.Var("N")])]))
			],
			
			// replicate/3
			"replicate/3": function( thread, point, atom ) {
				var elem = atom.args[0], times = atom.args[1], list = atom.args[2];
				if( pl.type.is_variable( times ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( times ) ) {
					thread.throw_error( pl.error.type( "integer", times, atom.indicator ) );
				} else if( times.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", times, atom.indicator ) );
				} else if( !pl.type.is_variable( list ) && !pl.type.is_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else {
					var replicate = new pl.type.Term( "[]" );
					for( var i = 0; i < times.value; i++ ) {
						replicate = new pl.type.Term( ".", [elem, replicate] );
					}
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [replicate, list] ) ), point.substitution, point )] );
				}
			},

			// sort/2
			"sort/2": function( thread, point, atom ) {
				var list = atom.args[0], expected = atom.args[1];
				if( pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( expected ) && !pl.type.is_fully_list( expected ) ) {
					thread.throw_error( pl.error.type( "list", expected, atom.indicator ) );
				} else {
					var arr = [];
					var pointer = list;
					while( pointer.indicator === "./2" ) {
						arr.push( pointer.args[0] );
						pointer = pointer.args[1];
					}
					if( pl.type.is_variable( pointer ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
					} else if( !pl.type.is_empty_list( pointer ) ) {
						thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
					} else {
						var sorted_arr = arr.sort( pl.compare );
						for( var i = sorted_arr.length-1; i > 0; i-- ) {
							if( sorted_arr[i].equals(sorted_arr[i-1]) )
								sorted_arr.splice(i,1);
						}
						var sorted_list = new pl.type.Term( "[]" );
						for( var i = sorted_arr.length-1; i >= 0; i-- ) {
							sorted_list = new pl.type.Term( ".", [sorted_arr[i], sorted_list] );
						}
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [sorted_list, expected] ) ), point.substitution, point )] );
					}
				}
			},
			
			// msort/2
			"msort/2": function( thread, point, atom ) {
				var list = atom.args[0], expected = atom.args[1];
				if( pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( expected ) && !pl.type.is_fully_list( expected ) ) {
					thread.throw_error( pl.error.type( "list", expected, atom.indicator ) );
				} else {
					var arr = [];
					var pointer = list;
					while( pointer.indicator === "./2" ) {
						arr.push( pointer.args[0] );
						pointer = pointer.args[1];
					}
					if( pl.type.is_variable( pointer ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
					} else if( !pl.type.is_empty_list( pointer ) ) {
						thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
					} else {
						var sorted_arr = arr.sort( pl.compare );
						var sorted_list = new pl.type.Term( "[]" );
						for( var i = sorted_arr.length - 1; i >= 0; i-- ) {
							sorted_list = new pl.type.Term( ".", [sorted_arr[i], sorted_list] );
						}
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [sorted_list, expected] ) ), point.substitution, point )] );
					}
				}
			},
			
			// keysort/2
			"keysort/2": function( thread, point, atom ) {
				var list = atom.args[0], expected = atom.args[1];
				if( pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( expected ) && !pl.type.is_fully_list( expected ) ) {
					thread.throw_error( pl.error.type( "list", expected, atom.indicator ) );
				} else {
					var arr = [];
					var elem;
					var pointer = list;
					while( pointer.indicator === "./2" ) {
						elem = pointer.args[0];
						if( pl.type.is_variable( elem ) ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( !pl.type.is_term( elem ) || elem.indicator !== "-/2" ) {
							thread.throw_error( pl.error.type( "pair", elem, atom.indicator ) );
							return;
						}
						elem.args[0].pair = elem.args[1];
						arr.push( elem.args[0] );
						pointer = pointer.args[1];
					}
					if( pl.type.is_variable( pointer ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
					} else if( !pl.type.is_empty_list( pointer ) ) {
						thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
					} else {
						var sorted_arr = arr.sort( pl.compare );
						var sorted_list = new pl.type.Term( "[]" );
						for( var i = sorted_arr.length - 1; i >= 0; i-- ) {
							sorted_list = new pl.type.Term( ".", [new pl.type.Term( "-", [sorted_arr[i], sorted_arr[i].pair] ), sorted_list] );
							delete sorted_arr[i].pair;
						}
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [sorted_list, expected] ) ), point.substitution, point )] );
					}
				}
			},
			
			// take/3
			"take/3": function( thread, point, atom ) {
				var number = atom.args[0], list = atom.args[1], take = atom.args[2];
				if( pl.type.is_variable( list ) || pl.type.is_variable( number ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else if( !pl.type.is_integer( number ) ) {
					thread.throw_error( pl.error.type( "integer", number, atom.indicator ) );
				} else if( !pl.type.is_variable( take ) && !pl.type.is_list( take ) ) {
					thread.throw_error( pl.error.type( "list", take, atom.indicator ) );
				} else {
					var i = number.value;
					var arr = [];
					var pointer = list;
					while( i > 0 && pointer.indicator === "./2" ) {
						arr.push( pointer.args[0] );
						pointer = pointer.args[1];
						i--;
					}
					if( i === 0 ) {
						var new_list = new pl.type.Term( "[]" );
						for( var i = arr.length - 1; i >= 0; i-- ) {
							new_list = new pl.type.Term( ".", [arr[i], new_list] );
						}
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [new_list, take] ) ), point.substitution, point )] );
					}
				}
			},
			
			// drop/3
			"drop/3": function( thread, point, atom ) {
				var number = atom.args[0], list = atom.args[1], drop = atom.args[2];
				if( pl.type.is_variable( list ) || pl.type.is_variable( number ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else if( !pl.type.is_integer( number ) ) {
					thread.throw_error( pl.error.type( "integer", number, atom.indicator ) );
				} else if( !pl.type.is_variable( drop ) && !pl.type.is_list( drop ) ) {
					thread.throw_error( pl.error.type( "list", drop, atom.indicator ) );
				} else {
					var i = number.value;
					var arr = [];
					var pointer = list;
					while( i > 0 && pointer.indicator === "./2" ) {
						arr.push( pointer.args[0] );
						pointer = pointer.args[1];
						i--;
					}
					if( i === 0 )
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [pointer, drop] ) ), point.substitution, point )] );
				}
			},
			
			// reverse/2
			"reverse/2": function( thread, point, atom ) {
				var list = atom.args[0], reversed = atom.args[1];
				var ins_list = pl.type.is_instantiated_list( list );
				var ins_reversed = pl.type.is_instantiated_list( reversed );
				if( pl.type.is_variable( list ) && pl.type.is_variable( reversed ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( list ) && !pl.type.is_fully_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else if( !pl.type.is_variable( reversed ) && !pl.type.is_fully_list( reversed ) ) {
					thread.throw_error( pl.error.type( "list", reversed, atom.indicator ) );
				} else if( !ins_list && !ins_reversed ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else {
					var pointer = ins_list ? list : reversed;
					var new_reversed = new pl.type.Term( "[]", [] );
					while( pointer.indicator === "./2" ) {
						new_reversed = new pl.type.Term( ".", [pointer.args[0], new_reversed] );
						pointer = pointer.args[1];
					}
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [new_reversed, ins_list ? reversed : list] ) ), point.substitution, point )] );
				}
			},
			
			// list_to_set/2
			"list_to_set/2": function( thread, point, atom ) {
				var list = atom.args[0], lset = atom.args[1];
				if( pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else {
					var pointer = list;
					var elems = [];
					while( pointer.indicator === "./2" ) {
						elems.push( pointer.args[0] );
						pointer = pointer.args[1];
					}
					if( pl.type.is_variable( pointer ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
					} else if( !pl.type.is_term( pointer ) || pointer.indicator !== "[]/0" ) {
						thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
					} else {
						var arr = [], nub = new pl.type.Term( "[]", [] );
						var match;
						for( var i = 0; i < elems.length; i++ ) {
							match = false
							for( var j = 0; j < arr.length && !match; j++ ) {
								match = pl.compare( elems[i], arr[j] ) === 0;
							}
							if( !match )
								arr.push( elems[i] );
						}
						for( i = arr.length - 1; i >= 0; i-- )
							nub = new pl.type.Term( ".", [arr[i],nub] );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [lset,nub] ) ), point.substitution, point )] );
					}
				}
			}
			
		};
	};
	
	var exports = ["append/2", "append/3", "member/2", "permutation/2", "maplist/2", "maplist/3", "maplist/4", "maplist/5", "maplist/6", "maplist/7", "maplist/8", "include/3", "exclude/3", "foldl/4", "sum_list/2", "max_list/2", "min_list/2", "prod_list/2", "last/2", "prefix/2", "nth0/3", "nth1/3", "nth0/4", "nth1/4", "length/2", "replicate/3", "select/3", "sort/2", "msort/2", "keysort/2", "take/3", "drop/3", "reverse/2", "list_to_set/2"];


	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "lists", predicates(), exports );
		};
	} else {
		new pl.type.Module( "lists", predicates(), exports );
	}

})( pl );

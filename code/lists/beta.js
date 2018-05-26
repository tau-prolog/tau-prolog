new pl.type.Module( "lists", {

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
	
	// map/3
	"map/3": [
		new pl.type.Rule(new pl.type.Term("map", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
		new pl.type.Rule(new pl.type.Term("map", [new pl.type.Var("F"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("S")])]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("F"),new pl.type.Var("L")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])])]),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("F2"),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("F2")]),new pl.type.Term("map", [new pl.type.Var("F"),new pl.type.Var("T"),new pl.type.Var("S")])])])])]))
	],
	
	// maplist/3
	"maplist/3": [
		new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
		new pl.type.Rule(new pl.type.Term("maplist", [new pl.type.Var("F"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("S")])]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("F"),new pl.type.Var("L")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])])]),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("F2"),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("F2")]),new pl.type.Term("maplist", [new pl.type.Var("F"),new pl.type.Var("T"),new pl.type.Var("S")])])])])]))
	],
	
	// filter/3
	"filter/3": [
		new pl.type.Rule(new pl.type.Term("filter", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]), null),
		new pl.type.Rule(new pl.type.Term("filter", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("L")]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P"),new pl.type.Var("A")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("A"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term("[]", [])]),new pl.type.Var("B")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("F"),new pl.type.Var("B")]),new pl.type.Term(",", [new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("F")]),new pl.type.Term(",", [new pl.type.Term("=", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("S")])]),new pl.type.Term("!", [])])]),new pl.type.Term("=", [new pl.type.Var("L"),new pl.type.Var("S")])]),new pl.type.Term("filter", [new pl.type.Var("P"),new pl.type.Var("T"),new pl.type.Var("S")])])])])]))
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
	
	// reduce/4
	"reduce/4": [
		new pl.type.Rule(new pl.type.Term("reduce", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Var("I"),new pl.type.Var("I")]), null),
		new pl.type.Rule(new pl.type.Term("reduce", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("I"),new pl.type.Var("R")]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P"),new pl.type.Var("L")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("I"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])])])]),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P2"),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P2")]),new pl.type.Term("reduce", [new pl.type.Var("P"),new pl.type.Var("T"),new pl.type.Var("X"),new pl.type.Var("R")])])])])]))
	],
	
	// foldl/4
	"foldl/4": [
		new pl.type.Rule(new pl.type.Term("foldl", [new pl.type.Var("_"),new pl.type.Term("[]", []),new pl.type.Var("I"),new pl.type.Var("I")]), null),
		new pl.type.Rule(new pl.type.Term("foldl", [new pl.type.Var("P"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("I"),new pl.type.Var("R")]), new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P"),new pl.type.Var("L")]),new pl.type.Term(",", [new pl.type.Term("append", [new pl.type.Var("L"),new pl.type.Term(".", [new pl.type.Var("I"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Term("[]", [])])])]),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("=..", [new pl.type.Var("P2"),new pl.type.Var("L2")]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("P2")]),new pl.type.Term("foldl", [new pl.type.Var("P"),new pl.type.Var("T"),new pl.type.Var("X"),new pl.type.Var("R")])])])])]))
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
	
	// nth0/3
	"nth0/3": [
		new pl.type.Rule(new pl.type.Term("nth0", [new pl.type.Num(0, false),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("_")]),new pl.type.Var("X")]), null),
		new pl.type.Rule(new pl.type.Term("nth0", [new pl.type.Var("N"),new pl.type.Term(".", [new pl.type.Var("_"),new pl.type.Var("Xs")]),new pl.type.Var("X")]), new pl.type.Term(",", [new pl.type.Term(">", [new pl.type.Var("N"),new pl.type.Num(0, false)]),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("M"),new pl.type.Term("-", [new pl.type.Var("N"),new pl.type.Num(1, false)])]),new pl.type.Term("nth0", [new pl.type.Var("M"),new pl.type.Var("Xs"),new pl.type.Var("X")])])]))
	],
	
	// nth1/3
	"nth1/3": [
		new pl.type.Rule(new pl.type.Term("nth1", [new pl.type.Num(1, false),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("_")]),new pl.type.Var("X")]), null),
		new pl.type.Rule(new pl.type.Term("nth1", [new pl.type.Var("N"),new pl.type.Term(".", [new pl.type.Var("_"),new pl.type.Var("Xs")]),new pl.type.Var("X")]), new pl.type.Term(",", [new pl.type.Term(">", [new pl.type.Var("N"),new pl.type.Num(1, false)]),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("M"),new pl.type.Term("-", [new pl.type.Var("N"),new pl.type.Num(1, false)])]),new pl.type.Term("nth1", [new pl.type.Var("M"),new pl.type.Var("Xs"),new pl.type.Var("X")])])]))
	],
	
	// nth0/4
	"nth0/4": [
		new pl.type.Rule(new pl.type.Term("nth0", [new pl.type.Num(0, false),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("H"),new pl.type.Var("T")]), null),
		new pl.type.Rule(new pl.type.Term("nth0", [new pl.type.Var("N"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("Xs")])]), new pl.type.Term(",", [new pl.type.Term(">", [new pl.type.Var("N"),new pl.type.Num(0, false)]),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("M"),new pl.type.Term("-", [new pl.type.Var("N"),new pl.type.Num(1, false)])]),new pl.type.Term("nth0", [new pl.type.Var("M"),new pl.type.Var("T"),new pl.type.Var("X"),new pl.type.Var("Xs")])])]))
	],
	
	// nth1/4
	"nth1/4": [
		new pl.type.Rule(new pl.type.Term("nth1", [new pl.type.Num(1, false),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("H"),new pl.type.Var("T")]), null),
		new pl.type.Rule(new pl.type.Term("nth1", [new pl.type.Var("N"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("T")]),new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("H"),new pl.type.Var("Xs")])]), new pl.type.Term(",", [new pl.type.Term(">", [new pl.type.Var("N"),new pl.type.Num(1, false)]),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("M"),new pl.type.Term("-", [new pl.type.Var("N"),new pl.type.Num(1, false)])]),new pl.type.Term("nth1", [new pl.type.Var("M"),new pl.type.Var("T"),new pl.type.Var("X"),new pl.type.Var("Xs")])])]))
	],
	
	// length/2
	"length/2": function( exec, point, atom ) {
		var list = atom.args[0], length = atom.args[1];
		if( !pl.type.is_variable( length ) && !pl.type.is_integer( length ) ) {
			exec.throwError( pl.error.type( "integer", length, atom.indicator ) );
		} else if( pl.type.is_integer( length ) && length.value < 0 ) {
			exec.throwError( pl.error.domain( "not_less_than_zero", length, atom.indicator ) );
		} else {
			var count = 0;
			var pointer = list;
			if( pl.type.is_term( list ) ) {
				while( pointer.indicator === "./2" ) {
					count++;
					pointer = pointer.args[1];
				}
			}
			if( pl.type.is_variable( pointer ) && pl.type.is_variable( length ) ) {
				var s1 = new pl.type.State( point.goal.replace( new pl.type.Term( ",", [
					new pl.type.Term( "=", [length, new pl.type.Num( 0, false )] ),
					new pl.type.Term( "=", [list, new pl.type.Term( "[]", [] )] )
				] ) ), point.substitution, point.parent );
				var newvar = exec.next_free_variable();
				var newcount = exec.next_free_variable();
				var s2 = new pl.type.State( point.goal.replace( new pl.type.Term( ",", [
					new pl.type.Term( "\\==", [pointer, length] ),
					new pl.type.Term( ",", [
						new pl.type.Term( "length", [newvar, newcount] ),
						new pl.type.Term( ",", [
							new pl.type.Term( "is", [length, new pl.type.Term( "+", [newcount, new pl.type.Num( 1, false )] )] ),
							new pl.type.Term( "=", [list, new pl.type.Term( ".", [exec.next_free_variable(), newvar] )] )
						] )
					] )
				] ) ), point.substitution, point.parent );
				exec.prepend( [s1,s2] );
			} else if( pl.type.is_term( pointer ) && pointer.indicator === "[]/0" ) {
				exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [length, new pl.type.Num( count, false )] ) ), point.substitution, point.parent )] );
			} else {
				var vars = [];
				var new_list = new pl.type.Term( "[]", [] );
				for( var i = 0; i < length.value; i++ )
					vars.push( exec.next_free_variable() );
				for( var i = length.value-1; i >= 0; i-- ) {
					new_list = new pl.type.Term( ".", [vars[i], new_list] );
				}
				exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [list, new_list] ) ), point.substitution, point.parent )] );
			}
		}
	},
	
	// replicate/3
	"replicate/3": function( exec, point, atom ) {
		var elem = atom.args[0], times = atom.args[1], list = atom.args[2];
		if( pl.type.is_variable( times ) ) {
			exec.throwError( pl.error.instantiation( atom.indicator ) );
		} else if( !pl.type.is_integer( times ) ) {
			exec.throwError( pl.error.type( "integer", times, atom.indicator ) );
		} else if( times.value < 0 ) {
			exec.throwError( pl.error.domain( "not_less_than_zero", times, atom.indicator ) );
		} else if( !pl.type.is_variable( list ) && !pl.type.is_list( list ) ) {
			exec.throwError( pl.error.type( "list", list, atom.indicator ) );
		} else {
			var replicate = new pl.type.Term( "[]" );
			for( var i = 0; i < times.value; i++ ) {
				replicate = new pl.type.Term( ".", [elem, replicate] );
			}
			exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [replicate, list] ) ), point.substitution, point.parent )] );
		}
	},
	
	// sort/2
	"sort/2": function( exec, point, atom ) {
		var list = atom.args[0], expected = atom.args[1];
		if( pl.type.is_variable( list ) ) {
			exec.throwError( pl.error.instantiation( atom.indicator ) );
		} else {
			var arr = [];
			var pointer = list;
			while( pointer.indicator === "./2" ) {
				arr.push( pointer.args[0] );
				pointer = pointer.args[1];
			}
			if( pl.type.is_variable( pointer ) ) {
				exec.throwError( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_empty_list( pointer ) ) {
				exec.throwError( pl.error.type( "list", list, atom.indicator ) );
			} else {
				var sorted_arr = arr.sort( pl.compare );
				var sorted_list = new pl.type.Term( "[]" );
				for( var i = sorted_arr.length - 1; i >= 0; i-- ) {
					sorted_list = new pl.type.Term( ".", [sorted_arr[i], sorted_list] );
				}
				exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [sorted_list, expected] ) ), point.substitution, point.parent )] );
			}
		}
	},
	
	// take/3
	"take/3": function( exec, point, atom ) {
		var number = atom.args[0], list = atom.args[1], take = atom.args[2];
		if( pl.type.is_variable( list ) || pl.type.is_variable( number ) ) {
			exec.throwError( pl.error.instantiation( atom.indicator ) );
		} else if( !pl.type.is_list( list ) ) {
			exec.throwError( pl.error.type( "list", list, atom.indicator ) );
		} else if( !pl.type.is_integer( number ) ) {
			exec.throwError( pl.error.type( "integer", number, atom.indicator ) );
		} else if( !pl.type.is_variable( take ) && !pl.type.is_list( take ) ) {
			exec.throwError( pl.error.type( "list", take, atom.indicator ) );
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
				exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [new_list, take] ) ), point.substitution, point.parent )] );
			}
		}
	},
	
	// drop/3
	"drop/3": function( exec, point, atom ) {
		var number = atom.args[0], list = atom.args[1], drop = atom.args[2];
		if( pl.type.is_variable( list ) || pl.type.is_variable( number ) ) {
			exec.throwError( pl.error.instantiation( atom.indicator ) );
		} else if( !pl.type.is_list( list ) ) {
			exec.throwError( pl.error.type( "list", list, atom.indicator ) );
		} else if( !pl.type.is_integer( number ) ) {
			exec.throwError( pl.error.type( "integer", number, atom.indicator ) );
		} else if( !pl.type.is_variable( drop ) && !pl.type.is_list( drop ) ) {
			exec.throwError( pl.error.type( "list", drop, atom.indicator ) );
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
				exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [pointer, drop] ) ), point.substitution, point.parent )] );
		}
	},
	
	// reverse/2
	"reverse/2": function( exec, point, atom ) {
		var list = atom.args[0], reversed = atom.args[1];
		var ins_list = pl.type.is_instantiated_list( list );
		var ins_reversed = pl.type.is_instantiated_list( reversed );
		if( pl.type.is_variable( list ) && pl.type.is_variable( reversed ) ) {
			exec.throwError( pl.error.instantiation( atom.indicator ) );
		} else if( !pl.type.is_variable( list ) && !pl.type.is_fully_list( list ) ) {
			exec.throwError( pl.error.type( "list", list, atom.indicator ) );
		} else if( !pl.type.is_variable( reversed ) && !pl.type.is_fully_list( reversed ) ) {
			exec.throwError( pl.error.type( "list", reversed, atom.indicator ) );
		} else if( !ins_list && !ins_reversed ) {
			exec.throwError( pl.error.instantiation( atom.indicator ) );
		} else {
			var pointer = ins_list ? list : reversed;
			var new_reversed = new pl.type.Term( "[]", [] );
			while( pointer.indicator === "./2" ) {
				new_reversed = new pl.type.Term( ".", [pointer.args[0], new_reversed] );
				pointer = pointer.args[1];
			}
			exec.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [new_reversed, ins_list ? reversed : list] ) ), point.substitution, point.parent )] );
		}
	}
	
} );

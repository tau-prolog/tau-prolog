var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// time/1
			"time/1": [
				new pl.type.Rule(new pl.type.Term("time", [new pl.type.Var("Goal")]), new pl.type.Term(",", [new pl.type.Term("get_time", [new pl.type.Var("T0")]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("Goal")]),new pl.type.Term(",", [new pl.type.Term("get_time", [new pl.type.Var("T1")]),new pl.type.Term(",", [new pl.type.Term("!", []),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("T"),new pl.type.Term("-", [new pl.type.Var("T1"),new pl.type.Var("T0")])]),new pl.type.Term("write", [new pl.type.Var("T")])])])])])]))
			],
			
			// statistics/0
			"statistics/0": function( thread, point, atom ) {
				var stats = "% Tau Prolog statistics\n";
				for(var x in statistics)
					stats += "%%% " + x + ": " + statistics[x](thread).toString() + "\n";
				thread.prepend([new pl.type.State(
					point.goal.replace(new pl.type.Term("write", [new pl.type.Term(stats)])),
					point.substitution,
					point
				)]);
			},
			
			// statistics/2
			"statistics/2": function( thread, point, atom ) {
				var key = atom.args[0], value = atom.args[1];
				if( !pl.type.is_variable( key ) && !pl.type.is_atom( key ) ) {
					thread.throw_error( pl.error.type( "atom", key, atom.indicator ) );
				} else if( !pl.type.is_variable( key ) && statistics[key.id] === undefined ) {
					thread.throw_error( pl.error.domain( "statistics_key", key, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( key ) ) {
						var value_ = statistics[key.id]( thread );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, value_] ) ), point.substitution, point )] );
					} else {
						var states = [];
						for( var x in statistics ) {
							var value_ = statistics[x]( thread );
							states.push( new pl.type.State( point.goal.replace(
								new pl.type.Term( ",", [
									new pl.type.Term( "=", [key, new pl.type.Term( x, [] )] ),
									new pl.type.Term( "=", [value, value_] )
								] )
							), point.substitution, point ) );
						}
						thread.prepend( states );
					}
				}
			}

		};
	};
	
	var exports = ["time/1", "statistics/0", "statistics/2"];
	
	var statistics = {
		
		// Total number of defined atoms
		atoms: function( thread ) {
			return new pl.type.Num( pl.statistics.getCountTerms(), false );
		},
		
		// Total number of clauses
		clauses: function( thread ) {
			var total = 0;
			for( var x in thread.session.rules )
				if( thread.session.rules[x] instanceof Array )
					total += thread.session.rules[x].length;
			for( var i = 0; i < thread.session.modules.length; i++ ) {
				var module = pl.module[thread.session.modules[i]];
				for( var j = 0; j < module.exports.length; j++ ) {
					var predicate = module.rules[module.exports[j]];
					if( predicate instanceof Array )
						total += predicate.length;
					}
			}
			return new pl.type.Num( total, false );
		},
		
		// Total cpu time
		cputime: function( thread ) {
			return new pl.type.Num( thread.cpu_time, false );
		},
		
		// wall time
		walltime: function( thread ) {
			return new pl.type.Num( Date.now(), false );
		},
		
		// Time stamp when thread was started
		epoch: function( thread ) {
			return new pl.type.Num( thread.epoch, false );
		},
		
		// Total number of resolution steps
		inferences: function( thread ) {
			return new pl.type.Num( thread.total_steps, false );
		},
		
		// Total number of defined modules
		modules:  function(thread) {
			var nb_modules = 0;
			for(var _module_id in thread.session.modules)
				nb_modules++;
			return new pl.type.Num(nb_modules, false);
		},
		
		// Total number of predicates
		predicates: function( thread ) {
			var total = Object.keys( thread.session.rules ).length;
			for( var i = 0; i < thread.session.modules.length; i++ ) {
				var module = pl.module[thread.session.modules[i]];
				total += module.exports.length;
			}
			return new pl.type.Num( total, false );
		},
		
		// Total number of threads in current session
		threads: function( thread ) {
			return new pl.type.Num( thread.session.total_threads, false );
		}
		
	};

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "statistics", predicates(), exports );
		};
	} else {
		new pl.type.Module( "statistics", predicates(), exports );
	}

})( pl );

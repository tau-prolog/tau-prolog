var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// time/1
			"time/1": function( thread, point, atom ) {
				var goal = atom.args[0];
				if( pl.type.is_variable( goal ) ) {
					thread.throw_error( pl.error.instantiation( thread.level ) );
				} else if( !pl.type.is_callable( goal ) ) {
					thread.throw_error( pl.error.type( "callable", goal, thread.level ) );
				} else {
					var points = thread.points;
					thread.points = [new pl.type.State( goal, point.substitution, point )];
					var t0 = Date.now();
					var c0 = pl.statistics.getCountTerms();
					var i0 = thread.total_steps;
					var format_success = thread.session.format_success;
					var format_error = thread.session.format_error;
					thread.session.format_success = function(x) { return x.substitution; };
					thread.session.format_error = function(x) { return x.goal; };
					var callback = function( answer ) {
						var t1 = Date.now();
						var c1 = pl.statistics.getCountTerms();
						var i1 = thread.total_steps;
						var newpoints = thread.points;
						thread.points = points;
						thread.session.format_success = format_success;
						thread.session.format_error = format_error;
						if( pl.type.is_error( answer ) ) {
							thread.throw_error( answer.args[0] );
						} else if( answer === null ) {
							thread.points = points;
							thread.prepend( [point] );
							thread.__calls.shift()( null );
						} else {
							console.log( "% Tau Prolog: executed in " + (t1-t0) + " milliseconds, " + (c1-c0) + " atoms created, " + (i1-i0) + " resolution steps performed.");
							if( answer !== false ) {
								for( var i = 0; i < newpoints.length; i++ ) {
									if( newpoints[i].goal === null )
										newpoints[i].goal = new pl.type.Term( "true", [] );
									newpoints[i].goal = point.goal.replace( new pl.type.Term( "time", [newpoints[i].goal] ) );
								}
								thread.points = points;
								thread.prepend( newpoints );
								thread.prepend( [ new pl.type.State( point.goal.apply(answer).replace(null), answer, point ) ] );
							}
						}
					};
					thread.__calls.unshift( callback );
				}
			},
			
			// statistics/0
			"statistics/0": function( thread, point, atom ) {
				var stats = "% Tau Prolog statistics";
				for(var x in statistics)
					stats += "\n%%% " + x + ": " + statistics[x](thread).toString();
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
			return new pl.type.Num( thread.cpu_time , false );
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
		modules:  function( thread ) {
			return new pl.type.Num( thread.session.modules.length, false );
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
		
		// [CPU time, CPU time since last]
		runtime: function( thread ) {
			return new pl.type.Term( ".", [new pl.type.Num( thread.cpu_time, false ), new pl.type.Term( ".", [new pl.type.Num( thread.cpu_time_last, false ), new pl.type.Term( "[]", [] )] )] );
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

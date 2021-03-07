var pl;
(function( pl ) {

	var predicates = function() {
		
		return {

			// get_seed/1
			"get_seed/1": function(thread, point, atom) {
				var seed = atom.args[0];
				if(!pl.type.is_variable(seed) && !pl.type.is_integer(seed)) {
					thread.throw_error(pl.error.type("integer", seed, atom.indicator));
				} else {
					var current_seed = thread.get_random_generator().get_seed();
					thread.prepend([new pl.type.State(
						point.goal.replace(new pl.type.Term("=", [
							new pl.type.Num(current_seed, false),
							seed
						])),
						point.substitution,
						point
					)]);
				}
			},

			// set_seed/1
			"set_seed/1": function(thread, point, atom) {
				var seed = atom.args[0];
				if(pl.type.is_variable(seed)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_integer(seed)) {
					thread.throw_error(pl.error.type("integer", seed, atom.indicator));
				} else {
					thread.get_random_generator().set_seed(seed.value);
					thread.success(point);
				}
			},
			
			// maybe/0
			"maybe/0": function( thread, point, _ ) {
				if( thread.get_random_generator().next() < 0.5 ) {
					thread.success( point );
				}
			},
			
			// maybe/1
			"maybe/1": function( thread, point, atom ) {
				var num = atom.args[0];
				if( thread.get_random_generator().next() < num.value ) {
					thread.success( point );
				}
			},
			
			// random/1
			"random/1": function( thread, point, atom ) {
				var rand = atom.args[0];
				if( !pl.type.is_variable( rand ) && !pl.type.is_number( rand ) ) {
					thread.throw_error( pl.error.type( "number", rand, atom.indicator ) );
				} else {
					var gen = thread.get_random_generator().next();
					thread.prepend( [new pl.type.State(
						point.goal.replace( new pl.type.Term( "=", [rand, new pl.type.Num( gen, true )] ) ),
						point.substitution, point 
					)] );
				}
			},
			
			// random/3
			"random/3": function( thread, point, atom ) {
				var lower = atom.args[0], upper = atom.args[1], rand = atom.args[2];
				if( pl.type.is_variable( lower ) || pl.type.is_variable( upper ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_number( lower ) ) {
					thread.throw_error( pl.error.type( "number", lower, atom.indicator ) );
				} else if( !pl.type.is_number( upper ) ) {
					thread.throw_error( pl.error.type( "number", upper, atom.indicator ) );
				} else if( !pl.type.is_variable( rand ) && !pl.type.is_number( rand ) ) {
					thread.throw_error( pl.error.type( "number", rand, atom.indicator ) );
				} else {
					if( lower.value < upper.value ) {
						var float = lower.is_float || upper.is_float;
						var gen = lower.value + thread.get_random_generator().next() * (upper.value - lower.value);
						if( !float )
							gen = Math.floor( gen );
						thread.prepend( [new pl.type.State(
							point.goal.replace( new pl.type.Term( "=", [rand, new pl.type.Num( gen, float )] ) ),
							point.substitution, point 
						)] );
					}
				}
			},
			
			// random_between/3
			"random_between/3": function( thread, point, atom ) {
				var lower = atom.args[0], upper = atom.args[1], rand = atom.args[2];
				if( pl.type.is_variable( lower ) || pl.type.is_variable( upper ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( lower ) ) {
					thread.throw_error( pl.error.type( "integer", lower, atom.indicator ) );
				} else if( !pl.type.is_integer( upper ) ) {
					thread.throw_error( pl.error.type( "integer", upper, atom.indicator ) );
				} else if( !pl.type.is_variable( rand ) && !pl.type.is_integer( rand ) ) {
					thread.throw_error( pl.error.type( "integer", rand, atom.indicator ) );
				} else {
					if( lower.value < upper.value ) {
						var gen = Math.floor(lower.value + thread.get_random_generator().next() * (upper.value - lower.value + 1));
						thread.prepend( [new pl.type.State(
							point.goal.replace( new pl.type.Term( "=", [rand, new pl.type.Num( gen, false )] ) ),
							point.substitution, point 
						)] );
					}
				}
			},
			
			// random_member/2
			"random_member/2": function( thread, point, atom ) {
				var member = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else {
					var array = [];
					var pointer = list;
					while( pointer.indicator === "./2" ) {
						array.push(pointer.args[0]);
						pointer = pointer.args[1];
					}
					if( array.length > 0 ) {
						var gen = Math.floor(thread.get_random_generator().next() * array.length);
						thread.prepend( [new pl.type.State(
							point.goal.replace( new pl.type.Term( "=", [member, array[gen]] ) ),
							point.substitution, point 
						)] );
					}
				}
			},
			
			// random_permutation/2
			"random_permutation/2": function( thread, point, atom ) {
				var i;
				var list = atom.args[0], permutation = atom.args[1];
				var ins_list = pl.type.is_instantiated_list( list );
				var ins_permutation = pl.type.is_instantiated_list( permutation );
				if( pl.type.is_variable( list ) && pl.type.is_variable( permutation ) || !ins_list && !ins_permutation ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( list ) && !pl.type.is_fully_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else if( !pl.type.is_variable( permutation ) && !pl.type.is_fully_list( permutation ) ) {
					thread.throw_error( pl.error.type( "list", permutation, atom.indicator ) );
				} else {
					var pointer = ins_list ? list : permutation;
					var array = [];
					while( pointer.indicator === "./2" ) {
						array.push( pointer.args[0] );
						pointer = pointer.args[1];
					}
					for( i = 0; i < array.length; i++ ) {
						var rand = Math.floor( thread.get_random_generator().next() * array.length );
						var tmp = array[i];
						array[i] = array[rand];
						array[rand] = tmp;
					}
					var new_list = new pl.type.Term( "[]", [] );
					for( i = array.length-1; i >= 0; i-- )
						new_list = new pl.type.Term( ".", [array[i], new_list] );
					thread.prepend( [new pl.type.State(
						point.goal.replace( new pl.type.Term( "=", [new_list, ins_list ? permutation : list] ) ),
						point.substitution, point 
					)] );
				}
			}
		
		};
		
	};
	
	var exports = ["get_seed/1", "set_seed/1", "maybe/0", "maybe/1", "random/1", "random/3", "random_between/3", "random_member/2", "random_permutation/2"];

	// Mulberry32 random generator
	var mulberry32 = function(seed) {
		this.set_seed(seed);
	};

	mulberry32.prototype.next = function() {
		var t = this.get_seed() + 0x6D2B79F5;
		this.set_seed(t);
		t = Math.imul(t ^ t >>> 15, t | 1);
		t ^= t + Math.imul(t ^ t >>> 7, t | 61);
		return ((t ^ t >>> 14) >>> 0) / 4294967296;
	};

	mulberry32.prototype.set_seed = function(seed) {
		this.seed = seed;
	};

	mulberry32.prototype.get_seed = function() {
		return this.seed;
	};

	// Extend Tau Prolog prototypes
	var extend = function(pl) {
		// Get the random generator of the session
		pl.type.Session.prototype.get_random_generator = function() {
			if(!this.random_generator)
				this.random_generator = new mulberry32(Date.now());
			return this.random_generator;
		};
		// Get the random generator of the thread
		pl.type.Thread.prototype.get_random_generator = function() {
			return this.session.get_random_generator();
		};
	};
	
	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			extend(p);
			new pl.type.Module( "random", predicates(), exports );
		};
	} else {
		extend(pl);
		new pl.type.Module( "random", predicates(), exports );
	}

})( pl );

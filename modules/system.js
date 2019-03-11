var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// sleep/1
			"sleep/1": function( thread, point, atom ) {
				var time = atom.args[0];
				if( pl.type.is_variable( time ) ) {
					thread.throw_error( pl.error.instantiation( thread.level ) );
				} else if( !pl.type.is_integer( time ) ) {
					thread.throw_error( pl.error.type( "integer", time, thread.level ) );
				} else {
					setTimeout( function() {
						thread.success( point );
						thread.again();
					}, time.value );
					return true;
				}
			},
			
			// async/1
			"async/1": function( thread, point, atom ) {
				var closure = atom.args[0];
				if( pl.type.is_variable( closure ) ) {
					thread.throw_error( pl.error.instantiation( thread.level ) ); 
				} else if( pl.type.is_callable( closure ) ) {
					var n_thread = new pl.type.Thread( thread.session );
					n_thread.add_goal( closure.clone(), true );
					thread.success( point );
					setTimeout( function() {
						n_thread.answers( function() {} );
					}, 0 );
				} else {
					thread.throw_error( pl.error.type( "callable", closure, thread.level ) ); 
				}
			}

		};
	};
	
	var exports = ["sleep/1", "async/1"];

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "system", predicates(), exports );
		};
	} else {
		new pl.type.Module( "system", predicates(), exports );
	}

})( pl );

var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// sleep/1
			"sleep/1": function( thread, point, atom ) {
				var time = atom.args[0];
				if( pl.type.is_variable( time ) ) {
					thread.throwError( pl.error.instantiation( thread.level ) );
				} else if( !pl.type.is_integer( time ) ) {
					thread.throwError( pl.error.type( "integer", time, thread.level ) );
				} else {
					setTimeout( function() {
						thread.success( point );
						thread.again();
					}, time.value );
					return true;
				}
			}

		};
	};
	
	var exports = ["sleep/1"];

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "system", predicates(), exports );
		};
	} else {
		new pl.type.Module( "system", predicates(), exports );
	}

})( pl );

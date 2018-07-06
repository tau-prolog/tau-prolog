var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// apply/3
			"apply/3": function( thread, point, atom ) {
				var name = atom.args[0], args = atom.args[1], result = atom.args[2];
				if( pl.type.is_variable( name ) || pl.type.is_variable( args ) ) {
					thread.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) ) {
					thread.throwError( pl.error.type( "atom", name, atom.indicator ) );
				} else if( !pl.type.is_list( args ) ) {
					thread.throwError( pl.error.type( "list", args, atom.indicator ) );
				}
				var dots = name.id.split(".");
				var fn = pl.__env;
				for( var i = 0; i < dots.length; i++ )
					if( fn !== undefined )
						fn = fn[dots[i]];
				if( typeof fn === "function" ) {
					var pointer = args;
					var pltojs;
					var arr = [];
					while( pointer.indicator === "./2" ) {
						pltojs = pointer.args[0].toJavaScript();
						if( pltojs === undefined ) {
							thread.throwError( pl.error.domain( "javascript_object", pointer.args[0], atom.indicator ) );
							return undefined;
						}
						arr.push( pltojs );
						pointer = pointer.args[1];
					}
					if( pl.type.is_variable( pointer ) ) {
						thread.throwError( pl.error.instantiation( atom.indicator ) );
						return;
					} else if( pointer.indicator !== "[]/0" ) {
						thread.throwError( pl.error.type( "list", args, atom.indicator ) );
						return
					}
					var value = fn.apply( null, arr );
					if( value === undefined ) {
						thread.success( point );
					} else {
						value = pl.fromJavaScript.apply( value );
						if( value !== undefined ) {
							thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, result] ) ), point.substitution, point )] );
						}
					}
				}
			}

		};
	};
	
	var exports = ["apply/3"];

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "js", predicates(), exports );
		};
	} else {
		new pl.type.Module( "js", predicates(), exports );
	}

})( pl );

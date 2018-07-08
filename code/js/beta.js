var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// global/1
			"global/1": function( thread, point, atom ) {
				thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [atom.args[0], new pl.type.JSValue( pl.__env )] ) ), point.substitution, point )] );
			},
			
			// apply/3:
			"apply/3": [
				new pl.type.Rule(new pl.type.Term("apply", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")]), new pl.type.Term(",", [new pl.type.Term("global", [new pl.type.Var("G")]),new pl.type.Term("apply", [new pl.type.Var("G"),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")])]))
			],
			
			// apply/4
			"apply/4": function( thread, point, atom ) {
				var context = atom.args[0], name = atom.args[1], args = atom.args[2], result = atom.args[3];
				if( pl.type.is_variable( context ) || pl.type.is_variable( name ) || pl.type.is_variable( args ) ) {
					thread.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) && (!pl.type.is_js_object( name ) || typeof name.value !== "function") ) {
					thread.throwError( pl.error.type( "atom_or_JSValueFUNCTION", name, atom.indicator ) );
				} else if( !pl.type.is_list( args ) ) {
					thread.throwError( pl.error.type( "list", args, atom.indicator ) );
				}
				var ctx = context.toJavaScript();
				var fn = pl.type.is_atom( name ) ? ctx[name.id] : name.toJavaScript();
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
					var value;
					try {
						value = fn.apply( ctx, arr );
					} catch( e ) {
						thread.throwError( pl.error.javascript( e.toString(), atom.indicator ) );
						return;
					}
					value = pl.fromJavaScript.apply( value );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, result] ) ), point.substitution, point )] );
				}
			},
			
			// prop/2:
			"prop/2": [
				new pl.type.Rule(new pl.type.Term("prop", [new pl.type.Var("X"),new pl.type.Var("Y")]), new pl.type.Term(",", [new pl.type.Term("global", [new pl.type.Var("G")]),new pl.type.Term("prop", [new pl.type.Var("G"),new pl.type.Var("X"),new pl.type.Var("Y")])]))
			],
			
			// prop/3
			"prop/3": function( thread, point, atom ) {
				var context = atom.args[0], name = atom.args[1], result = atom.args[2];
				if( pl.type.is_variable( context ) ) {
					thread.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( name ) && !pl.type.is_atom( name ) ) {
					thread.throwError( pl.error.type( "atom", name, atom.indicator ) );
				} else {
					if( pl.type.is_atom( name ) ) {
						var fn = context.toJavaScript()[name.id];
						if( fn !== undefined ) {
							fn = pl.fromJavaScript.apply( fn );
							thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [fn, result] ) ), point.substitution, point )] );
						}
					} else {
						var fn = context.toJavaScript();
						var states = [];
						for( var x in fn ) {
							if( fn.hasOwnProperty( x ) ) {
								var fn_ = pl.fromJavaScript.apply( fn[x] );
								states.push( new pl.type.State( point.goal.replace( new pl.type.Term( ",", [
									new pl.type.Term( "=", [fn_, result] ),
									new pl.type.Term( "=", [new pl.type.Term(x, []), name] )
								]) ), point.substitution, point ) );
							}
						}
						thread.prepend( states );
					}
				}
			}

		};
	};
	
	var exports = ["global/1", "apply/3", "apply/4", "prop/2", "prop/3"];



	// JS OBJECTS
	
	// Is a JS object
	pl.type.is_js_object = function( obj ) {
		return obj instanceof pl.type.JSValue;
	};

	// Ordering relation
	pl.type.order.push( pl.type.JSValue );

	// JSValue Prolog object
	pl.type.JSValue = function( value ) {
		this.value = value;
	}

	// toString
	pl.type.JSValue.prototype.toString = function() {
		return "JSValue" + (typeof this.value).toUpperCase();
	};

	// clone
	pl.type.JSValue.prototype.clone = function() {
		return new pl.type.JSValue( this.value );
	};

	// equals
	pl.type.JSValue.prototype.equals = function( obj ) {
		return pl.type.is_js_object( obj ) && this.value === obj.value;
	};

	// rename
	pl.type.JSValue.prototype.rename = function( _ ) {
		return this;
	};

	// get variables
	pl.type.JSValue.prototype.variables = function() {
		return [];
	};

	// apply substitutions
	pl.type.JSValue.prototype.apply = function( _ ) {
		return this;
	};

	// unify
	pl.type.JSValue.prototype.unify = function( obj, _ ) {
		if( pl.type.is_js_object( obj ) && this.value === obj.value ) {
			return new pl.type.State( obj, new pl.type.Substitution() );
		}
		return null;
	};

	// interpret
	pl.type.JSValue.prototype.interpret = function( indicator ) {
		return pl.error.instantiation( indicator );
	};

	// compare
	pl.type.JSValue.prototype.compare = function( obj ) {
		if( this.value === obj.value ) {
			return 0;
		} else if( this.value < obj.value ) {
			return -1;
		} else if( this.value > obj.value ) {
			return 1;
		}
	};
	
	// to javascript
	pl.type.JSValue.prototype.toJavaScript = function() {
		return this.value;
	};
	
	// from javascrip
	pl.fromJavaScript.conversion.any = function( obj ) {
		return new pl.type.JSValue( obj );
	};
	
	
	
	// JavaScript error
	pl.error.javascript = function( error, indicator ) {
		return new pl.type.Term( "error", [new pl.type.Term( "javascript_error", [new pl.type.Term( error )] ), pl.utils.str_indicator( indicator )] );
	};
	
	


	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "js", predicates(), exports );
		};
	} else {
		new pl.type.Module( "js", predicates(), exports );
	}

})( pl );

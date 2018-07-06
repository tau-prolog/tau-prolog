var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// apply/3
			"apply/3": function( thread, point, atom ) {
				var name = atom.args[0], args = atom.args[1], result = atom.args[2];
				if( pl.type.is_variable( name ) || pl.type.is_variable( args ) ) {
					thread.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) && (!pl.type.is_js_object( name ) || typeof name.value !== "function") ) {
					thread.throwError( pl.error.type( "atom_or_JSValueFUNCTION", name, atom.indicator ) );
				} else if( !pl.type.is_list( args ) ) {
					thread.throwError( pl.error.type( "list", args, atom.indicator ) );
				}
				var fn;
				if( pl.type.is_atom( name ) ) {
					var dots = name.id.split(".");
					fn = pl.__env;
					for( var i = 0; i < dots.length; i++ )
						if( fn !== undefined )
							fn = fn[dots[i]];
				} else {
					fn = name.value;
				}
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
						value = fn.apply( null, arr );
					} catch( e ) {
						thread.throwError( pl.error.javascript( e.toString(), atom.indicator ) );
						return;
					}
					if( value === undefined ) {
						thread.success( point );
					} else {
						value = pl.fromJavaScript.apply( value );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, result] ) ), point.substitution, point )] );
					}
				}
			},
			
			// apply/4
			"apply/4": function( thread, point, atom ) {
				var obj = atom.args[0], name = atom.args[1], args = atom.args[2], result = atom.args[3];
				if( pl.type.is_variable( obj ) || pl.type.is_variable( name ) || pl.type.is_variable( args ) ) {
					thread.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) ) {
					thread.throwError( pl.error.type( "atom", name, atom.indicator ) );
				} else if( !pl.type.is_list( args ) ) {
					thread.throwError( pl.error.type( "list", args, atom.indicator ) );
				}
				obj = obj.toJavaScript();
				if( obj !== undefined && obj !== null && typeof obj[name.id] === "function" ) {
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
						value = obj[name.id].apply( obj, arr );
					} catch( e ) {
						thread.throwError( pl.error.javascript( e, atom.indicator ) );
						return;
					}
					if( value === undefined ) {
						thread.success( point );
					} else {
						value = pl.fromJavaScript.apply( value );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, result] ) ), point.substitution, point )] );
					}
				}
			}

		};
	};
	
	var exports = ["apply/3", "apply/4"];



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
		return new pl.type.Term( "error", [new pl.type.Term( "javascript_error", [new pl.type.Term( error )] ), new pl.type.Term( indicator )] );
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

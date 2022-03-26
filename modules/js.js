var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// global/1
			"global/1": function( thread, point, atom ) {
				thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [atom.args[0], pl.fromJavaScript.apply(pl.__env)] ) ), point.substitution, point )] );
			},
			
			// apply/3:
			"apply/3": [
				new pl.type.Rule(new pl.type.Term("apply", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")]), new pl.type.Term(",", [new pl.type.Term("global", [new pl.type.Var("G")]),new pl.type.Term("apply", [new pl.type.Var("G"),new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")])]))
			],
			
			// apply/4
			"apply/4": function( thread, point, atom ) {
				var context = atom.args[0], name = atom.args[1], args = atom.args[2], result = atom.args[3];
				if( pl.type.is_variable( context ) || pl.type.is_variable( name ) || pl.type.is_variable( args ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) && (!pl.type.is_js_object( name ) || typeof name.value !== "function") ) {
					thread.throw_error( pl.error.type( "atom_or_JSValueFUNCTION", name, atom.indicator ) );
				} else if( !pl.type.is_list( args ) ) {
					thread.throw_error( pl.error.type( "list", args, atom.indicator ) );
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
							thread.throw_error( pl.error.domain( "javascript_object", pointer.args[0], atom.indicator ) );
							return undefined;
						}
						arr.push( pltojs );
						pointer = pointer.args[1];
					}
					if( pl.type.is_variable( pointer ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
						return;
					} else if( pointer.indicator !== "[]/0" ) {
						thread.throw_error( pl.error.type( "list", args, atom.indicator ) );
						return
					}
					var value;
					try {
						value = fn.apply( ctx, arr );
					} catch( e ) {
						thread.throw_error( pl.error.javascript( e.toString(), atom.indicator ) );
						return;
					}
					value = pl.fromJavaScript.apply( value );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, result] ) ), point.substitution, point )] );
				}
			},

			// prop/2 (deprecated)
			"prop/2": [
				new pl.type.Rule(new pl.type.Term("prop", [new pl.type.Var("X"),new pl.type.Var("Y")]), new pl.type.Term("get_prop", [new pl.type.Var("X"),new pl.type.Var("Y")]))
			],

			// prop/3 (deprecated)
			"prop/3": [
				new pl.type.Rule(new pl.type.Term("prop", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")]), new pl.type.Term("get_prop", [new pl.type.Var("X"),new pl.type.Var("Y"),new pl.type.Var("Z")]))
			],
			
			// get_prop/2
			"get_prop/2": [
				new pl.type.Rule(new pl.type.Term("get_prop", [new pl.type.Var("X"),new pl.type.Var("Y")]), new pl.type.Term(",", [new pl.type.Term("global", [new pl.type.Var("G")]),new pl.type.Term("get_prop", [new pl.type.Var("G"),new pl.type.Var("X"),new pl.type.Var("Y")])]))
			],
			
			// get_prop/3
			"get_prop/3": function( thread, point, atom ) {
				var context = atom.args[0], name = atom.args[1], result = atom.args[2];
				if( pl.type.is_variable( context ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( name ) && !pl.type.is_atom( name ) ) {
					thread.throw_error( pl.error.type( "atom", name, atom.indicator ) );
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
			},

			// set_prop/2:
			"set_prop/2": [
				new pl.type.Rule(new pl.type.Term("set_prop", [new pl.type.Var("X"),new pl.type.Var("Y")]), new pl.type.Term(",", [new pl.type.Term("global", [new pl.type.Var("G")]),new pl.type.Term("set_prop", [new pl.type.Var("G"),new pl.type.Var("X"),new pl.type.Var("Y")])]))
			],

			// set_prop/3
			"set_prop/3": function(thread, point, atom) {
				var context = atom.args[0], key = atom.args[1], value = atom.args[2];
				if(pl.type.is_variable(context) || pl.type.is_variable(key)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(key)) {
					thread.throw_error(pl.error.type("atom", key, atom.indicator));
				} else {
					var obj = context.toJavaScript();
					if(obj !== undefined) {
						obj[key.id] = value.toJavaScript();
						thread.success(point);
					}
				}
			},

			// new/3
			"new/3": function(thread, point, atom) {
				var obj = atom.args[0], args = atom.args[1], instance = atom.args[2];
				if(pl.type.is_variable(obj) || pl.type.is_variable(args)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_js_object(obj)) {
					thread.throw_error(pl.error.type("JsValueOBJECT", args, atom.indicator));
				} else if(!pl.type.is_list(args)) {
					thread.throw_error(pl.error.type("list", args, atom.indicator));
				} else {
					var arr_args = [];
					var pointer = args;
					while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
						arr_args.push(pointer.args[0].toJavaScript());
						pointer = pointer.args[1];
					}
					if(!pl.type.is_term(pointer) || pointer.indicator !== "[]/0") {
						thread.throw_error(pl.error.type("list", args, atom.indicator));
						return;
					}
					try {
						var result = pl.fromJavaScript.apply(
							new (Function.prototype.bind.apply(obj.value, [null].concat(arr_args)))
						);
						thread.prepend([
							new pl.type.State(
								point.goal.replace(new pl.type.Term("=", [instance, result])),
								point.substitution,
								point
							)
						]);
					} catch(error) {
						thread.throw_error(pl.error.javascript(error.toString(), atom.indicator));
					}
				}
			},

			// json_prolog/2
			"json_prolog/2": function( thread, point, atom ) {
				var json = atom.args[0], prolog = atom.args[1];
				if( pl.type.is_variable(json) && pl.type.is_variable(prolog) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable(json) && (!pl.type.is_js_object(json) || typeof(json.value) !== "object")) {
					thread.throw_error( pl.error.type( "JsValueOBJECT", json, atom.indicator ) );
				} else if( !pl.type.is_variable(prolog) && !pl.type.is_list(prolog) ) {
					thread.throw_error( pl.error.type( "list", prolog, atom.indicator ) );
				} else {
					if(pl.type.is_variable(prolog)) {
						var list = pl.fromJavaScript.apply(json.value, true);
						thread.prepend([new pl.type.State(
							point.goal.replace(new pl.type.Term("=", [prolog, list])),
							point.substitution,
							point
						)]);
					} else {
						var obj = new pl.type.JSValue(prolog.toJavaScript());
						thread.prepend([new pl.type.State(
							point.goal.replace(new pl.type.Term("=", [json, obj])),
							point.substitution,
							point
						)]);
					}
				}
			},

			// json_atom/2
			"json_atom/2": function( thread, point, atom ) {
				var json = atom.args[0], prolog = atom.args[1];
				if( pl.type.is_variable(json) && pl.type.is_variable(prolog) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable(json) && (!pl.type.is_js_object(json) || typeof(json.value) !== "object")) {
					thread.throw_error( pl.error.type( "JsValueOBJECT", json, atom.indicator ) );
				} else if( !pl.type.is_variable(prolog) && !pl.type.is_atom(prolog) ) {
					thread.throw_error( pl.error.type( "atom", prolog, atom.indicator ) );
				} else {
					if(pl.type.is_variable(prolog)) {
						try {
							var jatom = new pl.type.Term(JSON.stringify(json.value), []);
							thread.prepend([new pl.type.State(
								point.goal.replace(new pl.type.Term("=", [prolog, jatom])),
								point.substitution,
								point
							)]);
						} catch(ex) {}
					} else {
						try {
							var obj = pl.fromJavaScript.apply(JSON.parse(prolog.id));
							thread.prepend([new pl.type.State(
								point.goal.replace(new pl.type.Term("=", [json, obj])),
								point.substitution,
								point
							)]);
						} catch(ex) {}
					}
				}
			},

			// ajax/3
			"ajax/3": [
				new pl.type.Rule(new pl.type.Term("ajax", [new pl.type.Var("Method"),new pl.type.Var("URL"),new pl.type.Var("Response")]), new pl.type.Term("ajax", [new pl.type.Var("Method"),new pl.type.Var("URL"),new pl.type.Var("Response"),new pl.type.Term("[]", [])]))
			],

			// ajax/4
			"ajax/4": function( thread, point, atom ) {
				var method = atom.args[0], url = atom.args[1], value = atom.args[2], options = atom.args[3];
				if(pl.type.is_variable(url) || pl.type.is_variable(method) || pl.type.is_variable(options)) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if(!pl.type.is_atom(url)) {
					thread.throw_error( pl.error.type( "atom", url, atom.indicator ) );
				} else if(!pl.type.is_atom(method)) {
					thread.throw_error( pl.error.type( "atom", method, atom.indicator ) );
				} else if(!pl.type.is_list(options)) {
					thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
				} else if(["connect", "delete", "get", "head", "options", "patch", "post", "put", "trace"].indexOf(method.id) === -1) {
					thread.throw_error( pl.error.domain( "http_method", method, atom.indicator ) );
				} else {
					var pointer = options;
					var opt_type = null;
					var opt_timeout = 0;
					var opt_credentials = "false";
					var opt_async = "true";
					var opt_mime = null;
					var opt_headers = [];
					var opt_body = new FormData();
					var opt_user = null;
					var opt_password = null;
					// Options
					while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
						var option = pointer.args[0];
						if(!pl.type.is_term(option) || option.args.length !== 1) {
							thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
							return;
						}
						var prop = option.args[0];
						// type/1
						if(option.indicator === "type/1") {
							if(!pl.type.is_atom(prop) || prop.id !== "text" && prop.id !== "json" && prop.id !== "document") {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_type = prop.id;
						// user/1
						} else if(option.indicator === "user/1") {
							if(!pl.type.is_atom(prop)) {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_user = prop.id;
						// password/1
						} else if(option.indicator === "password/1") {
							if(!pl.type.is_atom(prop)) {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_password = prop.id;
						// timeout/1
						} else if(option.indicator === "timeout/1") {
							if(!pl.type.is_integer(prop) || prop.value < 0) {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_timeout = prop.value;
						// async/1
						} else if(option.indicator === "async/1") {
							if(!pl.type.is_atom(prop) || prop.id !== "true" && prop.id !== "false") {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_async = prop.id;
						// credentials/1
						} else if(option.indicator === "credentials/1") {
							if(!pl.type.is_atom(prop) || prop.id !== "true" && prop.id !== "false") {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_credentials = prop.id;
						// mime/1
						} else if(option.indicator === "mime/1") {
							if(!pl.type.is_atom(prop)) {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							opt_mime = prop.id;
						// headers/1
						} else if(option.indicator === "headers/1") {
							if(!pl.type.is_list(prop)) {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							var hpointer = prop;
							while(pl.type.is_term(hpointer) && hpointer.indicator === "./2") {
								var header = hpointer.args[0];
								if(!pl.type.is_term(header) || header.indicator !== "-/2" || !pl.type.is_atom(header.args[0]) || !pl.type.is_atom(header.args[1])) {
									thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
									return;
								}
								opt_headers.push({header: header.args[0].id, value: header.args[1].id});
								hpointer = hpointer.args[1];
							}
							if(pl.type.is_variable(hpointer)) {
								thread.throw_error( pl.error.instantiation( atom.indicator ) );
								return;
							} else if(!pl.type.is_term(hpointer) || hpointer.indicator !== "[]/0") {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
						// body/1
						} else if(option.indicator === "body/1") {
							if(!pl.type.is_list(prop) && (pl.type.is_dom_object === undefined || !pl.type.is_dom_object(prop)) && !pl.type.is_atom(prop)) {
								thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
								return;
							}
							if(pl.type.is_list(prop)) {
								var hpointer = prop;
								while(pl.type.is_term(hpointer) && hpointer.indicator === "./2") {
									var body = hpointer.args[0];
									if(!pl.type.is_term(body) || body.indicator !== "-/2" || !pl.type.is_atom(body.args[0]) || !pl.type.is_atom(body.args[1])) {
										thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
										return;
									}
									opt_body.append(body.args[0].id, body.args[1].id);
									hpointer = hpointer.args[1];
								}
								if(pl.type.is_variable(hpointer)) {
									thread.throw_error( pl.error.instantiation( atom.indicator ) );
									return;
								} else if(!pl.type.is_term(hpointer) || hpointer.indicator !== "[]/0") {
									thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
									return;
								}
							} else if(pl.type.is_atom(prop)) {
								opt_body = prop.id;
							} else {
								opt_body = prop.value;
							}
						// otherwise
						} else {
							thread.throw_error( pl.error.domain( "ajax_option", option, atom.indicator ) );
							return;
						}
						pointer = pointer.args[1];
					}
					if(pl.type.is_variable(pointer)) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
						return;
					} else if(!pl.type.is_term(pointer) || pointer.indicator !== "[]/0") {
						thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
						return;
					}
					// Request
					var xhttp = new XMLHttpRequest();
					if(opt_mime !== null)
						xhttp.overrideMimeType(opt_mime);
					var fn = function() {
						if(this.readyState == 4) {
							if(this.status == 200) {
								// Get response
								var data = null;
								var content_type = this.getResponseHeader("Content-Type");
								if(this.responseType === "json" && this.response) {
									data = pl.fromJavaScript.apply(this.response);
								} else if(this.responseType === "" && content_type.indexOf("application/json") !== -1) {
									try {
										data = pl.fromJavaScript.apply(JSON.parse(this.responseText));
									} catch(e) {}
								}
								if(data === null) {
									if((this.responseType === "document" || this.responseType === "" && content_type.indexOf("text/html") !== -1 || this.responseType === "" && content_type.indexOf("application/xml") !== -1) && this.responseXML !== null && pl.type.DOM !== undefined) {
										data = new pl.type.DOM( this.responseXML );
									} else if(this.responseType === "" || this.responseType === "text") {
										data = new pl.type.Term(this.responseText, []);
									}
								}
								// Add answer
								if(data !== null)
									thread.prepend( [
										new pl.type.State(
											point.goal.replace(new pl.type.Term("=", [value, data])),
											point.substitution,
											point
										)
									] );
							}
							if(opt_async === "true")
								thread.again();
						}
					};
					xhttp.onreadystatechange = fn;
					xhttp.open(method.id.toUpperCase(), url.id, opt_async === "true", opt_user, opt_password);
					if(opt_type !== null && opt_async === "true")
						xhttp.responseType = opt_type;
					xhttp.withCredentials = opt_credentials === "true";
					if(opt_async === "true")
						xhttp.timeout = opt_timeout;
					for(var i = 0; i < opt_headers.length; i++)
						xhttp.setRequestHeader(opt_headers[i].header, opt_headers[i].value);
					xhttp.send(opt_body);
					if(opt_async === "true")
						return true;
					else
						fn.apply(xhttp);
				}
			}

		};
	};
	
	var exports = ["global/1", "apply/3", "apply/4", "prop/2", "prop/3", "get_prop/2", "get_prop/3", "set_prop/2", "set_prop/3", "new/3", "json_prolog/2", "json_atom/2", "ajax/3", "ajax/4"];	

	// JS OBJECTS
	function define_properties() {

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
			return "<javascript>(" + (typeof this.value).toLowerCase() + ")";
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
		pl.type.JSValue.prototype.unify = function(obj, occurs_check) {
			if(pl.type.is_js_object(obj) && this.value === obj.value)
				return new pl.type.Substitution();
			if(pl.type.is_term(obj) && obj.indicator === "{}/1") {
				var left = [], right = [];
				var pointer = obj.args[0];
				var props = [];
				while(pl.type.is_term(pointer) && pointer.indicator === ",/2") {
					props.push(pointer.args[0]);
					pointer = pointer.args[1];
				}
				props.push(pointer);
				for(var i = 0; i < props.length; i++) {
					var bind = props[i];
					if(!pl.type.is_term(bind) || bind.indicator !== ":/2")
						return null;
					var name = bind.args[0];
					if(!pl.type.is_atom(name) || !this.value.hasOwnProperty(name.id))
						return null;
					var value = pl.fromJavaScript.apply(this.value[name.id]);
					right.push(bind.args[1]);
					left.push(value);
				}
				return pl.unify(left, right, occurs_check);
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

		// from javascript
		pl.fromJavaScript.conversion.any = function( obj ) {
			return new pl.type.JSValue( obj );
		};

		// JavaScript error
		pl.error.javascript = function( error, indicator ) {
			return new pl.type.Term( "error", [new pl.type.Term( "javascript_error", [new pl.type.Term( error )] ), pl.utils.str_indicator( indicator )] );
		};
	}
	


	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			define_properties();
			new pl.type.Module( "js", predicates(), exports );
		};
	} else {
		define_properties();
		new pl.type.Module( "js", predicates(), exports );
	}

})( pl );

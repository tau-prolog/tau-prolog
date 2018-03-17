(function() {
	
	// PARSER

	var indexOf;
	if(!Array.prototype.indexOf) {
		indexOf = function(array, elem) {
			var len = array.length;
			for(var i = 0; i < len; i++) {
				if(elem === array[i]) return i;
			}
			return -1;
		};
	} else {
		indexOf = function(array, elem) {
			return array.indexOf(elem);
		};
	}
	
	var reduce = function(array, fn) {
		if(array.length === 0) return undefined;
		var elem = array[0];
		var len = array.length;
		for(var i = 1; i < len; i++) {
			elem = fn(elem, array[i]);
		}
		return elem;
	};
	
	var map;
	if(!Array.prototype.map) {
		map = function(array, fn) {
			var a = [];
			var len = array.length;
			for(var i = 0; i < len; i++) {
				a.push( fn(array[i]) );
			}
			return a;
		};
	} else {
		map = function(array, fn) {
			return array.map(fn);
		};
	}
	
	
	var ERROR = 0;
	var SUCCESS = 1;
	
	
	var regex_escape = /(\\a)|(\\b)|(\\f)|(\\n)|(\\r)|(\\t)|(\\v)|\\x([0-9a-fA-F]+)\\|\\([0-7]+)\\|(\\\\)|(\\')|(\\")|(\\`)|(\\.)|(.)/g;
	var escape_map = {"\\a": 7, "\\b": 8, "\\f": 12, "\\n": 10, "\\r": 13, "\\t": 9, "\\v": 11};
	function escape(str) {
		var s = [];
		str.replace(regex_escape, function(match, a, b, f, n, r, t, v, hex, octal, back, single, double, backquote, error, char) {
			switch(true) {
				case hex !== undefined:
					s.push( parseInt(hex, 16) );
					return "";
				case octal !== undefined:
					s.push( parseInt(octal, 8) );
					return "";
				case back !== undefined:
				case single !== undefined:
				case double !== undefined:
				case backquote !== undefined:
					s.push( match.substr(1).charCodeAt(0) );
					return "";
				case char !== undefined:
					s.push( char.charCodeAt(0) );
					return "";
				case error !== undefined:
					throw "escape";
				default:
					s.push(escape_map[match]);
					return "";
			}
		});
		return s;
	}
	
	
	function escapeStr(str) {
		return map(escape(str), function(c) { return String.fromCharCode(c); }).join("");
	}
	
	
	function convertNum(num) {
		var n = num.substr(2);
		switch(num.substr(0,2).toLowerCase()) {
			case "0x":
				return parseInt(n, 16);
			case "0b":
				return parseInt(n, 2);
			case "0o":
				return parseInt(n, 8);
			case "0'":
				return escape(n)[0];
			default:
				return parseFloat(num);
		}
	}
	
	
	var tokenize = (function() {
	
		var rules = {
			whitespace: /^\s*(?:(?:%.*)|(?:\/\*(?:\n|\r|.)*?\*\/)|(?:\s+))\s*/,
			variable: /^(?:[A-Z_][a-zA-Z0-9_]*)/,
			point: /^\./,
			compound: /^(([a-z][0-9a-zA-Z_]*|[#\$\&\*\+\-\.\/\:\<\=\>\?@\^\~\\]+|'(?:[^']*?(?:\\(?:x?\d+)?\\)*(?:'')*(?:\\')*)*')\()/,
			atom: /^(,|;|[a-z][0-9a-zA-Z_]*|[#\$\&\*\+\-\.\/\:\<\=\>\?@\^\~\\]+|'(?:[^']*?(?:\\(?:x?\d+)?\\)*(?:'')*(?:\\')*)*')/,
			number: /^(?:0o[0-7]+|0x[0-9a-f]+|0b[01]+|0'(?:''|\\[abfnrtv\\'"`]|\\x?\d+\\|.)|\d+(?:\.\d+(?:e[+-]?\d+)?)?)/i,
			string: /^(?:"([^"]|""|\\")*"|`([^`]|``|\\`)*`)/,
			l_brace: /^(?:\[)/,
			r_brace: /^(?:\])/,
			l_bracket: /^(?:\{)/,
			r_bracket: /^(?:\})/,
			bar: /^(?:\|)/,
			l_paren: /^(?:\()/,
			r_paren: /^(?:\))/,
			cut: /^(?:\!)/,
			error: /^./
		};
		
		function replace( session, text ) {
			if( session.flag.char_conversion.id === "on" ) {
				return text.replace(/./g, function(char) {
					return session.__char_conversion[char] || char;
				});
			}
			return text;
		}
		
	
	
		// start 0, line 1
		return function ( session, text, position, start, line ) {
			var tokens = [];
			var len = position;
			var last_is_blank = false;
			text = replace( session, text.substr(position) );
	
	
			while(text !== "") {
				var matches = [];
	
				// Elimina los saltos de linea
				/*if(/^\n/.exec(text) !== null) {
					line++;
					start = 0;
					len++;
					text = text.replace(/\n/, "");
					last_is_blank = true;
					continue;
				}*/
				
				// Busca todas las coincidencias
				for(var rule in rules) {
					if(rules.hasOwnProperty(rule)) {
						var matchs = rules[rule].exec( text );
						var match = matchs ? matchs[0] : "";
						matches.push({
							value: match,
							name: rule,
							matches: matchs
						});
					}
				}
				
				// Filtra la coincidencia mas larga
				var token = reduce( matches, function(a, b) {
					return a.value.length >= b.value.length ? a : b;
				} );
				
				token.start = start;
				token.line = line;

				text = text.replace(token.value, "");
				start += token.value.length;
				len += token.value.length;
				var is_compound = false;

				switch(token.name) {
					case "compound":
						token.value = token.value.substr(0, token.value.length - 1).replace(/^'(.*)'$/, function(m, e) {return e;});
						is_compound = true;
						break;
					case "atom":
						if(token.value.charAt(0) == "'") {
							token.value = escapeStr( token.value.substr(1, token.value.length - 2).replace(/''/g, "'").replace(/\\\n/g, "") );
						}
						break;
					case "number":
						token.float = token.value.match(/[.eE]/) !== null;
						token.value = convertNum( token.value );
						token.blank = last_is_blank;
						break;
					case "string":
						if(token.value.charAt(0) == "`") token.value = token.value.replace(/``/g, "`");
						else if(token.value.charAt(0) == '"') token.value = token.value.replace(/""/g, '"');
						token.value = escape( token.value.substr(1, token.value.length - 2) );
						break;
					case "point":
						tokens.push( token );
						return {
							tokens: tokens,
							len: len,
							start: start,
							line: line
						};
					case "whitespace":
						last_is_blank = true;
						var ms = token.value.match(/\n/g);
						if(ms) {
							line += ms.length;
							start = 0;
						}
						continue;
				}

				tokens.push( token );
				if(is_compound) {
					tokens.push({
						start: start - 1,
						line: line,
						value: "(",
						name: "l_paren"
					});
				}
				last_is_blank = false;
			}

			return {
				tokens: tokens,
				len: len,
				start: start,
				line: line
			};
		};
	
	})();
	
	
	
	function parseOperator(session, tokens, start, priority) {
		if(priority == "0") return parseExpr(session, tokens, start);
		if(!tokens[start]) return {
			type: ERROR,
			value: pl.error.syntax(tokens[start - 1], "expression expected")
		};

		var error = null;
		var expr, expr2, token, classes;
		
		var max_priority = session.__get_max_priority();
		var next_priority = session.__get_next_priority(priority);
		var aux_start = start;
	
		if(tokens[start] && tokens[start].name == "atom" && session.__lookup_operator_classes(priority, tokens[start].value)) {
			token = tokens[start];
			start++;

			classes = session.__lookup_operator_classes(priority, token.value);
			if(indexOf(classes, "fy") > -1 || indexOf(classes, "fx") > -1) {
				var number = tokens[start];
				if(token.value == "-" && number && number.name == "number" && !number.blank) {
					return {
						value: new Num( -number.value, number.float ),
						len: ++start,
						type: SUCCESS
					};
				}
			}
			
			if(indexOf(classes, "fy") > -1) {
				expr = parseOperator(session, tokens, start, priority);
				if(expr.type != ERROR) {
					return {
						value: new Term(token.value, [expr.value]),
						len: expr.len,
						type: SUCCESS
					};
				} else {
					error = expr;
					return expr;
				}
			}
			
			else if(indexOf(classes, "fx") > -1) {
				expr = parseOperator(session, tokens, start, next_priority);
				if(expr.type != ERROR) {
					return {
						value: new Term(token.value, [expr.value]),
						len: expr.len,
						type: SUCCESS
					};
				} else {
					error = expr;
					return expr;
				}
			}
		}
	
	
		start = aux_start;
		expr = parseOperator(session, tokens, start, next_priority);
		if(expr.type !== ERROR) {
			start = expr.len;
			token = tokens[start];

			if(!token) return expr;

			if((token.name == "atom" || token.name == "compound") && session.__lookup_operator_classes(priority, token.value)) {
	
				var is_compound = token.name == "compound";
				var next_priority_lt = is_compound ? max_priority : next_priority;
				var next_priority_eq = is_compound ? max_priority : priority;
		
				if(is_compound) session.__push_comma_state(false);
				classes = session.__lookup_operator_classes(priority, token.value);
	
	
				if(indexOf(classes, "xf") > -1) {
					if(is_compound) session.__pop_comma_state();
					return {
						value: new Term(token.value, [expr.value]),
						len: ++expr.len
					};
				}
				else if (indexOf(classes, "xfx") > -1) {
					expr2 = parseOperator(session, tokens, start + 1, next_priority_lt);
					if(is_compound) session.__pop_comma_state();
					if(expr2.type != ERROR) {
						return {
							value: new Term(token.value, [expr.value, expr2.value]),
							len: expr2.len,
							type: SUCCESS
						};
					} else {
						return expr2;
					}
				}
				else if (indexOf(classes, "xfy") > -1) {
					expr2 = parseOperator(session, tokens, start + 1, next_priority_eq);
					if(is_compound) session.__pop_comma_state();
					if(expr2.type != ERROR) {
						return {
							value: new Term(token.value, [expr.value, expr2.value]),
							len: expr2.len,
							type: SUCCESS
						};
					} else {
						return expr2;
					}
				}
				else {
					if(is_compound) session.__pop_comma_state();
					while(true) {
						start = expr.len;
						token = tokens[start];
						if(token && (token.name == "atom" || token.name == "compound") && session.__lookup_operator_classes(priority, token.value)) {
							classes = session.__lookup_operator_classes(priority, token.value);
							if( indexOf(classes, "yf") > -1 ) {
								expr = {
									value: new Term(token.value, [expr.value]),
									len: ++start,
									type: SUCCESS
								};
							}
							else if( indexOf(classes, "yfx") > -1 ) {
								expr2 = parseOperator(session, tokens, ++start, next_priority_lt);
								if(expr2.type == ERROR) {
									expr = expr2;
									break;
								}
								start = expr2.len;
								expr = {
									value: new Term(token.value, [expr.value, expr2.value]),
									len: start,
									type: SUCCESS
								};
							}
							else {
								break;
							}
						} else {
							break;
						}
					}
				}
			}
	
			return expr;
		} else {
			error = expr;
		}
	
		return error;
	}
	
	
	function parseExpr(session, tokens, start) {
		var i, expr, expr2, priority, values;
	
		if(!tokens[start]) return {
			type: ERROR,
			value: pl.error.syntax(tokens[start], "expression expected")
		};


		var token = tokens[start++];
		switch(token.name) {
			case "variable":
				return {
					value: new Var(token.value),
					len: start,
					type: SUCCESS
				};
			case "cut":
				return {
					value: new Term(token.value),
					len: start,
					type: SUCCESS
				};
			case "string":
				var string;
				switch(session.flag.double_quotes.id) {
					case "codes":
						string = new Term("[]");
						for(i = token.value.length - 1; i >= 0; i--) {
							string = new Term(".", [new Num(token.value[i], false), string]);
						}
						break;
					case "chars":
						string = new Term("[]");
						for(i = token.value.length - 1; i >= 0; i--) {
							string = new Term(".", [new Term(String.fromCharCode(token.value[i])), string]);
						}
						break;
					case "atom":
						string = "";
						for(i = 0; i < token.value.length; i++) {
							string += String.fromCharCode(token.value[i]);
						}
						string = new Term( string );
						break;
				}
				return {
					value: string,
					len: start,
					type: SUCCESS
				};
			case "number":
				return {
					value: new Num(token.value, token.float),
					len: start,
					type: SUCCESS
				};
			case "l_bracket":
				session.__push_comma_state(true);
				var args = [];
				expr = parseOperator(session, tokens, start, session.__get_max_priority());
				session.__pop_comma_state();
				if(expr.type != ERROR) {
					args = [expr.value];
					start = expr.len;
				}
				if(tokens[start] && tokens[start].name == "r_bracket") {
					return {
						value: new Term("{}", args),
						len: ++start,
						type: SUCCESS
					};
				} else {
					token = tokens[start];
					return {
						type: ERROR,
						value: pl.error.syntax(token ? token : tokens[start-1], "} expected", !token)
					};
				}
				break;
			case "l_paren":
				session.__push_comma_state(true);
				expr = parseOperator(session, tokens, start, session.__get_max_priority());
				session.__pop_comma_state();
				if(expr.type != ERROR) {
					start = expr.len;
					if(tokens[start] && tokens[start].name == "r_paren") {
						expr.len = ++start;
						return expr;
					} else {
						token = tokens[start];
						return {
							type: ERROR,
							value: pl.error.syntax(token ? token : tokens[start-1], ", or ) expected", !token)
						};
					}
				} else {
					return expr;
				}
				break;
			case "l_brace":
				priority = session.__get_max_priority();
				session.__push_comma_state(false);
				expr = parseOperator(session, tokens, start, priority);
				values = [];
				var tail = new Term("[]");
				if(expr.type != ERROR) {
					values.push(expr.value);
					start = expr.len;


					while(tokens[start] && tokens[start].name == "atom" && tokens[start].value == ",") {
						expr2 = parseOperator(session, tokens, ++start, priority);
						if(expr2.type != ERROR) {
							values.push(expr2.value);
							start = expr2.len;
						} else {
							session.__pop_comma_state();
							return {
								type: ERROR,
								value: pl.error.syntax(tokens[start], ", or ] expected")
							};
						}
					}

					if(tokens[start] && tokens[start].name == "bar") {
						start++;
						var expr3 = parseOperator(session, tokens, start, priority);
						if(expr3.type != ERROR) {
							tail = expr3.value;
							start = expr3.len;
						} else {
							session.__pop_comma_state();
							token = tokens[start];
							return {
								type: ERROR,
								value: pl.error.syntax(token ? token : tokens[start-1], "expression expected", !token)
							};
						}
					}
				}

				session.__pop_comma_state();
				if(tokens[start] && tokens[start].name == "r_brace") {
					start++;
					for(i = values.length - 1; i >= 0; i--) {
						tail = new Term(".", [values[i], tail]);
					}
					return {
						value: tail,
						len: start,
						type: SUCCESS
					};
				} else {
					token = tokens[start];
					return {
						type: ERROR,
						value: pl.error.syntax(token ? token : tokens[start-1], values.length ? "|, ] or , expected" : "] expected", !token)
					};
				}
				break;
			case "atom":
				if( token.value === "-" && tokens[start] && tokens[start].name === "number" && !tokens[start].blank) {
					return {
						value: new Num(-tokens[start].value, tokens[start].float),
						len: ++start,
						type: SUCCESS
					};
				}
				return {
					value: new Term(token.value),
					len: start,
					type: SUCCESS
				};
			case "compound":
				start++;
				priority = session.__get_max_priority();
				session.__push_comma_state(false);
				expr = parseOperator(session, tokens, start, priority);
				if(expr.type != ERROR) {
					values = [expr.value];
					start = expr.len;
					while(tokens[start] && tokens[start].name == "atom" && tokens[start].value == ",") {
						expr2 = parseOperator(session, tokens, ++start, priority);
						if(expr2.type != ERROR) {
							values.push(expr2.value);
							start = expr2.len;
						} else {
							session.__pop_comma_state();
							return expr2;
						}
					}

					session.__pop_comma_state();
					if(tokens[start] && tokens[start].name == "r_paren") {
						start++;
						return {
							value: new Term(token.value, values),
							len: start,
							type: SUCCESS
						};
					} else {
						token = tokens[start];
						return {
							type: ERROR,
							value: pl.error.syntax(token ? token : tokens[start-1], ", or ) expected", !token)
						};
					}

				} else {
					session.__pop_comma_state();
					return expr;
				}
				break;
		}

		return {
			type: ERROR,
			value: pl.error.syntax(token, "unexpected token")
		};
	}
	
	
	function parseRule(session, tokens, start) {
	
		var expr = parseOperator(session, tokens, start, session.__get_max_priority());
		if(expr.type != ERROR) {
			start = expr.len;
			if(tokens[start] && tokens[start].name == "point") {
				start++;
				if( pl.type.is_term(expr.value) ) {
					if(expr.value.indicator == ":-/2") {
						return {
							value: new Rule(expr.value.args[0], expr.value.args[1]),
							len: start,
							type: SUCCESS
						};
					}
					else if(expr.value.indicator == "-->/2") {
						var dcg = rule_to_dcg(new Rule(expr.value.args[0], expr.value.args[1]), session);
						return {
							value: dcg,
							len: start,
							type: pl.type.is_rule( dcg ) ? SUCCESS : ERROR
						};
					}
					else {
						return {
							value: new Rule(expr.value, null),
							len: start,
							type: SUCCESS
						};
					}
				}
				else {
					return {
						type: ERROR,
						value: pl.error.syntax(tokens[start], "callable expected")
					};
				}
			} else {
				var token = tokens[start];
				return {
					type: ERROR,
					value: pl.error.syntax(token ? token : tokens[start-1], ". or expression expected", !token)
				};
			}
		}
		return expr;
	}
	
	
	function parseProgram(session, string) {
		session.__stack_comma = [true];
		
		var line = 1;
		var start = 0;
		var position = 0;
		var result;
		
		do {
			
			var tokenize_state = tokenize( session, string, position, start, line );
			var tokens = tokenize_state.tokens;
			line = tokenize_state.line;
			start = tokenize_state.start;
			position = tokenize_state.len;
			
			if(tokens.length === 0) break;
			
			var expr = parseRule(session, tokens, 0);
	
			if(expr.type !== ERROR) {
				if(expr.value.body === null && expr.value.head.indicator === ":-/1") {
					result = session.run_directive(expr.value.head.args[0]);
				} else {
					result = session.add_rule(expr.value);
				}
	
				if(!result) {
					return new Term("throw", [result.value]);
				}
			} else {
				return new Term("throw", [expr.value]);
			}
			
		} while( string.length != position );
		
		return true;
	}
	
	
	function parseQuery(session, string) {
		session.__stack_comma = [true];
		
		var line = 1;
		var start = 0;
		var position = 0;
		
		do {
			var tokenize_state = tokenize( session, string, position, start, line );
			var tokens = tokenize_state.tokens;
			line = tokenize_state.line;
			start = tokenize_state.start;
			position = tokenize_state.len;
			
			if(tokens.length === 0) break;
			
			var expr = parseOperator(session, tokens, 0, session.__get_max_priority());
			if(expr.type !== ERROR) {
				var expr_position = expr.len;
				if(tokens[expr_position] && tokens[expr_position].name === "point") {
					session.add_goal(expr.value);
				} else {
					var token = tokens[expr_position];
					return new Term("throw", [pl.error.syntax(token ? token : tokens[expr_position-1], ". or expression expected", !token)]);
				}
			} else {
				return new Term("throw", [expr.value]);
			}
			
		} while(string.length != position);
		
		return true;
	}
	
	
	
	
	
	
	
	
	// UTILS

	// Rule to DCG
	function rule_to_dcg(rule, session) {
		rule = rule.rename( session );
		var begin = session.next_free_variable();
		var dcg = body_to_dcg( rule.body, begin, session );
		if( dcg.error ) return dcg.value;
		rule.body = dcg.value;
		rule.head.args = rule.head.args.concat([begin,dcg.variable]);
		rule.head = new Term(rule.head.id, rule.head.args);
		return rule;
	}

	// Body to DCG
	function body_to_dcg(expr, last, session) {
		var free;
		if( pl.type.is_term( expr ) && expr.indicator === ",/2" ) {
			var left = body_to_dcg(expr.args[0], last, session);
			if( left.error ) return left;
			var right = body_to_dcg(expr.args[1], left.variable, session);
			if( right.error ) return right;
			return {
				value: new Term(',', [left.value, right.value]),
				variable: right.variable,
				error: false
			};
		} else if( pl.type.is_term( expr ) && expr.indicator === "{}/1" ) {
			return {
				value: expr.args[0],
				variable: last,
				error: false
			};
		} else if( pl.type.is_empty_list( expr ) ) {
			return {
				value: new Term("true", []),
				variable: last,
				error: false
			};
		} else if( pl.type.is_list( expr ) ) {
			free = session.next_free_variable();
			var pointer = expr;
			var prev;
			while( pointer.indicator == "./2" ) {
				prev = pointer;
				pointer = pointer.args[1];
			}
			if( pl.type.is_variable( pointer ) ) {
				return {
					value: pl.error.instantiation("DCG"),
					variable: last,
					error: true
				};
			} else if( !pl.type.is_empty_list( pointer ) ) {
				return {
					value: pl.error.type("list", expr, "DCG"),
					variable: last,
					error: true
				};
			} else {
				prev.args[1] = free;
				return {
					value: new Term("=", [last, expr]),
					variable: free,
					error: false
				};
			}
		} else if( pl.type.is_callable( expr ) ) {
			free = session.next_free_variable();
			expr.args = expr.args.concat([last,free]);
			return {
				value: expr,
				variable: free,
				error: false
			};
		} else {
			return {
				value: pl.error.type( "callable", expr, "DCG" ),
				variable: last,
				error: true
			};
		}
	}
	
	// String to Prolog number
	function strToNum( string ) {
		var regex = /^(([0-9]+)|([0-9]+\.[0-9]+)|(0b[01]+)|(0x[0-9a-f]+)|(0o[0-7]+))$/i;
		if( !regex.test( string ) ) return false;
		if( string.charAt(0) === "0" && string.length > 2 ) {
			var sub = string.substr( 2 );
			switch( string.charAt(1).toLowerCase() ) {
				case "b": return new Num( parseInt( sub, 2 ), false );
				case "o": return new Num( parseInt( sub, 8 ), false );
				case "x": return new Num( parseInt( sub, 16 ), false );
			}
		}
		if( indexOf(string, "." ) !== -1 ) {
			return new Num( parseFloat( string ), true );
		} else {
			return new Num( parseInt( string ), false );
		}
	}
	
	// Remove element from array
	function remove( array, element ) {
		for( var i = array.length - 1; i >= 0; i-- ) {
			if( array[i] === element ) {
				array.splice(i, 1);
			}
		}
	}
	
	

	// PROLOG OBJECTS
	
	// Variables
	function Var( id ) {
		this.id = id;
	}
	
	// Numbers
	function Num( value, is_float ) {
		this.is_float = is_float !== undefined ? is_float : parseInt( value ) !== value;
		this.value = this.is_float ? value : parseInt( value );
	}
	
	// Terms
	function Term( id, args ) {
		this.id = id;
		this.args = args || [];
		this.indicator = id + "/" + this.args.length;
	}
	
	// Substitutions
	function Substitution( links ) {
		links = links || {};
		this.links = links;
	}
	
	// States
	function State( goal, subs, parent ) {
		subs = subs || new Substitution();
		parent = parent || null;
		this.goal = goal;
		this.substitution = subs;
		this.parent = parent;
	}
	
	// Rules
	function Rule( head, body ) {
		this.head = head;
		this.body = body;
	}

	// Session
	function Session( limit ) {
		limit = limit === undefined || limit <= 0 ? 1000 : limit;
		this.rules = {};
		this.rename = 0;
		this.level = "top_level/0";
		this.points = [];
		this.variables = [];
		this.renamed_variables = {};
		this.public = [];
		this.limit = limit;
		this.current_limit = limit;
		this.flag = {	
			bounded: pl.flag.bounded.value,
			max_integer: pl.flag.max_integer.value,
			min_integer: pl.flag.min_integer.value,
			integer_rounding_function: pl.flag.integer_rounding_function.value,
			char_conversion: pl.flag.char_conversion.value,
			debug: pl.flag.debug.value,
			max_arity: pl.flag.max_arity.value,
			unknown: pl.flag.unknown.value,
			double_quotes: pl.flag.double_quotes.value
		};
		this.warnings = [];
		this.__loaded_modules = [];
		this.__char_conversion = {};
		this.__operators = {
			1200: { ":-": ["fx", "xfx"],  "-->": ["xfx"], "?-": ["fx"] },
			1100: { ";": ["xfy"] },
			1050: { "->": ["xfy"] },
			1000: { ",": ["xfy"] },
			900: { "\\+": ["fy"] },
			700: {
				"=": ["xfx"], "\\=": ["xfx"], "==": ["xfx"], "\\==": ["xfx"],
				"@<": ["xfx"], "@=<": ["xfx"], "@>": ["xfx"], "@>=": ["xfx"],
				"=..": ["xfx"], "is": ["xfx"], "=:=": ["xfx"], "=\\=": ["xfx"],
				"<": ["xfx"], "=<": ["xfx"], ">": ["xfx"], ">=": ["xfx"]
			},
			500: { "+": ["yfx"], "-": ["yfx"], "/\\": ["yfx"], "\\/": ["yfx"] },
			400: {
				"*": ["yfx"], "/": ["yfx"], "//": ["yfx"], "rem": ["yfx"],
				"mod": ["yfx"], "<<": ["yfx"], ">>": ["yfx"]
			},
			200: { "**": ["xfx"], "^": ["xfy"], "-": ["fy"], "\\": ["fy"] }
		};
		this.__stack_comma = [true];
		this.__minus_operator = true;
		this.__calls = [];
	}
	
	// Modules
	function Module( id, rules ) {
		this.id = id;
		this.rules = rules;
		pl.module[id] = this;
	}



	// PROLOG OBJECTS TO STRING
	
	// Variables
	Var.prototype.toString = function() {
		return this.id;
	};
	
	// Numbers
	Num.prototype.toString = function() {
		return this.is_float && indexOf(this.value.toString(), ".") === -1 ? this.value + ".0" : this.value.toString();
	};
	
	// Terms
	Term.prototype.toString = function() {
		if( pl.type.is_operator( this ) ) {
			if (this.args.length === 0) {
				return this.id;
			} else if (this.args.length === 1) {
				return this.id + "(" + this.args[0] + ")";
			} else {
				return "(" + this.args[0] + this.id + this.args[1] + ")";
			}
		}
		switch( this.indicator ){
			case "[]/0":
			case "!/0":
				return this.id;
			case "./2":
				var list = "[" + this.args[0].toString();
				var pointer = this.args[1];
				while( pointer.indicator === "./2" ) {
					list += ", " + pointer.args[0].toString();
					pointer = pointer.args[1];
				}
				if( pointer.indicator !== "[]/0" ) {
					list += "|" + pointer.toString();
				}
				list += "]";
				return list;
			case ",/2":
			case ";/2":
				return "(" + this.args[0] + this.id + this.args[1] + ")";
			default:
				var id = this.id;
				var first = id.charAt(0);
				if( first !== first.toLowerCase() || first === first.toUpperCase() ) {
					id = "'" + id + "'";
				}
				return id + (this.args.length ? "(" + this.args.join(", ") + ")" : "");
		}
	};
	
	// Substitutions
	Substitution.prototype.toString = function() {
		var str = "{";
		for( var link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			if( str != "{" ) {
				str += ", ";
			}
			str += link + "/" + this.links[link].toString();
		}
		str += "}";
		return str;
	};
	
	// States
	State.prototype.toString = function() {
		if( this.goal === null ) {
			return "<" + this.substitution.toString() + ">";
		} else {
			return "<" + this.goal.toString() + ", " + this.substitution.toString() + ">";
		}
	};
	
	// Rules
	Rule.prototype.toString = function() {
		if( !this.body ) {
			return this.head + ".";
		} else {
			return this.head + " :- " + this.body + ".";
		}
	};
	
	
	
	// CLONE PROLOG OBJECTS
	
	// Variables
	Var.prototype.clone = function() {
		return new Var( this.id );
	};
	
	// Numbers
	Num.prototype.clone = function() {
		return new Num( this.value, this.is_float );
	};
	
	// Terms
	Term.prototype.clone = function() {
		if( this.args.length === 0 )
			return this;
		return new Term( this.id, map( this.args, function( arg ) {
			return arg.clone();
		} ) );
	};
	
	// Substitutions
	Substitution.prototype.clone = function() {
		var links = {};
		for( var link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			links[link] = this.links[link].clone();
		}
		return new Substitution( links );
	};
	
	// States
	State.prototype.clone = function() {
		return new State( this.goal.clone(), this.substitution.clone(), this.parent );
	};
	
	// Rules
	Rule.prototype.clone = function() {
		return new Rule( this.head.clone(), this.body !== null ? this.body.clone() : null );
	};
	
	
	
	
	// COMPARE PROLOG OBJECTS
	
	// Variables
	Var.prototype.equals = function( obj ) {
		return pl.type.is_variable( obj ) && this.id === obj.id;
	};
	
	// Numbers
	Num.prototype.equals = function( obj ) {
		return pl.type.is_number( obj ) && this.value === obj.value && this.is_float === obj.is_float;
	};
	
	// Terms
	Term.prototype.equals = function( obj ) {
		if( !pl.type.is_term( obj ) || this.indicator !== obj.indicator ) {
			return false;
		}
		for( var i = 0; i < this.args.length; i++ ) {
			if( !this.args[i].equals( obj.args[i] ) ) {
				return false;
			}
		}
		return true;
	};
	
	// Substitutions
	Substitution.prototype.equals = function( obj ) {
	var link;
		if( !pl.type.is_substitution( obj ) ) {
			return false;
		}
		for( link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			if( !obj.links[link] || !this.links[link].equals( obj.links[link] ) ) {
				return false;
			}
		}
		for( link in obj.links ) {
			if(!obj.links.hasOwnProperty(link)) continue;
			if( !this.links[link] ) {
				return false;
			}
		}
		return true;
	};
	
	// States
	State.prototype.equals = function( obj ) {
		return pl.type.is_state( obj ) && this.goal.equals( obj.goal ) && this.substitution.equals( obj.substitution ) && this.parent == obj.parent;
	};
	
	// Rules
	Rule.prototype.equals = function( obj ) {
		return pl.type.is_rule( obj ) && this.head.equals( obj.head ) && (this.body === null && obj.body === null || this.body !== null && this.body.equals( obj.body ));
	};
	
	
	
	// RENAME VARIABLES OF PROLOG OBJECTS
	
	// Variables
	Var.prototype.rename = function( session ) {
		return session.get_free_variable( this );
	};
	
	// Numbers
	Num.prototype.rename = function( _ ) {
		return this;
	};
	
	// Terms
	Term.prototype.rename = function( session ) {
		if( this.args.length === 0 )
			return this;
		return new Term( this.id, map( this.args, function( arg ) {
			return arg.rename( session );
		} ) );
	};
	
	// Rules
	Rule.prototype.rename = function( session ) {
		return new Rule( this.head.rename( session ), this.body !== null ? this.body.rename( session ) : null );
	};
	
	
	
	// GET VARIABLES FROM PROLOG OBJECTS
	
	// Variables
	Var.prototype.variables = function() {
		return [this.id];
	};
	
	// Numbers
	Num.prototype.variables = function() {
		return [];
	};
	
	// Terms
	Term.prototype.variables = function() {
		return [].concat.apply( [], map( this.args, function( arg ) {
			return arg.variables();
		} ) );
	};
	
	// Rules
	Rule.prototype.variables = function() {
		if( this.body === null ) {
			return this.head.variables();
		} else {
			return this.head.variables().concat( this.body.variables() );
		}
	};
	
	
	
	// APPLY SUBSTITUTIONS TO PROLOG OBJECTS
	
	// Variables
	Var.prototype.apply = function( subs ) {
		if( subs.lookup( this.id ) ) {
			return subs.lookup( this.id );
		}
		return this;
	};
	
	// Numbers
	Num.prototype.apply = function( _ ) {
		return this;
	};
	
	// Terms
	Term.prototype.apply = function( subs ) {
		if( this.args.length === 0 )
			return this;
		return new Term( this.id, map( this.args, function( arg ) {
			return arg.apply( subs );
		} ) );
	};
	
	// Rules
	Rule.prototype.apply = function( subs ) {
		return new Rule( this.head.apply( subs ), this.body !== null ? this.body.apply( subs ) : null );
	};
	
	// Substitutions
	Substitution.prototype.apply = function( subs ) {
		var link, links = {};
		for( link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			links[link] = this.links[link].apply(subs);
		}
		for( link in subs.links ) {
			if(!subs.links.hasOwnProperty(link)) continue;
			links[link] = subs.links[link];
		}
		return new Substitution( links );
	};
	
	
	
	// UNIFY PROLOG OBJECTS
	
	// Variables
	Var.prototype.unify = function( obj, occurs_check ) {
		if( occurs_check && indexOf( obj.variables(), this.id ) !== -1 && !pl.type.is_variable( obj ) ) {
			return null;
		}
		var links = {};
		links[this.id] = obj;
		return new State( obj, new Substitution( links ) );
	};
	
	// Numbers
	Num.prototype.unify = function( obj, _ ) {
		if( pl.type.is_number( obj ) && this.value == obj.value && this.is_float == obj.is_float ) {
			return new State( obj, new Substitution() );
		}
		return null;
	};
	
	// Terms
	Term.prototype.unify = function( obj, occurs_check ) {
		if( pl.type.is_term( obj ) && this.indicator == obj.indicator ) {
			var subs = new Substitution();
			for( var i = 0; i < this.args.length; i++ ) {
				var state = pl.unify( this.args[i].apply( subs ), obj.args[i].apply( subs ), occurs_check );
				if( state === null ) {
					return null;
				}
				subs = subs.apply( state.substitution );
			}
			return new State( this.apply( subs ), subs );
		}
		return null;
	};
	
	
	
	// SELECTION FUNCTION
	
	// Select term
	Term.prototype.select = function() {
		if( this.indicator === ",/2" ) {
			return this.args[0].select();
		} else {
			return this;
		}
	};
	
	// Replace term
	Term.prototype.replace = function( expr ) {
		if( this.indicator === ",/2" ) {
			if( this.args[0].indicator === ",/2" ) {
				return new Term( ",", [this.args[0].replace( expr ), this.args[1]] );
			} else {
				return expr === null ? this.args[1] : new Term( ",", [expr, this.args[1]] );
			}
		} else {
			return expr;
		}
	};

	// Search term
	Term.prototype.search = function( expr ) {
		if( this.indicator === ",/2" ) {
			return this.args[0].search( expr ) || this.args[1].search( expr );
		} else {
			return this === expr;
		}
	};
	
	
	
	// PROLOG SESSIONS

	// Add a rule
	Session.prototype.add_rule = function( rule ) {
		if(!this.rules[rule.head.indicator]) {
			this.rules[rule.head.indicator] = [];
		}
		this.rules[rule.head.indicator].push(rule);
		return true;
	};

	// Run a directive
	Session.prototype.run_directive = function( directive ) {
		if( pl.type.is_directive( directive ) ) {
			pl.directive[directive.indicator]( this, directive );
			return true;
		}
		return false;
	};
	
	// Push comma state
	Session.prototype.__push_comma_state = function( new_state ) {
		this.__stack_comma.push( new_state );
		if( new_state ) this.__operators[1000][","] = ["xfy"];
		else delete this.__operators[1000][","];
	};
	
	// Pop comma state
	Session.prototype.__pop_comma_state = function() {
		this.__stack_comma.pop();
		var value = this.__stack_comma[this.__stack_comma.length - 1];
		if( value ) this.__operators[1000][","] = ["xfy"];
		else delete this.__operators[1000][","];
	};
	
	// Get maximum priority of the operators
	Session.prototype.__get_max_priority = function() {
		var max = 0;
		for( var key in this.__operators ) {
			if( !this.__operators.hasOwnProperty(key) ) continue;
			var n = parseInt(key);
			if( n > max ) max = n;
		}
		return max.toString();
	};
	
	// Get next priority of the operators
	Session.prototype.__get_next_priority = function( priority ) {
		var max = 0;
		priority = parseInt( priority );
		for( var key in this.__operators ) {
			if( !this.__operators.hasOwnProperty(key) ) continue;
			var n = parseInt(key);
			if( n > max && n < priority ) max = n;
		}
		return max.toString();
	};
	
	// Get classes of an operator
	Session.prototype.__lookup_operator_classes = function( priority, operator ) {
		if( this.__operators[priority] ) {
			return this.__operators[priority][operator] || false;
		}
		return false;
	};
	
	// Throw a warning
	Session.prototype.throw_warning = function( warning ) {
		this.warnings.push( warning );
	};

	// Add a goal
	Session.prototype.add_goal = function( goal ) {
		this.points.push( new State( goal, new Substitution(), null, null ) );
		this.variables = goal.variables();
	};

	// Consult a program from a string
	Session.prototype.consult = function( string ) {
		this.__stack_comma = [true];
		return parseProgram( this, string );
	};

	// Query goal from a string (without ?-)
	Session.prototype.query = function( string ) {
		this.__stack_comma = [true];
		return parseQuery( this, string );
	};
	
	// Get free variable
	Session.prototype.get_free_variable = function( variable ) {
		if( variable.id === "_" || this.renamed_variables[variable.id] === undefined ) {
			this.rename++;
			while( indexOf( this.variables, pl.format_variable( this.rename ) ) !== -1 ) {
				this.rename++;
			}
			if( variable.id === "_" ) {
				return new Var( pl.format_variable( this.rename ) );
			} else {
				this.renamed_variables[variable.id] = pl.format_variable( this.rename );
			}
		}
		return new Var( this.renamed_variables[variable.id] );
	};
	
	// Get next free variable
	Session.prototype.next_free_variable = function() {
		this.rename++;
		while( indexOf( this.variables, pl.format_variable( this.rename ) ) !== -1 ) {
			this.rename++;
		}
		return new Var( pl.format_variable( this.rename ) );
	};
	
	// Check if a predicate is public
	Session.prototype.is_public_predicate = function( indicator ) {
		return indexOf( this.public, indicator ) !== -1;
	};
	
	// Copy context from other session
	Session.prototype.copy_context = function( session ) {
		this.rules = session.rules;
		this.rename = session.rename;
		this.level = session.level;
		this.points = session.points;
		this.variables = session.variables;
		this.renamed_variables = session.renamed_variables;
		this.public = session.public;
		this.limit = session.limit;
		this.current_limit = session.limit;
		this.flag = session.flag;
		this.warnings = session.warnings;
		this.__loaded_modules = session.__loaded_modules;
		this.__char_conversion = session.__char_conversion;
		this.__operators = session.__operators;
		this.__stack_comma = session.__stack_comma;
		this.__minus_operator = session.__minus_operator;
	};
	
	// Insert states at the beginning
	Session.prototype.prepend = function( states ) {
		this.points = states.concat( this.points );
	};
	
	// Remove the selected term and prepend the current state
	Session.prototype.success = function( point ) {
		this.prepend( [new State( point.goal.replace( null ), point.substitution, point ) ] );
	};
	
	// Throw error
	Session.prototype.throwError = function( error ) {
		this.prepend( [new State( new Term( "throw", [error] ), new Substitution(), null, null )] );
	};
	
	// Resolution step
	Session.prototype.step = function() {
		if( this.points.length === 0 ) {
			return;
		}
		var asyn = false;
		var point = this.points.shift();
		if( pl.type.is_term( point.goal ) ) {
			var atom = point.goal.select();
			var states = [];
			if( atom !== null ) {
				if( pl.type.is_builtin( atom ) ) {
					this.__call_indicator = atom.indicator;
					asyn = pl.predicate[atom.indicator]( this, point, atom );
				} else if( this.rules[atom.indicator] instanceof Function ) {
					asyn = this.rules[atom.indicator]( this, point, atom );
				} else if( this.rules[atom.indicator] ) {
					var lx = new Term( "$tau:level", [new Term( atom.indicator, [] )] );
					var ly = new Term( "$tau:level", [new Term( this.level, [] )] );
					for( var _rule in this.rules[atom.indicator] ) {
						if(!this.rules[atom.indicator].hasOwnProperty(_rule)) continue;
						var rule = this.rules[atom.indicator][_rule];
						this.renamed_variables = {};
						rule = rule.rename( this );
						if( rule.body !== null ) {
							rule.body = new Term( ",", [lx, new Term( ",", [rule.body, ly] ) ] );
						}
						var state = pl.unify( atom, rule.head );
						if( state !== null ) {
							state.goal = point.goal.replace( rule.body );
							if( state.goal !== null ) {
								state.goal = state.goal.apply( state.substitution );
							}
							state.substitution = point.substitution.apply( state.substitution );
							state.parent = point;
							states.push( state );
						}
					}
					this.prepend( states );
				} else {
					if( !this.is_public_predicate( atom.indicator ) ) {
						if( this.flag.unknown.id === "error" ) {
							this.throwError( pl.error.existence( "procedure", atom.indicator, this.level ) );
						} else if( this.flag.unknown.id === "warning" ) {
							this.throw_warning( "unknown procedure " + atom.indicator + " (from " + this.level + ")" );
						}
					}
				}
			}
		} else if( pl.type.is_variable( point.goal ) ) {
			this.throwError( pl.error.instantiation( this.level ) );
		} else {
			this.throwError( pl.error.type( "callable", point.goal, this.level ) );
		}
		return asyn;
	};
	
	// Find next computed answer
	Session.prototype.answer = function( success ) {
		success = success || console.log;
		this.__calls.push( success );
		if( this.__calls.length > 1 ) {
			return;
		}
		this.again();
	};
	
	// Find all computed answers
	Session.prototype.answers = function( limit, callback, answers ) {
		answers = answers || [];
		var session = this;
		this.__calls = [];
		if( limit > 0 ) {
			this.answer( function( answer ) {
				answers.push( answer );
				if( answer !== null && answer !== false ) {
					session.answers( limit - 1, callback, answers );
				} else {
					callback( answers );
				}
			} );
		} else {
			answers.push( null );
			callback( answers );
		}
	};

	// Again finding next computed answer
	Session.prototype.again = function() {
		var answer;
		while( this.__calls.length > 0 ) {
			this.warnings = [];
			this.current_limit = this.limit;
			while( this.current_limit > 0 && this.points.length > 0 && this.points[0].goal !== null && !pl.type.is_error( this.points[0].goal ) ) {
				this.current_limit--;
				if( this.step() === true ) {
					return;
				}
			}
			var success = this.__calls.shift();
			if( this.current_limit <= 0 ) {
				success( null );
			} else if( this.points.length === 0 ) {
				success( false );
			} else if( pl.type.is_error( this.points[0].goal ) ) {
				answer = this.points.shift().goal;
				this.points = [];
				success( answer );
			} else {
				answer = this.points.shift().substitution;
				if( pl.type.is_substitution( answer ) ) {
					answer = answer.filter( this.variables );
				}
				success( answer );
			}
		}
	};
	
	
	
	// INTERPRET EXPRESSIONS
	
	// Variables
	Var.prototype.interpret = function( session ) {
		return pl.error.instantiation( session.level );
	};
	
	// Numbers
	Num.prototype.interpret = function( session ) {
		return this;
	};
	
	// Terms
	Term.prototype.interpret = function( session ) {
		if( pl.type.is_unitary_list( this ) ) {
			return this.args[0].interpret( session );
		} else {
			return pl.operate( session, this );
		}
	};
	
	
	
	// COMPOSE FINAL ANSWER
	
	// Variables
	Var.prototype.compose = function( subs, variable ) {
		if( this.id !== variable && subs.lookup( this.id ) ) {
			return subs.lookup( this.id ).compose( subs, this.id );
		} else {
			return this;
		}
	};
	
	// Numbers
	Num.prototype.compose = function( _1, _2 ) {
		return this;
	};
	
	// Terms
	Term.prototype.compose = function( subs, variable ) {
		return new Term( this.id, map( this.args, function( arg ) {
			return arg.compose( subs, variable );
		} ) );
	};
	
	// Substitutions
	Substitution.prototype.compose = function( subs ) {
		var links = {};
		for( var link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			links[link] = this.links[link].compose( subs, link );
		}
		return new Substitution( links );
	};
	
	
	
	// COMPARE PROLOG OBJECTS
	
	// Variables
	Var.prototype.compare = function( obj ) {
		if( this.id < obj.id ) {
			return -1;
		} else if( this.id > obj.id ) {
			return 1;
		} else {
			return 0;
		}
	};
	
	// Numbers
	Num.prototype.compare = function( obj ) {
		if( this.value === obj.value && this.is_float === obj.is_float ) {
			return 0;
		} else if( this.value < obj.value || this.value === obj.value && this.is_float && !obj.is_float ) {
			return -1;
		} else if( this.value > obj.value ) {
			return 1;
		}
	};
	
	// Terms
	Term.prototype.compare = function( obj ) {
		if( this.args.length < obj.args.length || this.args.length === obj.args.length && this.id < obj.id ) {
			return -1;
		} else if( this.args.length > obj.args.length || this.args.length === obj.args.length && this.id > obj.id ) {
			return 1;
		} else {
			for( var i = 0; i < this.args.length; i++ ) {
				var arg = pl.compare( this.args[i], obj.args[i] );
				if( arg !== 0 ) {
					return arg;
				}
			}
			return 0;
		}
	};
	

	
	// SUBSTITUTIONS
	
	// Lookup variable
	Substitution.prototype.lookup = function( variable ) {
		if( this.links[variable] ) {
			return this.links[variable];
		} else {
			return null;
		}
	};
	
	// Filter variables
	Substitution.prototype.filter = function( variables ) {
		var links = {};
		for( var _variable in variables ) {
			if(!variables.hasOwnProperty(_variable)) continue;
			var variable = variables[_variable];
			if( this.links[variable] ) {
				links[variable] = this.links[variable];
			}
		}
		return new Substitution( links );
	};
	
	// Exclude variables
	Substitution.prototype.exclude = function( variables ) {
		var links = {};
		for( var variable in this.links ) {
			if(!this.links.hasOwnProperty(variable)) continue;
			if( indexOf( variables, variable ) === -1 ) {
				links[variable] = this.links[variable];
			}
		}
		return new Substitution( links );
	};
	
	// Add link
	Substitution.prototype.add = function( variable, value ) {
		var subs = new Substitution();
		subs.links[variable] = value;
		return this.apply( subs );
	};
	
	
	
	// GENERATE JAVASCRIPT CODE FROM PROLOG OBJECTS
	
	// Variables
	Var.prototype.compile = function() {
		return 'new pl.type.Var("' + this.id.toString() + '")';
	};
	
	// Numbers
	Num.prototype.compile = function() {
		return 'new pl.type.Num(' + this.value.toString() + ', ' + this.is_float.toString() + ')';
	};
	
	// Terms
	Term.prototype.compile = function() {
		return 'new pl.type.Term("' + this.id.replace(/"/g, '\\"') + '", [' + map( this.args, function( arg ) {
			return arg.compile();
		} ) + '])';
	};
	
	// Rules
	Rule.prototype.compile = function() {
		return 'new pl.type.Rule(' + this.head.compile() + ', ' + (this.body === null ? 'null' : this.body.compile()) + ')';
	};
	
	// Sessions
	Session.prototype.compile = function() {
		var str, obj = [], rules;
		for( var _indicator in this.rules ) {
			if(!this.rules.hasOwnProperty(_indicator)) continue;
			var indicator = this.rules[_indicator];
			rules = [];
			str = "\"" + _indicator + "\": [";
			for( var i = 0; i < indicator.length; i++ ) {
				rules.push( indicator[i].compile() );
			}
			str += rules.join();
			str += "]";
			obj.push( str );
		}
		return "{" + obj.join() + "};";
	};
	
	
	
	// PROLOG

	var pl = {
		
		// Modules
		module: {},
		
		// Types
		type: {
			
			// Objects
			Var: Var,
			Num: Num,
			Term: Term,
			Rule: Rule,
			State: State,
			Module: Module,
			Session: Session,
			Substitution: Substitution,
			
			// Order
			order: [Var, Num, Term],
			
			// Compare types
			compare: function( x, y ) {
				var ord_x = indexOf( pl.type.order, x.constructor );
				var ord_y = indexOf( pl.type.order, y.constructor );
				if( ord_x < ord_y ) {
					return -1;
				} else if( ord_x > ord_y ) {
					return 1;
				} else {
					return 0;
				}
			},
			
			// Is a substitution
			is_substitution: function( obj ) {
				return obj instanceof Substitution;
			},
			
			// Is a state
			is_state: function( obj ) {
				return obj instanceof State;
			},
			
			// Is a rule
			is_rule: function( obj ) {
				return obj instanceof Rule;
			},
			
			// Is a variable
			is_variable: function( obj ) {
				return obj instanceof Var;
			},
			
			// Is an anonymous variable
			is_anonymous_var: function( obj ) {
				return obj instanceof Var && obj.id === "_";
			},
			
			// Is a callable term
			is_callable: function( obj ) {
				return obj instanceof Term;
			},
			
			// Is a number
			is_number: function( obj ) {
				return obj instanceof Num;
			},
			
			// Is an integer
			is_integer: function( obj ) {
				return obj instanceof Num && !obj.is_float;
			},
			
			// Is a float
			is_float: function( obj ) {
				return obj instanceof Num && obj.is_float;
			},
			
			// Is a term
			is_term: function( obj ) {
				return obj instanceof Term;
			},
			
			// Is an atom
			is_atom: function( obj ) {
				return obj instanceof Term && obj.args.length === 0;
			},
			
			// Is atomic
			is_atomic: function( obj ) {
				return obj instanceof Term && obj.args.length === 0 || obj instanceof Num;
			},
			
			// Is compound
			is_compound: function( obj ) {
				return obj instanceof Term && obj.args.length > 0;
			},
			
			// Is a list
			is_list: function( obj ) {
				return obj instanceof Term && (obj.indicator === "[]/0" || obj.indicator === "./2");
			},
			
			// Is an empty list
			is_empty_list: function( obj ) {
				return obj instanceof Term && obj.indicator === "[]/0";
			},
			
			// Is a non empty list
			is_non_empty_list: function( obj ) {
				return obj instanceof Term && obj.indicator === "./2";
			},
			
			// Is a fully list
			is_fully_list: function( obj ) {
				while( obj instanceof Term && obj.indicator === "./2" ) {
					obj = obj.args[1];
				}
				return obj instanceof Var || obj instanceof Term && obj.indicator === "[]/0";
			},
			
			// Is a instantiated list
			is_instantiated_list: function( obj ) {
				while( obj instanceof Term && obj.indicator === "./2" ) {
					obj = obj.args[1];
				}
				return obj instanceof Term && obj.indicator === "[]/0";
			},
			
			// Is an unitary list
			is_unitary_list: function( obj ) {
				return obj instanceof Term && obj.indicator === "./2" && obj.args[1] instanceof Term && obj.args[1].indicator === "[]/0";
			},
			
			// Is a character
			is_character: function( obj ) {
				return obj instanceof Term && obj.id.length === 1;
			},
			
			// Is a character
			is_character_code: function( obj ) {
				return obj instanceof Num && !obj.is_float;
			},
			
			// Is an operator
			is_operator: function( obj ) {
				return obj instanceof Term && pl.arithmetic.evaluation[obj.indicator];
			},
			
			// Is a directive
			is_directive: function( obj ) {
				return obj instanceof Term && pl.directive[obj.indicator] !== undefined;
			},
			
			// Is a built-in predicate
			is_builtin: function( obj ) {
				return obj instanceof Term && pl.predicate[obj.indicator] !== undefined;
			},
			
			// Is an error
			is_error: function( obj ) {
				return obj instanceof Term && obj.indicator === "throw/1";
			},
			
			// Is a predicate indicator
			is_predicate_indicator: function( obj ) {
				return obj instanceof Term && obj.indicator === "//2" && obj.args[0] instanceof Term && obj.args[0].args.length === 0 && obj.args[1] instanceof Num && obj.args[1].is_float === false;
			},
			
			// Is a flag
			is_flag: function( obj ) {
				return obj instanceof Term && obj.args.length === 0 && pl.flag[obj.id] !== undefined;
			},
			
			// Is a valid value for a flag
			is_value_flag: function( flag, obj ) {
				if( !pl.type.is_flag( flag ) ) return false;
				for( var value in pl.flag[flag.id].allowed ) {
					if(!pl.flag[flag.id].allowed.hasOwnProperty(value)) continue;
					if( pl.flag[flag.id].allowed[value].equals( obj ) ) return true;
				}
				return false;
			},
			
			// Is a modifiable flag
			is_modifiable_flag: function( obj ) {
				return pl.type.is_flag( obj ) && pl.flag[obj.id].changeable;
			},
			
			// Is an existing module
			is_module: function( obj ) {
				return obj instanceof Term && obj.indicator === "library/1" && obj.args[0] instanceof Term && obj.args[0].args.length === 0 && pl.module[obj.args[0].id] !== undefined;
			}
			
		},

		// Arithmetic functions
		arithmetic: {
			
			// Evaluation
			evaluation: {
				"e/0": {
					type_args: null,
					type_result: true,
					fn: function( _ ) { return Math.E; }
				},
				"pi/0": {
					type_args: null,
					type_result: true,
					fn: function( _ ) { return Math.PI; }
				},
				"+/1": {
					type_args: null,
					type_result: null,
					fn: function( x, _ ) { return x; }
				},
				"-/1": {
					type_args: null,
					type_result: null,
					fn: function( x, _ ) { return -x; }
				},
				"\\/1": {
					type_args: false,
					type_result: false,
					fn: function( x, _ ) { return ~x; }
				},
				"abs/1": {
					type_args: null,
					type_result: null,
					fn: function( x, _ ) { return Math.abs( x ); }
				},
				"sign/1": {
					type_args: null,
					type_result: null,
					fn: function( x, _ ) { return Math.sign( x ); }
				},
				"float_integer_part/1": {
					type_args: true,
					type_result: false,
					fn: function( x, _ ) { return parseInt( x ); }
				},
				"float_fractional_part/1": {
					type_args: true,
					type_result: true,
					fn: function( x, _ ) { return x - parseInt( x ); }
				},
				"float/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return parseFloat( x ); }
				},
				"floor/1": {
					type_args: true,
					type_result: false,
					fn: function( x, _ ) { return Math.floor( x ); }
				},
				"truncate/1": {
					type_args: true,
					type_result: false,
					fn: function( x, _ ) { return parseInt( x ); }
				},
				"round/1": {
					type_args: true,
					type_result: false,
					fn: function( x, _ ) { return Math.round( x ); }
				},
				"ceiling/1": {
					type_args: true,
					type_result: false,
					fn: function( x, _ ) { return Math.ceil( x ); }
				},
				"sin/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.sin( x ); }
				},
				"cos/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.cos( x ); }
				},
				"tan/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.tan( x ); }
				},
				"asin/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.asin( x ); }
				},
				"acos/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.acos( x ); }
				},
				"atan/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.atan( x ); }
				},
				"exp/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.exp( x ); }
				},
				"sqrt/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.sqrt( x ); }
				},
				"log/1": {
					type_args: null,
					type_result: true,
					fn: function( x, session ) { return x > 0 ? Math.log( x ) : pl.error.evaluation( "undefined", session.__call_indicator ); }
				},
				"+/2": {
					type_args: null,
					type_result: null,
					fn: function( x, y, _ ) { return x + y; }
				},
				"-/2": {
					type_args: null,
					type_result: null,
					fn:  function( x, y, _ ) { return x - y; }
				},
				"*/2": {
					type_args: null,
					type_result: null,
					fn: function( x, y, _ ) { return x * y; }
				},
				"//2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, session ) { return y ? x / y : pl.error.evaluation( "zero_division", session.__call_indicator ); }
				},
				"///2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, session ) { return y ? parseInt( x / y ) : pl.error.evaluation( "zero_division", session.__call_indicator ); }
				},
				"**/2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, _ ) { return Math.pow(x, y); }
				},
				"<</2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, _ ) { return x << y; }
				},
				">>/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, _ ) { return x >> y; }
				},
				"/\\/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, _ ) { return x & y; }
				},
				"\\//2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, _ ) { return x | y; }
				},
				"rem/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, session ) { return y ? x % y : pl.error.evaluation( "zero_division", session.__call_indicator ); }
				},
				"mod/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, session ) { return y ? x - parseInt( x / y ) * y : pl.error.evaluation( "zero_division", session.__call_indicator ); }
				}
				
			}
			
		},
		
		// Directives
		directive: {
			
			// dynamic/1
			"dynamic/1": function( session, atom ) {
				var indicator = atom.args[0];
				if( pl.type.is_variable( indicator ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_compound( indicator ) || indicator.indicator !== "//2" ) {
					session.throwError( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				} else if( pl.type.is_variable( indicator.args[0] ) || pl.type.is_variable( indicator.args[1] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( indicator.args[0] ) ) {
					session.throwError( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
				} else if( !pl.type.is_integer( indicator.args[1] ) ) {
					session.throwError( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
				} else {
					session.public.push( atom.args[0].args[0].id + "/" + atom.args[0].args[1].value );
				}
			},
			
			// use_module/1
			"use_module/1": function( session, atom ) {
				var module = atom.args[0];
				if( pl.type.is_variable( module ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_term( module ) ) {
					session.throwError( pl.error.type( "term", module, atom.indicator ) );
				} else {
					if( pl.type.is_module( module ) ) {
						var name = module.args[0].id;
						if( indexOf( session.__loaded_modules, name ) === -1 ) {
							session.__loaded_modules.push( name );
							var get_module = pl.module[name].rules;
							for( var predicate in get_module ) {
								if(!get_module.hasOwnProperty(predicate)) continue;
								session.rules[predicate] = get_module[predicate];
							}
						}
					} else {
						// TODO
						// error no existe modulo
					}
				}
			},
			
			// char_conversion/2
			"char_conversion/2": function( session, atom ) {
				var inchar = atom.args[0], outchar = atom.args[1];
				if( pl.type.is_variable( inchar ) || pl.type.is_variable( outchar ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_character( inchar ) ) {
					session.throwError( pl.error.type( "character", inchar, atom.indicator ) );
				} else if( !pl.type.is_character( outchar ) ) {
					session.throwError( pl.error.type( "character", outchar, atom.indicator ) );
				} else {
					if( inchar.id === outchar.id ) {
						delete session.__char_conversion[inchar.id];
					} else {
						session.__char_conversion[inchar.id] = outchar.id;
					}
				}
			},
			
			// op/3
			"op/3": function( session, atom ) {
				var priority = atom.args[0], type = atom.args[1], operator = atom.args[2];
				if( pl.type.is_variable( priority ) || pl.type.is_variable( type ) || pl.type.is_variable( operator ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( priority ) ) {
					session.throwError( pl.error.type( "integer", priority, atom.indicator ) );
				} else if( !pl.type.is_atom( type ) ) {
					session.throwError( pl.error.type( "atom", type, atom.indicator ) );
				} else if( !pl.type.is_atom( operator ) ) {
					session.throwError( pl.error.type( "atom", operator, atom.indicator ) );
				} else if( priority.value < 0 || priority.value > 1200 ) {
					session.throwError( pl.error.domain( "operator_priority", priority, atom.indicator ) );
				} else if( operator.id === "," ) {
					session.throwError( pl.error.permission( "modify", "operator", operator, atom.indicator ) );
				} else if( ["fy", "fx", "yf", "xf", "xfx", "yfx", "xfy"].indexOf( type.id ) === -1 ) {
					session.throwError( pl.error.domain( "operator_specifier", type, atom.indicator ) );
				} else {
					var fix = { prefix: null, infix: null, postfix: null };
					for( var p in session.__operators ) {
						if(!session.__operators.hasOwnProperty(p)) continue;
						var classes = session.__operators[p][operator.id];
						if( classes ) {
							if( indexOf( classes, "fx" ) !== -1 ) { fix.prefix = { priority: p, type: "fx" }; }
							if( indexOf( classes, "fy" ) !== -1 ) { fix.prefix = { priority: p, type: "fy" }; }
							if( indexOf( classes, "xf" ) !== -1 ) { fix.postfix = { priority: p, type: "xf" }; }
							if( indexOf( classes, "yf" ) !== -1 ) { fix.postfix = { priority: p, type: "yf" }; }
							if( indexOf( classes, "xfx" ) !== -1 ) { fix.infix = { priority: p, type: "xfx" }; }
							if( indexOf( classes, "xfy" ) !== -1 ) { fix.infix = { priority: p, type: "xfy" }; }
							if( indexOf( classes, "yfx" ) !== -1 ) { fix.infix = { priority: p, type: "yfx" }; }
						}
					}
					var current_class;
					switch( type.id ) {
						case "fy": case "fx": current_class = "prefix"; break;
						case "yf": case "xf": current_class = "postfix"; break;
						default: current_class = "infix"; break;
					}
					if( ((fix.prefix && current_class === "prefix" || fix.postfix && current_class === "postfix" || fix.infix && current_class === "infix")
						&& fix[current_class].type !== type.id || fix.infix && current_class === "postfix" || fix.postfix && current_class === "infix") && priority.value !== 0 ) {
						session.throwError( pl.error.permission( "create", "operator", operator, atom.indicator ) );
					} else {
						if( fix[current_class] ) {
							remove( session.__operators[fix[current_class].priority][operator.id], type.id );
							if( session.__operators[fix[current_class].priority][operator.id].length === 0 ) {
								delete session.__operators[fix[current_class].priority][operator.id];
							}
						}
						if( priority.value > 0 ) {
							if( !session.__operators[priority.value] ) session.__operators[priority.value.toString()] = {};
							if( !session.__operators[priority.value][operator.id] ) session.__operators[priority.value][operator.id] = [];
							session.__operators[priority.value][operator.id].push( type.id );
						}
					}
				}
			}
			
		},
		
		// Built-in predicates
		predicate: {
			
			// TAU PROLOG
			
			// $tau:level/1
			"$tau:level/1": function( session, point, atom ) {
				session.level = atom.args[0].id;
				session.success( point );
			},
		
			// LOGIC AND CONTROL STRUCTURES
		
			// ;/2 (disjunction)
			";/2": function( session, point, atom ) {
				var left = new State( point.goal.replace( atom.args[0] ), point.substitution, point );
				var right = new State( point.goal.replace( atom.args[1] ), point.substitution, point );
				session.prepend( [left, right] );
			},
			
			// !/0 (cut)
			"!/0": function( session, point, atom ) {
				var parent_cut, states = [];
				parent_cut = point;
				while( parent_cut.parent !== null && parent_cut.parent.goal.search( atom ) )
					parent_cut = parent_cut.parent;
				for( var i = 0; i < session.points.length; i++ ) {
					var state = session.points[i];
					var node = state.parent;
					while( node !== null && node !== parent_cut.parent ) {
						node = node.parent;
					}
					if( node === null && node !== parent_cut.parent )
						states.push( state );
				}
				session.points = [new State( point.goal.replace( null ), point.substitution, point )].concat( states );
			},
			
			// \+ (negation)
			"\\+/1": function( session, point, atom ) {
				var goal = atom.args[0];
				if( pl.type.is_variable( goal ) ) {
					session.throwError( pl.error.instantiation( session.level ) );
				} else if( !pl.type.is_callable( goal ) ) {
					session.throwError( pl.error.type( "callable", goal, session.level ) );
				} else {
					var points = session.points;
					session.points = [new State( atom.args[0], point.substitution, point )];
					var callback = function( answer ) {
						if( answer === false )
							session.success( point );
						else if( pl.type.is_error( answer ) )
							session.throwError( answer.args[0] );
						else if( answer === null ) {
							session.points = [point].concat( points );
							session.__calls.shift()( null );
						} else
							session.points = points;
					};
					session.__calls.unshift( callback );
				}
			},
			
			// ->/2 (implication)
			"->/2": function( session, point, atom ) {
				var goal = point.goal.replace( new Term( ",", [atom.args[0], new Term( ",", [new Term( "!" ), atom.args[1]] )] ) );
				session.prepend( [new State( goal, point.substitution, point )] );
			},
			
			// fail/0
			"fail/0": function( _1, _2, _3 ) {},
			
			// true/0
			"true/0": function( session, point, _ ) {
				session.success( point );
			},
			
			// call/1
			"call/1": function( session, point, atom ) {
				var goal = atom.args[0];
				if( pl.type.is_variable( goal ) ) {
					session.throwError( pl.error.instantiation( session.level ) );
				} else if( !pl.type.is_callable( goal ) ) {
					session.throwError( pl.error.type( "callable", goal, session.level ) );
				} else {
					session.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
				}
			},
			
			// once/1
			"once/1": function( session, point, atom ) {
				var goal = atom.args[0];
				if( pl.type.is_variable( goal ) ) {
					session.throwError( pl.error.instantiation( session.level ) );
				} else if( !pl.type.is_callable( goal ) ) {
					session.throwError( pl.error.type( "callable", goal, session.level ) );
				} else {
					var session2 = session.program.session( goal, session.limit );
					session2.copy_context( session );
					var answer = session2.answer();
					if( pl.type.is_error( answer ) ) {
						session.throwError( answer.args[0] );
					} else if( pl.type.is_substitution( answer ) ) {
						session.prepend( [new State( point.goal.apply( answer ).replace( null ), point.substitution.apply( answer ), point )] );
					}
					session.copy_context( session2 );
				}
			},
			
			// repeat/0
			"repeat/0": function( session, point, _ ) {
				session.prepend( [new State( point.goal.replace( null ), point.substitution, point ), point] );
			},
			
			// EXCEPTIONS
			
			// throw/1
			"throw/1": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.throwError( pl.error.instantiation( session.level ) );
				} else {
					session.throwError( atom.args[0] );
				}
			},
			
			// catch/3
			"catch/3": function( session, point, atom ) {
				var points = session.points;
				session.points = [];
				session.prepend( [new State( atom.args[0], point.substitution, point )] );
				var callback = function( answer ) {
					var call_points = session.points;
					session.points = points;
					if( pl.type.is_error( answer ) ) {
						var states = [];
						for( var i = 0; i < session.points.length; i++ ) {
							var state = session.points[i];
							var node = state.parent;
							while( node !== null && node !== point.parent ) {
								node = node.parent;
							}
							if( node === null && node !== point.parent )
								states.push( state );
						}
						session.points = states;
						var state = pl.unify( answer.args[0], atom.args[1], false );
						if( state !== null ) {
							state.substitution = state.substitution.apply( point.substitution.exclude( point.exclude ? point.exclude : [] ) );
							state.goal = point.goal.replace( atom.args[2] ).apply( state.substitution );
							session.prepend( [state] );
						} else {
							session.throwError( answer.args[0] );
						}
					} else if( answer !== false ) {
						var answer_state = answer === null ? [] : [new State(
							point.goal.apply( answer ).replace( null ),
							point.substitution.apply( answer ),
							point
						)];
						var catch_points = map( call_points, function( state ) {
							if( state.goal === null )
								state.goal = new Term( "true", [] );
							state = new State(
								point.goal.replace( new Term( "catch", [state.goal, atom.args[1], atom.args[2]] ) ),
								point.substitution.apply( state.substitution ),
								state.parent
							);
							state.exclude = atom.args[0].variables();
							return state;
						} );
						session.prepend( answer_state.concat( catch_points ) );
						if( answer === null ) {
							this.current_limit = 0;
							session.__calls.shift()( null );
						}
					}
				};
				session.__calls.unshift( callback );
			},
			
			// UNIFICATION
			
			// =/2 (unification)
			"=/2": function( session, point, atom ) {
				var state = pl.unify( atom.args[0], atom.args[1], false );
				if( state !== null ) {
					state.goal = point.goal.apply( state.substitution ).replace( null );
					state.substitution = point.substitution.apply( state.substitution );
					state.parent = point;
					session.prepend( [state] );
				}
			},
			
			// unify_with_occurs_check/2
			"unify_with_occurs_check/2": function( session, point, atom ) {
				var state = pl.unify( atom.args[0], atom.args[1], true );
				if( state !== null ) {
					state.goal = point.goal.apply( state.substitution ).replace( null );
					state.substitution = point.substitution.apply( state.substitution );
					state.parent = point;
					session.prepend( [state] );
				}
			},
			
			// \=/2
			"\\=/2": function( session, point, atom ) {
				var state = pl.unify( atom.args[0], atom.args[1] );
				state.parent = point;
				if( state === null ) {
					session.success( point );
				}
			},
			
			// ALL SOLUTIONS
			
			// findall/3
			"findall/3": function( session, point, atom ) {
				var template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
				if( pl.type.is_variable( goal ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( goal ) ) {
					session.throwError( pl.error.type( "callable", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( instances ) && !pl.type.is_list( instances ) ) {
					session.throwError( pl.error.type( "list", instances, atom.indicator ) );
				} else {
					var variable = session.next_free_variable();
					var newGoal = new Term( ",", [goal, new Term( "=", [variable, template] )] );
					var points = session.points;
					var variables = session.variables;
					var limit = session.limit;
					session.points = [new State( newGoal, new Substitution(), point )];
					session.variables = [variable.id];
					var answers = [];
					var callback = function( answer ) {
						if( answer !== false && answer !== null && !pl.type.is_error( answer ) ) {
							session.__calls.unshift( callback );
							answers.push( answer.links[variable.id] );
							session.limit = session.current_limit;
						} else {
							session.points = points;
							session.variables = variables;
							session.limit = limit;
							if( pl.type.is_error( answer ) ) {
								session.throwError( answer.args[0] );
							} else if( session.current_limit > 0 ) {
								var list = new Term( "[]" );
								for( var i = answers.length - 1; i >= 0; i-- ) {
									list = new Term( ".", [answers[i], list] );
								}
								session.prepend( [new State( point.goal.replace( new Term( "=", [instances, list] ) ), point.substitution, point )] );
							}
						}
					};
					session.__calls.unshift( callback );
				}
			},
			
			// bagof/3
			"bagof/3": function( session, point, atom ) {
				var answer, template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
				if( pl.type.is_variable( goal ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( goal ) ) {
					session.throwError( pl.error.type( "callable", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( instances ) && !pl.type.is_list( instances ) ) {
					session.throwError( pl.error.type( "list", instances, atom.indicator ) );
				} else {
					var variable = session.next_free_variable();
					var template_vars;
					if( goal.indicator === "^/2" ) {
						template_vars = goal.args[0].variables();
						goal = goal.args[1];
					} else {
						template_vars = [];
					}
					template_vars = template_vars.concat( template.variables() );
					var free_vars = goal.variables().filter( function( v ){
						return indexOf( template_vars, v ) === -1;
					} );
					var list_vars = new Term( "[]" );
					for( var i = free_vars.length - 1; i >= 0; i-- ) {
						list_vars = new Term( ".", [ new Var( free_vars[i] ), list_vars ] );
					}
					var newGoal = new Term( ",", [goal, new Term( "=", [variable, new Term( ",", [list_vars, template] )] )] );
					var points = session.points;
					var variables = session.variables;
					var limit = session.limit;
					session.points = [new State( newGoal, new Substitution(), point )];
					session.variables = [variable.id];
					var answers = [];
					var callback = function( answer ) {
						if( answer !== false && answer !== null && !pl.type.is_error( answer ) ) {
							session.__calls.unshift( callback );
							var match = false;
							var arg_vars = answer.links[variable.id].args[0];
							var arg_template = answer.links[variable.id].args[1];
							for( var _elem in answers ) {
								if(!answers.hasOwnProperty(_elem)) continue;
								var elem = answers[_elem];
								if( elem.variables.equals( arg_vars ) ) {
									elem.answers.push( arg_template );
									match = true;
									break;
								}
							}
							if( !match ) {
								answers.push( {variables: arg_vars, answers: [arg_template]} );
							}
							session.limit = session.current_limit;
						} else {
							session.points = points;
							session.variables = variables;
							session.limit = limit;
							if( pl.type.is_error( answer ) ) {
								session.throwError( answer.args[0] );
							} else if( session.current_limit > 0 ) {
								var states = [];
								for( var i = 0; i < answers.length; i++ ) {
									answer = answers[i].answers;
									var list = new Term( "[]" );
									for( var j = answer.length - 1; j >= 0; j-- ) {
										list = new Term( ".", [answer[j], list] );
									}
									states.push( new State(
										point.goal.replace( new Term( ",", [new Term( "=", [list_vars, answers[i].variables] ), new Term( "=", [instances, list] )] ) ),
										point.substitution, point
									) );
								}
								session.prepend( states );
							}
						}
					};
					session.__calls.unshift( callback );
				}
			},
	
			// setof/3
			"setof/3": function( session, point, atom ) {
				var answer, template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
				if( pl.type.is_variable( goal ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( goal ) ) {
					session.throwError( pl.error.type( "callable", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( instances ) && !pl.type.is_list( instances ) ) {
					session.throwError( pl.error.type( "list", instances, atom.indicator ) );
				} else {
					var variable = session.next_free_variable();
					var template_vars;
					if( goal.indicator === "^/2" ) {
						template_vars = goal.args[0].variables();
						goal = goal.args[1];
					} else {
						template_vars = [];
					}
					template_vars = template_vars.concat( template.variables() );
					var free_vars = goal.variables().filter( function( v ){
						return indexOf( template_vars, v ) === -1;
					} );
					var list_vars = new Term( "[]" );
					for( var i = free_vars.length - 1; i >= 0; i-- ) {
						list_vars = new Term( ".", [ new Var( free_vars[i] ), list_vars ] );
					}
					var newGoal = new Term( ",", [goal, new Term( "=", [variable, new Term( ",", [list_vars, template] )] )] );
					var points = session.points;
					var variables = session.variables;
					var limit = session.limit;
					session.points = [new State( newGoal, new Substitution(), point )];
					session.variables = [variable.id];
					var answers = [];
					var callback = function( answer ) {
						if( answer !== false && answer !== null && !pl.type.is_error( answer ) ) {
							session.__calls.unshift( callback );
							var match = false;
							var arg_vars = answer.links[variable.id].args[0];
							var arg_template = answer.links[variable.id].args[1];
							for( var _elem in answers ) {
								if(!answers.hasOwnProperty(_elem)) continue;
								var elem = answers[_elem];
								if( elem.variables.equals( arg_vars ) ) {
									elem.answers.push( arg_template );
									match = true;
									break;
								}
							}
							if( !match ) {
								answers.push( {variables: arg_vars, answers: [arg_template]} );
							}
							session.limit = session.current_limit;
						} else {
							session.points = points;
							session.variables = variables;
							session.limit = limit;
							if( pl.type.is_error( answer ) ) {
								session.throwError( answer.args[0] );
							} else if( session.current_limit > 0 ) {
								var states = [];
								for( var i = 0; i < answers.length; i++ ) {
									answer = answers[i].answers.sort( pl.compare );
									var list = new Term( "[]" );
									for( var j = answer.length - 1; j >= 0; j-- ) {
										list = new Term( ".", [answer[j], list] );
									}
									states.push( new State(
										point.goal.replace( new Term( ",", [new Term( "=", [list_vars, answers[i].variables] ), new Term( "=", [instances, list] )] ) ),
										point.substitution, point
									) );
								}
								session.prepend( states );
							}
						}
					};
					session.__calls.unshift( callback );
				}
			},
			
			// TERM CREATION AND DECOMPOSITION
			
			// functor/3
			"functor/3": function( session, point, atom ) {
				var subs;
				if( pl.type.is_variable( atom.args[0] ) && (pl.type.is_variable( atom.args[1] ) || pl.type.is_variable( atom.args[2] )) ) {
					session.throwError( pl.error.instantiation( "functor/3" ) );
				} else if( !pl.type.is_variable( atom.args[2] ) && !pl.type.is_integer( atom.args[2] ) ) {
					session.throwError( pl.error.type( "integer", atom.args[2], "functor/3" ) );
				} else if( !pl.type.is_variable( atom.args[1] ) && !pl.type.is_atomic( atom.args[1] ) ) {
					session.throwError( pl.error.type( "atomic", atom.args[1], "functor/3" ) );
				} else if( pl.type.is_variable( atom.args[0] ) ) {
					if( atom.args[2].value >= 0 ) {
						subs = new Substitution();
						var args = [];
						for( var i = 0; i < atom.args[2].value; i++ ) {
							session.rename++;
							while( indexOf( session.variables, pl.format_variable( session.rename ) ) !== -1 ) {
								session.rename++;
							}
							args.push( new Var( pl.format_variable( session.rename ) ) );
						}
						subs = new Substitution().add( atom.args[0].id, new Term( atom.args[1].id, args ) );
						session.prepend( [new State( point.goal.apply( subs ).replace( null ), point.substitution.apply( subs ), point )] );
					}
				} else {
					var id = new Term( atom.args[0].id, [] );
					var length = new Num( atom.args[0].args.length, false );
					var goal = new Term( ",", [new Term( "=", [id, atom.args[1]] ), new Term( "=", [length, atom.args[2]] )] );
					session.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
				}
			},
			
			// arg/3
			"arg/3": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) || pl.type.is_variable( atom.args[1] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( atom.args[0].value < 0 ) {
					session.throwError( pl.error.domain( "not_less_than_zero", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_compound( atom.args[1] ) ) {
					session.throwError( pl.error.type( "compound", atom.args[1], atom.indicator ) );
				} else {
					var n = atom.args[0].value;
					if( n > 0 && n <= atom.args[1].args.length ) {
						var goal = new Term( "=", [atom.args[1].args[n-1], atom.args[2]] );
						session.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
					}
				}
			},
			
			// =../2 (univ)
			"=../2": function( session, point, atom ) {
				var list;
				if( pl.type.is_variable( atom.args[0] ) && (pl.type.is_variable( atom.args[1] )
				|| pl.type.is_non_empty_list( atom.args[1] ) && pl.type.is_variable( atom.args[1].args[0] )) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_fully_list( atom.args[1] ) ) {
					session.throwError( pl.error.type( "list", atom.args[1], atom.indicator ) );
				} else if( !pl.type.is_variable( atom.args[0] ) ) {
					if( pl.type.is_atomic( atom.args[0] ) ) {
						list = new Term( ".", [atom.args[0], new Term( "[]" )] );
					} else {
						list = new Term( "[]" );
						for( var i = atom.args[0].args.length - 1; i >= 0; i-- ) {
							list = new Term( ".", [atom.args[0].args[i], list] );
						}
						list = new Term( ".", [new Term( atom.args[0].id ), list] );
					}
					session.prepend( [new State( point.goal.replace( new Term( "=", [list, atom.args[1]] ) ), point.substitution, point )] );
				} else if( !pl.type.is_variable( atom.args[1] ) ) {
					var args = [];
					list = atom.args[1].args[1];
					while( list.indicator === "./2" ) {
						args.push( list.args[0] );
						list = list.args[1];
					}
					if( pl.type.is_variable( atom.args[0] ) && pl.type.is_variable( list ) ) {
						session.throwError( pl.error.instantiation( atom.indicator ) );
					} else if( args.length === 0 && pl.type.is_compound( atom.args[1].args[0] ) ) {
						session.throwError( pl.error.type( "atomic", atom.args[1].args[0], atom.indicator ) );
					} else if( args.length > 0 && (pl.type.is_compound( atom.args[1].args[0] ) || pl.type.is_number( atom.args[1].args[0] )) ) {
						session.throwError( pl.error.type( "atom", atom.args[1].args[0], atom.indicator ) );
					} else {
						if( args.length === 0 ) {
							session.prepend( [new State( point.goal.replace( new Term( "=", [atom.args[1].args[0], atom.args[0]], point ) ), point.substitution, point )] );
						} else {
							session.prepend( [new State( point.goal.replace( new Term( "=", [new Term( atom.args[1].args[0].id, args ), atom.args[0]] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// copy_term/2
			"copy_term/2": function( session, point, atom ) {
				var state = pl.unify( atom.args[0], atom.args[1], false );
				if( state !== null ) {
					var subs = state.substitution.filter( atom.args[1].variables() );
					for( var variable in subs.links ) {
						if(!subs.links.hasOwnProperty(variable)) continue;
						if( pl.type.is_variable( subs.links[variable] ) ) {
							delete subs.links[variable];
						}
					}
					state.goal = point.goal.apply( subs ).replace( null );
					state.substitution = point.substitution.apply( subs );
					session.prepend( [state] );
				}
			},
			
			// CLAUSE RETRIEVAL AND INFORMATION
			
			// clause/2
			"clause/2": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					session.throwError( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_variable( atom.args[1] ) && !pl.type.is_callable( atom.args[1] ) ) {
					session.throwError( pl.error.type( "callable", atom.args[1], atom.indicator ) );
				} else if( session.rules[atom.args[0].indicator] !== undefined ) {
					if( session.is_public_predicate( atom.args[0].indicator ) ) {
						var states = [];
						for( var _rule in session.rules[atom.args[0].indicator] ) {
							if(!session.rules[atom.args[0].indicator].hasOwnProperty(_rule)) continue;
							var rule = session.rules[atom.args[0].indicator][_rule];
							session.renamed_variables = {};
							rule = rule.rename( session );
							if( rule.body === null ) {
								rule.body = new Term( "true" );
							}
							var goal = new Term( ",", [new Term( "=", [rule.head, atom.args[0]] ), new Term( "=", [rule.body, atom.args[1]] )] );
							states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
						}
						session.prepend( states );
					} else {
						session.throwError( pl.error.permission( "access", "private_procedure", atom.args[0].indicator, atom.indicator ) );
					}
				}
			},
			
			// current_predicate/1
			"current_predicate/1": function( session, point, atom ) {
				var indicator = atom.args[0];
				if( !pl.type.is_variable( indicator ) && (!pl.type.is_compound( indicator ) || indicator.indicator !== "//2") ) {
					session.throwError( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				} else if( !pl.type.is_variable( indicator ) && !pl.type.is_variable( indicator.args[0] ) && !pl.type.is_atom( indicator.args[0] ) ) {
					session.throwError( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
				} else if( !pl.type.is_variable( indicator ) && !pl.type.is_variable( indicator.args[1] ) && !pl.type.is_integer( indicator.args[1] ) ) {
					session.throwError( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
				} else {
					var states = [];
					for( var i in session.rules ) {
						if(!session.rules.hasOwnProperty(i)) continue;
						var index = i.lastIndexOf( "/" );
						var name = i.substr( 0, index );
						var arity = parseInt( i.substr( index+1, i.length-(index+1) ) );
						var predicate = new Term( "/", [new Term( name ), new Num( arity, false )] );
						var goal = new Term( "=", [predicate, indicator] );
						states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
					}
					session.prepend( states );
				}
			},
			
			// CLAUSE CREATION AND DESTRUCTION
			
			// asserta/1
			"asserta/1": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					session.throwError( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else {
					var head, body;
					if( atom.args[0].indicator === ":-/2" ) {
						head = atom.args[0].args[0];
						body = atom.args[0].args[1];
					} else {
						head = atom.args[0];
						body = null;
					}
					if( !pl.type.is_callable( head ) ) {
						session.throwError( pl.error.type( "callable", head, atom.indicator ) );
					} else if( body !== null && !pl.type.is_callable( body ) ) {
						session.throwError( pl.error.type( "callable", body, atom.indicator ) );
					} else if( session.is_public_predicate( head.indicator ) ) {
						if( session.rules[head.indicator] === undefined ) {
							session.rules[head.indicator] = [];
						}
						session.rules[head.indicator] = [new Rule( head, body )].concat( session.rules[head.indicator] );
						session.success( point );
					} else {
						session.throwError( pl.error.permission( "modify", "static_procedure", head.indicator, atom.indicator ) );
					}
				}
			},
			
			// assertz/1
			"assertz/1": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					session.throwError( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else {
					var head, body;
					if( atom.args[0].indicator === ":-/2" ) {
						head = atom.args[0].args[0];
						body = atom.args[0].args[1];
					} else {
						head = atom.args[0];
						body = null;
					}
					if( !pl.type.is_callable( head ) ) {
						session.throwError( pl.error.type( "callable", head, atom.indicator ) );
					} else if( body !== null && !pl.type.is_callable( body ) ) {
						session.throwError( pl.error.type( "callable", body, atom.indicator ) );
					} else if( session.is_public_predicate( head.indicator ) ) {
						if( session.rules[head.indicator] === undefined ) {
							session.rules[head.indicator] = [];
						}
						session.rules[head.indicator].push( new Rule( head, body ) );
						session.success( point );
					} else {
						session.throwError( pl.error.permission( "modify", "static_procedure", head.indicator, atom.indicator ) );
					}
				}
			},
			
			// retract/1
			"retract/1": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					session.throwError( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else {
					var head, body;
					if( atom.args[0].indicator === ":-/2" ) {
						head = atom.args[0].args[0];
						body = atom.args[0].args[1];
					} else {
						head = atom.args[0];
						body = null;
					}
					if( session.is_public_predicate( head.indicator ) ) {
						if( session.rules[head.indicator] !== undefined ) {
							for( var i = 0; i < session.rules[atom.args[0].indicator].length; i++ ) {
								session.renamed_variables = {};
								var rule = session.rules[atom.args[0].indicator][i].rename( session );
								var state = pl.unify( head, rule.head );
								if( state !== null ) {
									if( body === null ) {
										body = new Term( "true" );
									}
									if( rule.body === null ) {
										rule.body = new Term( "true" );
									}
									var subs = state.substitution;
									if( pl.unify( rule.body.apply( subs ), body.apply( subs ) ) !== null ) {
										session.rules[atom.args[0].indicator].splice( i, 1 );
										session.success( point );
										return;
									}
								}
							}
						}
					} else {
						session.throwError( pl.error.permission( "modify", "static_procedure", head.indicator, atom.indicator ) );
					}
				}
			},
			
			
			// abolish/1
			"abolish/1": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) || pl.type.is_term( atom.args[0] ) && atom.args[0].indicator === "//2"
				&& (pl.type.is_variable( atom.args[0].args[0] ) || pl.type.is_variable( atom.args[0].args[1] )) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_term( atom.args[0] ) || atom.args[0].indicator !== "//2" ) {
					session.throwError( pl.error.type( "predicate_indicator", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_atom( atom.args[0].args[0] ) ) {
					session.throwError( pl.error.type( "atom", atom.args[0].args[0], atom.indicator ) );
				} else if( !pl.type.is_integer( atom.args[0].args[1] ) ) {
					session.throwError( pl.error.type( "integer", atom.args[0].args[1], atom.indicator ) );
				} else if( atom.args[0].args[1].value < 0 ) {
					session.throwError( pl.error.domain( "not_less_than_zero", atom.args[0].args[1], atom.indicator ) );
				} else if( pl.type.is_number(session.flag.max_arity) && atom.args[0].args[1].value > session.flag.max_arity.value ) {
					session.throwError( pl.error.representation( "max_arity", atom.indicator ) );
				} else {
					var indicator = atom.args[0].args[0].id + "/" + atom.args[0].args[1].value;
					if( session.is_public_predicate( indicator ) ) {
						delete session.rules[indicator];
						session.success( point );
					} else {
						session.throwError( pl.error.permission( "modify", "static_procedure", indicator, atom.indicator ) );
					}
				}
			},
			
			// ATOM PROCESSING
			
			// atom_length/2
			"atom_length/2": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( atom.args[0] ) ) {
					session.throwError( pl.error.type( "atom", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_variable( atom.args[1] ) && !pl.type.is_integer( atom.args[1] ) ) {
					session.throwError( pl.error.type( "integer", atom.args[1], atom.indicator ) );
				} else if( pl.type.is_integer( atom.args[1] ) && atom.args[1].value < 0 ) {
					session.throwError( pl.error.domain( "not_less_than_zero", atom.args[1], atom.indicator ) );
				} else {
					var length = new Num( atom.args[0].id.length, false );
					session.prepend( [new State( point.goal.replace( new Term( "=", [length, atom.args[1]] ) ), point.substitution, point )] );
				}
			},
			
			// atom_concat/3
			"atom_concat/3": function( session, point, atom ) {
				var str, goal, start = atom.args[0], end = atom.args[1], whole = atom.args[2];
				if( pl.type.is_variable( whole ) && (pl.type.is_variable( start ) || pl.type.is_variable( end )) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( start ) && !pl.type.is_atom( start ) ) {
					session.throwError( pl.error.type( "atom", start, atom.indicator ) );
				} else if( !pl.type.is_variable( end ) && !pl.type.is_atom( end ) ) {
					session.throwError( pl.error.type( "atom", end, atom.indicator ) );
				} else if( !pl.type.is_variable( whole ) && !pl.type.is_atom( whole ) ) {
					session.throwError( pl.error.type( "atom", whole, atom.indicator ) );
				} else {
					var v1 = pl.type.is_variable( start );
					var v2 = pl.type.is_variable( end );
					//var v3 = pl.type.is_variable( whole );
					if( !v1 && !v2 ) {
						goal = new Term( "=", [whole, new Term( start.id + end.id )] );
						session.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
					} else if( v1 && !v2 ) {
						str = whole.id.substr( 0, whole.id.length - end.id.length );
						if( str + end.id === whole.id ) {
							goal = new Term( "=", [start, new Term( str )] );
							session.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
						}
					} else if( v2 && !v1 ) {
						str = whole.id.substr( start.id.length );
						if( start.id + str === whole.id ) {
							goal = new Term( "=", [end, new Term( str )] );
							session.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
						}
					} else {
						var states = [];
						for( var i = 0; i <= whole.id.length; i++ ) {
							var atom1 = new Term( whole.id.substr( 0, i ) );
							var atom2 = new Term( whole.id.substr( i ) );
							goal = new Term( ",", [new Term( "=", [atom1, start] ), new Term( "=", [atom2, end] )] );
							states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
						}
						session.prepend( states );
					}
				}
			},
			
			// sub_atom/5
			"sub_atom/5": function( session, point, atom ) {
				var i, atom1 = atom.args[0], before = atom.args[1], length = atom.args[2], after = atom.args[3], subatom = atom.args[4];
				if( pl.type.is_variable( atom1 ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( before ) && !pl.type.is_integer( before ) ) {
					session.throwError( pl.error.type( "integer", before, atom.indicator ) );
				} else if( !pl.type.is_variable( length ) && !pl.type.is_integer( length ) ) {
					session.throwError( pl.error.type( "integer", length, atom.indicator ) );
				} else if( !pl.type.is_variable( after ) && !pl.type.is_integer( after ) ) {
					session.throwError( pl.error.type( "integer", after, atom.indicator ) );
				} else if( pl.type.is_integer( before ) && before.value < 0 ) {
					session.throwError( pl.error.domain( "not_less_than_zero", before, atom.indicator ) );
				} else if( pl.type.is_integer( length ) && length.value < 0 ) {
					session.throwError( pl.error.domain( "not_less_than_zero", length, atom.indicator ) );
				} else if( pl.type.is_integer( after ) && after.value < 0 ) {
					session.throwError( pl.error.domain( "not_less_than_zero", after, atom.indicator ) );
				} else {
					var bs = [], ls = [], as = [];
					if( pl.type.is_variable( before ) ) {
						for( i = 0; i <= atom1.id.length; i++ ) {
							bs.push( i );
						}
					} else {
						bs.push( before.value );
					}
					if( pl.type.is_variable( length ) ) {
						for( i = 0; i <= atom1.id.length; i++ ) {
							ls.push( i );
						}
					} else {
						ls.push( length.value );
					}
					if( pl.type.is_variable( after ) ) {
						for( i = 0; i <= atom1.id.length; i++ ) {
							as.push( i );
						}
					} else {
						as.push( after.value );
					}
					var states = [];
					for( var _i in bs ) {
						if(!bs.hasOwnProperty(_i)) continue;
						i = bs[_i];
						for( var _j in ls ) {
							if(!ls.hasOwnProperty(_j)) continue;
							var j = ls[_j];
							var k = atom1.id.length - i - j;
							if( indexOf( as, k ) !== -1 ) {
							if( i+j+k === atom1.id.length ) {
									var str = atom1.id.substr( i, j );
									if( atom1.id === atom1.id.substr( 0, i ) + str + atom1.id.substr( i+j, k ) ) {
										var pl1 = new Term( "=", [new Term( str ), subatom] );
										var pl2 = new Term( "=", [before, new Num( i )] );
										var pl3 = new Term( "=", [length, new Num( j )] );
										var pl4 = new Term( "=", [after, new Num( k )] );
										var goal = new Term( ",", [ new Term( ",", [ new Term( ",", [pl2, pl3] ), pl4] ), pl1] );
										states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
									}
								}
							}
						}
					}
					session.prepend( states );
				}
			},
			
			// atom_chars/2
			"atom_chars/2": function( session, point, atom ) {
				var atom1 = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( atom1 ) && pl.type.is_variable( list ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( atom1 ) && !pl.type.is_atom( atom1 ) ) {
					session.throwError( pl.error.type( "atom", atom1, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( atom1 ) ) {
						var list1 = new Term( "[]" );
						for( var i = atom1.id.length-1; i >= 0; i-- ) {
							list1 = new Term( ".", [new Term( atom1.id.charAt( i ) ), list1] );
						}
						session.prepend( [new State( point.goal.replace( new Term( "=", [list, list1] ) ), point.substitution, point )] );
					} else {			
						var pointer = list;
						var v = pl.type.is_variable( atom1 );
						var str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) && v ) {
									session.throwError( pl.error.instantiation( atom.indicator ) );
									return;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									session.throwError( pl.error.type( "character", pointer.args[0], atom.indicator ) );
									return;
								}
							} else {
								str += pointer.args[0].id;
							}
							pointer = pointer.args[1];
						}
						if( pl.type.is_variable( pointer ) && v ) {
							session.throwError( pl.error.instantiation( atom.indicator ) );
						} else if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
							session.throwError( pl.error.type( "list", list, atom.indicator ) );
						} else {
							session.prepend( [new State( point.goal.replace( new Term( "=", [new Term( str ), atom1] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// atom_codes/2
			"atom_codes/2": function( session, point, atom ) {
				var atom1 = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( atom1 ) && pl.type.is_variable( list ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( atom1 ) && !pl.type.is_atom( atom1 ) ) {
					session.throwError( pl.error.type( "atom", atom1, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( atom1 ) ) {
						var list1 = new Term( "[]" );
						for( var i = atom1.id.length-1; i >= 0; i-- ) {
							list1 = new Term( ".", [new Num( atom1.id.charCodeAt( i ), false ), list1] );
						}
						session.prepend( [new State( point.goal.replace( new Term( "=", [list, list1] ) ), point.substitution, point )] );
					} else {			
						var pointer = list;
						var v = pl.type.is_variable( atom1 );
						var str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character_code( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) && v ) {
									session.throwError( pl.error.instantiation( atom.indicator ) );
									return;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									session.throwError( pl.error.representation( "character_code", atom.indicator ) );
									return;
								}
							} else {
								str += String.fromCharCode( pointer.args[0].value );
							}
							pointer = pointer.args[1];
						}
						if( pl.type.is_variable( pointer ) && v ) {
							session.throwError( pl.error.instantiation( atom.indicator ) );
						} else if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
							session.throwError( pl.error.type( "list", list, atom.indicator ) );
						} else {
							session.prepend( [new State( point.goal.replace( new Term( "=", [new Term( str ), atom1] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// char_code/2
			"char_code/2": function( session, point, atom ) {
				var char = atom.args[0], code = atom.args[1];
				if( pl.type.is_variable( char ) && pl.type.is_variable( code ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( char ) && !pl.type.is_character( char ) ) {
					session.throwError( pl.error.type( "character", char, atom.indicator ) );
				} else if( !pl.type.is_variable( code ) && !pl.type.is_integer( code ) ) {
					session.throwError( pl.error.type( "integer", code, atom.indicator ) );
				} else if( !pl.type.is_variable( code ) && !pl.type.is_character_code( code ) ) {
					session.throwError( pl.error.representation( "character_code", atom.indicator ) );
				} else {
					if( pl.type.is_variable( code ) ) {
						var code1 = new Num( char.id.charCodeAt( 0 ), false );
						session.prepend( [new State( point.goal.replace( new Term( "=", [code1, code] ) ), point.substitution, point )] );
					} else {
						var char1 = new Term( String.fromCharCode( code.value ) );
						session.prepend( [new State( point.goal.replace( new Term( "=", [char1, char] ) ), point.substitution, point )] );
					}
				}
			},
			
			// number_chars/2
			"number_chars/2": function( session, point, atom ) {
				var str, num = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( num ) && pl.type.is_variable( list ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( num ) && !pl.type.is_number( num ) ) {
					session.throwError( pl.error.type( "number", num, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( list ) ) {	
						var pointer = list;
						var v = pl.type.is_variable( num );
						str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) && v ) {
									session.throwError( pl.error.instantiation( atom.indicator ) );
									return;
								} else if( pl.type.is_variable( pointer.args[0] ) ) {
									str = false;
									break;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									session.throwError( pl.error.type( "character", pointer.args[0], atom.indicator ) );
									return;
								}
							} else {

								str += pointer.args[0].id;
							}
							pointer = pointer.args[1];
						}
						if( str ) {
							if( pl.type.is_variable( pointer ) && v ) {
								session.throwError( pl.error.instantiation( atom.indicator ) );
							} else if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
								session.throwError( pl.error.type( "list", list, atom.indicator ) );
							} else {
								var num2 = strToNum( str );
								if( num2 === false ) {
									session.throwError( pl.error.syntax_by_predicate( "parseable_number", list, atom.indicator ) );
								} else {
									session.prepend( [new State( point.goal.replace( new Term( "=", [num, num2] ) ), point.substitution, point )] );
								}
								return;
							}
						}
					}
					if( !pl.type.is_variable( num ) ) {
						str = num.toString();
						var list2 = new Term( "[]" );
						for( var i = str.length - 1; i >= 0; i-- ) {
							list2 = new Term( ".", [ new Term( str.charAt( i ) ), list2 ] );
						}
						session.prepend( [new State( point.goal.replace( new Term( "=", [list, list2] ) ), point.substitution, point )] );
					}
				}
			},
			
			// number_codes/2
			"number_codes/2": function( session, point, atom ) {
				var str, num = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( num ) && pl.type.is_variable( list ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( num ) && !pl.type.is_number( num ) ) {
					session.throwError( pl.error.type( "number", num, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( list ) ) {	
						var pointer = list;
						var v = pl.type.is_variable( num );
						str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character_code( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) && v ) {
									session.throwError( pl.error.instantiation( atom.indicator ) );
									return;
								} else if( pl.type.is_variable( pointer.args[0] ) ) {
									str = false;
									break;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									session.throwError( pl.error.representation( "character_code", atom.indicator ) );
									return;
								}
							} else {
								str += String.fromCharCode( pointer.args[0].value );
							}
							pointer = pointer.args[1];
						}
						if( str ) {
							if( pl.type.is_variable( pointer ) && v ) {
								session.throwError( pl.error.instantiation( atom.indicator ) );
							} else if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
								session.throwError( pl.error.type( "list", list, atom.indicator ) );
							} else {
								var num2 = strToNum( str );
								if( num2 === false ) {
									session.throwError( pl.error.syntax_by_predicate( "parseable_number", list, atom.indicator ) );
								} else {
									session.prepend( [new State( point.goal.replace( new Term( "=", [num, num2] ) ), point.substitution, point )] );
								}
								return;
							}
						}
					}
					if( !pl.type.is_variable( num ) ) {
						str = num.toString();
						var list2 = new Term( "[]" );
						for( var i = str.length - 1; i >= 0; i-- ) {
							list2 = new Term( ".", [ new Num( str.charCodeAt( i ), false ), list2 ] );
						}
						session.prepend( [new State( point.goal.replace( new Term( "=", [list, list2] ) ), point.substitution, point )] );
					}
				}
			},
			
			// TERM COMPARISON
			
			"@=</2": function( session, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) <= 0 ) {
					session.success( point );
				}
			},
			
			"==/2": function( session, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) === 0 ) {
					session.success( point );
				}
			},
			
			"\\==/2": function( session, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) !== 0 ) {
					session.success( point );
				}
			},
			
			"@</2": function( session, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) < 0 ) {
					session.success( point );
				}
			},
			
			"@>/2": function( session, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) > 0 ) {
					session.success( point );
				}
			},
			
			"@>=/2": function( session, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) >= 0 ) {
					session.success( point );
				}
			},
			
			// EVALUATION
			
			// is/2
			"is/2": function( session, point, atom ) {
				var op = atom.args[1].interpret( session );
				if( !pl.type.is_number( op ) ) {
					session.throwError( op );
				} else {
					session.prepend( [new State( point.goal.replace( new Term( "=", [atom.args[0], op], session.level ) ), point.substitution, point )] );
				}
			},
			
			// =:=/2
			"=:=/2": function( session, point, atom ) {
				var cmp = pl.arithmetic_compare( session, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					session.throwError( cmp );
				} else if( cmp === 0 ) {
					session.success( point );
				}
			},
			
			// =\=/2
			"=\\=/2": function( session, point, atom ) {
				var cmp = pl.arithmetic_compare( session, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					session.throwError( cmp );
				} else if( cmp !== 0 ) {
					session.success( point );
				}
			},
			
			// </2
			"</2": function( session, point, atom ) {
				var cmp = pl.arithmetic_compare( session, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					session.throwError( cmp );
				} else if( cmp < 0 ) {
					session.success( point );
				}
			},
			
			// =</2
			"=</2": function( session, point, atom ) {
				var cmp = pl.arithmetic_compare( session, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					session.throwError( cmp );
				} else if( cmp <= 0 ) {
					session.success( point );
				}
			},
			
			// >/2
			">/2": function( session, point, atom ) {
				var cmp = pl.arithmetic_compare( session, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					session.throwError( cmp );
				} else if( cmp > 0 ) {
					session.success( point );
				}
			},
			
			// >=/2
			">=/2": function( session, point, atom ) {
				var cmp = pl.arithmetic_compare( session, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					session.throwError( cmp );
				} else if( cmp >= 0 ) {
					session.success( point );
				}
			},
			
			// TYPE TEST
			
			// var/1
			"var/1": function( session, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// atom/1
			"atom/1": function( session, point, atom ) {
				if( pl.type.is_atom( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// atomic/1
			"atomic/1": function( session, point, atom ) {
				if( pl.type.is_atomic( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// compound/1
			"compound/1": function( session, point, atom ) {
				if( pl.type.is_compound( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// integer/1
			"integer/1": function( session, point, atom ) {
				if( pl.type.is_integer( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// float/1
			"float/1": function( session, point, atom ) {
				if( pl.type.is_float( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// number/1
			"number/1": function( session, point, atom ) {
				if( pl.type.is_number( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// nonvar/1
			"nonvar/1": function( session, point, atom ) {
				if( !pl.type.is_variable( atom.args[0] ) ) {
					session.success( point );
				}
			},
			
			// IMPLEMENTATION DEFINED HOOKS
			
			// halt/0
			"halt/0": function( session, point, _ ) {
				session.points = [];
			},
			
			// halt/1
			"halt/1": function( session, point, atom ) {
				var int = atom.args[0];
				if( pl.type.is_variable( int ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( int ) ) {
					session.throwError( pl.error.type( "integer", int, atom.indicator ) );
				} else {
					session.points = [];
				}
			},
			
			// current_prolog_flag/2
			"current_prolog_flag/2": function( session, point, atom ) {
				var flag = atom.args[0], value = atom.args[1];
				if( !pl.type.is_variable( flag ) && !pl.type.is_atom( flag ) ) {
					session.throwError( pl.error.type( "atom", flag, atom.indicator ) );
				} else if( !pl.type.is_variable( flag ) && !pl.type.is_flag( flag ) ) {
					session.throwError( pl.error.domain( "prolog_flag", flag, atom.indicator ) );
				} else {
					var states = [];
					for( var name in pl.flag ) {
						if(!pl.flag.hasOwnProperty(name)) continue;
						var goal = new Term( ",", [new Term( "=", [new Term( name ), flag] ), new Term( "=", [session.flag[name], value] )] );
						states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
					}
					session.prepend( states );
				}
			},
			
			// set_prolog_flag/2
			"set_prolog_flag/2": function( session, point, atom ) {
				var flag = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( flag ) || pl.type.is_variable( value ) ) {
					session.throwError( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( flag ) ) {
					session.throwError( pl.error.type( "atom", flag, atom.indicator ) );
				} else if( !pl.type.is_flag( flag ) ) {
					session.throwError( pl.error.domain( "prolog_flag", flag, atom.indicator ) );
				} else if( !pl.type.is_value_flag( flag, value ) ) {
					session.throwError( pl.error.domain( "flag_value", new Term( "+", [flag, value] ), atom.indicator ) );
				} else if( !pl.type.is_modifiable_flag( flag ) ) {
					session.throwError( pl.error.permission( "modify", "flag", flag ) );
				} else {
					session.flag[flag.id] = value;
					session.success( point );
				}
			}
			
		},
		
		// Flags
		flag: {
			
			// Bounded numbers
			bounded: {
				allowed: [new Term( "true" ), new Term( "false" )],
				value: new Term( "true" ),
				changeable: false
			},
			
			// Maximum integer
			max_integer: {
				allowed: [new Num( Number.MAX_SAFE_INTEGER )],
				value: new Num( Number.MAX_SAFE_INTEGER ),
				changeable: false
			},
			
			// Minimum integer
			min_integer: {
				allowed: [new Num( Number.MIN_SAFE_INTEGER )],
				value: new Num( Number.MIN_SAFE_INTEGER ),
				changeable: false
			},
			
			// Rounding function
			integer_rounding_function: {
				allowed: [new Term( "down" ), new Term( "toward_zero" )],
				value: new Term( "toward_zero" ),
				changeable: false
			},
			
			// Character conversion
			char_conversion : {
				allowed: [new Term( "on" ), new Term( "off" )],
				value: new Term( "on" ),
				changeable: true
			},
			
			// Debugger
			debug: {
				allowed: [new Term( "on" ), new Term( "off" )],
				value: new Term( "off" ),
				changeable: true
			},
			
			// Maximum arity of predicates
			max_arity: {
				allowed: [new Term( "unbounded" )],
				value: new Term( "unbounded" ),
				changeable: false
			},
			
			// Unkwnow predicates behavior
			unknown: {
				allowed: [new Term( "error" ), new Term( "fail" ), new Term( "warning" )],
				value: new Term( "error" ),
				changeable: true
			},
			
			// Double quotes behavior
			double_quotes: {
				allowed: [new Term( "chars" ), new Term( "codes" ), new Term( "atom" )],
				value: new Term( "codes" ),
				changeable: true
			}
			
		},
		
		// Unify
		unify: function( obj1, obj2, occurs_check ) {
			occurs_check = occurs_check === undefined ? false : occurs_check;
			if( this.type.is_anonymous_var( obj1 ) ) {
				return new State( obj2, new Substitution() );
			} else if( this.type.is_anonymous_var( obj2 ) ) {
				return new State( obj1, new Substitution());
			} else if( this.type.is_variable( obj2 ) ) {
				var links = {};
				if( occurs_check && indexOf( obj1.variables(), obj2.id ) !== -1 && !pl.type.is_variable( obj1 ) ) {
					return null;
				}
				links[obj2.id] = obj1;
				return new State( obj1, new Substitution( links ) );
			} else {
				return obj1.unify( obj2, occurs_check );
			}
		},
		
		// Compare
		compare: function( obj1, obj2 ) {
			var type = pl.type.compare( obj1, obj2 );
			return type !== 0 ? type : obj1.compare( obj2 );
		},
		
		// Arithmetic comparison
		arithmetic_compare: function( session, obj1, obj2 ) {
			var expr1 = obj1.interpret( session );
			if( !pl.type.is_number( expr1 ) ) {
				return expr1;
			} else {
				var expr2 = obj2.interpret( session );
				if( !pl.type.is_number( expr2 ) ) {
					return expr2;
				} else {
					return pl.compare( expr1, expr2 );
				}
			}
		},
		
		// Operate
		operate: function( session, obj ) {
			if( pl.type.is_operator( obj ) ) {
				var op = pl.type.is_operator( obj );
				var args = [], value;
				var type = false;
				for( var i = 0; i < obj.args.length; i++ ) {
					value = obj.args[i].interpret( session );
					if( !pl.type.is_number( value ) ) {
						return value;
					} else if( op.type_args !== null && value.is_float !== op.type_args ) {
						return pl.error.type( op.type_args ? "float" : "integer", value, session.__call_indicator );
					} else {
						args.push( value.value );
					}
					type = type || value.is_float;
				}
				args.push( session );
				value = pl.arithmetic.evaluation[obj.indicator].fn.apply( this, args );
				type = op.type_result === null ? type : op.type_result;
				if( pl.type.is_term( value ) ) {
					return value;
				} else if( value === Number.POSITIVE_INFINITY || value === Number.NEGATIVE_INFINITY ) {
					return pl.error.evaluation( "overflow", session.__call_indicator );
				} else if( type === false && session.flag.bounded.id === "true" && (value > session.flag.max_integer.value || value < session.flag.min_integer.value) ) {
					return pl.error.evaluation( "int_overflow", session.__call_indicator );
				} else {
					return new Num( value, type );
				}
			} else {
				return pl.error.type( "evaluable", obj.indicator, session.__call_indicator );
			}
		},
		
		// Errors
		error: {
			
			// Existence error
			existence: function( type, object, indicator ) {
				return new Term( "error", [new Term( "existence_error", [new Term( type ), new Term( object )] ), new Term( indicator )] );
			},
			
			// Type error
			type: function( expected, found, indicator ) {
				return new Term( "error", [new Term( "type_error", [new Term( expected ), found] ), new Term( indicator )] );
			},
			
			// Instantation error
			instantiation: function( indicator ) {
				return new Term( "error", [new Term( "instantiation_error" ), new Term( indicator )] );
			},
			
			// Domain error
			domain: function( expected, found, indicator ) {
				return new Term( "error", [new Term( "domain_error", [new Term( expected ), found]), new Term( indicator )] );
			},
			
			// Representation error
			representation: function( flag, indicator ) {
				return new Term( "error", [new Term( "representation_error", [new Term( flag )] ), new Term( indicator )] );
			},
			
			// Permission error
			permission: function( operation, type, found, indicator ) {
				return new Term( "error", [new Term( "permission_error", [new Term( operation ), new Term( type ), found] ), new Term( indicator )] );
			},
			
			// Evaluation error
			evaluation: function( error, indicator ) {
				return new Term( "error", [new Term( "evaluation_error", [new Term( error )] ), new Term( indicator )] );
			},
			
			// Syntax error
			syntax: function( token, expected, last ) {
				var position = last ? token.start + token.value.length : token.start;
				var found = last ? new Term("token_not_found") : new Term("found", [new Term(token.value)]);
				return new Term( "error", [new Term( "syntax_error", [new Term( "line", [new Num(token.line)] ), new Term( "column", [new Num(position+1)] ), found, new Term( "cause", [new Term( expected )] )] )] );
			},
			
			// Syntax error by predicate
			syntax_by_predicate: function( expected, found, indicator ) {
				return new Term( "error", [new Term( "syntax_error", [new Term( expected ), found ] ), new Term( indicator )] );
			}
			
		},
		
		// Format of renamed variables
		format_variable: function( variable ) {
			return "_" + variable;
		},
		
		// Format of computed answers
		format_answer: function( answer ) {
			if( pl.type.is_error( answer ) ) {
				return "uncaught exception: " + answer.args[0].toString();
			} else if( answer === false ) {
				return "false.";
			} else if( answer === null ) {
				return "limit exceeded ;";
			} else {
				var i = 0;
				var str = "";
				for( var link in answer.links ) {
					if(!answer.links.hasOwnProperty(link)) continue;
					i++;
					if( str !== "" ) {
						str += ", ";
					}
					str += link.toString() + " = " + answer.links[link].toString();
				}
				if( i === 0 ) {
					return "true ;";
				} else {
					return str + " ;";
				}
			}
		},
		
		// Create new session
		create: function( limit ) {
			return new pl.type.Session( limit );
		}
		
	};

	window.pl = pl;
	
})();

(function() {
	
	// VERSION
	var version = { major: 0, minor: 2, patch: 82, status: "beta" };



	// IO FILE SYSTEM

	// Virtual file system for browser
	tau_file_system = {
		// Current files
		files: {},
		// Open file
		open: function( path, type, mode ) {
			var file = tau_file_system.files[path];
			if( !file ) {
				if( mode === "read" )
					return null;
				file = {
					path: path,
					text: "",
					type: type,
					get: function( length, position ) {
						if( position === this.text.length ) {
							return "end_of_file";
						} else if( position > this.text.length ) {
							return "end_of_file";
						} else {
							return this.text.substring( position, position+length );
						}
					},
					put: function( text, position ) {
						if( position === "end_of_file" ) {
							this.text += text;
							return true;
						} else if( position === "past_end_of_file" ) {
							return null;
						} else {
							this.text = this.text.substring(0, position) + text + this.text.substring(position+text.length);
							return true;
						}
					},
					get_byte: function( position ) {
						if( position === "end_of_stream" )
							return -1;
						var index = Math.floor(position/2);
						if( this.text.length <= index )
							return -1;
						var code = codePointAt( this.text[Math.floor(position/2)], 0 );
						if( position % 2 === 0 )
							return code & 0xff;
						else
							return code / 256 >>> 0;
					},
					put_byte: function( byte, position ) {
						var index = position === "end_of_stream" ? this.text.length : Math.floor(position/2);
						if( this.text.length < index )
							return null;
						var code = this.text.length === index ? -1 : codePointAt( this.text[Math.floor(position/2)], 0 );
						if( position % 2 === 0 ) {
							code = code / 256 >>> 0;
							code = ((code & 0xff) << 8) | (byte & 0xff);
						} else {
							code = code & 0xff;
							code = ((byte & 0xff) << 8) | (code & 0xff);
						}
						if( this.text.length === index )
							this.text += fromCodePoint( code );
						else 
							this.text = this.text.substring( 0, index ) + fromCodePoint( code ) + this.text.substring( index+1 );
						return true;
					},
					flush: function() {
						return true;
					},
					close: function() {
						var file = tau_file_system.files[this.path];
						if( !file ) {
							return null;
						} else {
							return true;
						}
					}
				};
				tau_file_system.files[path] = file;
			}
			if( mode === "write" )
				file.text = "";
			return file;
		},
	};

	// User input for browser
	tau_user_input = {
		buffer: "",
		get: function( length, _ ) {
			var text;
			while( tau_user_input.buffer.length < length ) {
				text = window.prompt();
				if( text ) {
					tau_user_input.buffer += text;
				}
			}
			text = tau_user_input.buffer.substr( 0, length );
			tau_user_input.buffer = tau_user_input.buffer.substr( length );
			return text;
		}
	};

	// User output for browser
	tau_user_output = {
		put: function( text, _ ) {
			console.log( text );
			return true;
		},
		flush: function() {
			return true;
		} 
	};

	// Virtual file system for Node.js
	nodejs_file_system = {
		// Open file
		open: function( path, type, mode ) {
			var fs = require('fs');
			var fd = fs.openSync( path, mode[0] );
			if( mode === "read" && !fs.existsSync( path ) )
				return null;
			return {
				get: function( length, position ) {
					var buffer = new Buffer( length );
					fs.readSync( fd, buffer, 0, length, position );
					return buffer.toString();
				},
				put: function( text, position ) {
					var buffer = Buffer.from( text );
					if( position === "end_of_file" )
						fs.writeSync( fd, buffer );
					else if( position === "past_end_of_file" )
						return null;
					else
						fs.writeSync( fd, buffer, 0, buffer.length, position );
					return true;
				},
				get_byte: function( position ) {
					return null;
				},
				put_byte: function( byte, position ) {
					return null;
				},
				flush: function() {
					return true;
				},
				close: function() {
					fs.closeSync( fd );
					return true;
				}
			};
		}
	};

	// User input for Node.js
	nodejs_user_input = {
		buffer: "",
		get: function( length, _ ) {
			var text;
			var readlineSync = require('readline-sync');
			while( nodejs_user_input.buffer.length < length )
				nodejs_user_input.buffer += readlineSync.question();
			text = nodejs_user_input.buffer.substr( 0, length );
			nodejs_user_input.buffer = nodejs_user_input.buffer.substr( length );
			return text;
		}
	};

	// User output for Node.js
	nodejs_user_output = {
		put: function( text, _ ) {
			process.stdout.write( text );
			return true;
		},
		flush: function() {
			return true;
		}
	};
	
	
	
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
	
	var filter;
	if(!Array.prototype.filter) {
		filter = function(array, fn) {
			var a = [];
			var len = array.length;
			for(var i = 0; i < len; i++) {
				if(fn(array[i]))
					a.push( array[i] );
			}
			return a;
		};
	} else {
		filter = function(array, fn) {
			return array.filter(fn);
		};
	}
	
	var codePointAt;
	if(!String.prototype.codePointAt) {
		codePointAt = function(str, i) {
			return str.charCodeAt(i);
		};
	} else {
		codePointAt = function(str, i) {
			return str.codePointAt(i);
		};
	}
	
	var fromCodePoint;
	if(!String.fromCodePoint) {
		fromCodePoint = function() {
			return String.fromCharCode.apply(null, arguments);
		};
	} else {
		fromCodePoint = function() {
			return String.fromCodePoint.apply(null, arguments);
		};
	}

	var stringLength;
	var regexAstralSymbols = /[\uD800-\uDBFF][\uDC00-\uDFFF]/g;
	if(Array.from)
		stringLength = function(str) {
			return Array.from(str).length;
		};
	else
		stringLength = function(str) {
			return str.replace(regexAstralSymbols, '_').length;
		};



	var ERROR = 0;
	var SUCCESS = 1;

	var regex_escape = /(\\a)|(\\b)|(\\f)|(\\n)|(\\r)|(\\t)|(\\v)|\\x([0-9a-fA-F]+)\\|\\([0-7]+)\\|(\\\\)|(\\')|('')|(\\")|(\\`)|(\\.)|(.)/g;
	var escape_map = {"\\a": 7, "\\b": 8, "\\f": 12, "\\n": 10, "\\r": 13, "\\t": 9, "\\v": 11};
	function escape(str) {
		var s = [];
		var _error = false;
		str.replace(regex_escape, function(match, a, b, f, n, r, t, v, hex, octal, back, single, dsingle, double, backquote, error, char) {
			switch(true) {
				case hex !== undefined:
					s.push( parseInt(hex, 16) );
					return "";
				case octal !== undefined:
					s.push( parseInt(octal, 8) );
					return "";
				case back !== undefined:
				case single !== undefined:
				case dsingle !== undefined:
				case double !== undefined:
				case backquote !== undefined:
					s.push( codePointAt(match.substr(1),0) );
					return "";
				case char !== undefined:
					s.push( codePointAt(char,0) );
					return "";
				case error !== undefined:
					_error = true;
				default:
					s.push(escape_map[match]);
					return "";
			}
		});
		if(_error)
			return null;
		return s;
	}

	// Escape atoms
	function escapeAtom(str, quote) {
		var atom = '';
		if( str === "\\" ) return null;
		if( str.length < 2 ) return str;
		try {
			str = str.replace(/\\([0-7]+)\\/g, function(match, g1) {
				return fromCodePoint(parseInt(g1, 8));
			});
			str = str.replace(/\\x([0-9a-fA-F]+)\\/g, function(match, g1) {
				return fromCodePoint(parseInt(g1, 16));
			});
		} catch(error) {
			return null;
		}
		for( var i = 0; i < str.length; i++) {
			var a = str.charAt(i);
			var b = str.charAt(i+1);
			if( a === quote && b === quote ) {
				i++;
				atom += quote;
			} else if( a === '\\' ) {
				if( ['a','b','f','n','r','t','v',"'",'"','\\','\a','\b','\f','\n','\r','\t','\v'].indexOf(b) !== -1 ) {
					i += 1;
					switch( b ) {
						case 'a': atom += '\a'; break;
						case 'b': atom += '\b'; break;
						case 'f': atom += '\f'; break;
						case 'n': atom += '\n'; break;
						case 'r': atom += '\r'; break;
						case 't': atom += '\t'; break;
						case 'v': atom += '\v'; break;
						case "'": atom += "'"; break;
						case '"': atom += '"'; break;
						case '\\': atom += '\\'; break;
					}
				} else {
					return null;
				}
			} else {
				atom += a;
			}
		}
		return atom;
	}
	
	// Redo escape
	function redoEscape(str) {
		var atom = '';
		for( var i = 0; i < str.length; i++) {
			switch( str.charAt(i) ) {
				case "'": atom += "\\'"; break;
				case '\\': atom += '\\\\'; break;
				//case '\a': atom += '\\a'; break;
				case '\b': atom += '\\b'; break;
				case '\f': atom += '\\f'; break;
				case '\n': atom += '\\n'; break;
				case '\r': atom += '\\r'; break;
				case '\t': atom += '\\t'; break;
				case '\v': atom += '\\v'; break;
				default: atom += str.charAt(i); break;
			}
		}
		return atom;
	}

	// String to num
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

	// Regular expressions for tokens
	var rules = {
		whitespace: /^\s*(?:(?:%.*)|(?:\/\*(?:\n|\r|.)*?\*\/)|(?:\s+))\s*/,
		variable: /^(?:[A-Z_][a-zA-Z0-9_]*)/,
		atom: /^(\!|,|;|[a-z][0-9a-zA-Z_]*|[#\$\&\*\+\-\.\/\:\<\=\>\?\@\^\~\\]+|'(?:(?:'')|(?:\\')|[^'])*')/,
		number: /^(?:0o[0-7]+|0x[0-9a-fA-F]+|0b[01]+|0'(?:''|\\[abfnrtv\\'"`]|\\x?\d+\\|[^\\])|\d+(?:\.\d+(?:[eE][+-]?\d+)?)?)/,
		string: /^(?:"([^"]|""|\\")*"|`([^`]|``|\\`)*`)/,
		l_brace: /^(?:\[)/,
		r_brace: /^(?:\])/,
		l_bracket: /^(?:\{)/,
		r_bracket: /^(?:\})/,
		bar: /^(?:\|)/,
		l_paren: /^(?:\()/,
		r_paren: /^(?:\))/
	};

	// Replace chars of char_conversion session
	function replace( thread, text ) {
		if( thread.get_flag( "char_conversion" ).id === "on" ) {
			return text.replace(/./g, function(char) {
				return thread.get_char_conversion( char );
			});
		}
		return text;
	}

	// Tokenize strings
	function Tokenizer(thread) {
		this.thread = thread;
		this.text = ""; // Current text to be analized
		this.tokens = []; // Consumed tokens
	}

	Tokenizer.prototype.set_last_tokens = function(tokens) {
		return this.tokens = tokens;
	};

	Tokenizer.prototype.new_text = function(text) {
		this.text = text;
		this.tokens = [];
	};

	Tokenizer.prototype.get_tokens = function(init) {
		var text;
		var len = 0; // Total length respect to text
		var line = 0;
		var start = 0;
		var tokens = [];
		var last_in_blank = false;

		if(init) {
			var token = this.tokens[init-1];
			len = token.len;
			text = replace( this.thread, this.text.substr(token.len) );
			line = token.line;
			start = token.start;
		}
		else
			text = this.text;


		// If there is nothing to be analized, return null
		if(/^\s*$/.test(text))
			return null;

		while(text !== "") {
			var matches = [];
			var last_is_blank = false;

			if(/^\n/.exec(text) !== null) {
				line++;
				start = 0;
				len++;
				text = text.replace(/\n/, "");
				last_in_blank = true;
				continue;
			}

			for(var rule in rules) {
				if(rules.hasOwnProperty(rule)) {
					var matchs = rules[rule].exec( text );
					if(matchs) {
						matches.push({
							value: matchs[0],
							name: rule,
							matches: matchs
						});
					}
				}
			}

			// Lexical error
			if(!matches.length)
				return this.set_last_tokens( [{ value: text, matches: [], name: "lexical", line: line, start: start }] );

			var token = reduce( matches, function(a, b) {
				return a.value.length >= b.value.length ? a : b;
			} );

			token.start = start;
			token.line = line;

			text = text.replace(token.value, "");
			start += token.value.length;
			len += token.value.length;

			switch(token.name) {
				case "atom":
					token.raw = token.value;
					if(token.value.charAt(0) === "'") {
						token.value = escapeAtom( token.value.substr(1, token.value.length - 2), "'" );
						if( token.value === null ) {
							token.name = "lexical";
							token.value = "unknown escape sequence";
						}
					}
					break;
				case "number":
					token.float = token.value.substring(0,2) !== "0x" && token.value.match(/[.eE]/) !== null && token.value !== "0'.";
					token.value = convertNum( token.value );
					token.blank = last_is_blank;
					break;
				case "string":
					var del = token.value.charAt(0);
					token.value = escapeAtom( token.value.substr(1, token.value.length - 2), del );
					if( token.value === null ) {
						token.name = "lexical";
						token.value = "unknown escape sequence";
					}
					break;
				case "whitespace":
					var last = tokens[tokens.length-1];
					if(last) last.space = true;
					last_is_blank = true;
					continue;
				case "r_bracket":
					if( tokens.length > 0 && tokens[tokens.length-1].name === "l_bracket" ) {
						token = tokens.pop();
						token.name = "atom";
						token.value = "{}";
						token.raw = "{}";
						token.space = false;
					}
					break;
				case "r_brace":
					if( tokens.length > 0 && tokens[tokens.length-1].name === "l_brace" ) {
						token = tokens.pop();
						token.name = "atom";
						token.value = "[]";
						token.raw = "[]";
						token.space = false;
					}
					break;
			}
			token.len = len;
			tokens.push( token );
			last_is_blank = false;
		}

		var t = this.set_last_tokens( tokens );
		return t.length === 0 ? null : t;
	};

	// Parse an expression
	function parseExpr(thread, tokens, start, priority, toplevel) {
		if(!tokens[start]) return {type: ERROR, value: pl.error.syntax(tokens[start-1], "expression expected", true)};
		var error;

		if(priority === "0") {
			var token = tokens[start];
			switch(token.name) {
				case "number":
					return {type: SUCCESS, len: start+1, value: new pl.type.Num(token.value, token.float)};
				case "variable":
					return {type: SUCCESS, len: start+1, value: new pl.type.Var(token.value)};
				case "string":
					var str;
					switch( thread.get_flag( "double_quotes" ).id ) {
						case "atom":;
							str = new Term( token.value, [] );
							break;
						case "codes":
							str = new Term( "[]", [] );
							for(var i = token.value.length-1; i >= 0; i-- )
								str = new Term( ".", [new pl.type.Num( codePointAt(token.value,i), false ), str] );
							break;
						case "chars":
							str = new Term( "[]", [] );
							for(var i = token.value.length-1; i >= 0; i-- )
								str = new Term( ".", [new pl.type.Term( token.value.charAt(i), [] ), str] );
							break;
					}
					return {type: SUCCESS, len: start+1, value: str};
				case "l_paren":
					var expr = parseExpr(thread, tokens, start+1, thread.__get_max_priority(), true);
					if(expr.type !== SUCCESS) return expr;
					if(tokens[expr.len] && tokens[expr.len].name === "r_paren") {
						expr.len++;
						return expr;
					}
					return {type: ERROR, derived: true, value: pl.error.syntax(tokens[expr.len] ? tokens[expr.len] : tokens[expr.len-1], ") or operator expected", !tokens[expr.len])}
				case "l_bracket":
					var expr = parseExpr(thread, tokens, start+1, thread.__get_max_priority(), true);
					if(expr.type !== SUCCESS) return expr;
					if(tokens[expr.len] && tokens[expr.len].name === "r_bracket") {
						expr.len++;
						expr.value = new Term( "{}", [expr.value] );
						return expr;
					}
					return {type: ERROR, derived: true, value: pl.error.syntax(tokens[expr.len] ? tokens[expr.len] : tokens[expr.len-1], "} or operator expected", !tokens[expr.len])}
			}
			// Compound term
			var result = parseTerm(thread, tokens, start, toplevel);
			if(result.type === SUCCESS || result.derived)
				return result;
			// List
			result = parseList(thread, tokens, start);
			if(result.type === SUCCESS || result.derived)
				return result;
			// Unexpected
			return {type: ERROR, derived: false, value: pl.error.syntax(tokens[start], "unexpected token")};
		}

		var max_priority = thread.__get_max_priority();
		var next_priority = thread.__get_next_priority(priority);
		var aux_start = start;
		
		// Prefix operators
		if(tokens[start].name === "atom" && tokens[start+1] && (tokens[start].space || tokens[start+1].name !== "l_paren")) {
			var token = tokens[start++];
			var classes = thread.__lookup_operator_classes(priority, token.value);
			
			// Associative prefix operator
			if(classes && classes.indexOf("fy") > -1) {
				var expr = parseExpr(thread, tokens, start, priority, toplevel);
				if(expr.type !== ERROR) {
					if( token.value === "-" && !token.space && pl.type.is_number( expr.value ) ) {
						return {
							value: new pl.type.Num(-expr.value.value, expr.value.is_float),
							len: expr.len,
							type: SUCCESS
						};
					} else {
						return {
							value: new pl.type.Term(token.value, [expr.value]),
							len: expr.len,
							type: SUCCESS
						};
					}
				} else {
					error = expr;
				}
			// Non-associative prefix operator
			} else if(classes && classes.indexOf("fx") > -1) {
				var expr = parseExpr(thread, tokens, start, next_priority, toplevel);
				if(expr.type !== ERROR) {
					return {
						value: new pl.type.Term(token.value, [expr.value]),
						len: expr.len,
						type: SUCCESS
					};
				} else {
					error = expr;
				}
			}
		}

		start = aux_start;
		var expr = parseExpr(thread, tokens, start, next_priority, toplevel);
		if(expr.type === SUCCESS) {
			start = expr.len;
			var token = tokens[start];
			if(tokens[start] && (
				tokens[start].name === "atom" && thread.__lookup_operator_classes(priority, token.value) ||
				tokens[start].name === "bar" && thread.__lookup_operator_classes(priority, "|")
			) ) {
				var next_priority_lt = next_priority;
				var next_priority_eq = priority;
				var classes = thread.__lookup_operator_classes(priority, token.value);

				if(classes.indexOf("xf") > -1) {
					return {
						value: new pl.type.Term(token.value, [expr.value]),
						len: ++expr.len,
						type: SUCCESS
					};
				} else if(classes.indexOf("xfx") > -1) {
					var expr2 = parseExpr(thread, tokens, start + 1, next_priority_lt, toplevel);
					if(expr2.type === SUCCESS) {
						return {
							value: new pl.type.Term(token.value, [expr.value, expr2.value]),
							len: expr2.len,
							type: SUCCESS
						};
					} else {
						expr2.derived = true;
						return expr2;
					}
				} else if(classes.indexOf("xfy") > -1) {
					var expr2 = parseExpr(thread, tokens, start + 1, next_priority_eq, toplevel);
					if(expr2.type === SUCCESS) {
						return {
							value: new pl.type.Term(token.value, [expr.value, expr2.value]),
							len: expr2.len,
							type: SUCCESS
						};
					} else {
						expr2.derived = true;
						return expr2;
					}
				} else if(expr.type !== ERROR) {
					while(true) {
						start = expr.len;
						var token = tokens[start];
						if(token && token.name === "atom" && thread.__lookup_operator_classes(priority, token.value)) {
							var classes = thread.__lookup_operator_classes(priority, token.value);
							if( classes.indexOf("yf") > -1 ) {
								expr = {
									value: new pl.type.Term(token.value, [expr.value]),
									len: ++start,
									type: SUCCESS
								};
							} else if( classes.indexOf("yfx") > -1 ) {
								var expr2 = parseExpr(thread, tokens, ++start, next_priority_lt, toplevel);
								if(expr2.type === ERROR) {
									expr2.derived = true;
									return expr2;
								}
								start = expr2.len;
								expr = {
									value: new pl.type.Term(token.value, [expr.value, expr2.value]),
									len: start,
									type: SUCCESS
								};
							} else { break; }
						} else { break; }
					}
				}
			} else {
				error = {type: ERROR, value: pl.error.syntax(tokens[expr.len-1], "operator expected")};
			}
			return expr;
		}
		return expr;
	}

	// Parse a compound term
	function parseTerm(thread, tokens, start, toplevel) {
		if(!tokens[start] || (tokens[start].name === "atom" && tokens[start].raw === "." && !toplevel && (tokens[start].space || !tokens[start+1] || tokens[start+1].name !== "l_paren")))
			return {type: ERROR, derived: false, value: pl.error.syntax(tokens[start-1], "unfounded token")};
		var atom = tokens[start];
		var exprs = [];
		if(tokens[start].name === "atom" && tokens[start].raw !== ",") {
			start++;
			if(tokens[start-1].space) return {type: SUCCESS, len: start, value: new pl.type.Term(atom.value, exprs)};
			if(tokens[start] && tokens[start].name === "l_paren") {
				if(tokens[start+1] && tokens[start+1].name === "r_paren") 
					return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start+1], "argument expected")};
				var expr = parseExpr(thread, tokens, ++start, "999", true);
				if(expr.type === ERROR) {
					if( expr.derived )
						return expr;
					else
						return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start] ? tokens[start] : tokens[start-1], "argument expected", !tokens[start])};
				}
				exprs.push(expr.value);
				start = expr.len;
				while(tokens[start] && tokens[start].name === "atom" && tokens[start].value === ",") {
					expr = parseExpr(thread, tokens, start+1, "999", true);
					if(expr.type === ERROR) {
						if( expr.derived )
							return expr;
						else
							return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start+1] ? tokens[start+1] : tokens[start], "argument expected", !tokens[start+1])};
					}
					exprs.push(expr.value);
					start = expr.len;
				}
				if(tokens[start] && tokens[start].name === "r_paren") start++;
				else return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start] ? tokens[start] : tokens[start-1], ", or ) expected", !tokens[start])};
			}
			return {type: SUCCESS, len: start, value: new pl.type.Term(atom.value, exprs)};
		}
		return {type: ERROR, derived: false, value: pl.error.syntax(tokens[start], "term expected")};
	}

	// Parse a list
	function parseList(thread, tokens, start) {
		if(!tokens[start]) 
			return {type: ERROR, derived: false, value: pl.error.syntax(tokens[start-1], "[ expected")};
		if(tokens[start] && tokens[start].name === "l_brace") {
			var expr = parseExpr(thread, tokens, ++start, "999", true);
			var exprs = [expr.value];
			var cons = undefined;

			if(expr.type === ERROR) {
				if(tokens[start] && tokens[start].name === "r_brace") {
					return {type: SUCCESS, len: start+1, value: new pl.type.Term("[]", [])};
				}
				return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start], "] expected")};
			}
			
			start = expr.len;

			while(tokens[start] && tokens[start].name === "atom" && tokens[start].value === ",") {
				expr = parseExpr(thread, tokens, start+1, "999", true);
				if(expr.type === ERROR) {
					if( expr.derived )
						return expr;
					else
						return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start+1] ? tokens[start+1] : tokens[start], "argument expected", !tokens[start+1])};
				}
				exprs.push(expr.value);
				start = expr.len;
			}
			var bar = false
			if(tokens[start] && tokens[start].name === "bar") {
				bar = true;
				expr = parseExpr(thread, tokens, start+1, "999", true);
				if(expr.type === ERROR) {
					if( expr.derived )
						return expr;
					else
						return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start+1] ? tokens[start+1] : tokens[start], "argument expected", !tokens[start+1])};
				}
				cons = expr.value;
				start = expr.len;
			}
			if(tokens[start] && tokens[start].name === "r_brace")
				return {type: SUCCESS, len: start+1, value: arrayToList(exprs, cons) };
			else
				return {type: ERROR, derived: true, value: pl.error.syntax(tokens[start] ? tokens[start] : tokens[start-1], bar ? "] expected" : ", or | or ] expected", !tokens[start])};
		}
		return {type: ERROR, derived: false, value: pl.error.syntax(tokens[start], "list expected")};
	}

	// Parse a rule
	function parseRule(thread, tokens, start) {
		var line = tokens[start].line;
		var expr = parseExpr(thread, tokens, start, thread.__get_max_priority(), false);
		var rule = null;
		var obj;
		if(expr.type !== ERROR) {
			start = expr.len;
			if(tokens[start] && tokens[start].name === "atom" && tokens[start].raw === ".") {
				start++;
				if( pl.type.is_term(expr.value) ) {
					if(expr.value.indicator === ":-/2") {
						rule = new pl.type.Rule(expr.value.args[0], body_conversion(expr.value.args[1]))
						obj = {
							value: rule,
							len: start,
							type: SUCCESS
						};
					} else if(expr.value.indicator === "-->/2") {
						rule = rule_to_dcg(new pl.type.Rule(expr.value.args[0], expr.value.args[1]), thread);
						if(!pl.type.is_rule(rule))
							return {
								value: rule,
								len: start,
								type: ERROR
							};
						rule.body = body_conversion( rule.body );
						obj = {
							value: rule,
							len: start,
							type: pl.type.is_rule( rule ) ? SUCCESS : ERROR
						};
					} else {
						rule = new pl.type.Rule(expr.value, null);
						obj = {
							value: rule,
							len: start,
							type: SUCCESS
						};
					}
					if( rule ) {
						var singleton = rule.singleton_variables();
						if( singleton.length > 0 )
							thread.throw_warning( pl.warning.singleton( singleton, rule.head.indicator, line ) );
					}
					return obj;
				} else {
					return { type: ERROR, value: pl.error.syntax(tokens[start], "callable expected") };
				}
			} else {
				return { type: ERROR, value: pl.error.syntax(tokens[start] ? tokens[start] : tokens[start-1], ". or operator expected") };
			}
		}
		return expr;
	}

	// Parse a program
	function parseProgram(thread, string, options) {
		options = options ? options : {};
		options.from = options.from ? options.from : "$tau-js";
		options.reconsult = options.reconsult !== undefined ? options.reconsult : true;
		var tokenizer = new Tokenizer( thread );
		var reconsulted = {};
		var indicator;
		tokenizer.new_text( string );
		var n = 0;
		var tokens = tokenizer.get_tokens( n );
		while( tokens !== null && tokens[n] ) {
			var expr = parseRule(thread, tokens, n);
			if( expr.type === ERROR ) {
				return new Term("throw", [expr.value]);
			} else {
				// Term expansion
				var term_expansion = thread.session.rules["term_expansion/2"];
				if(term_expansion && term_expansion.length > 0) {
					var n_thread = new Thread( thread.session );
					var term = expr.value.body ? new Term(":-", [expr.value.head, expr.value.body]) : expr.value.head;
					term = term.rename( thread.session );
					n_thread.query("term_expansion(" + term.toString() + ", X).");
					n_thread.answer(function(answer) {
						if(answer && !pl.type.is_error(answer) && pl.type.is_term(answer.links['X'])) {
							var term = answer.links['X'];
							var rule = term.indicator === ":-/2" ? new Rule(term.args[0], term.args[1]) : new Rule( term, null ) ;
							parseProgramExpansion(thread, options, reconsulted, {value: rule, len: expr.len, type: expr.type});
						} else {
							parseProgramExpansion(thread, options, reconsulted, expr);
						}
					});
				} else {
					parseProgramExpansion(thread, options, reconsulted, expr);
				}
				n = expr.len;
				if(expr.value.body === null && expr.value.head.indicator === ":-/1" && 
				   expr.value.head.args[0].indicator === "char_conversion/2") {
					tokens = tokenizer.get_tokens( n );
					n = 0;
				}
			}
		}
		return true;
	}

	function parseGoalExpansion(thread, head, term, set, origin) {
		var n_thread = new Thread( thread.session );
		n_thread.__goal_expansion = true;
		var varterm = thread.next_free_variable();
		var varhead = thread.next_free_variable();
		var goal = varhead + " = " + head + ", goal_expansion(" + term + ", " + varterm + ").";
		n_thread.query(goal);
		n_thread.answer(function(answer) {
			if(answer && !pl.type.is_error(answer) && answer.links[varterm]) {
				set(answer.links[varhead], body_conversion(answer.links[varterm]));
				parseGoalExpansion(thread, origin.head(), origin.term(), origin.set, origin);
			}
		});
	}

	function parseQueryExpansion(thread, term) {
		var n_thread = new Thread( thread.session );
		n_thread.__goal_expansion = true;
		var varterm = thread.next_free_variable();
		var goal = "goal_expansion(" + term + ", " + varterm + ").";
		n_thread.query(goal);
		var variables = n_thread.head_point().substitution.domain();
		n_thread.answer(function(answer) {
			if(answer && !pl.type.is_error(answer) && answer.links[varterm]) {
				for(var i = 0; i < variables.length; i++) {
					if(variables[i] !== varterm.id && answer.links[variables[i]]) {
						var subs = new Substitution();
						subs.links[answer.links[variables[i]]] = variables[i];
						answer.links[varterm] = answer.links[varterm].apply( subs );
					}
				}
				parseQueryExpansion(thread, body_conversion(answer.links[varterm]));
			} else {
				thread.add_goal(term);
			}
		});
	}

	function parseProgramExpansion(thread, options, reconsulted, expr) {
		if(expr.value.body === null && expr.value.head.indicator === "?-/1") {
			var n_thread = new Thread( thread.session );
			n_thread.add_goal( expr.value.head.args[0] );
			n_thread.answer( function( answer ) {
				if( pl.type.is_error( answer ) ) {
					thread.throw_warning( answer.args[0] );
				} else if( answer === false || answer === null ) {
					thread.throw_warning( pl.warning.failed_goal( expr.value.head.args[0], expr.len ) );
				}
			} );
		} else if(expr.value.body === null && expr.value.head.indicator === ":-/1") {
			thread.run_directive(expr.value.head.args[0]);
		} else {
			indicator = expr.value.head.indicator;
			if( options.reconsult !== false && reconsulted[indicator] !== true && !thread.is_multifile_predicate( indicator ) ) {
				thread.session.rules[indicator] = filter( thread.session.rules[indicator] || [], function( rule ) { return rule.dynamic; } );
				reconsulted[indicator] = true;
			}
			var goal_expansion = thread.session.rules["goal_expansion/2"];
			if(expr.value.body !== null && goal_expansion && goal_expansion.length > 0) {
				thread.renamed_variables = {};
				var origin = {
					head: function() { return expr.value.head; },
					term: function() { return expr.value.body; },
					set: function(h, p){
						expr.value.head = h;
						expr.value.body = p;
					}
				};
				parseGoalExpansion(thread, expr.value.head, body_conversion(expr.value.body), origin.set, origin);
			}
			thread.add_rule(expr.value, options);
		}
	}
	
	// Parse a query
	function parseQuery(thread, string) {
		var tokenizer = new Tokenizer( thread );
		tokenizer.new_text( string );
		var n = 0;
		do {
			var tokens = tokenizer.get_tokens( n );
			if( tokens === null ) break;
			var expr = parseExpr(thread, tokens, 0, thread.__get_max_priority(), false);
			if(expr.type !== ERROR) {
				var expr_position = expr.len;
				var tokens_pos = expr_position;
				if(tokens[expr_position] && tokens[expr_position].name === "atom" && tokens[expr_position].raw === ".") {
					expr.value = body_conversion(expr.value);
					// Goal expansion
					var goal_expansion = thread.session.rules["goal_expansion/2"];
					if(!thread.__goal_expansion && goal_expansion && goal_expansion.length > 0) {
						parseQueryExpansion(thread, expr.value);
					} else {
						thread.add_goal( expr.value );
					}
				} else {
					var token = tokens[expr_position];
					return new Term("throw", [pl.error.syntax(token ? token : tokens[expr_position-1], ". or operator expected", !token)] );
				}
				
				n = expr.len + 1;
			} else {
				return new Term("throw", [expr.value]);
			}
		} while( true );
		return true;
	}


	
	// UTILS

	// Rule to DCG
	function rule_to_dcg(rule, thread) {
		rule = rule.rename( thread );
		var begin = thread.next_free_variable();
		var dcg = body_to_dcg( rule.body, begin, thread );
		if( dcg.error )
			return dcg.value;
		rule.body = dcg.value;
		// push-back lists
		if(rule.head.indicator === ",/2") {
			var terminals = rule.head.args[1];
			rule.head = rule.head.args[0];
			var last = thread.next_free_variable();
			var pointer = terminals;
			if(!pl.type.is_list(pointer)) {
				return pl.error.type("list", pointer, "DCG/0");
			}
			if(pointer.indicator === "[]/0") {
				terminals = dcg.variable;
			} else {
				while(pointer.indicator === "./2" && pl.type.is_list(pointer) && pointer.args[1].indicator !== "[]/0") {
					pointer = pointer.args[1];
				}
				if(pl.type.is_variable(pointer))
					return pl.error.instantiation("DCG/0");
				else if(!pl.type.is_list(pointer))
					return pl.error.type("list", terminals, "DCG/0");
				pointer.args[1] = dcg.variable;
			}
			rule.body = new Term(",", [rule.body, new Term("=", [last, terminals])]);
			rule.head = new Term(rule.head.id, rule.head.args.concat([begin, last]));
		} else {
			// replace first assignment
			var first_assign = rule.body;
			if(pl.type.is_term(first_assign) && first_assign.indicator === ",/2")
				first_assign = first_assign.args[0];
			if(pl.type.is_term(first_assign) && first_assign.indicator === "=/2" &&
			   pl.type.is_variable(first_assign.args[0]) && first_assign.args[0] === begin) {
				begin = first_assign.args[1];
				rule.body = rule.body.replace(null);
			}
			// add last variable
			rule.head = new Term(rule.head.id, rule.head.args.concat([begin, dcg.variable]));
		}
		return rule;
	}

	// Body to DCG
	function body_to_dcg(expr, last, thread) {
		var free;
		if( pl.type.is_term( expr ) && expr.indicator === "!/0" ) {
			free = thread.next_free_variable();
			return {
				value: new Term(",", [expr, new Term("=", [last, free])]),
				variable: free,
				error: false
			};
		} else if( pl.type.is_term( expr ) && expr.indicator === "\\+/1" ) {
			var left = body_to_dcg(expr.args[0], last, thread);
			if( left.error ) return left;
			return {
				value: new Term(expr.id, [left.value]),
				variable: last,
				error: false
			};
		} else if( pl.type.is_term( expr ) && (expr.indicator === ",/2" || expr.indicator === "->/2") ) {
			var left = body_to_dcg(expr.args[0], last, thread);
			if( left.error ) return left;
			var right = body_to_dcg(expr.args[1], left.variable, thread);
			if( right.error ) return right;
			return {
				value: new Term(expr.id, [left.value, right.value]),
				variable: right.variable,
				error: false
			};
		} else if( pl.type.is_term( expr ) && expr.indicator === ";/2" ) {
			var left = body_to_dcg(expr.args[0], last, thread);
			if( left.error ) return left;
			var right = body_to_dcg(expr.args[1], last, thread);
			if( right.error ) return right;
			return {
				value: new Term(",", [new Term(";", [left.value, right.value]), new Term("=", [left.variable, right.variable])]),
				variable: right.variable,
				error: false
			};
		} else if( pl.type.is_term( expr ) && expr.indicator === "{}/1" ) {
			free = thread.next_free_variable();
			return {
				value: new Term(",", [expr.args[0], new Term("=", [last, free])]),
				variable: free,
				error: false
			};
		} else if( pl.type.is_empty_list( expr ) ) {
			return {
				value: new Term("true", []),
				variable: last,
				error: false
			};
		} else if( pl.type.is_list( expr ) ) {
			free = thread.next_free_variable();
			var pointer = expr;
			var prev;
			while( pointer.indicator === "./2" ) {
				prev = pointer;
				pointer = pointer.args[1];
			}
			if( pl.type.is_variable( pointer ) ) {
				return {
					value: pl.error.instantiation("DCG/0"),
					variable: last,
					error: true
				};
			} else if( !pl.type.is_empty_list( pointer ) ) {
				return {
					value: pl.error.type("list", expr, "DCG/0"),
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
			free = thread.next_free_variable();
			expr = new Term( expr.id, expr.args.concat([last,free]) );
			return {
				value: expr,
				variable: free,
				error: false
			};
		} else {
			return {
				value: pl.error.type( "callable", expr, "DCG/0" ),
				variable: last,
				error: true
			};
		}
	}
	
	// Body conversion
	function body_conversion( expr ) {
		if( pl.type.is_variable( expr ) )
			return new Term( "call", [expr] );
		else if( pl.type.is_term( expr ) && [",/2", ";/2", "->/2"].indexOf(expr.indicator) !== -1 )
			return new Term( expr.id, [body_conversion( expr.args[0] ), body_conversion( expr.args[1] )] );
		return expr;
	}
	
	// List to Prolog list
	function arrayToList( array, cons ) {
		var list = cons ? cons : new Term( "[]", [] );
		for(var i = array.length-1; i >= 0; i-- )
			list = new Term( ".", [array[i], list] );
		return list;
	}
	
	// Remove element from array
	function remove( array, element ) {
		for( var i = array.length - 1; i >= 0; i-- ) {
			if( array[i] === element ) {
				array.splice(i, 1);
			}
		}
	}
	
	// Remove duplicate elements
	function nub( array ) {
		var seen = {};
		var unique = [];
		for( var i = 0; i < array.length; i++ ) {
			if( !(array[i] in seen) ) {
				unique.push( array[i] );
				seen[array[i]] = true;
			}
		}
		return unique;
	}
	
	// Retract a rule
	function retract( thread, point, indicator, rule ) {
		if( thread.session.rules[indicator] !== null ) {
			for( var i = 0; i < thread.session.rules[indicator].length; i++ ) {
				if( thread.session.rules[indicator][i] === rule ) {
					thread.session.rules[indicator].splice( i, 1 );
					thread.success( point );
					break;
				}
			}
		}
	}
	
	// call/n
	function callN( n ) {
		return function ( thread, point, atom ) {
			var closure = atom.args[0], args = atom.args.slice(1, n);
			if( pl.type.is_variable( closure ) ) {
				thread.throw_error( pl.error.instantiation( thread.level ) );
			} else if( !pl.type.is_callable( closure ) ) {
				thread.throw_error( pl.error.type( "callable", closure, thread.level ) );
			} else {
				var goal = new Term( closure.id, closure.args.concat( args ) );
				thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
			}
		};
	}
	
	// String to indicator
	function str_indicator( str ) {
		for( var i = str.length - 1; i >= 0; i-- )
			if( str.charAt(i) === "/" )
				return new Term( "/", [new Term( str.substring(0, i) ), new Num( parseInt(str.substring(i+1)), false )] );
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
	var term_ref = 0;
	function Term( id, args, ref ) {
		term_ref++;
		this.ref = ref || term_ref;
		this.id = id;
		this.args = args || [];
		this.indicator = id + "/" + this.args.length;
	}

	// Streams
	var stream_ref = 0;
	function Stream( stream, mode, alias, type, reposition, eof_action ) {
		this.id = stream_ref++;
		this.stream = stream;
		this.mode = mode; // "read" or "write" or "append"
		this.alias = alias;
		this.type = type !== undefined ? type : "text"; // "text" or "binary"
		this.reposition = reposition !== undefined ? reposition : true; // true or false
		this.eof_action = eof_action !== undefined ? eof_action : "eof_code"; // "error" or "eof_code" or "reset"
		this.position = this.mode === "append" ? "end_of_stream" : 0;
		this.output = this.mode === "write" || this.mode === "append";
		this.input = this.mode === "read";
	}
	
	// Substitutions
	function Substitution( links, attrs ) {
		links = links || {};
		attrs = attrs || {};
		this.links = links;
		this.attrs = attrs;
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
	function Rule( head, body, dynamic ) {
		this.head = head;
		this.body = body;
		this.dynamic = dynamic ? dynamic : false;
	}

	// Session
	function Session( limit ) {
		limit = limit === undefined || limit <= 0 ? 1000 : limit;
		this.rules = {};
		this.src_predicates = {};
		this.rename = 0;
		this.modules = [];
		this.thread = new Thread( this );
		this.total_threads = 1;
		this.renamed_variables = {};
		this.public_predicates = {};
		this.multifile_predicates = {};
		this.limit = limit;
		this.streams = {
			"user_input": new Stream(
				nodejs_flag ? nodejs_user_input : tau_user_input,
				"read", "user_input", "text", false, "reset" ),
			"user_output": new Stream(
				nodejs_flag ? nodejs_user_output : tau_user_output,
				"write", "user_output", "text", false, "eof_code" )
		};
		this.file_system = nodejs_flag ? nodejs_file_system : tau_file_system;
		this.standard_input = this.streams["user_input"];
		this.standard_output = this.streams["user_output"];
		this.current_input = this.streams["user_input"];
		this.current_output = this.streams["user_output"];
		this.format_success = function( state ) { return state.substitution; };
		this.format_error = function( state ) { return state.goal; };
		this.flag = {	
			bounded: pl.flag.bounded.value,
			max_integer: pl.flag.max_integer.value,
			min_integer: pl.flag.min_integer.value,
			integer_rounding_function: pl.flag.integer_rounding_function.value,
			char_conversion: pl.flag.char_conversion.value,
			debug: pl.flag.debug.value,
			max_arity: pl.flag.max_arity.value,
			unknown: pl.flag.unknown.value,
			double_quotes: pl.flag.double_quotes.value,
			occurs_check: pl.flag.occurs_check.value,
			dialect: pl.flag.dialect.value,
			version_data: pl.flag.version_data.value,
			nodejs: pl.flag.nodejs.value,
			argv: pl.flag.argv.value
		};
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
			600: { ":": ["xfy"] },
			500: { "+": ["yfx"], "-": ["yfx"], "/\\": ["yfx"], "\\/": ["yfx"] },
			400: {
				"*": ["yfx"], "/": ["yfx"], "//": ["yfx"], "rem": ["yfx"],
				"mod": ["yfx"], "<<": ["yfx"], ">>": ["yfx"]
			},
			200: { "**": ["xfx"], "^": ["xfy"], "-": ["fy"], "+": ["fy"], "\\": ["fy"] }
		};
	}
	
	// Threads
	function Thread( session ) {
		this.epoch = Date.now();
		this.session = session;
		this.session.total_threads++;
		this.total_steps = 0;
		this.cpu_time = 0;
		this.cpu_time_last = 0;
		this.points = [];
		this.debugger = false;
		this.debugger_states = [];
		this.level = "top_level/0";
		this.__calls = [];
		this.current_limit = this.session.limit;
		this.warnings = [];
		this.__goal_expansion = false;
	}
	
	// Modules
	function Module( id, rules, exports ) {
		this.id = id;
		this.rules = rules;
		this.exports = exports;
		pl.module[id] = this;
	}
	
	Module.prototype.exports_predicate = function( indicator ) {
		return this.exports.indexOf( indicator ) !== -1;
	};



	// UNIFY PROLOG OBJECTS
	
	// Variables
	Var.prototype.unify = function( obj, occurs_check ) {
		if( occurs_check && indexOf( obj.variables(), this.id ) !== -1 && !pl.type.is_variable( obj ) ) {
			return null;
		}
		var links = {};
		links[this.id] = obj;
		return new Substitution( links );
	};
	
	// Numbers
	Num.prototype.unify = function( obj, _ ) {
		if( pl.type.is_number( obj ) && this.value === obj.value && this.is_float === obj.is_float ) {
			return new Substitution();
		}
		return null;
	};
	
	// Terms
	Term.prototype.unify = function( obj, occurs_check ) {
		if( pl.type.is_term( obj ) && this.indicator === obj.indicator ) {
			var subs = new Substitution();
			for( var i = 0; i < this.args.length; i++ ) {
				var mgu = pl.unify( this.args[i].apply( subs ), obj.args[i].apply( subs ), occurs_check );
				if( mgu === null )
					return null;
				for( var x in mgu.links )
					subs.links[x] = mgu.links[x];
				subs = subs.apply( mgu );
			}
			return subs;
		}
		return null;
	};

	// Streams
	Stream.prototype.unify = function( obj, occurs_check ) {
		if( pl.type.is_stream( obj ) && this.id === obj.id ) {
			return new Substitution();
		}
		return null;
	};
	
	

	// PROLOG OBJECTS TO STRING
	
	// Variables
	Var.prototype.toString = function( _ ) {
		return this.id;
	};
	
	// Numbers
	Num.prototype.toString = function( _ ) {
		return this.is_float && indexOf(this.value.toString(), ".") === -1 ? this.value + ".0" : this.value.toString();
	};
	
	// Terms
	Term.prototype.toString = function( options, priority, from ) {
		options = !options ? {} : options;
		options.quoted = options.quoted === undefined ? true: options.quoted;
		options.ignore_ops = options.ignore_ops === undefined ? false : options.ignore_ops;
		options.numbervars = options.numbervars === undefined ? false : options.numbervars;
		priority = priority === undefined ? {priority: 999, class: "", indicator: ""} : priority;
		from = from === undefined ? "" : from;
		if( options.numbervars && this.indicator === "$VAR/1" && pl.type.is_integer( this.args[0] ) && this.args[0].value >= 0 ) {
			var i = this.args[0].value;
			var number = Math.floor( i/26 );
			var letter =  i % 26;
			return "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[letter] + (number !== 0 ? number : "");
		}
		switch( this.indicator ){
			case "[]/0":
			case "{}/0":
			case "!/0":
				return this.id;
			case "{}/1":
				return "{" + this.args[0].toString( options ) + "}";
			case "./2":
				if( options.ignore_ops === false ) {
					var list = "[" + this.args[0].toString( options );
					var pointer = this.args[1];
					while( pointer.indicator === "./2" ) {
						list += ", " + pointer.args[0].toString( options );
						pointer = pointer.args[1];
					}
					if( pointer.indicator !== "[]/0" ) {
						list += "|" + pointer.toString( options );
					}
					list += "]";
					return list;
				}
			default:
				var id = this.id;
				var operator = options.session ? options.session.lookup_operator( this.id, this.args.length ) : null;
				if( options.session === undefined || options.ignore_ops || operator === null ) {
					if( options.quoted && ! /^(!|;|[a-z][0-9a-zA-Z_]*|[#\$\&\*\+\-\.\/\:\<\=\>\?\@\^\~\\]+)$/.test( id ) && id !== "{}" && id !== "[]" )
						id = "'" + redoEscape(id) + "'";
					return id + (this.args.length ? "(" + map( this.args,
						function(x) { return x.toString( options); }
					).join(", ") + ")" : "");
				} else {
					var priority_op = parseInt(operator.priority);
					var priority_arg = parseInt(priority.priority);
					var cond = priority_op > priority_arg || priority_op === priority_arg && (
						operator.class === "xfx" ||
						operator.class === "xfy" && this.indicator !== priority.indicator ||
						operator.class === "yfx" && this.indicator !== priority.indicator ||
						this.indicator === priority.indicator && operator.class === "yfx" && from === "right" ||
						this.indicator === priority.indicator && operator.class === "xfy" && from === "left");
					operator.indicator = this.indicator;
					var lpar = cond ? "(" : "";
					var rpar = cond ? ")" : "";
					if( this.args.length === 0 ) {
						return "(" + this.id + ")";
					} else if( ["fy","fx"].indexOf( operator.class) !== -1 ) {
						return lpar + id + this.args[0].toString( options, operator ) + rpar;
					} else if( ["yf","xf"].indexOf( operator.class) !== -1 ) {
						return lpar + this.args[0].toString( options, operator ) + id + rpar;
					} else {
						return lpar + this.args[0].toString( options, operator, "left" ) + this.id + this.args[1].toString( options, operator, "right" ) +  rpar;
					}
				}
		}
	};

	// Streams
	Stream.prototype.toString = function( _ ) {
		return "<stream>(" + this.id + ")";
	};
	
	// Substitutions
	Substitution.prototype.toString = function( options ) {
		var str = "{";
		for( var link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			if( str !== "{" ) {
				str += ", ";
			}
			str += link + "/" + this.links[link].toString( options );
		}
		str += "}";
		return str;
	};
	
	// States
	State.prototype.toString = function( options ) {
		if( this.goal === null ) {
			return "<" + this.substitution.toString( options ) + ">";
		} else {
			return "<" + this.goal.toString( options ) + ", " + this.substitution.toString( options ) + ">";
		}
	};
	
	// Rules
	Rule.prototype.toString = function( options ) {
		if( !this.body ) {
			return this.head.toString( options ) + ".";
		} else {
			return this.head.toString( options ) + " :- " + this.body.toString( options ) + ".";
		}
	};
	
	// Session
	Session.prototype.toString = function( options ) {
		var str = "";
		for(var i = 0; i < this.modules.length; i++) {
			str += ":- use_module(library(" + this.modules[i] + ")).\n";
		}
		str += "\n";
		for(key in this.rules) {
			for(i = 0; i < this.rules[key].length; i++) {
				str += this.rules[key][i].toString( options );
				str += "\n";
			}
		}
		return str;
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
		return new Term( this.id, map( this.args, function( arg ) {
			return arg.clone();
		} ) );
	};

	// Streams
	Stream.prototype.clone = function() {
		return new Stram( this.stream, this.mode, this.alias, this.type, this.reposition, this.eof_action );
	};
	
	// Substitutions
	Substitution.prototype.clone = function() {
		var links = {};
		var attrs = {};
		for( var link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			links[link] = this.links[link].clone();
		}
		for( var attr in this.attrs ) {
			if(!this.attrs.hasOwnProperty(attrs)) continue;
			attrs[attr] = {};
			for( var m in this.attrs[attr] ) {
				if(!this.attrs[attr].hasOwnProperty(m)) continue;
				attrs[attr][m] = this.attrs[attr][m].clone();
			}
		}
		return new Substitution( links, attrs );
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

	// Streams
	Stream.prototype.equals = function( obj ) {
		return pl.type.is_stream( obj ) && this.id === obj.id;
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
		return pl.type.is_state( obj ) && this.goal.equals( obj.goal ) && this.substitution.equals( obj.substitution ) && this.parent === obj.parent;
	};
	
	// Rules
	Rule.prototype.equals = function( obj ) {
		return pl.type.is_rule( obj ) && this.head.equals( obj.head ) && (this.body === null && obj.body === null || this.body !== null && this.body.equals( obj.body ));
	};
	
	
	
	// RENAME VARIABLES OF PROLOG OBJECTS
	
	// Variables
	Var.prototype.rename = function( thread ) {
		return thread.get_free_variable( this );
	};
	
	// Numbers
	Num.prototype.rename = function( _ ) {
		return this;
	};
	
	// Terms
	Term.prototype.rename = function( thread ) {
		// atom
		/*if(this.args.length === 0)
			return this;*/
		// list
		if( this.indicator === "./2" ) {
			var arr = [], pointer = this;
			var last_neq = -1, pointer_neq = null, i = 0;
			while( pointer.indicator === "./2" ) {
				var app = pointer.args[0].rename(thread);
				var cmp = app == pointer.args[0];
				arr.push(app);
				pointer = pointer.args[1];
				if(!cmp) {
					last_neq = i;
					pointer_neq = pointer;
				}
				i++;
			}
			var list = pointer.rename(thread);
			var cmp = list == pointer;
			if(last_neq === -1 && cmp)
				return this;
			var start = cmp ? last_neq : arr.length-1;
			var list = cmp ? pointer_neq : list;
			for(var i = start; i >= 0; i--) {
				list = new Term( ".", [arr[i], list] );
			}
			return list;
		}
		// compound term
		var eq = true;
		var args = [];
		for(var i = 0; i < this.args.length; i++) {
			var app = this.args[i].rename(thread);
			eq = eq && this.args[i] == app;
			args.push(app);
		}
		/*if(eq)
			return this;*/
		return new Term(this.id, args);
	};

	// Streams
	Stream.prototype.rename = function( thread ) {
		return this;
	};
	
	// Rules
	Rule.prototype.rename = function( thread ) {
		return new Rule( this.head.rename( thread ), this.body !== null ? this.body.rename( thread ) : null );
	};
	
	
	
	// GET ID OF VARIABLES FROM PROLOG OBJECTS
	
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

	// Streams
	Stream.prototype.variables = function() {
		return [];
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
		// atom
		if(this.args.length === 0)
			return this;
		// list
		if( this.indicator === "./2" ) {
			var arr = [], pointer = this;
			var last_neq = -1, pointer_neq = null, i = 0;
			while( pointer.indicator === "./2" ) {
				var app = pointer.args[0].apply(subs);
				var cmp = app == pointer.args[0];
				arr.push(app);
				pointer = pointer.args[1];
				if(!cmp) {
					last_neq = i;
					pointer_neq = pointer;
				}
				i++;
			}
			var list = pointer.apply(subs);
			var cmp = list == pointer;
			if(last_neq === -1 && cmp)
				return this;
			var start = cmp ? last_neq : arr.length-1;
			var list = cmp ? pointer_neq : list;
			for(var i = start; i >= 0; i--) {
				list = new Term( ".", [arr[i], list] );
			}
			return list;
		}
		// compound term
		var eq = true;
		var args = [];
		for(var i = 0; i < this.args.length; i++) {
			var app = this.args[i].apply(subs);
			eq = eq && this.args[i] == app;
			args.push(app);
		}
		if(eq)
			return this;
		return new Term(this.id, args, this.ref);
	};

	// Streams
	Stream.prototype.apply = function( _ ) {
		return this;
	};
	
	// Rules
	Rule.prototype.apply = function( subs ) {
		return new Rule( this.head.apply( subs ), this.body !== null ? this.body.apply( subs ) : null );
	};
	
	// Substitutions
	Substitution.prototype.apply = function( subs ) {
		var link, links = {}, attr, attrs = {}, m;
		for( link in this.links ) {
			if(!this.links.hasOwnProperty(link)) continue;
			links[link] = this.links[link].apply(subs);
		}
		for( attr in this.attrs ) {
			if(!this.attrs.hasOwnProperty(attr)) continue;
			attrs[attr] = {};
			for( m in this.attrs[attr] ) {
				if(!this.attrs[attr].hasOwnProperty(m)) continue;
				attrs[attr][m] = this.attrs[attr][m].apply(subs);
			}
		}
		return new Substitution( links, attrs );
	};
	
	
	
	// SELECTION FUNCTION
	
	// Select term
	Term.prototype.select = function() {
		var pointer = this;
		while( pointer.indicator === ",/2" )
			pointer = pointer.args[0];
		return pointer;
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
		if(this == expr || this.ref === expr.ref)
			return true;
		for( var i = 0; i < this.args.length; i++ )
			if( pl.type.is_term( this.args[i] ) && this.args[i].search( expr ) )
				return true;
		return false;
	};
	
	
	
	// PROLOG SESSIONS AND THREADS

	// Get current input
	Session.prototype.get_current_input = function() {
		return this.current_input;
	};
	Thread.prototype.get_current_input = function() {
		return this.session.get_current_input();
	};

	// Get current output
	Session.prototype.get_current_output = function() {
		return this.current_output;
	};
	Thread.prototype.get_current_output = function() {
		return this.session.get_current_output();
	};

	// Set current input
	Session.prototype.set_current_input = function( input ) {
		this.current_input = input;
	};
	Thread.prototype.set_current_input = function( input ) {
		return this.session.set_current_input( input );
	};

	// Set current output
	Session.prototype.set_current_output = function( output ) {
		this.current_output = output;
	};
	Thread.prototype.set_current_output = function( output ) {
		return this.session.set_current_output( output);
	};

	// Get stream by alias
	Session.prototype.get_stream_by_alias = function( alias ) {
		return this.streams[alias];
	};
	Thread.prototype.get_stream_by_alias = function( alias ) {
		return this.session.get_stream_by_alias( alias );
	};

	// Open file
	Session.prototype.file_system_open = function( path, type, mode ) {
		return this.file_system.open( path, type, mode );
	};
	Thread.prototype.file_system_open = function( path, type, mode ) {
		return this.session.file_system_open( path, type, mode );
	};

	// Get conversion of the char
	Session.prototype.get_char_conversion = function( char ) {
		return this.__char_conversion[char] || char;
	};
	Thread.prototype.get_char_conversion = function( char ) {
		return this.session.get_char_conversion( char );
	};
	
	// Parse an expression
	Session.prototype.parse = function( string ) {
		return this.thread.parse( string );
	};
	Thread.prototype.parse = function( string ) {
		var tokenizer = new Tokenizer( this );
		tokenizer.new_text( string );
		var tokens = tokenizer.get_tokens();
		if( tokens === null )
			return false;
		var expr = parseExpr(this, tokens, 0, this.__get_max_priority(), false);
		if( expr.len !== tokens.length )
			return false;
		return { value: expr.value, expr: expr, tokens: tokens };
	};
	
	// Get flag value
	Session.prototype.get_flag = function( flag ) {
		return this.flag[flag];
	};
	Thread.prototype.get_flag = function( flag ) {
		return this.session.get_flag( flag );
	};

	// Add a rule
	Session.prototype.add_rule = function( rule, options ) {
		options = options ? options : {};
		options.from = options.from ? options.from : "$tau-js";
		this.src_predicates[rule.head.indicator] = options.from;
		if(!this.rules[rule.head.indicator]) {
			this.rules[rule.head.indicator] = [];
		}
		this.rules[rule.head.indicator].push(rule);
		if( !this.public_predicates.hasOwnProperty( rule.head.indicator ) )
			this.public_predicates[rule.head.indicator] = false;
		return true;
	};
	Thread.prototype.add_rule = function( rule, options ) {
		return this.session.add_rule( rule, options );
	};

	// Run a directive
	Session.prototype.run_directive = function( directive ) {
		this.thread.run_directive( directive );
	};
	Thread.prototype.run_directive = function( directive ) {
		if( pl.type.is_directive( directive ) ) {
			pl.directive[directive.indicator]( this, directive );
			return true;
		}
		return false;
	};
	
	// Get maximum priority of the operators
	Session.prototype.__get_max_priority = function() {
		return "1200";
	};
	Thread.prototype.__get_max_priority = function() {
		return this.session.__get_max_priority();
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
	Thread.prototype.__get_next_priority = function( priority ) {
		return this.session.__get_next_priority( priority );
	};
	
	// Get classes of an operator
	Session.prototype.__lookup_operator_classes = function( priority, operator ) {
		if( this.__operators.hasOwnProperty( priority ) && this.__operators[priority][operator] instanceof Array ) {
			return this.__operators[priority][operator]  || false;
		}
		return false;
	};
	Thread.prototype.__lookup_operator_classes = function( priority, operator ) {
		return this.session.__lookup_operator_classes( priority, operator );
	};

	// Get operator
	Session.prototype.lookup_operator = function( name, arity ) {
		for(var p in this.__operators)
			if(this.__operators[p][name])
				for(var i = 0; i < this.__operators[p][name].length; i++)
					if( arity === 0 || this.__operators[p][name][i].length === arity+1 )
						return {priority: p, class: this.__operators[p][name][i]};
		return null;
	};
	Thread.prototype.lookup_operator = function( name, arity ) {
		return this.session.lookup_operator( name, arity );
	};
	
	// Throw a warning
	Session.prototype.throw_warning = function( warning ) {
		this.thread.throw_warning( warning );
	};
	Thread.prototype.throw_warning = function( warning ) {
		this.warnings.push( warning );
	};
	
	// Get warnings
	Session.prototype.get_warnings = function() {
		return this.thread.get_warnings();
	};
	Thread.prototype.get_warnings = function() {
		return this.warnings;
	};

	// Add a goal
	Session.prototype.add_goal = function( goal, unique ) {
		this.thread.add_goal( goal, unique );
	};
	Thread.prototype.add_goal = function( goal, unique, parent ) {
		parent = parent ? parent : null;
		if( unique === true )
			this.points = [];
		var vars = goal.variables();
		var links = {};
		for( var i = 0; i < vars.length; i++ )
			links[vars[i]] = new Var(vars[i]);
		this.points.push( new State( goal, new Substitution(links), parent ) );
	};

	// Consult a program from a string
	Session.prototype.consult = function( program, options ) {
		return this.thread.consult( program, options );
	};
	Thread.prototype.consult = function( program, options ) {
		var string = "";
		// string
		if( typeof program === "string" ) {
			string = program;
			var len = string.length;
			// script id
			if( !nodejs_flag && program != "" && document.getElementById( string ) ) {
				var script = document.getElementById( string );
				var type = script.getAttribute( "type" );
				if( type !== null && type.replace( / /g, "" ).toLowerCase() === "text/prolog" ) {
					string = script.text;
				}
			// file (node.js)
			} else if( nodejs_flag ) {
				var fs = require("fs");
				const isFile = fs.existsSync(program);
				if(isFile) string = fs.readFileSync( program ).toString();
				else string = program;
			// http request
			} else if( program != "" && !(/\s/.test(program)) ) {
				try {
					var xhttp = new XMLHttpRequest();
					xhttp.onreadystatechange = function() {
						if( this.readyState == 4 && this.status == 200 )
							string = xhttp.responseText;
					}
					xhttp.open("GET", program, false);
					xhttp.send();
				} catch(ex) {}
			}
		// html
		} else if( program.nodeName ) {
			switch( program.nodeName.toLowerCase() ) {
				case "input":
				case "textarea":
					string = program.value;
					break;
				default:
					string = program.innerHTML;
					break;
			}
		} else {
			return false;
		}
		this.warnings = [];
		return parseProgram( this, string, options );
	};

	// Query goal from a string (without ?-)
	Session.prototype.query = function( string ) {
		return this.thread.query( string );
	};
	Thread.prototype.query = function( string ) {
		this.points = [];
		this.debugger_points = [];
		return parseQuery( this, string );
	};
	
	// Get first choice point
	Session.prototype.head_point = function() {
		return this.thread.head_point();
	};
	Thread.prototype.head_point = function() {
		return this.points[this.points.length-1];
	};
	
	// Get free variable
	Session.prototype.get_free_variable = function( variable ) {
		return this.thread.get_free_variable( variable );
	};
	Thread.prototype.get_free_variable = function( variable ) {
		var variables = [];
		if( variable.id === "_" || this.session.renamed_variables[variable.id] === undefined ) {
			this.session.rename++;
			if( this.current_point )
				variables = this.current_point.substitution.domain();
			while( indexOf( variables, pl.format_variable( this.session.rename ) ) !== -1 ) {
				this.session.rename++;
			}
			if( variable.id === "_" ) {
				return new Var( pl.format_variable( this.session.rename ) );
			} else {
				this.session.renamed_variables[variable.id] = pl.format_variable( this.session.rename );
			}
		}
		return new Var( this.session.renamed_variables[variable.id] );
	};
	
	// Get next free variable
	Session.prototype.next_free_variable = function() {
		return this.thread.next_free_variable();
	};
	Thread.prototype.next_free_variable = function() {
		this.session.rename++;
		var variables = [];
		if( this.current_point )
			variables = this.current_point.substitution.domain();
		while( indexOf( variables, pl.format_variable( this.session.rename ) ) !== -1 ) {
			this.session.rename++;
		}
		return new Var( pl.format_variable( this.session.rename ) );
	};
	
	// Check if a predicate is public
	Session.prototype.is_public_predicate = function( indicator ) {
		return !this.public_predicates.hasOwnProperty( indicator ) || this.public_predicates[indicator] === true;
	};
	Thread.prototype.is_public_predicate = function( indicator ) {
		return this.session.is_public_predicate( indicator );
	};
	
	// Check if a predicate is multifile
	Session.prototype.is_multifile_predicate = function( indicator ) {
		return this.multifile_predicates.hasOwnProperty( indicator ) && this.multifile_predicates[indicator] === true;
	};
	Thread.prototype.is_multifile_predicate = function( indicator ) {
		return this.session.is_multifile_predicate( indicator );
	};
	
	// Insert states at the beginning
	Session.prototype.prepend = function( states ) {
		return this.thread.prepend( states );
	};
	Thread.prototype.prepend = function( states ) {
		for(var i = states.length-1; i >= 0; i--)
			this.points.push( states[i] );
	};
	
	// Remove the selected term and prepend the current state
	Session.prototype.success = function( point, parent ) {
		return this.thread.success( point, parent );
	}
	Thread.prototype.success = function( point, parent ) {
		var parent = typeof parent === "undefined" ? point : parent;
		this.prepend( [new State( point.goal.replace( null ), point.substitution, parent ) ] );
	};
	
	// Throw error
	Session.prototype.throw_error = function( error ) {
		return this.thread.throw_error( error );
	};
	Thread.prototype.throw_error = function( error ) {
		this.prepend( [new State( new Term( "throw", [error] ), new Substitution(), null, null )] );
	};
	
	// Selection rule
	Session.prototype.step_rule = function( mod, atom ) {
		return this.thread.step_rule( mod, atom );
	}
	Thread.prototype.step_rule = function( mod, atom ) {
		var name = atom.indicator;
		if( mod === "user" )
			mod = null;
		if( mod === null && this.session.rules.hasOwnProperty(name) )
			return this.session.rules[name];
		var modules = mod === null ? this.session.modules : (indexOf(this.session.modules, mod) === -1 ? [] : [mod]);
		for( var i = 0; i < modules.length; i++ ) {
			var module = pl.module[modules[i]];
			if( module.rules.hasOwnProperty(name) && (module.rules.hasOwnProperty(this.level) || module.exports_predicate(name)) )
				return pl.module[modules[i]].rules[name];
		}
		return null;
	};
	
	// Resolution step
	Session.prototype.step = function() {
		return this.thread.step();
	}
	Thread.prototype.step = function() {
		if( this.points.length === 0 ) {
			return;
		}
		var asyn = false;
		var point = this.points.pop();
		this.current_point = point;
		if( this.debugger )
			this.debugger_states.push( point );
		
		if( pl.type.is_term( point.goal ) ) {
			
			var atom = point.goal.select();
			var mod = null;
			var states = [];
			if( atom !== null ) {

				this.total_steps++;
				var level = point;
				while( level.parent !== null && level.parent.goal.search( atom ) )
					level = level.parent;
				this.level = level.parent === null ? "top_level/0" : level.parent.goal.select().indicator;
				
				if( pl.type.is_term( atom ) && atom.indicator === ":/2" ) {
					mod = atom.args[0].id;
					atom = atom.args[1];
				}

				if( mod === null && pl.type.is_builtin( atom ) ) {
					this.__call_indicator = atom.indicator;
					asyn = pl.predicate[atom.indicator]( this, point, atom );
				} else {
					var srule = this.step_rule(mod, atom);
					if( srule === null ) {
						if( !this.session.rules.hasOwnProperty( atom.indicator ) ) {
							if( this.get_flag( "unknown" ).id === "error" ) {
								this.throw_error( pl.error.existence( "procedure", atom.indicator, this.level ) );
							} else if( this.get_flag( "unknown" ).id === "warning" ) {
								this.throw_warning( "unknown procedure " + atom.indicator + " (from " + this.level + ")" );
							}
						}
					} else if( srule instanceof Function ) {
						asyn = srule( this, point, atom );
					} else {
						// Goal expansion
						if( this.__goal_expansion && atom.indicator === "goal_expansion/2" )
							srule = srule.concat(pl.predicate["goal_expansion/2"]);
						for( var _rule in srule ) {
							if(!srule.hasOwnProperty(_rule)) continue;
							var rule = srule[_rule];
							this.session.renamed_variables = {};
							rule = rule.rename( this );
							var occurs_check = this.get_flag( "occurs_check" ).indicator === "true/0";
							var state = new State();
							var mgu = pl.unify( atom, rule.head, occurs_check );
							if( mgu !== null ) {
								state.goal = point.goal.replace( rule.body );
								if( state.goal !== null ) {
									state.goal = state.goal.apply( mgu );
								}
								state.substitution = point.substitution.apply( mgu );
								state.parent = point;
								states.push( state );
							}
						}
						this.prepend( states );
					}
				}
			}
		} else if( pl.type.is_variable( point.goal ) ) {
			this.throw_error( pl.error.instantiation( this.level ) );
		} else {
			this.throw_error( pl.error.type( "callable", point.goal, this.level ) );
		}
		return asyn;
	};
	
	// Find next computed answer
	Session.prototype.answer = function( success ) {
		return this.thread.answer( success );
	}
	Thread.prototype.answer = function( success ) {
		success = success || function( _ ) { };
		this.__calls.push( success );
		if( this.__calls.length > 1 ) {
			return;
		}
		this.again();
	};
	
	// Find all computed answers
	Session.prototype.answers = function( callback, max, after ) {
		return this.thread.answers( callback, max, after );
	}
	Thread.prototype.answers = function( callback, max, after ) {
		var answers = max || 1000;
		var thread = this;
		if( max <= 0 ) {
			if(after)
				after();
			return;
		}
		this.answer( function( answer ) {
			callback( answer );
			if( answer !== false ) {
				setTimeout( function() {
					thread.answers( callback, max-1, after );
				}, 1 );
			} else if(after) {
				after();
			}
		} );
	};

	// Again finding next computed answer
	Session.prototype.again = function( reset_limit ) {
		return this.thread.again( reset_limit );
	};
	Thread.prototype.again = function( reset_limit ) {
		var answer;
		var t0 = Date.now();
		while( this.__calls.length > 0 ) {
			this.warnings = [];
			if( reset_limit !== false )
				this.current_limit = this.session.limit;
			while( this.current_limit > 0 && this.points.length > 0 && this.head_point().goal !== null && !pl.type.is_error( this.head_point().goal ) ) {
				this.current_limit--;
				if( this.step() === true ) {
					return;
				}
			}
			var t1 = Date.now();
			this.cpu_time_last = t1-t0;
			this.cpu_time += this.cpu_time_last;
			var success = this.__calls.shift();
			if( this.current_limit <= 0 ) {
				success( null );
			} else if( this.points.length === 0 ) {
				success( false );
			} else if( pl.type.is_error( this.head_point().goal ) ) {
				answer = this.session.format_error( this.points.pop() );
				this.points = [];
				success( answer );
			} else {
				if( this.debugger )
					this.debugger_states.push( this.head_point() );
				answer = this.session.format_success( this.points.pop() );
				success( answer );
			}
		}
	};
	
	// Unfolding transformation
	Session.prototype.unfold = function( rule ) {
		if(rule.body === null)
			return false;
		var head = rule.head;
		var body = rule.body;
		var atom = body.select();
		var thread = new Thread( this );
		var unfolded = [];
		thread.add_goal( atom );
		thread.step();
		for( var i = thread.points.length-1; i >= 0; i-- ) {
			var point = thread.points[i];
			var head2 = head.apply( point.substitution );
			var body2 = body.replace( point.goal );
			if( body2 !== null )
				body2 = body2.apply( point.substitution );
			unfolded.push( new Rule( head2, body2 ) );
		}
		var rules = this.rules[head.indicator];
		var index = indexOf( rules, rule );
		if( unfolded.length > 0 && index !== -1 ) {
			rules.splice.apply( rules, [index, 1].concat(unfolded) );
			return true;
		}
		return false;
	};
	Thread.prototype.unfold = function(rule) {
		return this.session.unfold(rule);
	};

	
	
	// INTERPRET EXPRESSIONS
	
	// Variables
	Var.prototype.interpret = function( thread ) {
		return pl.error.instantiation( thread.level );
	};
	
	// Numbers
	Num.prototype.interpret = function( thread ) {
		return this;
	};
	
	// Terms
	Term.prototype.interpret = function( thread ) {
		if( pl.type.is_unitary_list( this ) ) {
			return this.args[0].interpret( thread );
		} else {
			return pl.operate( thread, this );
		}
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
	Substitution.prototype.filter = function( predicate ) {
		var links = {};
		for( var id in this.links ) {
			if(!this.links.hasOwnProperty(id)) continue;
			var value = this.links[id];
			if( predicate( id, value ) ) {
				links[id] = value;
			}
		}
		return new Substitution( links, this.attrs );
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
		return new Substitution( links, this.attrs );
	};
	
	// Add link
	Substitution.prototype.add = function( variable, value ) {
		this.links[variable] = value;
	};
	
	// Get domain
	Substitution.prototype.domain = function( plain ) {
		var f = plain === true ? function(x){return x;} : function(x){return new Var(x);};
		var vars = [];
		for( var x in this.links )
			vars.push( f(x) );
		return vars;
	};

	// Get an attribute
	Substitution.prototype.get_attribute = function( variable, module ) {
		if( this.attrs[variable] )
			return this.attrs[variable][module];
	}

	// Set an attribute (in a new substitution)
	Substitution.prototype.set_attribute = function( variable, module, value ) {
		var subs = new Substitution( this.links );
		for( var v in this.attrs ) {
			if( v === variable ) {
				subs.attrs[v] = {};
				for( var m in this.attrs[v] ) {
					subs.attrs[v][m] = this.attrs[v][m];
				}
			} else {
				subs.attrs[v] = this.attrs[v];
			}
		}
		if( !subs.attrs[variable] ) {
			subs.attrs[variable] = {};
		}
		subs.attrs[variable][module] = value;
		return subs;
	}

	// Check if a variables has attributes
	Substitution.prototype.has_attributes = function( variable ) {
		return this.attrs[variable] && this.attrs[variable] !== {};
	}
	
	
	
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
	
	
	
	// PROLOG TO JAVASCRIPT
	Var.prototype.toJavaScript = function() {
		return this.toString();
	};
	
	// Numbers
	Num.prototype.toJavaScript = function() {
		return this.value;
	};
	
	// Terms
	Term.prototype.toJavaScript = function() {
		// Atom => String
		if( this.args.length === 0 && this.indicator !== "[]/0" ) {
			return this.toString();
		} else if( pl.type.is_list( this ) ) {
			// List => Array
			var all_obj = true;
			var arr = [];
			var obj = {};
			var pointer = this;
			var value;
			while( pointer.indicator === "./2" ) {
				value = pointer.args[0].toJavaScript();
				arr.push( value );
				all_obj = all_obj && pl.type.is_term(pointer.args[0]) && pointer.args[0].indicator === "-/2" && pl.type.is_atom(pointer.args[0].args[0]);
				if(all_obj)
					obj[pointer.args[0].args[0].id] = pointer.args[0].args[1].toJavaScript();
				pointer = pointer.args[1];
			}
			if( pointer.indicator === "[]/0" )
				return all_obj && arr.length > 0 ? obj : arr;

		}
		return this.toString();
	};
	
	
	
	// RULES
	
	// Return singleton variables in the session
	Rule.prototype.singleton_variables = function() {
		var variables = this.head.variables();
		var count = {};
		var singleton = [];
		if( this.body !== null )
			variables = variables.concat( this.body.variables() );
		for( var i = 0; i < variables.length; i++ ) {
			if( count[variables[i]] === undefined )
				count[variables[i]] = 0;
			count[variables[i]]++;
		}
		for( var key in count )
			if( key !== "_" && count[key] === 1 )
				singleton.push( key );
		return singleton;
	};



	// NODEJS

	var nodejs_flag = typeof module !== 'undefined' && module.exports !== undefined;

	var nodejs_arguments = nodejs_flag ?
		arrayToList( map(process.argv.slice(1), function(arg) { return new Term( arg ); })) :
		new Term("[]", []);
	
	
	
	// PROLOG

	var pl = {
		
		// Environment
		__env: nodejs_flag ? global : window,
		
		// Modules
		module: {},
		
		// Version
		version: version,
		
		// Parser
		parser: {
			tokenizer: Tokenizer,
			expression: parseExpr
		},
		
		// Utils
		utils: {
			
			// String to indicator
			str_indicator: str_indicator,
			// Code point at
			codePointAt: codePointAt,
			// From code point
			fromCodePoint: fromCodePoint
			
		},
		
		// Statistics
		statistics: {
			
			// Number of created terms
			getCountTerms: function() {
				return term_ref;
			}
			
		},
		
		// JavaScript to Prolog
		fromJavaScript: {
			
			// Type testing
			test: {
				
				// Boolean
				boolean: function( obj, tobj ) {
					return obj === true || obj === false;
				},
				
				// Number
				number: function( obj, tobj ) {
					return typeof obj === "number";
				},
				
				// String
				string: function( obj, tobj ) {
					return typeof obj === "string";
				},
				
				// List
				list: function( obj, tobj ) {
					return obj instanceof Array;
				},
				
				// Variable
				variable: function( obj, tobj ) {
					return obj === undefined;
				},

				// Object
				object: function( obj, tobj ) {
					tobj = tobj === undefined ? false : tobj;
					return tobj && !(obj instanceof Array) && typeof obj === "object";
				},
				
				// Any
				any: function( _, tobj ) {
					return true;
				}
				
			},
			
			// Function conversion
			conversion: {
				
				// Bolean
				boolean: function( obj, tobj ) {
					return new Term( obj ? "true" : "false", [] );
				},
				
				// Number
				number: function( obj, tobj ) {
					return new Num( obj, obj % 1 !== 0 );
				},
				
				// String
				string: function( obj, tobj ) {
					return new Term( obj, [] );
				},
				
				// List
				list: function( obj, tobj ) {
					tobj = tobj === undefined ? false : tobj;
					var arr = [];
					var elem;
					for( var i = 0; i < obj.length; i++ ) {
						elem = pl.fromJavaScript.apply( obj[i], tobj );
						if( elem === undefined )
							return undefined;
						arr.push( elem );
					}
					return arrayToList( arr );
				},
				
				// Variable
				variable: function( obj, tobj ) {
					return new Var( "_" );
				},

				// Object
				object: function( obj, tobj ) {
					tobj = tobj === undefined ? false : tobj;
					var list = new Term("[]", []);
					var arr = [];
					for(var prop in obj) {
						if(!obj.hasOwnProperty(prop)) continue;
						arr.push(new Term("-", [
							pl.fromJavaScript.apply(prop, tobj),
							pl.fromJavaScript.apply(obj[prop], tobj)
						]));
					}
					return arrayToList(arr);
				},
				
				// Any
				any: function( obj, tobj ) {
					return undefined;
				}
				
			},
			
			// Transform object
			apply: function( obj, tobj ) {
				tobj = tobj === undefined ? false : tobj;
				for( var i in pl.fromJavaScript.test )
					if( i !== "any" && pl.fromJavaScript.test[i]( obj, tobj ) )
						return pl.fromJavaScript.conversion[i]( obj, tobj );
				return pl.fromJavaScript.conversion.any( obj, tobj );
			}
		},
		
		// Types
		type: {
			
			// Objects
			Var: Var,
			Num: Num,
			Term: Term,
			Rule: Rule,
			State: State,
			Stream: Stream,
			Module: Module,
			Thread: Thread,
			Session: Session,
			Substitution: Substitution,
			
			// Order
			order: [Var, Num, Term, Stream],
			
			// Compare types
			compare: function( x, y ) {
				var ord_x = indexOf( pl.type.order, x.constructor );
				var ord_y = indexOf( pl.type.order, y.constructor );
				if( ord_x < ord_y ) {
					return -1;
				} else if( ord_x > ord_y ) {
					return 1;
				} else {
					if( x.constructor === Num )
						if( x.is_float && y.is_float )
							return 0;
						else if( x.is_float )
							return -1;
						else if( y.is_float )
							return 1;
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

			// Is a stream
			is_stream: function( obj ) {
				return obj instanceof Stream;
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
			
			// Is a ground term
			is_ground: function( obj ) {
				if( obj instanceof Var ) return false;
				if( obj instanceof Term )
					for( var i = 0; i < obj.args.length; i++ )
						if( !pl.type.is_ground( obj.args[i] ) )
							return false;
				return true;
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
				return obj instanceof Term && (obj.id.length === 1 || obj.id.length > 0 && obj.id.length <= 2 && codePointAt( obj.id, 0 ) >= 65536);
			},
			
			// Is a character
			is_character_code: function( obj ) {
				return obj instanceof Num && !obj.is_float && obj.value >= 0 && obj.value <= 1114111;
			},

			// Is a byte
			is_byte: function( obj ) {
				return obj instanceof Num && !obj.is_float && obj.value >= 0 && obj.value <= 255;
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
				return obj instanceof Term && pl.predicate[obj.indicator] !== undefined && obj.indicator !== "goal_expansion/2";
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

			// Is a io mode
			is_io_mode: function( obj ) {
				return pl.type.is_atom( obj ) && ["read","write","append"].indexOf( obj.id ) !== -1;
			},

			// Is a stream option
			is_stream_option: function( obj ) {
				return pl.type.is_term( obj ) && (
					obj.indicator === "alias/1" && pl.type.is_atom(obj.args[0]) ||
					obj.indicator === "reposition/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false") ||
					obj.indicator === "type/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "text" || obj.args[0].id === "binary") ||
					obj.indicator === "eof_action/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "error" || obj.args[0].id === "eof_code" || obj.args[0].id === "reset")
				);
			},

			// Is a stream position
			is_stream_position: function( obj ) {
				return pl.type.is_integer( obj ) && obj.value >= 0 || pl.type.is_atom( obj ) && (obj.id === "end_of_stream" || obj.id === "past_end_of_stream");
			},

			// Is a stream property
			is_stream_property: function( obj ) {
				return pl.type.is_term( obj ) && (
					obj.indicator === "input/0" || 
					obj.indicator === "output/0" || 
					obj.indicator === "alias/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom( obj.args[0] )) ||
					obj.indicator === "file_name/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom( obj.args[0] )) ||
					obj.indicator === "position/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_stream_position( obj.args[0] )) ||
					obj.indicator === "reposition/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false")) ||
					obj.indicator === "type/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "text" || obj.args[0].id === "binary")) ||
					obj.indicator === "mode/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "read" || obj.args[0].id === "write" || obj.args[0].id === "append")) ||
					obj.indicator === "eof_action/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "error" || obj.args[0].id === "eof_code" || obj.args[0].id === "reset")) ||
					obj.indicator === "end_of_stream/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "at" || obj.args[0].id === "past" || obj.args[0].id === "not"))
				);
			},

			// Is a streamable term
			is_streamable: function( obj ) {
				return obj.__proto__.stream !== undefined;
			},

			// Is a read option
			is_read_option: function( obj ) {
				return pl.type.is_term( obj ) && ["variables/1","variable_names/1","singletons/1"].indexOf( obj.indicator ) !== -1;
			},

			// Is a write option
			is_write_option: function( obj ) {
				return pl.type.is_term( obj ) && (
					obj.indicator === "quoted/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false") ||
					obj.indicator === "ignore_ops/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false") ||
					obj.indicator === "numbervars/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false")
				);
			},

			// Is a close option
			is_close_option: function( obj ) {
				return pl.type.is_term( obj ) &&
					obj.indicator === "force/1" &&
					pl.type.is_atom(obj.args[0]) &&
					(obj.args[0].id === "true" || obj.args[0].id === "false");
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
				"tau/0": {
					type_args: null,
					type_result: true,
					fn: function( _ ) { return 2*Math.PI; }
				},
				"epsilon/0": {
					type_args: null,
					type_result: true,
					fn: function( _ ) { return Number.EPSILON; }
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
				"atan2/2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, _ ) { return Math.atan2( x, y ); }
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
					fn: function( x, thread ) { return x > 0 ? Math.log( x ) : pl.error.evaluation( "undefined", thread.__call_indicator ); }
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
					fn: function( x, y, thread ) { return y ? x / y : pl.error.evaluation( "zero_division", thread.__call_indicator ); }
				},
				"///2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, thread ) { return y ? parseInt( x / y ) : pl.error.evaluation( "zero_division", thread.__call_indicator ); }
				},
				"**/2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, _ ) { return Math.pow(x, y); }
				},
				"^/2": {
					type_args: null,
					type_result: null,
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
				"xor/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, _ ) { return x ^ y; }
				},
				"rem/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, thread ) { return y ? x % y : pl.error.evaluation( "zero_division", thread.__call_indicator ); }
				},
				"mod/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, thread ) { return y ? x - parseInt( x / y ) * y : pl.error.evaluation( "zero_division", thread.__call_indicator ); }
				},
				"max/2": {
					type_args: null,
					type_result: null,
					fn: function( x, y, _ ) { return Math.max( x, y ); }
				},
				"min/2": {
					type_args: null,
					type_result: null,
					fn: function( x, y, _ ) { return Math.min( x, y ); }
				}
				
			}
			
		},
		
		// Directives
		directive: {
			
			// dynamic/1
			"dynamic/1": function( thread, atom ) {
				var indicator = atom.args[0];
				if( pl.type.is_variable( indicator ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_compound( indicator ) || indicator.indicator !== "//2" ) {
					thread.throw_error( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				} else if( pl.type.is_variable( indicator.args[0] ) || pl.type.is_variable( indicator.args[1] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( indicator.args[0] ) ) {
					thread.throw_error( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
				} else if( !pl.type.is_integer( indicator.args[1] ) ) {
					thread.throw_error( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
				} else {
					var key = atom.args[0].args[0].id + "/" + atom.args[0].args[1].value;
					thread.session.public_predicates[key] = true;
					if( !thread.session.rules[key] )
						thread.session.rules[key] = [];
				}
			},
			
			// multifile/1
			"multifile/1": function( thread, atom ) {
				var indicator = atom.args[0];
				if( pl.type.is_variable( indicator ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_compound( indicator ) || indicator.indicator !== "//2" ) {
					thread.throw_error( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				} else if( pl.type.is_variable( indicator.args[0] ) || pl.type.is_variable( indicator.args[1] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( indicator.args[0] ) ) {
					thread.throw_error( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
				} else if( !pl.type.is_integer( indicator.args[1] ) ) {
					thread.throw_error( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
				} else {
					thread.session.multifile_predicates[atom.args[0].args[0].id + "/" + atom.args[0].args[1].value] = true;
				}
			},
			
			// set_prolog_flag
			"set_prolog_flag/2": function( thread, atom ) {
				var flag = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( flag ) || pl.type.is_variable( value ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( flag ) ) {
					thread.throw_error( pl.error.type( "atom", flag, atom.indicator ) );
				} else if( !pl.type.is_flag( flag ) ) {
					thread.throw_error( pl.error.domain( "prolog_flag", flag, atom.indicator ) );
				} else if( !pl.type.is_value_flag( flag, value ) ) {
					thread.throw_error( pl.error.domain( "flag_value", new Term( "+", [flag, value] ), atom.indicator ) );
				} else if( !pl.type.is_modifiable_flag( flag ) ) {
					thread.throw_error( pl.error.permission( "modify", "flag", flag ) );
				} else {
					thread.session.flag[flag.id] = value;
				}
			},
			
			// use_module/1
			"use_module/1": function( thread, atom ) {
				var module = atom.args[0];
				if( pl.type.is_variable( module ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_term( module ) ) {
					thread.throw_error( pl.error.type( "term", module, atom.indicator ) );
				} else {
					if( pl.type.is_module( module ) ) {
						var name = module.args[0].id;
						if( indexOf(thread.session.modules, name) === -1 )
							thread.session.modules.push( name );
					} else {
						// TODO
						// error no existe modulo
					}
				}
			},
			
			// char_conversion/2
			"char_conversion/2": function( thread, atom ) {
				var inchar = atom.args[0], outchar = atom.args[1];
				if( pl.type.is_variable( inchar ) || pl.type.is_variable( outchar ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_character( inchar ) ) {
					thread.throw_error( pl.error.type( "character", inchar, atom.indicator ) );
				} else if( !pl.type.is_character( outchar ) ) {
					thread.throw_error( pl.error.type( "character", outchar, atom.indicator ) );
				} else {
					if( inchar.id === outchar.id ) {
						delete thread.session.__char_conversion[inchar.id];
					} else {
						thread.session.__char_conversion[inchar.id] = outchar.id;
					}
				}
			},
			
			// op/3
			"op/3": function( thread, atom ) {
				var priority = atom.args[0], type = atom.args[1], operator = atom.args[2];
				if( pl.type.is_variable( priority ) || pl.type.is_variable( type ) || pl.type.is_variable( operator ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( priority ) ) {
					thread.throw_error( pl.error.type( "integer", priority, atom.indicator ) );
				} else if( !pl.type.is_atom( type ) ) {
					thread.throw_error( pl.error.type( "atom", type, atom.indicator ) );
				} else if( !pl.type.is_atom( operator ) ) {
					thread.throw_error( pl.error.type( "atom", operator, atom.indicator ) );
				} else if( priority.value < 0 || priority.value > 1200 ) {
					thread.throw_error( pl.error.domain( "operator_priority", priority, atom.indicator ) );
				} else if( operator.id === "," ) {
					thread.throw_error( pl.error.permission( "modify", "operator", operator, atom.indicator ) );
				} else if( operator.id === "|" && (priority.value < 1001 || type.id.length !== 3 ) ) {
					thread.throw_error( pl.error.permission( "modify", "operator", operator, atom.indicator ) );
				} else if( ["fy", "fx", "yf", "xf", "xfx", "yfx", "xfy"].indexOf( type.id ) === -1 ) {
					thread.throw_error( pl.error.domain( "operator_specifier", type, atom.indicator ) );
				} else {
					var fix = { prefix: null, infix: null, postfix: null };
					for( var p in thread.session.__operators ) {
						if(!thread.session.__operators.hasOwnProperty(p)) continue;
						var classes = thread.session.__operators[p][operator.id];
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
						thread.throw_error( pl.error.permission( "create", "operator", operator, atom.indicator ) );
					} else {
						if( fix[current_class] ) {
							remove( thread.session.__operators[fix[current_class].priority][operator.id], type.id );
							if( thread.session.__operators[fix[current_class].priority][operator.id].length === 0 ) {
								delete thread.session.__operators[fix[current_class].priority][operator.id];
							}
						}
						if( priority.value > 0 ) {
							if( !thread.session.__operators[priority.value] ) thread.session.__operators[priority.value.toString()] = {};
							if( !thread.session.__operators[priority.value][operator.id] ) thread.session.__operators[priority.value][operator.id] = [];
							thread.session.__operators[priority.value][operator.id].push( type.id );
						}
						return true;
					}
				}
			}
			
		},
		
		// Built-in predicates
		predicate: {

			// TERM AND GOAL EXPANSION

			"goal_expansion/2": [
				new Rule(new Term("goal_expansion", [new Term(",", [new Var("X"),new Var("Y")]),new Term(",", [new Var("X_"),new Var("Y_")])]), new Term(";", [new Term(",", [new Term("goal_expansion", [new Var("X"),new Var("X_")]),new Term(";", [new Term("goal_expansion", [new Var("Y"),new Var("Y_")]),new Term("=", [new Var("Y_"),new Var("Y")])])]),new Term(",", [new Term("=", [new Var("X"),new Var("X_")]),new Term("goal_expansion", [new Var("Y"),new Var("Y_")])])])),
				new Rule(new Term("goal_expansion", [new Term(";", [new Var("X"),new Var("Y")]),new Term(";", [new Var("X_"),new Var("Y_")])]), new Term(";", [new Term(",", [new Term("goal_expansion", [new Var("X"),new Var("X_")]),new Term(";", [new Term("goal_expansion", [new Var("Y"),new Var("Y_")]),new Term("=", [new Var("Y_"),new Var("Y")])])]),new Term(",", [new Term("=", [new Var("X"),new Var("X_")]),new Term("goal_expansion", [new Var("Y"),new Var("Y_")])])])),
				new Rule(new Term("goal_expansion", [new Term("->", [new Var("X"),new Var("Y")]),new Term("->", [new Var("X_"),new Var("Y_")])]), new Term(";", [new Term(",", [new Term("goal_expansion", [new Var("X"),new Var("X_")]),new Term(";", [new Term("goal_expansion", [new Var("Y"),new Var("Y_")]),new Term("=", [new Var("Y_"),new Var("Y")])])]),new Term(",", [new Term("=", [new Var("X"),new Var("X_")]),new Term("goal_expansion", [new Var("Y"),new Var("Y_")])])])),
				new Rule(new Term("goal_expansion", [new Term("catch", [new Var("X"),new Var("Y"),new Var("Z")]),new Term("catch", [new Var("X_"),new Var("Y"),new Var("Z_")])]), new Term(";", [new Term(",", [new Term("goal_expansion", [new Var("X"),new Var("X_")]),new Term(";", [new Term("goal_expansion", [new Var("Z"),new Var("Z_")]),new Term("=", [new Var("Z_"),new Var("Z")])])]),new Term(",", [new Term("=", [new Var("X_"),new Var("X")]),new Term("goal_expansion", [new Var("Z"),new Var("Z_")])])])),
				new Rule(new Term("goal_expansion", [new Term("\\+", [new Var("X")]),new Term("\\+", [new Var("X_")])]), new Term(",", [new Term("nonvar", [new Var("X")]),new Term("goal_expansion", [new Var("X"),new Var("X_")])])),
				new Rule(new Term("goal_expansion", [new Term("once", [new Var("X")]),new Term("once", [new Var("X_")])]), new Term(",", [new Term("nonvar", [new Var("X")]),new Term("goal_expansion", [new Var("X"),new Var("X_")])])),
				new Rule(new Term("goal_expansion", [new Term("findall", [new Var("X"),new Var("Y"),new Var("Z")]),new Term("findall", [new Var("X"),new Var("Y_"),new Var("Z")])]), new Term("goal_expansion", [new Var("Y"),new Var("Y_")])),
				new Rule(new Term("goal_expansion", [new Term("setof", [new Var("X"),new Var("Y"),new Var("Z")]),new Term("findall", [new Var("X"),new Var("Y_"),new Var("Z")])]), new Term("goal_expansion", [new Var("Y"),new Var("Y_")])),
				new Rule(new Term("goal_expansion", [new Term("bagof", [new Var("X"),new Var("Y"),new Var("Z")]),new Term("findall", [new Var("X"),new Var("Y_"),new Var("Z")])]), new Term("goal_expansion", [new Var("Y"),new Var("Y_")])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X")]),new Term("call", [new Var("X_")])]), new Term(",", [new Term("nonvar", [new Var("X")]),new Term("goal_expansion", [new Var("X"),new Var("X_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term("[]", [])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1"),new Var("A2")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term(".", [new Var("A2"),new Term("[]", [])])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1"),new Var("A2"),new Var("A3")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term(".", [new Var("A2"),new Term(".", [new Var("A3"),new Term("[]", [])])])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1"),new Var("A2"),new Var("A3"),new Var("A4")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term(".", [new Var("A2"),new Term(".", [new Var("A3"),new Term(".", [new Var("A4"),new Term("[]", [])])])])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1"),new Var("A2"),new Var("A3"),new Var("A4"),new Var("A5")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term(".", [new Var("A2"),new Term(".", [new Var("A3"),new Term(".", [new Var("A4"),new Term(".", [new Var("A5"),new Term("[]", [])])])])])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1"),new Var("A2"),new Var("A3"),new Var("A4"),new Var("A5"),new Var("A6")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term(".", [new Var("A2"),new Term(".", [new Var("A3"),new Term(".", [new Var("A4"),new Term(".", [new Var("A5"),new Term(".", [new Var("A6"),new Term("[]", [])])])])])])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])])),
				new Rule(new Term("goal_expansion", [new Term("call", [new Var("X"),new Var("A1"),new Var("A2"),new Var("A3"),new Var("A4"),new Var("A5"),new Var("A6"),new Var("A7")]),new Term("call", [new Var("F_")])]), new Term(",", [new Term("=..", [new Var("F"),new Term(".", [new Var("X"),new Term(".", [new Var("A1"),new Term(".", [new Var("A2"),new Term(".", [new Var("A3"),new Term(".", [new Var("A4"),new Term(".", [new Var("A5"),new Term(".", [new Var("A6"),new Term(".", [new Var("A7"),new Term("[]", [])])])])])])])])])]),new Term("goal_expansion", [new Var("F"),new Var("F_")])]))
			],



			// ATTRIBUTED VARIABLES
			
			//put_attr/3
			"put_attr/3": function( thread, point, atom ) {
				var variable = atom.args[0], module = atom.args[1], value = atom.args[2];
				if( !pl.type.is_variable(variable) ) {
					thread.throw_error( pl.error.type( "variable", variable, atom.indicator ) );
				} else if( !pl.type.is_atom(module) ) {
					thread.throw_error( pl.error.type( "atom", module, atom.indicator ) );
				} else {
					var subs = point.substitution.set_attribute( variable.id, module, value );
					thread.prepend( [new State( point.goal.replace(null), subs, point )] );
				}
			},

			// get_attr/3
			"get_attr/3": function( thread, point, atom ) {
				var variable = atom.args[0], module = atom.args[1], value = atom.args[2];
				if( !pl.type.is_variable(variable) ) {
					thread.throw_error( pl.error.type( "variable", variable, atom.indicator ) );
				} else if( !pl.type.is_atom(module) ) {
					thread.throw_error( pl.error.type( "atom", module, atom.indicator ) );
				} else {
					var attr = point.substitution.get_attribute( variable.id, module );
					if( attr ) {
						thread.prepend( [new State(
							point.goal.replace( new Term("=", [value, attr]) ),
							point.substitution,
							point
						)] );
					}
				}
			},


			
			// INPUT AND OUTPUT
			
			// op/3
			"op/3": function( thread, point, atom ) {
				if( pl.directive["op/3"]( thread, atom ) )
					thread.success( point );
			},
			
			// current_op/3
			"current_op/3": function( thread, point, atom ) {
				var priority = atom.args[0], specifier = atom.args[1], operator = atom.args[2];
				var points = [];
				for( var p in thread.session.__operators )
					for( var o in thread.session.__operators[p] )
						for( var i = 0; i < thread.session.__operators[p][o].length; i++ )
							points.push( new State(
								point.goal.replace(
									new Term( ",", [
										new Term( "=", [new Num( p, false ), priority] ),
										new Term( ",", [
											new Term( "=", [new Term( thread.session.__operators[p][o][i], [] ), specifier] ),
											new Term( "=", [new Term( o, [] ), operator] )
										] )
									] )
								),
								point.substitution,
								point
							) );
				thread.prepend( points );
			},
		


			// LOGIC AND CONTROL STRUCTURES
		
			// ;/2 (disjunction)
			";/2": function( thread, point, atom ) {
				var left = atom.args[0], right = atom.args[1];
				if( pl.type.is_term( left ) && left.indicator === "->/2" ) {
					var cond = left.args[0], then = left.args[1], otherwise = right;
					var goal_fst = point.goal.replace( new Term( ",", [cond, new Term( ",", [new Term( "!" ), then] )] ) );
					var goal_snd = point.goal.replace( new Term( ",", [new Term( "!" ), otherwise] ) );
					thread.prepend( [
						new State( goal_fst, point.substitution, point ),
						new State( goal_snd, point.substitution, point )
					] );
				} else {
					thread.prepend([
						new State( point.goal.replace( left ), point.substitution, point ),
						new State( point.goal.replace( right ), point.substitution, point )
					]);
				}
			},
			
			// !/0 (cut)
			"!/0": function( thread, point, atom ) {
				var parent_cut, last_cut, states = [];
				parent_cut = point;
				last_cut = null;
				while( parent_cut.parent !== null && parent_cut.parent.goal.search( atom ) ) {
					last_cut = parent_cut;
					parent_cut = parent_cut.parent;
					if(parent_cut.goal !== null) {
						var selected = parent_cut.goal.select();
						if( selected && selected.id === "call" && selected.search(atom) ) {
							parent_cut = last_cut;
							break;
						}
					}
				}
				for( var i = thread.points.length-1; i >= 0; i-- ) {
					var state = thread.points[i];
					var node = state.parent;
					while( node !== null && node !== parent_cut.parent ) {
						node = node.parent;
					}
					if( node === null && node !== parent_cut.parent )
						states.push( state );
				}
				thread.points = states.reverse();
				thread.success( point );
			},
			
			// \+ (negation)
			"\\+/1": function( thread, point, atom ) {
				var goal = atom.args[0];
				if( pl.type.is_variable( goal ) ) {
					thread.throw_error( pl.error.instantiation( thread.level ) );
				} else if( !pl.type.is_callable( goal ) ) {
					thread.throw_error( pl.error.type( "callable", goal, thread.level ) );
				} else {
					// TRANSPARENT VERSION OF THE NEGATION
					/*var neg_thread;
					if(point.negation_thread) {
						neg_thread = point.negation_thread;
					} else {
						neg_thread = new Thread( thread.session );
						neg_thread.add_goal( goal );
						point.negation_thread = neg_thread;
					}
					neg_thread.answer( function( answer ) {
						if(answer === false) {
							thread.success( point );
						} else if(pl.type.is_error( answer )) {
							thread.throw_error( answer.args[0] );
						} else if(answer === null) {
							thread.prepend( [point] );
							thread.current_limit = 0;
						}
						thread.again( answer !== null );
					} );
					return true;*/
					
					// '\+'(X) :- call(X), !, fail.
					// '\+'(_).
					thread.prepend( [
						new State( point.goal.replace( new Term( ",", [new Term( ",", [ new Term( "call", [goal] ), new Term( "!", [] ) ] ), new Term( "fail", [] ) ] ) ), point.substitution, point ),
						new State( point.goal.replace( null ), point.substitution, point )
					] );
				}
			},
			
			// ->/2 (implication)
			"->/2": function( thread, point, atom ) {
				var cond = atom.args[0], then = atom.args[1];
				var goal = point.goal.replace( new Term( ",", [cond, new Term( ",", [new Term( "!" ), then] )] ) );
				thread.prepend( [new State( goal, point.substitution, point )] );
			},
			
			// fail/0
			"fail/0": function( _1, _2, _3 ) {},
			
			// false/0
			"false/0": function( _1, _2, _3 ) {},
			
			// true/0
			"true/0": function( thread, point, _ ) {
				thread.success( point );
			},
			
			// call/1..8
			"call/1": callN(1),
			"call/2": callN(2),
			"call/3": callN(3),
			"call/4": callN(4),
			"call/5": callN(5),
			"call/6": callN(6),
			"call/7": callN(7),
			"call/8": callN(8),
			
			// once/1
			"once/1": function( thread, point, atom ) {
				var goal = atom.args[0];
				thread.prepend( [new State( point.goal.replace( new Term( ",", [new Term( "call", [goal] ), new Term( "!", [] )] ) ), point.substitution, point )] );
			},
			
			// forall/2
			"forall/2": function( thread, point, atom ) {
				var generate = atom.args[0], test = atom.args[1];
				thread.prepend( [new State( point.goal.replace( new Term( "\\+", [new Term( ",", [new Term( "call", [generate] ), new Term( "\\+", [new Term( "call", [test] )] )] )] ) ), point.substitution, point )] );
			},
			
			// repeat/0
			"repeat/0": function( thread, point, _ ) {
				thread.prepend( [new State( point.goal.replace( null ), point.substitution, point ), point] );
			},
			
			// EXCEPTIONS
			
			// throw/1
			"throw/1": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.throw_error( pl.error.instantiation( thread.level ) );
				} else {
					thread.throw_error( atom.args[0] );
				}
			},
			
			// catch/3
			"catch/3": function( thread, point, atom ) {
				var points = thread.points;
				thread.points = [];
				thread.prepend( [new State( atom.args[0], point.substitution, point )] );
				var format_success = thread.session.format_success;
				var format_error = thread.session.format_error;
				thread.session.format_success = function(x) { return x.substitution; };
				thread.session.format_error = function(x) { return x.goal; };
				var callback = function( answer ) {
					var call_points = thread.points;
					thread.points = points;
					thread.session.format_success = format_success;
					thread.session.format_error = format_error;
					if( pl.type.is_error( answer ) ) {
						var states = [];
						for( var i = thread.points.length-1 ; i >= 0; i-- ) {
							var state = thread.points[i];
							var node = state.parent;
							while( node !== null && node !== point.parent ) {
								node = node.parent;
							}
							if( node === null && node !== point.parent )
								states.push( state );
						}
						thread.points = states;
						var occurs_check = thread.get_flag( "occurs_check" ).indicator === "true/0";
						var state = new State();
						var mgu = pl.unify( answer.args[0], atom.args[1], occurs_check );
						if( mgu !== null ) {
							state.substitution = point.substitution.apply( mgu );
							state.goal = point.goal.replace( atom.args[2] ).apply( mgu );
							state.parent = point;
							thread.prepend( [state] );
						} else {
							thread.throw_error( answer.args[0] );
						}
					} else if( answer !== false ) {
						var answer_state = answer === null ? [] : [new State(
							point.goal.apply( answer ).replace( null ),
							point.substitution.apply( answer ),
							point
						)];
						var filter_points = [];
						for( var i = call_points.length-1; i >= 0; i-- ) {
							filter_points.push( call_points[i] );
							var selected = call_points[i].goal !== null ? call_points[i].goal.select() : null;
							if( pl.type.is_term( selected ) && selected.indicator === "!/0" )
								break;
						}
						var catch_points = map( filter_points, function( state ) {
							if( state.goal === null )
								state.goal = new Term( "true", [] );
							state = new State(
								point.goal.replace( new Term( "catch", [state.goal, atom.args[1], atom.args[2]] ) ),
								point.substitution.apply( state.substitution ),
								state.parent
							);
							state.exclude = atom.args[0].variables();
							return state;
						} ).reverse();
						thread.prepend( catch_points );
						thread.prepend( answer_state );
						if( answer === null ) {
							this.current_limit = 0;
							thread.__calls.shift()( null );
						}
					}
				};
				thread.__calls.unshift( callback );
			},
			
			// UNIFICATION
			
			// =/2 (unification)
			"=/2": function( thread, point, atom ) {
				var occurs_check = thread.get_flag( "occurs_check" ).indicator === "true/0";
				var state = new State();
				var mgu = pl.unify( atom.args[0], atom.args[1], occurs_check );
				if( mgu !== null ) {
					state.goal = point.goal.apply( mgu ).replace( null );
					state.substitution = point.substitution.apply( mgu );
					state.parent = point;
					thread.prepend( [state] );
				}
			},
			
			// unify_with_occurs_check/2
			"unify_with_occurs_check/2": function( thread, point, atom ) {
				var state = new State();
				var mgu = pl.unify( atom.args[0], atom.args[1], true );
				if( mgu !== null ) {
					state.goal = point.goal.apply( mgu ).replace( null );
					state.substitution = point.substitution.apply( mgu );
					state.parent = point;
					thread.prepend( [state] );
				}
			},
			
			// \=/2
			"\\=/2": function( thread, point, atom ) {
				var occurs_check = thread.get_flag( "occurs_check" ).indicator === "true/0";
				var mgu = pl.unify( atom.args[0], atom.args[1], occurs_check );
				if( mgu === null ) {
					thread.success( point );
				}
			},
			
			// subsumes_term/2
			"subsumes_term/2": function( thread, point, atom ) {
				var occurs_check = thread.get_flag( "occurs_check" ).indicator === "true/0";
				var mgu = pl.unify( atom.args[1], atom.args[0], occurs_check );
				if( mgu !== null && atom.args[1].apply( mgu ).equals( atom.args[1] ) ) {
					thread.success( point );
				}
			},
			
			// ALL SOLUTIONS
			
			// findall/3
			"findall/3": function( thread, point, atom ) {
				var template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
				if( pl.type.is_variable( goal ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( goal ) ) {
					thread.throw_error( pl.error.type( "callable", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( instances ) && !pl.type.is_list( instances ) ) {
					thread.throw_error( pl.error.type( "list", instances, atom.indicator ) );
				} else {
					var variable = thread.next_free_variable();
					var newGoal = new Term( ",", [goal, new Term( "=", [variable, template] )] );
					var points = thread.points;
					var limit = thread.session.limit;
					var format_success = thread.session.format_success;
					thread.session.format_success = function(x) { return x.substitution; };
					thread.add_goal( newGoal, true, point );
					var answers = [];
					var callback = function( answer ) {
						if( answer !== false && answer !== null && !pl.type.is_error( answer ) ) {
							thread.__calls.unshift( callback );
							answers.push( answer.links[variable.id] );
							thread.session.limit = thread.current_limit;
						} else {
							thread.points = points;
							thread.session.limit = limit;
							thread.session.format_success = format_success;
							if( pl.type.is_error( answer ) ) {
								thread.throw_error( answer.args[0] );
							} else if( thread.current_limit > 0 ) {
								var list = new Term( "[]" );
								for( var i = answers.length - 1; i >= 0; i-- ) {
									list = new Term( ".", [answers[i], list] );
								}
								thread.prepend( [new State( point.goal.replace( new Term( "=", [instances, list] ) ), point.substitution, point )] );
							}
						}
					};
					thread.__calls.unshift( callback );
				}
			},
			
			// bagof/3
			"bagof/3": function( thread, point, atom ) {
				var answer, template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
				if( pl.type.is_variable( goal ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( goal ) ) {
					thread.throw_error( pl.error.type( "callable", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( instances ) && !pl.type.is_list( instances ) ) {
					thread.throw_error( pl.error.type( "list", instances, atom.indicator ) );
				} else {
					var variable = thread.next_free_variable();
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
					var points = thread.points;
					var limit = thread.session.limit;
					var format_success = thread.session.format_success;
					thread.session.format_success = function(x) { return x.substitution; };
					thread.add_goal( newGoal, true, point );
					var answers = [];
					var callback = function( answer ) {
						if( answer !== false && answer !== null && !pl.type.is_error( answer ) ) {
							thread.__calls.unshift( callback );
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
							thread.session.limit = thread.current_limit;
						} else {
							thread.points = points;
							thread.session.limit = limit;
							thread.session.format_success = format_success;
							if( pl.type.is_error( answer ) ) {
								thread.throw_error( answer.args[0] );
							} else if( thread.current_limit > 0 ) {
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
								thread.prepend( states );
							}
						}
					};
					thread.__calls.unshift( callback );
				}
			},
	
			// setof/3
			"setof/3": function( thread, point, atom ) {
				var answer, template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
				if( pl.type.is_variable( goal ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( goal ) ) {
					thread.throw_error( pl.error.type( "callable", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( instances ) && !pl.type.is_list( instances ) ) {
					thread.throw_error( pl.error.type( "list", instances, atom.indicator ) );
				} else {
					var variable = thread.next_free_variable();
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
					var points = thread.points;
					var limit = thread.session.limit;
					var format_success = thread.session.format_success;
					thread.session.format_success = function(x) { return x.substitution; };
					thread.add_goal( newGoal, true, point );
					var answers = [];
					var callback = function( answer ) {
						if( answer !== false && answer !== null && !pl.type.is_error( answer ) ) {
							thread.__calls.unshift( callback );
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
							thread.session.limit = thread.current_limit;
						} else {
							thread.points = points;
							thread.session.limit = limit;
							thread.session.format_success = format_success;
							if( pl.type.is_error( answer ) ) {
								thread.throw_error( answer.args[0] );
							} else if( thread.current_limit > 0 ) {
								var states = [];
								for( var i = 0; i < answers.length; i++ ) {
									answer = answers[i].answers.sort( pl.compare );
									for( var i = answer.length-1; i > 0; i-- ) {
										if( answer[i].equals(answer[i-1]) )
										answer.splice(i,1);
									}
									var list = new Term( "[]" );
									for( var j = answer.length - 1; j >= 0; j-- ) {
										list = new Term( ".", [answer[j], list] );
									}
									states.push( new State(
										point.goal.replace( new Term( ",", [new Term( "=", [list_vars, answers[i].variables] ), new Term( "=", [instances, list] )] ) ),
										point.substitution, point
									) );
								}
								thread.prepend( states );
							}
						}
					};
					thread.__calls.unshift( callback );
				}
			},
			
			// TERM CREATION AND DECOMPOSITION
			
			// functor/3
			"functor/3": function( thread, point, atom ) {
				var subs;
				var term = atom.args[0], name = atom.args[1], arity = atom.args[2];
				if( pl.type.is_variable( term ) && (pl.type.is_variable( name ) || pl.type.is_variable( arity )) ) {
					thread.throw_error( pl.error.instantiation( "functor/3" ) );
				} else if( !pl.type.is_variable( arity ) && !pl.type.is_integer( arity ) ) {
					thread.throw_error( pl.error.type( "integer", atom.args[2], "functor/3" ) );
				} else if( !pl.type.is_variable( name ) && !pl.type.is_atomic( name ) ) {
					thread.throw_error( pl.error.type( "atomic", atom.args[1], "functor/3" ) );
				} else if( pl.type.is_integer( name ) && pl.type.is_integer( arity ) && arity.value !== 0 ) {
					thread.throw_error( pl.error.type( "atom", atom.args[1], "functor/3" ) );
				} else if( pl.type.is_variable( term ) ) {
					if( atom.args[2].value >= 0 ) {
						var args = [];
						for( var i = 0; i < arity.value; i++ )
							args.push( thread.next_free_variable() );
						var functor = pl.type.is_integer( name ) ? name : new Term( name.id, args );
						thread.prepend( [new State( point.goal.replace( new Term( "=", [term, functor] ) ), point.substitution, point )] );
					}
				} else {
					var id = pl.type.is_integer( term ) ? term : new Term( term.id, [] );
					var length = pl.type.is_integer( term ) ? new Num( 0, false ) : new Num( term.args.length, false );
					var goal = new Term( ",", [new Term( "=", [id, name] ), new Term( "=", [length, arity] )] );
					thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
				}
			},
			
			// arg/3
			"arg/3": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) || pl.type.is_variable( atom.args[1] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( atom.args[0].value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_compound( atom.args[1] ) ) {
					thread.throw_error( pl.error.type( "compound", atom.args[1], atom.indicator ) );
				} else {
					var n = atom.args[0].value;
					if( n > 0 && n <= atom.args[1].args.length ) {
						var goal = new Term( "=", [atom.args[1].args[n-1], atom.args[2]] );
						thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
					}
				}
			},
			
			// =../2 (univ)
			"=../2": function( thread, point, atom ) {
				var list;
				if( pl.type.is_variable( atom.args[0] ) && (pl.type.is_variable( atom.args[1] )
				|| pl.type.is_non_empty_list( atom.args[1] ) && pl.type.is_variable( atom.args[1].args[0] )) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_fully_list( atom.args[1] ) ) {
					thread.throw_error( pl.error.type( "list", atom.args[1], atom.indicator ) );
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
					thread.prepend( [new State( point.goal.replace( new Term( "=", [list, atom.args[1]] ) ), point.substitution, point )] );
				} else if( !pl.type.is_variable( atom.args[1] ) ) {
					var args = [];
					list = atom.args[1].args[1];
					while( list.indicator === "./2" ) {
						args.push( list.args[0] );
						list = list.args[1];
					}
					if( pl.type.is_variable( atom.args[0] ) && pl.type.is_variable( list ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
					} else if( args.length === 0 && pl.type.is_compound( atom.args[1].args[0] ) ) {
						thread.throw_error( pl.error.type( "atomic", atom.args[1].args[0], atom.indicator ) );
					} else if( args.length > 0 && (pl.type.is_compound( atom.args[1].args[0] ) || pl.type.is_number( atom.args[1].args[0] )) ) {
						thread.throw_error( pl.error.type( "atom", atom.args[1].args[0], atom.indicator ) );
					} else {
						if( args.length === 0 ) {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [atom.args[1].args[0], atom.args[0]], point ) ), point.substitution, point )] );
						} else {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [new Term( atom.args[1].args[0].id, args ), atom.args[0]] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// copy_term/2
			"copy_term/2": function( thread, point, atom ) {
				var renamed = atom.args[0].rename( thread );
				thread.prepend( [new State( point.goal.replace( new Term( "=", [renamed, atom.args[1]] ) ), point.substitution, point.parent )] );
			},
			
			// term_variables/2
			"term_variables/2": function( thread, point, atom ) {
				var term = atom.args[0], vars = atom.args[1];
				if( !pl.type.is_fully_list( vars ) ) {
					thread.throw_error( pl.error.type( "list", vars, atom.indicator ) );
				} else {
					var list = arrayToList( map( nub( term.variables() ), function(v) {
						return new Var(v);
					} ) );
					thread.prepend( [new State( point.goal.replace( new Term( "=", [vars, list] ) ), point.substitution, point )] );
				}
			},
			
			// CLAUSE RETRIEVAL AND INFORMATION
			
			// clause/2
			"clause/2": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					thread.throw_error( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_variable( atom.args[1] ) && !pl.type.is_callable( atom.args[1] ) ) {
					thread.throw_error( pl.error.type( "callable", atom.args[1], atom.indicator ) );
				} else if( thread.session.rules[atom.args[0].indicator] !== undefined ) {
					if( thread.is_public_predicate( atom.args[0].indicator ) ) {
						var states = [];
						for( var _rule in thread.session.rules[atom.args[0].indicator] ) {
							if(!thread.session.rules[atom.args[0].indicator].hasOwnProperty(_rule)) continue;
							var rule = thread.session.rules[atom.args[0].indicator][_rule];
							thread.session.renamed_variables = {};
							rule = rule.rename( thread );
							if( rule.body === null ) {
								rule.body = new Term( "true" );
							}
							var goal = new Term( ",", [new Term( "=", [rule.head, atom.args[0]] ), new Term( "=", [rule.body, atom.args[1]] )] );
							states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
						}
						thread.prepend( states );
					} else {
						thread.throw_error( pl.error.permission( "access", "private_procedure", atom.args[0].indicator, atom.indicator ) );
					}
				}
			},
			
			// current_predicate/1
			"current_predicate/1": function( thread, point, atom ) {
				var indicator = atom.args[0];
				if( !pl.type.is_variable( indicator ) && (!pl.type.is_compound( indicator ) || indicator.indicator !== "//2") ) {
					thread.throw_error( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				} else if( !pl.type.is_variable( indicator ) && !pl.type.is_variable( indicator.args[0] ) && !pl.type.is_atom( indicator.args[0] ) ) {
					thread.throw_error( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
				} else if( !pl.type.is_variable( indicator ) && !pl.type.is_variable( indicator.args[1] ) && !pl.type.is_integer( indicator.args[1] ) ) {
					thread.throw_error( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
				} else {
					var states = [];
					for( var i in thread.session.rules ) {
						if(!thread.session.rules.hasOwnProperty(i)) continue;
						var index = i.lastIndexOf( "/" );
						var name = i.substr( 0, index );
						var arity = parseInt( i.substr( index+1, i.length-(index+1) ) );
						var predicate = new Term( "/", [new Term( name ), new Num( arity, false )] );
						var goal = new Term( "=", [predicate, indicator] );
						states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
					}
					thread.prepend( states );
				}
			},
			
			// CLAUSE CREATION AND DESTRUCTION
			
			// asserta/1
			"asserta/1": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					thread.throw_error( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else {
					var head, body;
					if( atom.args[0].indicator === ":-/2" ) {
						head = atom.args[0].args[0];
						body = body_conversion( atom.args[0].args[1] );
					} else {
						head = atom.args[0];
						body = null;
					}
					if( !pl.type.is_callable( head ) ) {
						thread.throw_error( pl.error.type( "callable", head, atom.indicator ) );
					} else if( body !== null && !pl.type.is_callable( body ) ) {
						thread.throw_error( pl.error.type( "callable", body, atom.indicator ) );
					} else if( thread.is_public_predicate( head.indicator ) ) {
						if( thread.session.rules[head.indicator] === undefined ) {
							thread.session.rules[head.indicator] = [];
						}
						thread.session.public_predicates[head.indicator] = true;
						thread.session.rules[head.indicator] = [new Rule( head, body, true )].concat( thread.session.rules[head.indicator] );
						thread.success( point );
					} else {
						thread.throw_error( pl.error.permission( "modify", "static_procedure", head.indicator, atom.indicator ) );
					}
				}
			},
			
			// assertz/1
			"assertz/1": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					thread.throw_error( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else {
					var head, body;
					if( atom.args[0].indicator === ":-/2" ) {
						head = atom.args[0].args[0];
						body = body_conversion( atom.args[0].args[1] );
					} else {
						head = atom.args[0];
						body = null;
					}
					if( !pl.type.is_callable( head ) ) {
						thread.throw_error( pl.error.type( "callable", head, atom.indicator ) );
					} else if( body !== null && !pl.type.is_callable( body ) ) {
						thread.throw_error( pl.error.type( "callable", body, atom.indicator ) );
					} else if( thread.is_public_predicate( head.indicator ) ) {
						if( thread.session.rules[head.indicator] === undefined ) {
							thread.session.rules[head.indicator] = [];
						}
						thread.session.public_predicates[head.indicator] = true;
						thread.session.rules[head.indicator].push( new Rule( head, body, true ) );
						thread.success( point );
					} else {
						thread.throw_error( pl.error.permission( "modify", "static_procedure", head.indicator, atom.indicator ) );
					}
				}
			},
			
			// retract/1
			"retract/1": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( atom.args[0] ) ) {
					thread.throw_error( pl.error.type( "callable", atom.args[0], atom.indicator ) );
				} else {
					var head, body;
					if( atom.args[0].indicator === ":-/2" ) {
						head = atom.args[0].args[0];
						body = atom.args[0].args[1];
					} else {
						head = atom.args[0];
						body = new Term( "true" );
					}
					if( typeof point.retract === "undefined" ) {
						if( thread.is_public_predicate( head.indicator ) ) {
							if( thread.session.rules[head.indicator] !== undefined ) {
								var states = [];
								for( var i = 0; i < thread.session.rules[head.indicator].length; i++ ) {
									thread.session.renamed_variables = {};
									var orule = thread.session.rules[head.indicator][i];
									var rule = orule.rename( thread );
									if( rule.body === null )
										rule.body = new Term( "true", [] );
									var occurs_check = thread.get_flag( "occurs_check" ).indicator === "true/0";
									var mgu = pl.unify( new Term( ",", [head, body] ), new Term( ",", [rule.head, rule.body] ), occurs_check );
									if( mgu !== null ) {
										var state = new State( point.goal.replace( new Term(",", [
											new Term( "retract", [ new Term( ":-", [head, body] ) ] ),
											new Term( ",", [
												new Term( "=", [head, rule.head] ),
												new Term( "=", [body, rule.body] )
											] )
										] ) ), point.substitution, point );
										state.retract = orule;
										states.push( state );
									}
								}
								thread.prepend( states );
							}
						} else {
							thread.throw_error( pl.error.permission( "modify", "static_procedure", head.indicator, atom.indicator ) );
						}
					} else {
						retract( thread, point, head.indicator, point.retract );
					}
				}
			},
			
			// retractall/1
			"retractall/1": function( thread, point, atom ) {
				var head = atom.args[0];
				if( pl.type.is_variable( head ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( head ) ) {
					thread.throw_error( pl.error.type( "callable", head, atom.indicator ) );
				} else {
				thread.prepend( [
						new State( point.goal.replace( new Term( ",", [
							new Term( "retract", [new pl.type.Term( ":-", [head, new Var( "_" )] )] ),
							new Term( "fail", [] )
						] ) ), point.substitution, point ),
						new State( point.goal.replace( null ), point.substitution, point )
					] );
				}
			},

			// abolish/1
			"abolish/1": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) || pl.type.is_term( atom.args[0] ) && atom.args[0].indicator === "//2"
				&& (pl.type.is_variable( atom.args[0].args[0] ) || pl.type.is_variable( atom.args[0].args[1] )) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_term( atom.args[0] ) || atom.args[0].indicator !== "//2" ) {
					thread.throw_error( pl.error.type( "predicate_indicator", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_atom( atom.args[0].args[0] ) ) {
					thread.throw_error( pl.error.type( "atom", atom.args[0].args[0], atom.indicator ) );
				} else if( !pl.type.is_integer( atom.args[0].args[1] ) ) {
					thread.throw_error( pl.error.type( "integer", atom.args[0].args[1], atom.indicator ) );
				} else if( atom.args[0].args[1].value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", atom.args[0].args[1], atom.indicator ) );
				} else if( pl.type.is_number(thread.get_flag( "max_arity" )) && atom.args[0].args[1].value > thread.get_flag( "max_arity" ).value ) {
					thread.throw_error( pl.error.representation( "max_arity", atom.indicator ) );
				} else {
					var indicator = atom.args[0].args[0].id + "/" + atom.args[0].args[1].value;
					if( thread.is_public_predicate( indicator ) ) {
						delete thread.session.rules[indicator];
						thread.success( point );
					} else {
						thread.throw_error( pl.error.permission( "modify", "static_procedure", indicator, atom.indicator ) );
					}
				}
			},
			
			// ATOM PROCESSING
			
			// atom_length/2
			"atom_length/2": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( atom.args[0] ) ) {
					thread.throw_error( pl.error.type( "atom", atom.args[0], atom.indicator ) );
				} else if( !pl.type.is_variable( atom.args[1] ) && !pl.type.is_integer( atom.args[1] ) ) {
					thread.throw_error( pl.error.type( "integer", atom.args[1], atom.indicator ) );
				} else if( pl.type.is_integer( atom.args[1] ) && atom.args[1].value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", atom.args[1], atom.indicator ) );
				} else {
					var length = new Num( stringLength(atom.args[0].id), false );
					thread.prepend( [new State( point.goal.replace( new Term( "=", [length, atom.args[1]] ) ), point.substitution, point )] );
				}
			},
			
			// atom_concat/3
			"atom_concat/3": function( thread, point, atom ) {
				var str, goal, start = atom.args[0], end = atom.args[1], whole = atom.args[2];
				if( pl.type.is_variable( whole ) && (pl.type.is_variable( start ) || pl.type.is_variable( end )) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( start ) && !pl.type.is_atom( start ) ) {
					thread.throw_error( pl.error.type( "atom", start, atom.indicator ) );
				} else if( !pl.type.is_variable( end ) && !pl.type.is_atom( end ) ) {
					thread.throw_error( pl.error.type( "atom", end, atom.indicator ) );
				} else if( !pl.type.is_variable( whole ) && !pl.type.is_atom( whole ) ) {
					thread.throw_error( pl.error.type( "atom", whole, atom.indicator ) );
				} else {
					var v1 = pl.type.is_variable( start );
					var v2 = pl.type.is_variable( end );
					//var v3 = pl.type.is_variable( whole );
					if( !v1 && !v2 ) {
						goal = new Term( "=", [whole, new Term( start.id + end.id )] );
						thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
					} else if( v1 && !v2 ) {
						str = whole.id.substr( 0, whole.id.length - end.id.length );
						if( str + end.id === whole.id ) {
							goal = new Term( "=", [start, new Term( str )] );
							thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
						}
					} else if( v2 && !v1 ) {
						str = whole.id.substr( start.id.length );
						if( start.id + str === whole.id ) {
							goal = new Term( "=", [end, new Term( str )] );
							thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
						}
					} else {
						var states = [];
						for( var i = 0; i <= whole.id.length; i++ ) {
							var atom1 = new Term( whole.id.substr( 0, i ) );
							var atom2 = new Term( whole.id.substr( i ) );
							goal = new Term( ",", [new Term( "=", [atom1, start] ), new Term( "=", [atom2, end] )] );
							states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
						}
						thread.prepend( states );
					}
				}
			},
			
			// sub_atom/5
			"sub_atom/5": function( thread, point, atom ) {
				var i, atom1 = atom.args[0], before = atom.args[1], length = atom.args[2], after = atom.args[3], subatom = atom.args[4];
				if( pl.type.is_variable( atom1 ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( before ) && !pl.type.is_integer( before ) ) {
					thread.throw_error( pl.error.type( "integer", before, atom.indicator ) );
				} else if( !pl.type.is_variable( length ) && !pl.type.is_integer( length ) ) {
					thread.throw_error( pl.error.type( "integer", length, atom.indicator ) );
				} else if( !pl.type.is_variable( after ) && !pl.type.is_integer( after ) ) {
					thread.throw_error( pl.error.type( "integer", after, atom.indicator ) );
				} else if( pl.type.is_integer( before ) && before.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", before, atom.indicator ) );
				} else if( pl.type.is_integer( length ) && length.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", length, atom.indicator ) );
				} else if( pl.type.is_integer( after ) && after.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", after, atom.indicator ) );
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
					thread.prepend( states );
				}
			},
			
			// atom_chars/2
			"atom_chars/2": function( thread, point, atom ) {
				var atom1 = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( atom1 ) && pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( atom1 ) && !pl.type.is_atom( atom1 ) ) {
					thread.throw_error( pl.error.type( "atom", atom1, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( atom1 ) ) {
						var list1 = new Term( "[]" );
						var unilen = stringLength(atom1.id);
						for( var i = unilen-1; i >= 0; i-- ) {
							list1 = new Term( ".", [new Term( atom1.id.charAt( i ) ), list1] );
						}
						thread.prepend( [new State( point.goal.replace( new Term( "=", [list, list1] ) ), point.substitution, point )] );
					} else {			
						var pointer = list;
						var v = pl.type.is_variable( atom1 );
						var str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) && v ) {
									thread.throw_error( pl.error.instantiation( atom.indicator ) );
									return;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									thread.throw_error( pl.error.type( "character", pointer.args[0], atom.indicator ) );
									return;
								}
							} else {
								str += pointer.args[0].id;
							}
							pointer = pointer.args[1];
						}
						if( pl.type.is_variable( pointer ) && v ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						} else if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
							thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
						} else {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [new Term( str ), atom1] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// atom_codes/2
			"atom_codes/2": function( thread, point, atom ) {
				var atom1 = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( atom1 ) && pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( atom1 ) && !pl.type.is_atom( atom1 ) ) {
					thread.throw_error( pl.error.type( "atom", atom1, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( atom1 ) ) {
						var list1 = new Term( "[]" );
						var unilen = stringLength(atom1.id);
						for( var i = unilen-1; i >= 0; i-- ) {
							list1 = new Term( ".", [new Num( codePointAt(atom1.id,i), false ), list1] );
						}
						thread.prepend( [new State( point.goal.replace( new Term( "=", [list, list1] ) ), point.substitution, point )] );
					} else {			
						var pointer = list;
						var v = pl.type.is_variable( atom1 );
						var str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character_code( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) && v ) {
									thread.throw_error( pl.error.instantiation( atom.indicator ) );
									return;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									thread.throw_error( pl.error.representation( "character_code", atom.indicator ) );
									return;
								}
							} else {
								str += fromCodePoint( pointer.args[0].value );
							}
							pointer = pointer.args[1];
						}
						if( pl.type.is_variable( pointer ) && v ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						} else if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
							thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
						} else {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [new Term( str ), atom1] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// char_code/2
			"char_code/2": function( thread, point, atom ) {
				var char = atom.args[0], code = atom.args[1];
				if( pl.type.is_variable( char ) && pl.type.is_variable( code ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( char ) && !pl.type.is_character( char ) ) {
					thread.throw_error( pl.error.type( "character", char, atom.indicator ) );
				} else if( !pl.type.is_variable( code ) && !pl.type.is_integer( code ) ) {
					thread.throw_error( pl.error.type( "integer", code, atom.indicator ) );
				} else if( !pl.type.is_variable( code ) && !pl.type.is_character_code( code ) ) {
					thread.throw_error( pl.error.representation( "character_code", atom.indicator ) );
				} else {
					if( pl.type.is_variable( code ) ) {
						var code1 = new Num( codePointAt(char.id,0 ), false );
						thread.prepend( [new State( point.goal.replace( new Term( "=", [code1, code] ) ), point.substitution, point )] );
					} else {
						var char1 = new Term( fromCodePoint( code.value ) );
						thread.prepend( [new State( point.goal.replace( new Term( "=", [char1, char] ) ), point.substitution, point )] );
					}
				}
			},
			
			// number_chars/2
			"number_chars/2": function( thread, point, atom ) {
				var str, num = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( num ) && pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( num ) && !pl.type.is_number( num ) ) {
					thread.throw_error( pl.error.type( "number", num, atom.indicator ) );
				} else if( !pl.type.is_variable( list ) && !pl.type.is_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else {
					var isvar = pl.type.is_variable( num );
					if( !pl.type.is_variable( list ) ) {	
						var pointer = list;
						var total = true;
						str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) ) {
									total = false;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									thread.throw_error( pl.error.type( "character", pointer.args[0], atom.indicator ) );
									return;
								}
							} else {
								str += pointer.args[0].id;
							}
							pointer = pointer.args[1];
						}
						total = total && pl.type.is_empty_list( pointer );
						if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
							thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
							return;
						}
						if( !total && isvar ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( total ) {
							if( pl.type.is_variable( pointer ) && isvar ) {
								thread.throw_error( pl.error.instantiation( atom.indicator ) );
								return;
							} else {
								var expr = thread.parse( str );
								var num2 = expr.value;
								if( !pl.type.is_number( num2 ) || expr.tokens[expr.tokens.length-1].space ) {
									thread.throw_error( pl.error.syntax_by_predicate( "parseable_number", atom.indicator ) );
								} else {
									thread.prepend( [new State( point.goal.replace( new Term( "=", [num, num2] ) ), point.substitution, point )] );
								}
								return;
							}
						}
					}
					if( !isvar ) {
						str = num.toString();
						var list2 = new Term( "[]" );
						for( var i = str.length - 1; i >= 0; i-- ) {
							list2 = new Term( ".", [ new Term( str.charAt( i ) ), list2 ] );
						}
						thread.prepend( [new State( point.goal.replace( new Term( "=", [list, list2] ) ), point.substitution, point )] );
					}
				}
			},
			
			// number_codes/2
			"number_codes/2": function( thread, point, atom ) {
				var str, num = atom.args[0], list = atom.args[1];
				if( pl.type.is_variable( num ) && pl.type.is_variable( list ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( num ) && !pl.type.is_number( num ) ) {
					thread.throw_error( pl.error.type( "number", num, atom.indicator ) );
				} else if( !pl.type.is_variable( list ) && !pl.type.is_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else {
					var isvar = pl.type.is_variable( num );
					if( !pl.type.is_variable( list ) ) {	
						var pointer = list;
						var total = true;
						str = "";
						while( pointer.indicator === "./2" ) {
							if( !pl.type.is_character_code( pointer.args[0] ) ) {
								if( pl.type.is_variable( pointer.args[0] ) ) {
									total = false;
								} else if( !pl.type.is_variable( pointer.args[0] ) ) {
									thread.throw_error( pl.error.type( "character_code", pointer.args[0], atom.indicator ) );
									return;
								}
							} else {
								str += fromCodePoint( pointer.args[0].value );
							}
							pointer = pointer.args[1];
						}
						total = total && pl.type.is_empty_list( pointer );
						if( !pl.type.is_empty_list( pointer ) && !pl.type.is_variable( pointer ) ) {
							thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
							return;
						}
						if( !total && isvar ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( total ) {
							if( pl.type.is_variable( pointer ) && isvar ) {
								thread.throw_error( pl.error.instantiation( atom.indicator ) );
								return;
							} else {
								var expr = thread.parse( str );
								var num2 = expr.value;
								if( !pl.type.is_number( num2 ) || expr.tokens[expr.tokens.length-1].space ) {
									thread.throw_error( pl.error.syntax_by_predicate( "parseable_number", atom.indicator ) );
								} else {
									thread.prepend( [new State( point.goal.replace( new Term( "=", [num, num2] ) ), point.substitution, point )] );
								}
								return;
							}
						}
					}
					if( !isvar ) {
						str = num.toString();
						var list2 = new Term( "[]" );
						for( var i = str.length - 1; i >= 0; i-- ) {
							list2 = new Term( ".", [ new Num( codePointAt(str,i), false ), list2 ] );
						}
						thread.prepend( [new State( point.goal.replace( new Term( "=", [list, list2] ) ), point.substitution, point )] );
					}
				}
			},
			
			// upcase_atom/2
			"upcase_atom/2": function( thread, point, atom ) {
				var original = atom.args[0], upcase = atom.args[1];
				if( pl.type.is_variable( original ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( original ) ) {
					thread.throw_error( pl.error.type( "atom", original, atom.indicator ) );
				} else if( !pl.type.is_variable( upcase ) && !pl.type.is_atom( upcase ) ) {
					thread.throw_error( pl.error.type( "atom", upcase, atom.indicator ) );
				} else {
					thread.prepend( [new State( point.goal.replace( new Term( "=", [upcase, new Term( original.id.toUpperCase(), [] )] ) ), point.substitution, point )] );
				}
			},
			
			// downcase_atom/2
			"downcase_atom/2": function( thread, point, atom ) {
				var original = atom.args[0], downcase = atom.args[1];
				if( pl.type.is_variable( original ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( original ) ) {
					thread.throw_error( pl.error.type( "atom", original, atom.indicator ) );
				} else if( !pl.type.is_variable( downcase ) && !pl.type.is_atom( downcase ) ) {
					thread.throw_error( pl.error.type( "atom", downcase, atom.indicator ) );
				} else {
					thread.prepend( [new State( point.goal.replace( new Term( "=", [downcase, new Term( original.id.toLowerCase(), [] )] ) ), point.substitution, point )] );
				}
			},
			
			// atomic_list_concat/2
			"atomic_list_concat/2": function( thread, point, atom ) {
				var list = atom.args[0], concat = atom.args[1];
				thread.prepend( [new State( point.goal.replace( new Term( "atomic_list_concat", [list, new Term( "", [] ), concat] ) ), point.substitution, point )] );
			},
			
			// atomic_list_concat/3
			"atomic_list_concat/3": function( thread, point, atom ) {
				var list = atom.args[0], separator = atom.args[1], concat = atom.args[2];
				if( pl.type.is_variable( separator ) || pl.type.is_variable( list ) && pl.type.is_variable( concat ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( list ) && !pl.type.is_list( list ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else if( !pl.type.is_variable( concat ) && !pl.type.is_atom( concat ) ) {
					thread.throw_error( pl.error.type( "atom", concat, atom.indicator ) );
				} else {
					if( !pl.type.is_variable( concat ) ) {
						var atomic = arrayToList( map(
							concat.id.split( separator.id ),
							function( id ) {
								return new Term( id, [] );
							}
						) );
						thread.prepend( [new State( point.goal.replace( new Term( "=", [atomic, list] ) ), point.substitution, point )] );
					} else {
						var id = "";
						var pointer = list;
						while( pl.type.is_term( pointer ) && pointer.indicator === "./2" ) {
							if( !pl.type.is_atom( pointer.args[0] ) && !pl.type.is_number( pointer.args[0] ) ) {
								thread.throw_error( pl.error.type( "atomic", pointer.args[0], atom.indicator ) );
								return;
							}
							if( id !== "" )
								id += separator.id;
							if( pl.type.is_atom( pointer.args[0] ) )
								id += pointer.args[0].id;
							else
								id += "" + pointer.args[0].value;
							pointer = pointer.args[1];
						}
						id = new Term( id, [] );
						if( pl.type.is_variable( pointer ) ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						} else if( !pl.type.is_term( pointer ) || pointer.indicator !== "[]/0" ) {
							thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
						} else {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [id, concat] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// TERM COMPARISON
			
			"@=</2": function( thread, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) <= 0 ) {
					thread.success( point );
				}
			},
			
			"==/2": function( thread, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) === 0 ) {
					thread.success( point );
				}
			},
			
			"\\==/2": function( thread, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) !== 0 ) {
					thread.success( point );
				}
			},
			
			"@</2": function( thread, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) < 0 ) {
					thread.success( point );
				}
			},
			
			"@>/2": function( thread, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) > 0 ) {
					thread.success( point );
				}
			},
			
			"@>=/2": function( thread, point, atom ) {
				if( pl.compare( atom.args[0], atom.args[1] ) >= 0 ) {
					thread.success( point );
				}
			},
			
			"compare/3": function( thread, point, atom ) {
				var order = atom.args[0], left = atom.args[1], right = atom.args[2];
				if( !pl.type.is_variable( order ) && !pl.type.is_atom( order ) ) {
					thread.throw_error( pl.error.type( "atom", order, atom.indicator ) );
				} else if( pl.type.is_atom( order ) && ["<", ">", "="].indexOf( order.id ) === -1 ) {
					thread.throw_error( pl.type.domain( "order", order, atom.indicator ) );
				} else {
					var compare = pl.compare( left, right );
					compare = compare === 0 ? "=" : (compare === -1 ? "<" : ">");
					thread.prepend( [new State( point.goal.replace( new Term( "=", [order, new Term( compare, [] )] ) ), point.substitution, point )] );
				}
			},
			
			// EVALUATION
			
			// is/2
			"is/2": function( thread, point, atom ) {
				var op = atom.args[1].interpret( thread );
				if( !pl.type.is_number( op ) ) {
					thread.throw_error( op );
				} else {
					thread.prepend( [new State( point.goal.replace( new Term( "=", [atom.args[0], op], thread.level ) ), point.substitution, point )] );
				}
			},
			
			// between/3
			"between/3": function( thread, point, atom ) {
				var lower = atom.args[0], upper = atom.args[1], bet = atom.args[2];
				if( pl.type.is_variable( lower ) || pl.type.is_variable( upper ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( lower ) ) {
					thread.throw_error( pl.error.type( "integer", lower, atom.indicator ) );
				} else if( !pl.type.is_integer( upper ) ) {
					thread.throw_error( pl.error.type( "integer", upper, atom.indicator ) );
				} else if( !pl.type.is_variable( bet ) && !pl.type.is_integer( bet ) ) {
					thread.throw_error( pl.error.type( "integer", bet, atom.indicator ) );
				} else {
					if( pl.type.is_variable( bet ) ) {
						var states = [new State( point.goal.replace( new Term( "=", [bet, lower] ) ), point.substitution, point )];
						if( lower.value < upper.value )
							states.push( new State( point.goal.replace( new Term( "between", [new Num( lower.value+1, false ), upper, bet] ) ), point.substitution, point ) );
						thread.prepend( states );
					} else if( lower.value <= bet.value && upper.value >= bet.value ) {
						thread.success( point );
					}
				}
			},
			
			// succ/2
			"succ/2": function( thread, point, atom ) {
				var n = atom.args[0], m = atom.args[1];
				if( pl.type.is_variable( n ) && pl.type.is_variable( m ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( n ) && !pl.type.is_integer( n ) ) {
					thread.throw_error( pl.error.type( "integer", n, atom.indicator ) );
				} else if( !pl.type.is_variable( m ) && !pl.type.is_integer( m ) ) {
					thread.throw_error( pl.error.type( "integer", m, atom.indicator ) );
				} else if( !pl.type.is_variable( n ) && n.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", n, atom.indicator ) );
				} else if( !pl.type.is_variable( m ) && m.value < 0 ) {
					thread.throw_error( pl.error.domain( "not_less_than_zero", m, atom.indicator ) );
				} else {
					if( pl.type.is_variable( m ) || m.value > 0 ) {
						if( pl.type.is_variable( n ) ) {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [n, new Num( m.value-1, false )] ) ), point.substitution, point )] );
						} else {
							thread.prepend( [new State( point.goal.replace( new Term( "=", [m, new Num( n.value+1, false )] ) ), point.substitution, point )] );
						}
					}
				}
			},
			
			// =:=/2
			"=:=/2": function( thread, point, atom ) {
				var cmp = pl.arithmetic_compare( thread, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					thread.throw_error( cmp );
				} else if( cmp === 0 ) {
					thread.success( point );
				}
			},
			
			// =\=/2
			"=\\=/2": function( thread, point, atom ) {
				var cmp = pl.arithmetic_compare( thread, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					thread.throw_error( cmp );
				} else if( cmp !== 0 ) {
					thread.success( point );
				}
			},
			
			// </2
			"</2": function( thread, point, atom ) {
				var cmp = pl.arithmetic_compare( thread, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					thread.throw_error( cmp );
				} else if( cmp < 0 ) {
					thread.success( point );
				}
			},
			
			// =</2
			"=</2": function( thread, point, atom ) {
				var cmp = pl.arithmetic_compare( thread, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					thread.throw_error( cmp );
				} else if( cmp <= 0 ) {
					thread.success( point );
				}
			},
			
			// >/2
			">/2": function( thread, point, atom ) {
				var cmp = pl.arithmetic_compare( thread, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					thread.throw_error( cmp );
				} else if( cmp > 0 ) {
					thread.success( point );
				}
			},
			
			// >=/2
			">=/2": function( thread, point, atom ) {
				var cmp = pl.arithmetic_compare( thread, atom.args[0], atom.args[1] );
				if( pl.type.is_term( cmp ) ) {
					thread.throw_error( cmp );
				} else if( cmp >= 0 ) {
					thread.success( point );
				}
			},
			
			// TYPE TEST
			
			// var/1
			"var/1": function( thread, point, atom ) {
				if( pl.type.is_variable( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// atom/1
			"atom/1": function( thread, point, atom ) {
				if( pl.type.is_atom( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// atomic/1
			"atomic/1": function( thread, point, atom ) {
				if( pl.type.is_atomic( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// compound/1
			"compound/1": function( thread, point, atom ) {
				if( pl.type.is_compound( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// integer/1
			"integer/1": function( thread, point, atom ) {
				if( pl.type.is_integer( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// float/1
			"float/1": function( thread, point, atom ) {
				if( pl.type.is_float( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// number/1
			"number/1": function( thread, point, atom ) {
				if( pl.type.is_number( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// nonvar/1
			"nonvar/1": function( thread, point, atom ) {
				if( !pl.type.is_variable( atom.args[0] ) ) {
					thread.success( point );
				}
			},
			
			// ground/1
			"ground/1": function( thread, point, atom ) {
				if( atom.variables().length === 0 ) {
					thread.success( point );
				}
			},
			
			// acyclic_term/1
			"acyclic_term/1": function( thread, point, atom ) {
				var test = point.substitution.apply( point.substitution );
				var variables = atom.args[0].variables();
				for( var i = 0; i < variables.length; i++ )
					if( point.substitution.links[variables[i]] !== undefined && !point.substitution.links[variables[i]].equals( test.links[variables[i]] ) )
						return;
				thread.success( point );
			},
			
			// callable/1
			"callable/1": function( thread, point, atom ) {
				if( pl.type.is_callable( atom.args[0] ) ) {
					thread.success( point );
				}
			},

			// is_list/1
			"is_list/1": function( thread, point, atom ) {
				var list = atom.args[0];
				while( pl.type.is_term( list ) && list.indicator === "./2" )
					list = list.args[1];
				if( pl.type.is_term( list ) && list.indicator === "[]/0" )
					thread.success( point );
			},



			// STREAM SELECTION AND CONTROL

			// current_input/1
			"current_input/1": function( thread, point, atom ) {
				var stream = atom.args[0];
				if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain("stream", stream, atom.indicator) );
				} else {
					if( pl.type.is_atom( stream ) && thread.get_stream_by_alias( stream.id ) )
						stream = thread.get_stream_by_alias( stream.id );
					thread.prepend( [new State(
						point.goal.replace(new Term("=", [stream, thread.get_current_input()])),
						point.substitution,
						point)
					] );
				}
			},

			// current_output/1
			"current_output/1": function( thread, point, atom ) {
				var stream = atom.args[0];
				if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain("stream_or_alias", stream, atom.indicator) );
				} else {
					if( pl.type.is_atom( stream ) && thread.get_stream_by_alias( stream.id ) )
						stream = thread.get_stream_by_alias( stream.id );
					thread.prepend( [new State(
						point.goal.replace(new Term("=", [stream, thread.get_current_output()])),
						point.substitution,
						point)
					] );
				}
			},

			// set_input/1
			"set_input/1": function( thread, point, atom ) {
				var input = atom.args[0];
				var stream = pl.type.is_stream( input ) ? input : thread.get_stream_by_alias( input.id );
				if( pl.type.is_variable( input ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( input ) && !pl.type.is_stream( input ) && !pl.type.is_atom( input ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", input, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) ) {
					thread.throw_error( pl.error.existence( "stream", input, atom.indicator ) );
				} else if( stream.output === true ) {
					thread.throw_error( pl.error.permission( "input", "stream", input, atom.indicator ) );
				} else {
					thread.set_current_input( stream );
					thread.success( point );
				}
			},

			// set_output/1
			"set_output/1": function( thread, point, atom ) {
				var output = atom.args[0];
				var stream = pl.type.is_stream( output ) ? output : thread.get_stream_by_alias( output.id );
				if( pl.type.is_variable( output ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( output ) && !pl.type.is_stream( output ) && !pl.type.is_atom( output ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", output, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) ) {
					thread.throw_error( pl.error.existence( "stream", output, atom.indicator ) );
				} else if( stream.input === true ) {
					thread.throw_error( pl.error.permission( "output", "stream", output, atom.indicator ) );
				} else {
					thread.set_current_output( stream );
					thread.success( point );
				}
			},

			// open/3
			"open/3": function( thread, point, atom ) {
				var dest = atom.args[0], mode = atom.args[1], stream = atom.args[2];
				thread.prepend( [new State(
					point.goal.replace(new Term("open", [dest, mode, stream, new Term("[]", [])])),
					point.substitution,
					point
				)] );
			},

			// open/4
			"open/4": function( thread, point, atom ) {
				var dest = atom.args[0], mode = atom.args[1], stream = atom.args[2], options = atom.args[3];
				if( pl.type.is_variable( dest ) || pl.type.is_variable( mode ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( mode ) && !pl.type.is_atom( mode ) ) {
					thread.throw_error( pl.error.type( "atom", mode, atom.indicator ) );
				} else if( !pl.type.is_list( options ) ) {
					thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.type( "variable", stream, atom.indicator ) );
				} else if( !pl.type.is_atom( dest ) && !pl.type.is_streamable( dest ) ) {
					thread.throw_error( pl.error.domain( "source_sink", dest, atom.indicator ) );
				} else if( !pl.type.is_io_mode( mode ) ) {
					thread.throw_error( pl.error.domain( "io_mode", mode, atom.indicator ) );
				} else {
					var obj_options = {};
					var pointer = options;
					var property;
					while( pl.type.is_term(pointer) && pointer.indicator === "./2" ) {
						property = pointer.args[0];
						if( pl.type.is_variable( property ) ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( !pl.type.is_stream_option( property ) ) {
							thread.throw_error( pl.error.domain( "stream_option", property, atom.indicator ) );
							return;
						}
						obj_options[property.id] = property.args[0].id;
						pointer = pointer.args[1];
					}
					if( pointer.indicator !== "[]/0" ) {
						if( pl.type.is_variable( pointer ) )
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						else
							thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
						return;
					} else {
						var alias = obj_options["alias"];
						if( alias && thread.get_stream_by_alias(alias) ) {
							thread.throw_error( pl.error.permission( "open", "source_sink", new Term("alias", [new Term(alias, [])]), atom.indicator ) );
							return;
						}
						if( !obj_options["type"] )
							obj_options["type"] = "text";
						var file;
						if( pl.type.is_atom( dest ) )
							file = thread.file_system_open( dest.id, obj_options["type"], mode.id );
						else
							file = dest.stream( obj_options["type"], mode.id );
						if( file === false ) {
							thread.throw_error( pl.error.permission( "open", "source_sink", dest, atom.indicator ) );
							return;
						} else if( file === null ) {
							thread.throw_error( pl.error.existence( "source_sink", dest, atom.indicator ) );
							return;
						}
						var newstream = new Stream(
							file, mode.id,
							obj_options["alias"],
							obj_options["type"],
							obj_options["reposition"] === "true",
							obj_options["eof_action"] );
						if( alias )
							thread.session.streams[alias] = newstream;
						else
							thread.session.streams[newstream.id] = newstream;
						thread.prepend( [new State(
							point.goal.replace( new Term( "=", [stream, newstream] ) ),
							point.substitution,
							point
						)] );
					}
				}
			},

			// close/1
			"close/1": function( thread, point, atom ) {
				var stream = atom.args[0];
				thread.prepend( [new State(
					point.goal.replace(new Term("close", [stream, new Term("[]", [])])),
					point.substitution,
					point
				)] );
			},

			// close/2
			"close/2": function( thread, point, atom ) {
				var stream = atom.args[0], options = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( options ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_list( options ) ) {
					thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else {
					// Get options
					var obj_options = {};
					var pointer = options;
					var property;
					while( pl.type.is_term(pointer) && pointer.indicator === "./2" ) {
						property = pointer.args[0];
						if( pl.type.is_variable( property ) ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( !pl.type.is_close_option( property ) ) {
							thread.throw_error( pl.error.domain( "close_option", property, atom.indicator ) );
							return;
						}
						obj_options[property.id] = property.args[0].id === "true";
						pointer = pointer.args[1];
					}
					if( pointer.indicator !== "[]/0" ) {
						if( pl.type.is_variable( pointer ) )
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						else
							thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
						return;
					} else {
						if( stream2 === thread.session.standard_input || stream2 === thread.session.standard_output ) {
							thread.success( point );
							return;
						} else if( stream2 === thread.session.current_input ) {
							thread.session.current_input = thread.session.standard_input;
						} else if( stream2 === thread.session.current_output ) {
							thread.session.current_output = thread.session.current_output;
						}
						if( stream2.alias !== null )
							delete thread.session.streams[stream2.alias];
						else
							delete thread.session.streams[stream2.id];
						if( stream2.output )
							stream2.stream.flush();
						var closed = stream2.stream.close();
						stream2.stream = null;
						if( obj_options.force === true || closed === true ) {
							thread.success( point );
						}
					}
				}
			},

			// flush_output/0
			"flush_output/0": function( thread, point, atom ) {
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("flush_output", [new Var("S")])]) ),
					point.substitution,
					point
				)] );
			},

			// flush_output/1
			"flush_output/1": function( thread, point, atom ) {
				var stream = atom.args[0];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream.input === true ) {
					thread.throw_error( pl.error.permission( "output", "stream", output, atom.indicator ) );
				} else {
					stream2.stream.flush();
					thread.success( point );
				}
			},

			// stream_property/2
			"stream_property/2": function( thread, point, atom ) {
				var stream = atom.args[0], property = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) && (!pl.type.is_stream( stream2 ) || stream2.stream === null) ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( !pl.type.is_variable( property ) && !pl.type.is_stream_property( property ) ) {
					thread.throw_error( pl.error.domain( "stream_property", property, atom.indicator ) );
				} else {
					var streams = [];
					var states = [];
					if( !pl.type.is_variable( stream ) )
						streams.push( stream2 );
					else
						for( var key in thread.session.streams )
							streams.push( thread.session.streams[key] );
					for( var i = 0; i < streams.length; i++ ) {
						var properties = [];
						if( streams[i].filename )
							properties.push( new Term( "file_name", [new Term(streams[i].file_name, [])] ) );
						properties.push( new Term( "mode", [new Term(streams[i].mode, [])] ) );
						properties.push( new Term( streams[i].input ? "input" : "output", [] ) );
						if( streams[i].alias )
							properties.push( new Term( "alias", [new Term(streams[i].alias, [])] ) );
						properties.push( new Term( "position", [
							typeof streams[i].position === "number" ?
								new Num( streams[i].position, false ) :
								new Term( streams[i].position, [] )
						] ) );
						properties.push( new Term( "end_of_stream", [new Term(
							streams[i].position === "end_of_stream" ? "at" :
							streams[i].position === "past_end_of_stream" ? "past" :
							"not", [])] ) );
						properties.push( new Term( "eof_action", [new Term(streams[i].eof_action, [])] ) );
						properties.push( new Term( "reposition", [new Term(streams[i].reposition ? "true" : "false", [])] ) );
						properties.push( new Term( "type", [new Term(streams[i].type, [])] ) );
						for( var j = 0; j < properties.length; j++ ) {
							states.push( new State(
								point.goal.replace( new Term( ",", [
									new Term("=", [pl.type.is_variable( stream ) ? stream : stream2, streams[i]]),
									new Term("=", [property, properties[j]])]) ),
								point.substitution,
								point
							) );
						}
					}
					thread.prepend( states );
				}
			},

			// at_end_of_stream/0
			"at_end_of_stream/0": function( thread, point, atom ) {
				thread.prepend( [new State(
					point.goal.replace(
						new Term(",", [new Term("current_input", [new Var("S")]),new Term(",", [
							new Term("stream_property", [new Var("S"),new Term("end_of_stream", [new Var("E")])]),
							new Term(",", [new Term("!", []),new Term(";", [new Term("=", [new Var("E"),
							new Term("at", [])]),new Term("=", [new Var("E"),new Term("past", [])])])])])])
					),
					point.substitution,
					point
				)] );
			},

			// at_end_of_stream/1
			"at_end_of_stream/1": function( thread, point, atom ) {
				var stream = atom.args[0];
				thread.prepend( [new State(
					point.goal.replace(
						new Term(",", [new Term("stream_property", [stream,new Term("end_of_stream", [new Var("E")])]),
						new Term(",", [new Term("!", []),new Term(";", [new Term("=", [new Var("E"),new Term("at", [])]),
						new Term("=", [new Var("E"),new Term("past", [])])])])])
					),
					point.substitution,
					point
				)] );
			},

			// set_stream_position/2
			"set_stream_position/2": function( thread, point, atom ) {
				var stream = atom.args[0], position = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( position ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( !pl.type.is_stream_position( position ) ) {
					thread.throw_error( pl.error.domain( "stream_position", position, atom.indicator ) );
				} else if( stream2.reposition === false ) {
					thread.throw_error( pl.error.permission( "reposition", "stream", stream, atom.indicator ) );
				} else {
					if( pl.type.is_integer( position ) )
						stream2.position = position.value;
					else
						stream2.position = position.id;
					thread.success( point );
				}
			},



			//  CHARACTER INPUT OUTPUT
			
			// get_char/1
			"get_char/1": function( thread, point, atom ) {
				var char = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("get_char", [new Var("S"),char])]) ),
					point.substitution,
					point
				)] );
			},

			// get_char/2
			"get_char/2": function( thread, point, atom ) {
				var stream = atom.args[0], char = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( char ) && !pl.type.is_character( char ) ) {
					thread.throw_error( pl.error.type( "in_character", char, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "input", "binary_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					var stream_char;
					if( stream2.position === "end_of_stream" ) {
						stream_char = "end_of_file";
						stream2.position = "past_end_of_stream";
					} else {
						stream_char = stream2.stream.get( 1, stream2.position );
						if( stream_char === null ) {
							thread.throw_error( pl.error.representation( "character", atom.indicator ) );
							return;
						}
						stream2.position++;
					}
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [new Term(stream_char,[]), char] ) ),
						point.substitution,
						point
					)] );
				}
			},

			// get_code/1
			"get_code/1": function( thread, point, atom ) {
				var code = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("get_code", [new Var("S"),code])]) ),
					point.substitution,
					point
				)] );
			},

			// get_code/2
			"get_code/2": function( thread, point, atom ) {
				var stream = atom.args[0], code = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( code ) && !pl.type.is_integer( code ) ) {
					thread.throw_error( pl.error.type( "integer", char, atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "input", "binary_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					var stream_code;
					if( stream2.position === "end_of_stream" ) {
						stream_code = -1;
						stream2.position = "past_end_of_stream";
					} else {
						stream_code = stream2.stream.get( 1, stream2.position );
						if( stream_code === null ) {
							thread.throw_error( pl.error.representation( "character", atom.indicator ) );
							return;
						}
						stream_code = codePointAt( stream_code, 0 );
						stream2.position++;
					}
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [new Num(stream_code, false), code] ) ),
						point.substitution,
						point
					)] );
				}
			},

			// peek_char/1
			"peek_char/1": function( thread, point, atom ) {
				var char = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("peek_char", [new Var("S"),char])]) ),
					point.substitution,
					point
				)] );
			},

			// peek_char/2
			"peek_char/2": function( thread, point, atom ) {
				var stream = atom.args[0], char = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( char ) && !pl.type.is_character( char ) ) {
					thread.throw_error( pl.error.type( "in_character", char, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "input", "binary_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					var stream_char;
					if( stream2.position === "end_of_stream" ) {
						stream_char = "end_of_file";
						stream2.position = "past_end_of_stream";
					} else {
						stream_char = stream2.stream.get( 1, stream2.position );
						if( stream_char === null ) {
							thread.throw_error( pl.error.representation( "character", atom.indicator ) );
							return;
						}
					}
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [new Term(stream_char,[]), char] ) ),
						point.substitution,
						point
					)] );
				}
			},

			// peek_code/1
			"peek_code/1": function( thread, point, atom ) {
				var code = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("peek_code", [new Var("S"),code])]) ),
					point.substitution,
					point
				)] );
			},

			// peek_code/2
			"peek_code/2": function( thread, point, atom ) {
				var stream = atom.args[0], code = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( code ) && !pl.type.is_integer( code ) ) {
					thread.throw_error( pl.error.type( "integer", char, atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "input", "binary_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					var stream_code;
					if( stream2.position === "end_of_stream" ) {
						stream_code = -1;
						stream2.position = "past_end_of_stream";
					} else {
						stream_code = stream2.stream.get( 1, stream2.position );
						if( stream_code === null ) {
							thread.throw_error( pl.error.representation( "character", atom.indicator ) );
							return;
						}
						stream_code = codePointAt( stream_code, 0 );
					}
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [new Num(stream_code, false), code] ) ),
						point.substitution,
						point
					)] );
				}
			},

			// put_char/1
			"put_char/1": function( thread, point, atom ) {
				var char = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_char", [new Var("S"),char])]) ),
					point.substitution,
					point
				)] );
			},

			// put_char/2
			"put_char/2": function( thread, point, atom ) {
				var stream = atom.args[0], char = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( char ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_character( char ) ) {
					thread.throw_error( pl.error.type( "character", char, atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.input ) {
					thread.throw_error( pl.error.permission( "output", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "output", "binary_stream", stream, atom.indicator ) );
				} else {
					if( stream2.stream.put( char.id, stream2.position ) ) {
						if(typeof stream2.position === "number")
							stream2.position++;
						thread.success( point );
					}
				}
			},

			// put_code/1
			"put_code/1": function( thread, point, atom ) {
				var code = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_code", [new Var("S"),code])]) ),
					point.substitution,
					point
				)] );
			},

			// put_code/2
			"put_code/2": function( thread, point, atom ) {
				var stream = atom.args[0], code = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( code ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( code ) ) {
					thread.throw_error( pl.error.type( "integer", code, atom.indicator ) );
				} else if( !pl.type.is_character_code( code ) ) {
					thread.throw_error( pl.error.representation( "character_code", atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.input ) {
					thread.throw_error( pl.error.permission( "output", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "output", "binary_stream", stream, atom.indicator ) );
				} else {
					if( stream2.stream.put_char( fromCodePoint( code.value ), stream2.position ) ) {
						if(typeof stream2.position === "number")
							stream2.position++;
						thread.success( point );
					}
				}
			},

			// nl/0
			"nl/0": function( thread, point, atom ) {
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_char", [new Var("S"), new Term("\n", [])])]) ),
					point.substitution,
					point
				)] );
			},

			// nl/1
			"nl/1": function( thread, point, atom ) {
				var stream = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term("put_char", [stream, new Term("\n", [])]) ),
					point.substitution,
					point
				)] );
			},



			// BYTE INPUT/OUTPUT

			// get_byte/1
			"get_byte/1": function( thread, point, atom ) {
				var byte = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("get_byte", [new Var("S"),byte])]) ),
					point.substitution,
					point
				)] );
			},

			// get_byte/2
			"get_byte/2": function( thread, point, atom ) {
				var stream = atom.args[0], byte = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( byte ) && !pl.type.is_byte( byte ) ) {
					thread.throw_error( pl.error.type( "in_byte", char, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "text" ) {
					thread.throw_error( pl.error.permission( "input", "text_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					var stream_byte;
					if( stream2.position === "end_of_stream" ) {
						stream_byte = "end_of_file";
						stream2.position = "past_end_of_stream";
					} else {
						stream_byte = stream2.stream.get_byte( stream2.position );
						if( stream_byte === null ) {
							thread.throw_error( pl.error.representation( "byte", atom.indicator ) );
							return;
						}
						stream2.position++;
					}
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [new Num(stream_byte,false), byte] ) ),
						point.substitution,
						point
					)] );
				}
			},
			
			// peek_byte/1
			"peek_byte/1": function( thread, point, atom ) {
				var byte = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("peek_byte", [new Var("S"),byte])]) ),
					point.substitution,
					point
				)] );
			},

			// peek_byte/2
			"peek_byte/2": function( thread, point, atom ) {
				var stream = atom.args[0], byte = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( byte ) && !pl.type.is_byte( byte ) ) {
					thread.throw_error( pl.error.type( "in_byte", char, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "text" ) {
					thread.throw_error( pl.error.permission( "input", "text_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					var stream_byte;
					if( stream2.position === "end_of_stream" ) {
						stream_byte = "end_of_file";
						stream2.position = "past_end_of_stream";
					} else {
						stream_byte = stream2.stream.get_byte( stream2.position );
						if( stream_byte === null ) {
							thread.throw_error( pl.error.representation( "byte", atom.indicator ) );
							return;
						}
					}
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [new Num(stream_byte,false), byte] ) ),
						point.substitution,
						point
					)] );
				}
			},

			// put_byte/1
			"put_byte/1": function( thread, point, atom ) {
				var byte = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_byte", [new Var("S"),byte])]) ),
					point.substitution,
					point
				)] );
			},

			// put_byte/2
			"put_byte/2": function( thread, point, atom ) {
				var stream = atom.args[0], byte = atom.args[1];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( byte ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_byte( byte ) ) {
					thread.throw_error( pl.error.type( "byte", byte, atom.indicator ) );
				} else if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.input ) {
					thread.throw_error( pl.error.permission( "output", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "text" ) {
					thread.throw_error( pl.error.permission( "output", "text_stream", stream, atom.indicator ) );
				} else {
					if( stream2.stream.put_byte( byte.value, stream2.position ) ) {
						if(typeof stream2.position === "number")
							stream2.position++;
						thread.success( point );
					}
				}
			},



			// TERM INPUT/OUTPUT

			// read/1
			"read/1": function( thread, point, atom ) {
				var term = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("read_term", [new Var("S"),term,new Term("[]",[])])]) ),
					point.substitution,
					point
				)] );
			},

			// read/2
			"read/2": function( thread, point, atom ) {
				var stream = atom.args[0], term = atom.args[1];
				thread.prepend( [new State( 
					point.goal.replace( new Term("read_term", [stream,term,new Term("[]",[])]) ),
					point.substitution,
					point
				)] );
			},

			// read_term/2
			"read_term/2": function( thread, point, atom ) {
				var term = atom.args[0], options = atom.args[1];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_input", [new Var("S")]),new Term("read_term", [new Var("S"),term,options])]) ),
					point.substitution,
					point
				)] );
			},

			// read_term/3
			"read_term/3": function( thread, point, atom ) {
				var stream = atom.args[0], term = atom.args[1], options = atom.args[2];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( options ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_list( options ) ) {
					thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.output ) {
					thread.throw_error( pl.error.permission( "input", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "input", "binary_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "input", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					// Get options
					var obj_options = {};
					var pointer = options;
					var property;
					while( pl.type.is_term(pointer) && pointer.indicator === "./2" ) {
						property = pointer.args[0];
						if( pl.type.is_variable( property ) ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( !pl.type.is_read_option( property ) ) {
							thread.throw_error( pl.error.domain( "read_option", property, atom.indicator ) );
							return;
						}
						obj_options[property.id] = property.args[0];
						pointer = pointer.args[1];
					}
					if( pointer.indicator !== "[]/0" ) {
						if( pl.type.is_variable( pointer ) )
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						else
							thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
						return;
					} else {
						var char, tokenizer, expr;
						var text = "";
						var tokens = [];
						var last_token = null;
						// Get term
						while( last_token === null || last_token.name !== "atom" || last_token.value !== "." ||
							(expr.type === ERROR && pl.flatten_error(new Term("throw",[expr.value])).found === "token_not_found") ) {
							char = stream2.stream.get( 1, stream2.position );
							if( char === null ) {
								thread.throw_error( pl.error.representation( "character", atom.indicator ) );
								return;
							}
							if( char === "end_of_file" || char === "past_end_of_file" ) {
								if( expr )
									thread.throw_error( pl.error.syntax( tokens[expr.len-1], ". or expression expected", false ) );
								else
									thread.throw_error( pl.error.syntax( null, "token not found", true ) );
								return;
							}
							stream2.position++;
							text += char;
							tokenizer = new Tokenizer( thread );
							tokenizer.new_text( text );
							tokens = tokenizer.get_tokens();
							last_token = tokens !== null && tokens.length > 0 ? tokens[tokens.length-1] : null;
							if( tokens === null )
								continue;
							expr = parseExpr(thread, tokens, 0, thread.__get_max_priority(), false);
						}
						// Succeed analyzing term
						if( expr.type === SUCCESS && expr.len === tokens.length-1 && last_token.value === "." ) {
							expr = expr.value.rename( thread );
							var eq = new Term( "=", [term, expr] );
							// Variables
							if( obj_options.variables ) {
								var vars = arrayToList( map( nub( expr.variables() ), function(v) { return new Var(v); } ) );
								eq = new Term( ",", [eq, new Term( "=", [obj_options.variables, vars] )] )
							}
							// Variable names
							if( obj_options.variable_names ) {
								var vars = arrayToList( map( nub( expr.variables() ), function(v) {
									var prop;
									for( prop in thread.session.renamed_variables ) {
										if( thread.session.renamed_variables.hasOwnProperty( prop ) ) {
											if( thread.session.renamed_variables[ prop ] === v )
												break;
										}
									}
									return new Term( "=", [new Term( prop, []), new Var(v)] );
								} ) );
								eq = new Term( ",", [eq, new Term( "=", [obj_options.variable_names, vars] )] )
							}
							// Singletons
							if( obj_options.singletons ) {
								var vars = arrayToList( map( new Rule( expr, null ).singleton_variables(), function(v) {
									var prop;
									for( prop in thread.session.renamed_variables ) {
										if( thread.session.renamed_variables.hasOwnProperty( prop ) ) {
											if( thread.session.renamed_variables[ prop ] === v )
												break;
										}
									}
									return new Term( "=", [new Term( prop, []), new Var(v)] );
								} ) );
								eq = new Term( ",", [eq, new Term( "=", [obj_options.singletons, vars] )] )
							}
							thread.prepend( [new State( point.goal.replace( eq ), point.substitution, point )] );
						// Failed analyzing term
						} else {
							if( expr.type === SUCCESS )
								thread.throw_error( pl.error.syntax( tokens[expr.len], "unexpected token", false ) );
							else
								thread.throw_error( expr.value );
						}
					}
				}
			},

			// write/1
			"write/1": function( thread, point, atom ) {
				var term = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("write", [new Var("S"),term])]) ),
					point.substitution,
					point
				)] );
			},
			
			// write/2
			"write/2": function( thread, point, atom ) {
				var stream = atom.args[0], term = atom.args[1];
				thread.prepend( [new State( 
					point.goal.replace( new Term("write_term", [stream, term,
						new Term(".", [new Term("quoted", [new Term("false", [])]),
							new Term(".", [new Term("ignore_ops", [new Term("false")]),
								new Term(".", [new Term("numbervars", [new Term("true")]), new Term("[]",[])])])])]) ),
					point.substitution,
					point
				)] );
			},
			
			// writeq/1
			"writeq/1": function( thread, point, atom ) {
				var term = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("writeq", [new Var("S"),term])]) ),
					point.substitution,
					point
				)] );
			},
			
			// writeq/2
			"writeq/2": function( thread, point, atom ) {
				var stream = atom.args[0], term = atom.args[1];
				thread.prepend( [new State( 
					point.goal.replace( new Term("write_term", [stream, term,
						new Term(".", [new Term("quoted", [new Term("true", [])]),
							new Term(".", [new Term("ignore_ops", [new Term("false")]),
								new Term(".", [new Term("numbervars", [new Term("true")]), new Term("[]",[])])])])]) ),
					point.substitution,
					point
				)] );
			},
			
			// write_canonical/1
			"write_canonical/1": function( thread, point, atom ) {
				var term = atom.args[0];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("write_canonical", [new Var("S"),term])]) ),
					point.substitution,
					point
				)] );
			},
			
			// write_canonical/2
			"write_canonical/2": function( thread, point, atom ) {
				var stream = atom.args[0], term = atom.args[1];
				thread.prepend( [new State( 
					point.goal.replace( new Term("write_term", [stream, term,
						new Term(".", [new Term("quoted", [new Term("true", [])]),
							new Term(".", [new Term("ignore_ops", [new Term("true")]),
								new Term(".", [new Term("numbervars", [new Term("false")]), new Term("[]",[])])])])]) ),
					point.substitution,
					point
				)] );
			},

			// write_term/2
			"write_term/2": function( thread, point, atom ) {
				var term = atom.args[0], options = atom.args[1];
				thread.prepend( [new State( 
					point.goal.replace( new Term(",", [new Term("current_output", [new Var("S")]),new Term("write_term", [new Var("S"),term,options])]) ),
					point.substitution,
					point
				)] );
			},
			
			// write_term/3
			"write_term/3": function( thread, point, atom ) {
				var stream = atom.args[0], term = atom.args[1], options = atom.args[2];
				var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
				if( pl.type.is_variable( stream ) || pl.type.is_variable( options ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_list( options ) ) {
					thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
				} else if( !pl.type.is_stream( stream ) && !pl.type.is_atom( stream ) ) {
					thread.throw_error( pl.error.domain( "stream_or_alias", stream, atom.indicator ) );
				} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
					thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
				} else if( stream2.input ) {
					thread.throw_error( pl.error.permission( "output", "stream", stream, atom.indicator ) );
				} else if( stream2.type === "binary" ) {
					thread.throw_error( pl.error.permission( "output", "binary_stream", stream, atom.indicator ) );
				} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "error" ) {
					thread.throw_error( pl.error.permission( "output", "past_end_of_stream", stream, atom.indicator ) );
				} else {
					// Get options
					var obj_options = {};
					var pointer = options;
					var property;
					while( pl.type.is_term(pointer) && pointer.indicator === "./2" ) {
						property = pointer.args[0];
						if( pl.type.is_variable( property ) ) {
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
							return;
						} else if( !pl.type.is_write_option( property ) ) {
							thread.throw_error( pl.error.domain( "write_option", property, atom.indicator ) );
							return;
						}
						obj_options[property.id] = property.args[0].id === "true";
						pointer = pointer.args[1];
					}
					if( pointer.indicator !== "[]/0" ) {
						if( pl.type.is_variable( pointer ) )
							thread.throw_error( pl.error.instantiation( atom.indicator ) );
						else
							thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
						return;
					} else {
						obj_options.session = thread.session;
						var text = term.toString( obj_options );
						stream2.stream.put( text, stream2.position );
						if( typeof stream2.position === "number" )
							stream2.position += text.length;
						thread.success( point );
					}
				}
			},


			
			// IMPLEMENTATION DEFINED HOOKS
			
			// halt/0
			"halt/0": function( thread, point, _ ) {
				if( nodejs_flag )
					process.exit();
				thread.points = [];
			},
			
			// halt/1
			"halt/1": function( thread, point, atom ) {
				var int = atom.args[0];
				if( pl.type.is_variable( int ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( int ) ) {
					thread.throw_error( pl.error.type( "integer", int, atom.indicator ) );
				} else {
					if( nodejs_flag )
						process.exit(int.value);
					thread.points = [];
				}
			},
			
			// current_prolog_flag/2
			"current_prolog_flag/2": function( thread, point, atom ) {
				var flag = atom.args[0], value = atom.args[1];
				if( !pl.type.is_variable( flag ) && !pl.type.is_atom( flag ) ) {
					thread.throw_error( pl.error.type( "atom", flag, atom.indicator ) );
				} else if( !pl.type.is_variable( flag ) && !pl.type.is_flag( flag ) ) {
					thread.throw_error( pl.error.domain( "prolog_flag", flag, atom.indicator ) );
				} else {
					var states = [];
					for( var name in pl.flag ) {
						if(!pl.flag.hasOwnProperty(name)) continue;
						var goal = new Term( ",", [new Term( "=", [new Term( name ), flag] ), new Term( "=", [thread.get_flag(name), value] )] );
						states.push( new State( point.goal.replace( goal ), point.substitution, point ) );
					}
					thread.prepend( states );
				}
			},
			
			// set_prolog_flag/2
			"set_prolog_flag/2": function( thread, point, atom ) {
				var flag = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( flag ) || pl.type.is_variable( value ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( flag ) ) {
					thread.throw_error( pl.error.type( "atom", flag, atom.indicator ) );
				} else if( !pl.type.is_flag( flag ) ) {
					thread.throw_error( pl.error.domain( "prolog_flag", flag, atom.indicator ) );
				} else if( !pl.type.is_value_flag( flag, value ) ) {
					thread.throw_error( pl.error.domain( "flag_value", new Term( "+", [flag, value] ), atom.indicator ) );
				} else if( !pl.type.is_modifiable_flag( flag ) ) {
					thread.throw_error( pl.error.permission( "modify", "flag", flag ) );
				} else {
					thread.session.flag[flag.id] = value;
					thread.success( point );
				}
			},



			// OPERATING SYSTEM INTERACTION

			// shell/1
			"shell/1": function( thread, point, atom ) {
				var command = atom.args[0];
				thread.prepend( [new State(
					point.goal.replace( new Term("shell", [command, new Num(0, false)]) ),
					point.substitution,
					point
				)] );
			},

			// shell/2
			"shell/2": function( thread, point, atom ) {
				var command = atom.args[0], status = atom.args[1];
				if( pl.type.is_variable(command) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom(command) ) {
					thread.throw_error( pl.error.type( "atom", command, atom.indicator ) );
				} else if( !pl.type.is_variable(status) && !pl.type.is_integer(status) ) {
					thread.throw_error( pl.error.type( "integer", status, atom.indicator ) );
				} else {
					if(nodejs_flag) {
						const { exec } = require('child_process');
						exec( command.id, function() {} ).on( 'exit', function(code) {
							thread.prepend( [new State(
								point.goal.replace( new Term("=", [status, new Num(code, false)]) ),
								point.substitution,
								point
							)] );
							thread.again();
						} );
						return true;
					} else {
						try {
							eval( command.id );
							thread.prepend( [new State(
								point.goal.replace( new Term("=", [status, new Num(0, false)]) ),
								point.substitution,
								point
							)] );
						} catch( error ) {
							thread.prepend( [new State(
								point.goal.replace( new Term("=", [status, new Num(1, false)]) ),
								point.substitution,
								point
							)] );
						}
					}
				}
			},



			// LOAD PROLOG SOURCE FILES

			// consult/1
			"consult/1": function( thread, point, atom ) {
				var src = atom.args[0];
				if(pl.type.is_variable(src)) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if(!pl.type.is_atom(src)) {
					thread.throw_error( pl.error.type( "atom", src, atom.indicator ) );
				} else {
					if(thread.consult( src.id ))
						thread.success(point);
				}
			},



			// TIME AND DATES

			// get_time/1
			"get_time/1": function( thread, point, atom ) {
				var time = atom.args[0];
				if(!pl.type.is_variable(time) && !pl.type.is_number(time)) {
					thread.throw_error( pl.error.type( "number", time, atom.indicator ) );
				} else {
					var current = new Num(Date.now(), true);
					thread.prepend( [new State(
						point.goal.replace( new Term( "=", [time, current] ) ), 
						point.substitution,
						point
					)] );
				}
			},



			// GRAMMARS

			// phrase/3
			"phrase/3": function( thread, point, atom ) {
				var grbody = atom.args[0], s0 = atom.args[1], s = atom.args[2];
				if( pl.type.is_variable( grbody ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_callable( grbody ) ) {
					thread.throw_error( pl.error.type( "callable", grbody, atom.indicator ) );
				} else {
					var goal = body_to_dcg( grbody.clone(), s0, thread );
					if(goal !== null) {
						thread.prepend( [new State(
							point.goal.replace( new Term( ",", [goal.value, new Term("=", [goal.variable, s])] ) ), 
							point.substitution,
							point
						)] );
					}
				}
			},

			// phrase/2
			"phrase/2": function( thread, point, atom ) {
				var grbody = atom.args[0], s0 = atom.args[1];
				thread.prepend( [new State(
					point.goal.replace( new Term( "phrase", [grbody, s0, new Term("[]", [])] ) ), 
					point.substitution,
					point
				)] );
			},



			// TAU PROLOG INFORMATION

			// version/0
			"version/0": function( thread, point, atom ) {
				var msg = "Welcome to Tau Prolog version " + version.major + "." + version.minor + "." + version.patch + "\n";
				msg += "Tau Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.\n";
				msg += "Please run ?- license. for legal details.\n";
				msg += "For online help and background, visit http:/tau-prolog.org";
				thread.prepend( [new State(
					point.goal.replace( new Term( "write", [new Term( msg, [] )] ) ), 
					point.substitution,
					point
				)] );
			},

			// license/0
			"license/0": function( thread, point, atom ) {
				var msg = "Tau Prolog. A Prolog interpreter in JavaScript.\n";
				msg += "Copyright (C) 2017 - 2020 José Antonio Riaza Valverde\n\n";
				msg += "Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:\n";
				msg += "1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.\n";
				msg += "2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.\n";
				msg += "3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.\n\n";
				msg += "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n\n";
				msg += "You should have received a copy of the BSD 3-Clause License along with this program. If not, see https://opensource.org/licenses/BSD-3-Clause";
				thread.prepend( [new State(
					point.goal.replace( new Term( "write", [new Term( msg, [] )] ) ), 
					point.substitution,
					point
				)] );
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
			char_conversion: {
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
			},
			
			// Occurs check behavior
			occurs_check: {
				allowed: [new Term( "false" ), new Term( "true" )],
				value: new Term( "false" ),
				changeable: true
			},
			
			// Dialect
			dialect: {
				allowed: [new Term( "tau" )],
				value: new Term( "tau" ),
				changeable: false
			},
			
			// Version
			version_data: {
				allowed: [new Term( "tau", [new Num(version.major,false), new Num(version.minor,false), new Num(version.patch,false), new Term(version.status)] )],
				value: new Term( "tau", [new Num(version.major,false), new Num(version.minor,false), new Num(version.patch,false), new Term(version.status)] ),
				changeable: false
			},
			
			// NodeJS
			nodejs: {
				allowed: [new Term( "yes" ), new Term( "no" )],
				value: new Term( nodejs_flag ? "yes" : "no" ),
				changeable: false
			},

			// Arguments
			argv: {
				allowed: [nodejs_arguments],
				value: nodejs_arguments,
				changeble: false
			}
			
		},
		
		// Unify
		unify: function( s, t, occurs_check ) {
			occurs_check = occurs_check === undefined ? false : occurs_check;
			var G = [{left: s, right: t}], links = {};
			while( G.length !== 0 ) {
				var eq = G.pop();
				s = eq.left;
				t = eq.right;
				if(s == t)
					continue;
				if( pl.type.is_term(s) && pl.type.is_term(t) ) {
					if( s.indicator !== t.indicator )
						return null;
					// list
					if(s.indicator === "./2") {
						var pointer_s = s, pointer_t = t;
						while(pointer_s.indicator === "./2" && pointer_t.indicator === "./2") {
							G.push( {left: pointer_s.args[0], right: pointer_t.args[0]} );
							pointer_s = pointer_s.args[1];
							pointer_t = pointer_t.args[1];
						}
						G.push( {left: pointer_s, right: pointer_t} );
					// compound term
					} else {
						for( var i = 0; i < s.args.length; i++ )
							G.push( {left: s.args[i], right: t.args[i]} );
					}
				} else if( pl.type.is_number(s) && pl.type.is_number(t) ) {
					if( s.value !== t.value || s.is_float !== t.is_float )
						return null;
				} else if( pl.type.is_variable(s) ) {
					// X = X
					if( pl.type.is_variable(t) && s.id === t.id )
						continue;
					// Occurs check
					if( occurs_check === true && indexOf( t.variables(), s.id ) !== -1 )
						return null;
					if( s.id !== "_" ) {
						var subs = new Substitution();
						subs.add( s.id, t );
						for( var i = 0; i < G.length; i++ ) {
							G[i].left = G[i].left.apply( subs );
							G[i].right = G[i].right.apply( subs );
						}
						for( var i in links )
							links[i] = links[i].apply( subs );
						links[s.id] = t;
					}
				} else if( pl.type.is_variable(t) ) {
					G.push( {left: t, right: s} );
				} else if( s.unify !== undefined ) {
					if( !s.unify(t) )
						return null;
				} else {
					return null;
				}
			}
			return new Substitution( links );
		},
		
		// Compare
		compare: function( obj1, obj2 ) {
			var type = pl.type.compare( obj1, obj2 );
			return type !== 0 ? type : obj1.compare( obj2 );
		},
		
		// Arithmetic comparison
		arithmetic_compare: function( thread, obj1, obj2 ) {
			var expr1 = obj1.interpret( thread );
			if( !pl.type.is_number( expr1 ) ) {
				return expr1;
			} else {
				var expr2 = obj2.interpret( thread );
				if( !pl.type.is_number( expr2 ) ) {
					return expr2;
				} else {
					return expr1.value < expr2.value ? -1 : (expr1.value > expr2.value ? 1 : 0);
				}
			}
		},
		
		// Operate
		operate: function( thread, obj ) {
			if( pl.type.is_operator( obj ) ) {
				var op = pl.type.is_operator( obj );
				var args = [], value;
				var type = false;
				for( var i = 0; i < obj.args.length; i++ ) {
					value = obj.args[i].interpret( thread );
					if( !pl.type.is_number( value ) ) {
						return value;
					} else if( op.type_args !== null && value.is_float !== op.type_args ) {
						return pl.error.type( op.type_args ? "float" : "integer", value, thread.__call_indicator );
					} else {
						args.push( value.value );
					}
					type = type || value.is_float;
				}
				args.push( thread );
				value = pl.arithmetic.evaluation[obj.indicator].fn.apply( this, args );
				type = op.type_result === null ? type : op.type_result;
				if( pl.type.is_term( value ) ) {
					return value;
				} else if( value === Number.POSITIVE_INFINITY || value === Number.NEGATIVE_INFINITY ) {
					return pl.error.evaluation( "overflow", thread.__call_indicator );
				} else if( type === false && thread.get_flag( "bounded" ).id === "true" && (value > thread.get_flag( "max_integer" ).value || value < thread.get_flag( "min_integer" ).value) ) {
					return pl.error.evaluation( "int_overflow", thread.__call_indicator );
				} else {
					return new Num( value, type );
				}
			} else {
				return pl.error.type( "evaluable", obj.indicator, thread.__call_indicator );
			}
		},
		
		// Errors
		error: {
			
			// Existence error
			existence: function( type, object, indicator ) {
				if( typeof object === "string" )
					object = str_indicator( object );
				return new Term( "error", [new Term( "existence_error", [new Term( type ), object] ), str_indicator( indicator )] );
			},
			
			// Type error
			type: function( expected, found, indicator ) {
				return new Term( "error", [new Term( "type_error", [new Term( expected ), found] ), str_indicator( indicator )] );
			},
			
			// Instantation error
			instantiation: function( indicator ) {
				return new Term( "error", [new Term( "instantiation_error" ), str_indicator( indicator )] );
			},
			
			// Domain error
			domain: function( expected, found, indicator ) {
				return new Term( "error", [new Term( "domain_error", [new Term( expected ), found]), str_indicator( indicator )] );
			},
			
			// Representation error
			representation: function( flag, indicator ) {
				return new Term( "error", [new Term( "representation_error", [new Term( flag )] ), str_indicator( indicator )] );
			},
			
			// Permission error
			permission: function( operation, type, found, indicator ) {
				return new Term( "error", [new Term( "permission_error", [new Term( operation ), new Term( type ), found] ), str_indicator( indicator )] );
			},
			
			// Evaluation error
			evaluation: function( error, indicator ) {
				return new Term( "error", [new Term( "evaluation_error", [new Term( error )] ), str_indicator( indicator )] );
			},
			
			// Syntax error
			syntax: function( token, expected, last ) {
				token = token || {value: "", line: 0, column: 0, matches: [""], start: 0};
				var position = last && token.matches.length > 0 ? token.start + token.matches[0].length : token.start;
				var found = last ? new Term("token_not_found") : new Term("found", [new Term(token.value.toString())]);
				var info = new Term( ".", [new Term( "line", [new Num(token.line+1)] ), new Term( ".", [new Term( "column", [new Num(position+1)] ), new Term( ".", [found, new Term( "[]", [] )] )] )] );
				return new Term( "error", [new Term( "syntax_error", [new Term( expected )] ), info] );
			},
			
			// Syntax error by predicate
			syntax_by_predicate: function( expected, indicator ) {
				return new Term( "error", [new Term( "syntax_error", [new Term( expected ) ] ), str_indicator( indicator )] );
			}
			
		},
		
		// Warnings
		warning: {
			
			// Singleton variables
			singleton: function( variables, rule, line ) {
				var list = new Term( "[]" );
				for( var i = variables.length-1; i >= 0; i-- )
					list = new Term( ".", [new Var(variables[i]), list] );
				return new Term( "warning", [new Term( "singleton_variables", [list, str_indicator(rule)]), new Term(".",[new Term( "line", [ new Num( line, false ) ]), new Term("[]")])] );
			},
			
			// Failed goal
			failed_goal: function( goal, line ) {
				return new Term( "warning", [new Term( "failed_goal", [goal]), new Term(".",[new Term( "line", [ new Num( line, false ) ]), new Term("[]")])] );
			}

		},
		
		// Format of renamed variables
		format_variable: function( variable ) {
			return "_" + variable;
		},
		
		// Format of computed answers
		format_answer: function( answer, thread, options ) {
			if( thread instanceof Session )
				thread = thread.thread;
			var options = options ? options : {};
			options.session = thread ? thread.session : undefined;
			if( pl.type.is_error( answer ) ) {
				return "uncaught exception: " + answer.args[0].toString();
			} else if( answer === false ) {
				return "false.";
			} else if( answer === null ) {
				return "limit exceeded ;";
			} else {
				var i = 0;
				var str = "";
				if( pl.type.is_substitution( answer ) ) {
					var dom = answer.domain( true );
					answer = answer.filter( function( id, value ) {
						return !pl.type.is_variable( value ) ||
							pl.type.is_variable( value ) && answer.has_attributes( id ) ||
							indexOf( dom, value.id ) !== -1 && id !== value.id;
					} );
				}
				for( var link in answer.links ) {
					if(!answer.links.hasOwnProperty(link))
						continue;
					if( pl.type.is_variable( answer.links[link] ) && link === answer.links[link].id ) {
						var attrs = answer.attrs[link];
						for( var module in attrs ) {
							if(!attrs.hasOwnProperty(module))
								continue;
							i++;
							if( str !== "" )
								str += ", ";
							str += "put_attr(" + link + ", " + module + ", " + attrs[module].toString(options) + ")";
						}
					} else {
						i++;
						if( str !== "" )
							str += ", ";
						str += link.toString( options ) + " = " +
							answer.links[link].toString( options, {priority: "700", class: "xfx", indicator: "=/2"}, "right" );
					}
				}
				var delimiter = typeof thread === "undefined" || thread.points.length > 0 ? " ;" : "."; 
				if( i === 0 ) {
					return "true" + delimiter;
				} else {
					return str + delimiter;
				}
			}
		},
		
		// Flatten default errors
		flatten_error: function( error ) {
			if( !pl.type.is_error( error ) ) return null;
			error = error.args[0];
			var obj = {};
			obj.type = error.args[0].id;
			obj.thrown = obj.type === "syntax_error" ? null : error.args[1].id;
			obj.expected = null;
			obj.found = null;
			obj.representation = null;
			obj.existence = null;
			obj.existence_type = null;
			obj.line = null;
			obj.column = null;
			obj.permission_operation = null;
			obj.permission_type = null;
			obj.evaluation_type = null;
			if( obj.type === "type_error" || obj.type === "domain_error" ) {
				obj.expected = error.args[0].args[0].id;
				obj.found = error.args[0].args[1].toString();
			} else if( obj.type === "syntax_error" ) {
				if( error.args[1].indicator === "./2" ) {
					obj.expected = error.args[0].args[0].id;
					obj.found = error.args[1].args[1].args[1].args[0];
					obj.found = obj.found.id === "token_not_found" ? obj.found.id : obj.found.args[0].id;
					obj.line = error.args[1].args[0].args[0].value;
					obj.column = error.args[1].args[1].args[0].args[0].value;
				} else {
					obj.thrown = error.args[1].id;
				}
			} else if( obj.type === "permission_error" ) {
				obj.found = error.args[0].args[2].toString();
				obj.permission_operation = error.args[0].args[0].id;
				obj.permission_type = error.args[0].args[1].id;
			} else if( obj.type === "evaluation_error" ) {
				obj.evaluation_type = error.args[0].args[0].id;
			} else if( obj.type === "representation_error" ) {
				obj.representation = error.args[0].args[0].id;
			} else if( obj.type === "existence_error" ) {
				obj.existence = error.args[0].args[1].toString();
				obj.existence_type = error.args[0].args[0].id;
			}
			return obj;
		},
		
		// Create new session
		create: function( limit ) {
			return new pl.type.Session( limit );
		}
		
	};

	if( typeof module !== 'undefined' ) {
		module.exports = pl;
	} else {
		window.pl = pl;
	}
	
})();
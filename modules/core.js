(function() {
	
	// VERSION
	var version = { major: 0, minor: 3, patch: 4, status: "beta" };



	// IO FILE SYSTEM
	
	// Virtual file
	function TauFile(name, type, parent, text) {
		text = text === undefined ? "" : text;
		this.name = name;
		this.type = type;
		this.parent = parent;
		this.text = text;
		this.created = Date.now() / 1000;
		this.modified = this.created;
	}

	TauFile.prototype.get = function(length, position) {
		if(position === this.text.length) {
			return "end_of_stream";
		} else if(position > this.text.length) {
			return "end_of_stream";
		} else {
			return this.text.substring(position, position+length);
		}
	};

	TauFile.prototype.eof = function(position) {
		return position === this.text.length;
	};

	TauFile.prototype.put = function(text, position) {
		if(position === "end_of_stream") {
			this.text += text;
			return true;
		} else if(position === "past_end_of_stream") {
			return null;
		} else {
			this.text = this.text.substring(0, position) + text + this.text.substring(position+text.length);
			return true;
		}
	};

	TauFile.prototype.get_byte = function(position) {
		if(position === "end_of_stream")
			return -1;
		var index = Math.floor(position/2);
		if(this.text.length <= index)
			return -1;
		var code = codePointAt(this.text[Math.floor(position/2)], 0);
		if(position % 2 === 0)
			return code & 0xff;
		else
			return code / 256 >>> 0;
	};

	TauFile.prototype.put_byte = function(byte, position) {
		var index = position === "end_of_stream" ? this.text.length : Math.floor(position/2);
		if(this.text.length < index)
			return null;
		var code = this.text.length === index ? -1 : codePointAt(this.text[Math.floor(position/2)], 0);
		if(position % 2 === 0) {
			code = code / 256 >>> 0;
			code = ((code & 0xff) << 8) | (byte & 0xff);
		} else {
			code = code & 0xff;
			code = ((byte & 0xff) << 8) | (code & 0xff);
		}
		if(this.text.length === index)
			this.text += fromCodePoint(code);
		else 
			this.text = this.text.substring(0, index) + fromCodePoint(code) + this.text.substring(index+1);
		return true;
	};

	TauFile.prototype.flush = function() {
		return true;
	};

	TauFile.prototype.close = function() {
		this.modified = Date.now() / 1000;
		return true;
	};

	TauFile.prototype.size = function() {
		return this.text.length;
	};

	// Virtual directory
	function TauDirectory(name, parent) {
		this.name = name;
		this.parent = parent;
		this.files = {};
		this.length = 0;
		this.created = Date.now() / 1000;
		this.modified = this.created;
	}

	TauDirectory.prototype.lookup = function(file) {
		if(this.files.hasOwnProperty(file))
			return this.files[file];
		return null;
	};

	TauDirectory.prototype.push = function(name, file) {
		if(!this.files.hasOwnProperty(name))
			this.length++;
		this.files[name] = file;
		this.modified = Date.now() / 1000;
	};

	TauDirectory.prototype.remove = function(name) {
		if(this.files.hasOwnProperty(name)) {
			this.length--;
			delete this.files[name];
			this.modified = Date.now() / 1000;
		}
	};

	TauDirectory.prototype.empty = function() {
		return this.length === 0;
	};

	TauDirectory.prototype.size = function() {
		return 4096;
	};

	// Virtual file system for browser
	tau_file_system = {
		// Current files
		files: new TauDirectory("/", "/", null),
		// Open file
		open: function(path, type, mode) {
			var dirs = path.replace(/\/$/, "").split("/");
			var dir = tau_file_system.files;
			var name = dirs[dirs.length-1];
			for(var i = 1; i < dirs.length-1; i++) {
				dir = dir.lookup(dirs[i]);
				if(!pl.type.is_directory(dir))
					return null;
			}
			var file = dir.lookup(name);
			if(file === null) {
				if(mode === "read")
					return null;
				file = new TauFile(name, type, dir);
				dir.push(name, file);
			} else if(!pl.type.is_file(file)) {
				return null;
			}
			if(mode === "write")
				file.text = "";
			return file;
		},
		// Get item
		get: function(path) {
			var dirs = path.replace(/\/$/, "").split("/");
			var file = tau_file_system.files;
			for(var i = 1; i < dirs.length; i++)
				if(pl.type.is_directory(file))
					file = file.lookup(dirs[i]);
				else
					return null;
			return file;
		}
	};

	// User input for browser
	tau_user_input = {
		buffer: "",
		get: function( length, _ ) {
			var text;
			while( tau_user_input.buffer.length < length ) {
				text = window.prompt();
				if( text.length === 0 )
					return "end_of_stream";
				if( text ) {
					tau_user_input.buffer += text;
				}
			}
			text = tau_user_input.buffer.substr( 0, length );
			tau_user_input.buffer = tau_user_input.buffer.substr( length );
			return text;
		},
		eof: function(_) {
			return false;
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

	// User error for browser
	tau_user_error = {
		put: function( text, _ ) {
			(console.error || console.log)( text );
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
			var fd, fs = require('fs');
			if( mode === "read" && !fs.existsSync( path ) )
				return null;
			try {
				fd = fs.openSync( path, mode[0] );
			} catch(ex) {
				return false;
			}
			return {
				get: function( length, position ) {
					var buffer = new Buffer( length );
					fs.readSync( fd, buffer, 0, length, position );
					var end_of_file = true;
					var text = buffer.toString();
					for(var i = 0; i < length && end_of_file; i++)
						end_of_file = text[i] === "\u0000";
					return end_of_file ? "end_of_stream" : buffer.toString();
				},
				eof: function(position) {
					var stats = fs.statSync(path)
					return position === stats["size"];
				},
				put: function( text, position ) {
					var buffer = Buffer.from( text );
					if( position === "end_of_stream" )
						fs.writeSync( fd, buffer );
					else if( position === "past_end_of_stream" )
						return null;
					else
						fs.writeSync( fd, buffer, 0, buffer.length, position );
					return true;
				},
				get_byte: function( position ) {
					try {
						var buffer = Buffer.alloc(1);
						var bytesRead = fs.readSync(fd, buffer, 0, 1, position);
						//var _text = buffer.toString("utf8", 0, bytesRead);
						var end_of_file = bytesRead < 1;
						return end_of_file ? "end_of_stream" : buffer.readUInt8(0);
					} catch(ex) {
						return "end_of_stream";
					}
				},
				put_byte: function(byte, position) {
					var buffer = Buffer.from([byte]);
					if(position === "end_of_stream")
						fs.writeSync(fd, buffer);
					else if(position === "past_end_of_stream")
						return null;
					else
						fs.writeSync(fd, buffer, 0, buffer.length, position);
					return true;
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
				nodejs_user_input.buffer += readlineSync.question("", {keepWhitespace: true}) + "\n";
			text = nodejs_user_input.buffer.substr( 0, length );
			nodejs_user_input.buffer = nodejs_user_input.buffer.substr( length );
			return text;
		},
		eof: function(length) {
			return false;
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

	// User error for Node.js
	nodejs_user_error = {
		put: function( text, _ ) {
			process.stderr.write( text );
			return true;
		},
		flush: function() {
			return true;
		} 
	};
	
	
	
	// COMPATITBILITY
	
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

	
	
	// PARSER

	var ERROR = 0;
	var SUCCESS = 1;

	var regex_escape = /(\\a)|(\\b)|(\\d)|(\\e)|(\\f)|(\\n)|(\\r)|(\\s)|(\\t)|(\\v)|\\x([0-9a-fA-F]+)\\|\\([0-7]+)\\|(\\\\)|(\\')|('')|(\\")|(\\`)|(\\.)|(.)/g;
	var escape_map = {"\\a": 7, "\\b": 8, "\\d": 127, "\\e": 27, "\\f": 12, "\\n": 10, "\\r": 13, "\\s": 32, "\\t": 9, "\\v": 11};
	function escape(str) {
		var stack = [];
		var _error = false;
		str.replace(regex_escape, function(match, a, b, d, e, f, n, r, s, t, v, hex, octal, back, single, dsingle, double, backquote, error, char) {
			switch(true) {
				case hex !== undefined:
					stack.push( parseInt(hex, 16) );
					return "";
				case octal !== undefined:
					stack.push( parseInt(octal, 8) );
					return "";
				case back !== undefined:
				case single !== undefined:
				case dsingle !== undefined:
				case double !== undefined:
				case backquote !== undefined:
					stack.push( codePointAt(match.substr(1),0) );
					return "";
				case char !== undefined:
					stack.push( codePointAt(char,0) );
					return "";
				case error !== undefined:
					_error = true;
				default:
					stack.push(escape_map[match]);
					return "";
			}
		});
		if(_error)
			return null;
		return stack;
	}

	// Escape atoms
	function escapeAtom(str, quote) {
		var atom = '';
		if( str === "\\" ) return null;
		if( str.length < 2 ) return str;
		try {
			str = str.replace(/((?:\\\\)+)|\\([0-7]+)\\/g, function(match, g1, g2) {
				return g1 || fromCodePoint(parseInt(g2, 8));
			});
			str = str.replace(/((?:\\\\)+)|\\x([0-9a-fA-F]+)\\/g, function(match, g1, g2) {
				return g1 || fromCodePoint(parseInt(g2, 16));
			});
			str = str.replace(/((?:\\\\)+)|\\u([0-9a-fA-F]{4})/g, function(match, g1, g2) {
				return g1 || fromCodePoint(parseInt(g2, 16));
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

	// Is graphic token
	function is_graphic_token(string) {
		return /^[#\$\&\*\+\-\.\/\:\<\=\>\?\@\^\~\\]+/.test(string);
	}

	// Regular expressions for tokens
	var rules = {
		whitespace: /^\s*(?:(?:%.*)|(?:\/\*(?:\n|\r|.)*?(?:\*\/|$))|(?:\s+))\s*/,
		variable: /^(?:[A-Z_][a-zA-Z0-9_]*)/,
		atom: /^(\!|,|;|[a-z][0-9a-zA-Z_]*|[#\$\&\*\+\-\.\/\:\<\=\>\?\@\^\~\\]+|'(?:(?:'')|(?:\\\\)|(?:\\')|[^'])*')/,
		number: /^(?:0o[0-7]+|0x[0-9a-fA-F]+|0b[01]+|0'(?:''|\\[abdefnrstv\\'"`]|\\x?\d+\\|[^\\])|\d+(?:\.\d+(?:[eE][+-]?\d+)?)?)/,
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
		var last_is_blank;

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
			last_is_blank = false;

			if(/^\n/.exec(text) !== null) {
				line++;
				start = 0;
				len++;
				text = text.replace(/\n/, "");
				last_is_blank = true;
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

			var nl = (token.value.match(/\n/g) || []).length;
			line += nl;
			if(nl > 0) 
				start = token.value.length - token.value.lastIndexOf("\n") - 1;
			token.line_count = line;
			token.line_position = start;

			switch(token.name) {
				case "atom":
					token.raw = token.value;
					if(token.value.charAt(0) === "'") {
						token.value = escapeAtom( token.value.substring(1, token.value.length - 1), "'" );
						if( token.value === null ) {
							token.name = "lexical";
							token.value = token.raw;
							token.error = "unknown_escape_sequence";
						}
					}
					break;
				case "number":
					var substr = token.value.substring(0,2);
					token.raw = token.value;
					token.float = substr !== "0x" && substr !== "0'" && token.value.match(/[.eE]/) !== null;
					token.value = convertNum( token.value );
					token.blank = last_is_blank;
					if(!token.float && pl.flag.bounded.value.indicator === "true/0" && token.value > pl.flag.max_integer.value.value) {
						token.name = "lexical";
						token.value = token.raw;
						token.error = "int_overflow";
					}
					break;
				case "string":
					var del = token.value.charAt(0);
					token.raw = token.value;
					token.value = escapeAtom( token.value.substring(1, token.value.length - 1), del );
					if( token.value === null ) {
						token.name = "lexical";
						token.value = token.raw;
						token.error = "unknown_escape_sequence"
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
			return {type: ERROR, derived: false, value: pl.error.syntax(tokens[start], token.error || "unexpected token")};
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
						rule = new pl.type.Rule(expr.value.args[0], body_conversion(expr.value.args[1]));
						obj = {
							value: rule,
							len: start,
							type: SUCCESS
						};
					} else if(expr.value.indicator === "-->/2") {
						rule = new pl.type.Rule(expr.value.args[0], body_conversion(expr.value.args[1]));
						rule = rule_to_dcg(rule, thread);
						rule.body = body_conversion(rule.body);
						if(!pl.type.is_rule(rule))
							return {
								value: rule,
								len: start,
								type: ERROR
							};
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
		var opts = {};
		options = options ? options : {};
		opts.success = options.success ? options.success : function(){};
		opts.error = options.error ? options.error : function(){};
		opts.from = options.from ? options.from : "$tau-js";
		opts.reconsult = options.reconsult !== undefined ? options.reconsult : true;
		opts.reconsulted = options.reconsulted === undefined ? {} : options.reconsulted;
		opts.context_module = options.context_module === undefined ? "user" : options.context_module;
		opts.initialization = options.initialization === undefined ? [] : options.initialization;
		opts.current_token = options.current_token === undefined ? 0 : options.current_token;
		opts.tokenizer = options.tokenizer === undefined ? null : options.tokenizer;
		opts.tokens = options.tokens === undefined ? null : options.tokens;
		opts.string = string;
		opts.term_expansion = false;
		var reconsulted = opts.reconsulted;
		var tokenizer = opts.tokenizer;
		var tokens = opts.tokens;
		if(tokenizer === null) {
			tokenizer = new Tokenizer(thread);
			tokenizer.new_text(string);
			opts.tokenizer = tokenizer;
			tokens = tokenizer.get_tokens(0);
			opts.tokens = tokens;
		}
		var n = opts.current_token;
		while(tokens !== null && tokens[n]) {
			var expr = parseRule(thread, tokens, n);
			opts.current_token = expr.len;
			if(expr.type === ERROR) {
				if(opts.error !== undefined)
				opts.error(new Term("throw", [expr.value]));
				return;
			} else {
				// Term expansion
				var context_module = opts.context_module;
				var term_expansion = thread.session.modules[context_module].rules["term_expansion/2"];
				if(term_expansion && term_expansion.length > 0) {
					opts.term_expansion = true;
					var n_thread = new Thread(thread.session);
					var term = expr.value.body ? new Term(":-", [expr.value.head, expr.value.body]) : expr.value.head;
					thread.session.renamed_variables = {};
					term = term.rename(thread.session);
					n_thread.query(context_module + ":term_expansion(" + term.toString({quoted: true}) + ", X).");
					n_thread.answer((function(thread, opts, reconsulted, expr) {
						return function(answer) {
							if(answer && !pl.type.is_error(answer) && pl.type.is_term(answer.links['X'])) {
								var term = answer.links['X'];
								var rule = term.indicator === ":-/2" ? new Rule(term.args[0], term.args[1]) : new Rule(term, null);
								parseProgramExpansion(thread, opts, reconsulted, {value: rule, len: expr.len, type: expr.type});
							} else {
								parseProgramExpansion(thread, opts, reconsulted, expr);
							}
						}
					})(thread, opts, reconsulted, expr));
					return;
				} else {
					opts.term_expansion = false;
					var async = parseProgramExpansion(thread, opts, reconsulted, expr);
					if(async)
						return;
					n = expr.len;
				}
			}
		}
		// run goals from initialization/1 directive
		var callback = opts.success;
		var nthread = new Thread(thread.session);
		for(var i = opts.initialization.length-1; i > 0; i--) {
			var next_callback = (function(init, callback) {
				return function(answer) {
					if(answer === null) {
						nthread.answer();
					} else if(pl.type.is_error(answer)) {
						opts.error(answer);
					} else {
						nthread.add_goal(init);
						nthread.answer(callback);
					}
				};
			})(opts.initialization[i], callback);
			callback = next_callback;
		}
		if(opts.initialization.length > 0) {
			nthread.add_goal(opts.initialization[0]);
			nthread.answer(callback);
		} else {
			callback();
		}
	}

	function parseGoalExpansion(thread, options, expr) {
		var n_thread = new Thread( thread.session );
		n_thread.__goal_expansion = true;
		var varterm = thread.next_free_variable();
		var varhead = thread.next_free_variable();
		var goal = varhead + " = " + expr.value.head + ", goal_expansion(" + expr.value.body.toString({
			quoted: true
		}) + ", " + varterm.toString({
			quoted: true
		}) + ").";
		n_thread.query(goal);
		n_thread.answer(function(answer) {
			if(answer && !pl.type.is_error(answer) && answer.links[varterm]) {
				expr.value.head = answer.links[varhead];
				expr.value.body = body_conversion(answer.links[varterm]);
				parseGoalExpansion(thread, options, expr);
			} else {
				thread.add_rule(expr.value, options);
				parseProgram(thread, options.string, options);
			}
		});
	}

	function parseQueryExpansion(thread, term, options) {
		var n_thread = new Thread(thread.session);
		n_thread.__goal_expansion = true;
		var varterm = thread.next_free_variable();
		var goal = "goal_expansion(" + term.toString({
			quoted: true
		}) + ", " + varterm.toString({
			quoted: true
		}) + ").";
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
				parseQueryExpansion(thread, body_conversion(answer.links[varterm]), options);
			} else {
				thread.add_goal(term);
				options.success(term);
				parseQuery(thread, options.string, options);
			}
		});
	}

	function parseProgramExpansion(thread, options, reconsulted, expr) {
		var async = options.term_expansion === true;
		if(expr.value.body === null && expr.value.head.indicator === "?-/1") {
			async = true;
			var n_thread = new Thread(thread.session);
			n_thread.add_goal(expr.value.head.args[0]);
			n_thread.answer(function(answer) {
				if(pl.type.is_error(answer)) {
					thread.throw_warning(answer.args[0]);
				} else if(answer === false || answer === null) {
					thread.throw_warning(pl.warning.failed_goal(expr.value.head.args[0], expr.len));
				}
				parseProgram(thread, options.string, options);
			});
		} else if(expr.value.body === null && expr.value.head.indicator === ":-/1") {
			var result = thread.run_directive(expr.value.head.args[0], options);
			async = async || (result === true);
			if(async)
				parseProgram(thread, options.string, options);
		} else {
			var context_module = options.context_module;
			var indicator = expr.value.head.indicator;
			if(expr.value.head.indicator === ":/2") {
				context_module = expr.value.head.args[0].id;
				indicator = expr.value.head.args[1].indicator;
			}
			if(!reconsulted.hasOwnProperty(context_module))
				reconsulted[context_module] = {};
			if(options.reconsult !== false && reconsulted[context_module][indicator] !== true && !thread.is_multifile_predicate(indicator)) {
				var get_module = thread.session.modules[context_module];
				if(context_module !== "system" && get_module && get_module.rules[indicator]) {
					get_module.rules[indicator] = filter(get_module.rules[indicator], function(rule) {
						return rule.dynamic;
					});
					get_module.update_indices_predicate(indicator);
				}
				reconsulted[context_module][indicator] = true;
			}
			var goal_expansion = thread.session.modules.user.rules["goal_expansion/2"];
			if(expr.value.body !== null && goal_expansion && goal_expansion.length > 0) {
				async = true;
				thread.session.renamed_variables = {};
				var origin = {
					head: function() { return expr.value.head; },
					term: function() { return expr.value.body; },
					set: function(h, p){
						expr.value.head = h;
						expr.value.body = p;
					}
				};
				parseGoalExpansion(thread, options, expr, body_conversion(expr.value.body), origin.set, origin);
			} else {
				thread.add_rule(expr.value, options);
				if(async)
					parseProgram(thread, options.string, options);
			}
		}
		return async;
	}
	
	// Parse a query
	function parseQuery(thread, string, options) {
		var opts = {};
		var callback = typeof options === "function" ? options : function(){};
		options = options === undefined || typeof options === "function" ? {} : options;
		opts.success = options.success === undefined ? callback : options.success;
		opts.error = options.error === undefined ? callback : options.error;
		opts.tokenizer = options.tokenizer === undefined ? null : options.tokenizer;
		opts.current_token = options.current_token === undefined ? 0 : options.current_token;
		opts.string = string;
		var tokenizer = opts.tokenizer;
		var n = opts.current_token;
		if(tokenizer === null) {
			tokenizer = new Tokenizer(thread);
			opts.tokenizer = tokenizer;
			tokenizer.new_text(string);
		}
		do {
			var tokens = tokenizer.get_tokens(n);
			if(tokens === null)
				break;
			var expr = parseExpr(thread, tokens, 0, thread.__get_max_priority(), false);
			if(expr.type !== ERROR) {
				var expr_position = expr.len;
				n = expr.len + 1;
				opts.current_token = n;
				if(tokens[expr_position] && tokens[expr_position].name === "atom" && tokens[expr_position].raw === ".") {
					expr.value = body_conversion(expr.value);
					// Goal expansion
					var goal_expansion = thread.session.modules.user.rules["goal_expansion/2"];
					if(!thread.__goal_expansion && goal_expansion && goal_expansion.length > 0) {
						parseQueryExpansion(thread, expr.value, opts);
						return;
					} else {
						thread.add_goal(expr.value);
						opts.success(expr.value);
					}
				} else {
					var token = tokens[expr_position];
					opts.error(
						new Term("throw", [
							pl.error.syntax(
								token ? token : tokens[expr_position-1],
								token && token.error ? token.error : ". or operator expected",
								!token
							)
						])
					);
					return;
				}
			} else {
				opts.error(new Term("throw", [expr.value]));
				return;
			}
		} while(true);
	}


	
	// UTILS

	// Rule to DCG
	function rule_to_dcg(rule, thread) {
		thread.session.renamed_variables = {};
		rule = rule.rename(thread);
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
			// add first and last variables to the head
			if(rule.head.indicator === ":/2")
				rule.head = new Term(":", [
					new Term(rule.head.args[0].id, []),
					new Term(rule.head.args[1].id, rule.head.args[1].args.concat([begin, dcg.variable]))
				]);
			else
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
		} else if( pl.type.is_term( expr ) && expr.indicator === ":/2" ) {
			var right = body_to_dcg(expr.args[1], last, thread);
			if( right.error ) return right;
			return {
				value: new Term(":", [expr.args[0], right.value]),
				variable: right.variable,
				error: false
			};
		} else if( pl.type.is_term( expr ) && expr.indicator === "\\+/1" ) {
			var left = body_to_dcg(expr.args[0], last, thread);
			if( left.error ) return left;
			free = thread.next_free_variable();
			return {
				value: new Term(",", [new Term(expr.id, [left.value]), new Term("=", [last, free])]),
				variable: free,
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
		else if( pl.type.is_term(expr) && expr.indicator === ":/2" ) {
			var body = body_conversion(expr.args[1]);
			return new Term(":", [expr.args[0], body]);
		}
		return expr;
	}
	
	// List to Prolog list
	function arrayToList( array, cons ) {
		var list = cons ? cons : new Term( "[]", [] );
		for(var i = array.length-1; i >= 0; i-- )
			list = new Term( ".", [array[i], list] );
		return list;
	}

	// Array difference
	function difference(xs, ys) {
		var zs = [];
		for(var i = 0; i < xs.length; i++) {
			if(indexOf(zs, xs[i]) === -1 && indexOf(ys, xs[i]) === -1)
				zs.push(xs[i]);
		}
		return zs;
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
	function retract(thread, point, indicator, rule, get_module) {
		if(get_module.rules[indicator]) {
			for(var i = 0; i < get_module.rules[indicator].length; i++) {
				if(get_module.rules[indicator][i] === rule) {
					get_module.rules[indicator].splice(i, 1);
					get_module.update_indices_predicate(indicator);
					thread.success( point );
					break;
				}
			}
		}
	}
	
	// call/n
	function callN(n) {
		return function(thread, point, atom) {
			var closure = atom.args[0], args = atom.args.slice(1, n);
			var module_atom;
			if(pl.type.is_term(closure) && closure.indicator === ":/2") {
				if(!pl.type.is_atom(closure.args[0])) {
					thread.throw_error(pl.error.type("module", closure.args[0], atom.indicator));
					return;
				}
				module_atom = closure.args[0];
				closure = closure.args[1];
			}
			if(pl.type.is_variable(closure)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(closure)) {
				thread.throw_error(pl.error.type("callable", closure, atom.indicator));
			} else {
				var goal = body_conversion(new Term(closure.id, closure.args.concat(args)));
				if(!pl.type.is_callable(goal)) {
					thread.throw_error(pl.error.type("callable", goal, atom.indicator));
					return;
				}
				if(module_atom)
					goal = new Term(":", [module_atom, goal]);
				thread.prepend([new State(point.goal.replace(goal), point.substitution, point)]);
			}
		};
	}
	
	// String to indicator
	function str_indicator( str ) {
		for( var i = str.length - 1; i >= 0; i-- )
			if( str.charAt(i) === "/" )
				return new Term( "/", [new Term( str.substring(0, i) ), new Num( parseInt(str.substring(i+1)), false )] );
	}

	// Greatest common divisor
	function gcd(a, b) {
		if(b === 0)
			return a;
		return Math.abs(gcd(b, a % b));
	}
	
	

	// PROLOG OBJECTS
	
	// Variables
	function Var( id ) {
		this.id = id;
		this.ground = false;
	}
	
	// Numbers
	function Num( value, is_float ) {
		this.is_float = is_float !== undefined ? is_float : Math.trunc(value) !== value;
		this.value = this.is_float ? value : Math.trunc(value);
		this.index = this.value;
		this.ground = true;
	}
	
	// Terms
	var term_ref = 0;
	function Term( id, args, ref ) {
		term_ref++;
		this.ref = ref || term_ref;
		this.id = id;
		this.args = args || [];
		this.indicator = id + "/" + this.args.length;
		this.index = this.indicator;
		this.ground = true;
		for(var i = 0; i < this.args.length; i++) {
			if(this.args[i].hasOwnProperty("ground") && this.args[i].ground === false) {
				this.ground = false;
				break;
			}
		}
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
		this.line_position = 0;
		this.line_count = 1;
		this.char_count = 0;
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
		limit = typeof limit === "number" && limit > 0 ? limit : null;
		this.rename = 0;
		this.modules = {};
		this.modules.user = new Module("user", {}, "all", {
			session: this,
			dependencies: ["system"]
		});
		this.modules.system = pl.modules.system;
		this.rules = this.modules.user.rules;
		this.total_threads = 0;
		this.renamed_variables = {};
		this.public_predicates = this.modules.user.public_predicates;
		this.multifile_predicates = this.modules.user.multifile_predicates;
		this.limit = limit;
		this.streams = {
			"user_input": new Stream(
				nodejs_flag ? nodejs_user_input : tau_user_input,
				"read", "user_input", "text", false, "reset" ),
			"user_output": new Stream(
				nodejs_flag ? nodejs_user_output : tau_user_output,
				"append", "user_output", "text", false, "reset" ),
			"user_error": new Stream(
				nodejs_flag ? nodejs_user_error : tau_user_error,
				"append", "user_error", "text", false, "reset" ),
		};
		this.file_system = nodejs_flag ? nodejs_file_system : tau_file_system;
		this.standard_input = this.streams["user_input"];
		this.standard_output = this.streams["user_output"];
		this.standard_error = this.streams["user_error"];
		this.current_input = this.streams["user_input"];
		this.current_output = this.streams["user_output"];
		this.working_directory = "/"; // only for browser
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
			1150: { "meta_predicate": ["fx"] },
			1100: { ";": ["xfy"] },
			1050: { "->": ["xfy"], "*->": ["xfy"] },
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
				"mod": ["yfx"], "<<": ["yfx"], ">>": ["yfx"], "div": ["yfx"]
			},
			200: { "**": ["xfx"], "^": ["xfy"], "-": ["fy"], "+": ["fy"], "\\": ["fy"] }
		};
		this.thread = new Thread( this );
	}
	
	// Threads
	function Thread( session ) {
		this.epoch = Date.now();
		this.session = session;
		this.session.total_threads++;
		this.format_success = session.format_success;
		this.format_error = session.format_error;
		this.total_steps = 0;
		this.cpu_time = 0;
		this.points = [];
		this.debugger = false;
		this.debugger_states = [];
		this.level = new Term("top_level");
		this.current_limit = this.session.limit;
		this.has_limit = this.session.limit !== null;
		this.warnings = [];
		this.__calls = [];
		this.__goal_expansion = false;
		this.__stacks = {};
	}
	
	// Modules
	function Module(id, rules, exports, options) {
		options = options === undefined ? {} : options;
		options.public_predicates = options.public_predicates === undefined ? {} : options.public_predicates;
		options.multifile_predicates = options.multifile_predicates === undefined ? {} : options.multifile_predicates;
		options.meta_predicates = options.meta_predicates === undefined ? {} : options.meta_predicates;
		options.session = options.session === undefined ? null : options.session;
		options.dependencies = options.dependencies === undefined ? [] : options.dependencies;
		this.id = id;
		this.rules = rules;
		this.indexed_clauses = {};
		this.non_indexable_clauses = {};
		this.public_predicates = options.public_predicates;
		this.multifile_predicates = options.multifile_predicates;
		this.meta_predicates = options.meta_predicates;
		this.src_predicates = {};
		this.dependencies = options.dependencies;
		this.exports = exports;
		this.is_library = options.session === null;
		this.modules = {};
		if(options.session) {
			options.session.modules[id] = this;
			for(var i = 0; i < options.dependencies.length; i++) {
				var lib = options.dependencies[i];
				if(!options.session.modules.hasOwnProperty(lib))
					options.session.modules[lib] = pl.modules[lib];
			}
		} else {
			pl.modules[id] = this;
		}
		if(exports !== "all") {
			for(var i = 0; i < exports.length; i++) {
				this.public_predicates[exports[i]] =
					options.public_predicates.hasOwnProperty(exports[i]) &&
					options.public_predicates[exports[i]] === true;
			}
		}
		this.update_indices_clauses();
	}
	
	// Check if a predicate is exported
	Module.prototype.exports_predicate = function(indicator) {
		return this.exports === "all" || indexOf(this.exports, indicator) !== -1;
	};

	// Check if a predicate is public
	Module.prototype.is_public_predicate = function(indicator) {
		return !this.public_predicates.hasOwnProperty(indicator) || this.public_predicates[indicator] === true;
	};
	
	// Check if a predicate is multifile
	Module.prototype.is_multifile_predicate = function( indicator ) {
		return this.multifile_predicates.hasOwnProperty(indicator) && this.multifile_predicates[indicator] === true;
	};

	// Check if a predicate is a meta-predicate
	Module.prototype.is_meta_predicate = function( indicator ) {
		if(this.meta_predicates.hasOwnProperty(indicator))
			return this.meta_predicates[indicator];
		return null;
	};

	// Update indices of all predicates
	Module.prototype.update_indices_clauses = function() {
		this.indexed_clauses = {};
		this.non_indexable_clauses = {};
		for(var indicator in this.rules)
			this.update_indices_predicate(indicator);
	};

	// Update indices of a predicate
	Module.prototype.update_indices_predicate = function(indicator) {
		this.indexed_clauses[indicator] = {};
		this.non_indexable_clauses[indicator] = [];
		if(!Array.isArray(this.rules[indicator]))
			return;
		for(var i = 0; i < this.rules[indicator].length; i++) {
			var clause = this.rules[indicator][i];
			this.add_index_predicate(clause);
		}
	};

	// Add indexed cluuse to a predicate
	Module.prototype.add_index_predicate = function(clause) {
		var indicator = clause.head.indicator;
		var index = clause.head.args.length > 0 ? clause.head.args[0].index : undefined;
		if(index) {
			if(!this.indexed_clauses.hasOwnProperty(indicator))
				this.indexed_clauses[indicator] = {};
			if(!this.indexed_clauses[indicator].hasOwnProperty(index)) {
				this.indexed_clauses[indicator][index] = [];
				if(this.non_indexable_clauses.hasOwnProperty(indicator))
					for(var j = 0; j < this.non_indexable_clauses[indicator].length; j++)
						this.indexed_clauses[indicator][index].push(this.non_indexable_clauses[indicator][j]);
			}
			this.indexed_clauses[indicator][index].push(clause);
		} else {
			if(!this.non_indexable_clauses.hasOwnProperty(indicator))
				this.non_indexable_clauses[indicator] = [];
			this.non_indexable_clauses[indicator].push(clause);
			for(var index in this.indexed_clauses[indicator])
				this.indexed_clauses[indicator][index].push(clause);
		}
	};



	// UNIFY PROLOG OBJECTS

	// Variables
	Var.prototype.unify = function(obj, occurs_check) {
		if(occurs_check && indexOf(obj.variables(), this.id) !== -1 && !pl.type.is_variable(obj))
			return null;
		var links = {};
		links[this.id] = obj;
		return new Substitution(links);
	};

	// Numbers
	Num.prototype.unify = function(obj, occurs_check) {
		if(pl.type.is_number(obj) && this.value === obj.value && this.is_float === obj.is_float)
			return new Substitution();
		return null;
	};

	// Terms
	Term.prototype.unify = function(obj, occurs_check) {
		if(!pl.type.is_term(obj) && obj.unify !== undefined) {
			return obj.unify(this, occurs_check);
		} else if(pl.type.is_term(obj) && this.indicator === obj.indicator) {
			var subs = new Substitution();
			for(var i = 0; i < this.args.length; i++) {
				var mgu = pl.unify(this.args[i].apply(subs), obj.args[i].apply(subs), occurs_check);
				if(mgu === null)
					return null;
				for(var x in mgu.links)
					subs.links[x] = mgu.links[x];
				subs = subs.apply(mgu);
			}
			return subs;
		}
		return null;
	};

	// Streams
	Stream.prototype.unify = function(obj, _occurs_check) {
		if(pl.type.is_stream(obj) && this.id === obj.id)
			return new Substitution();
		return null;
	};

	Stream.prototype.compare = function(obj) {
		if(this.id < obj.id)
			return -1;
		else if(this.id === obj.id)
			return 0;
		else
			return 1;
	};
	
	

	// PROLOG OBJECTS TO STRING
	
	// Variables
	Var.prototype.toString = function( options ) {
		options = options === undefined ? {} : options;
		if(options.variable_names) {
			var pointer = options.variable_names;
			while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
				var head = pointer.args[0];
				if(pl.type.is_term(head) && head.indicator === "=/2"
				&& pl.type.is_variable(head.args[1]) && head.args[1].id === this.id
				&& pl.type.is_atom(head.args[0]))
					return head.args[0].id;
				pointer = pointer.args[1];
			}
		}
		return this.id;
	};
	
	// Numbers
	Num.prototype.toString = function( _ ) {
		var str = this.value.toString();
		var e = str.indexOf("e");
		if(e !== -1) {
			if(str.indexOf(".") !== -1)
				return str;
			else
				return str.replace("e", ".0e");
		}
		return this.is_float && indexOf(str, ".") === -1 ? this.value + ".0" : str;
	};
	
	// Terms
	Term.prototype.toString = function( options, priority, from ) {
		options = !options ? {} : options;
		options.quoted = options.quoted === undefined ? false: options.quoted;
		options.ignore_ops = options.ignore_ops === undefined ? false : options.ignore_ops;
		options.numbervars = options.numbervars === undefined ? false : options.numbervars;
		options.variable_names = options.variable_names === undefined ? false : options.variable_names;
		priority = priority === undefined ? {priority: 1200, class: "", indicator: ""} : priority;
		from = from === undefined ? "" : from;
		var arg_priority = {priority: 999, class: "", indicator: ""};
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
				if( options.ignore_ops === false ) {
					return "{" + this.args[0].toString( options ) + "}";
				} else {
					return "{}(" + this.args[0].toString( options ) + ")";
				}
			case "./2":
				if( options.ignore_ops === false ) {
					var list = "[" + this.args[0].toString( options, arg_priority );
					var pointer = this.args[1];
					while( pointer.indicator === "./2" ) {
						list += "," + pointer.args[0].toString( options, arg_priority );
						pointer = pointer.args[1];
					}
					if( pointer.indicator !== "[]/0" ) {
						list += "|" + pointer.toString( options, arg_priority );
					}
					list += "]";
					return list;
				}
			default:
				var id = this.id;
				var operator = options.session ? options.session.lookup_operator( this.id, this.args.length ) : null;
				if( options.session === undefined || options.ignore_ops || operator === null ) {
					if( options.quoted && (! /^(!|[a-z][0-9a-zA-Z_]*|[#\$\&\*\+\-\.\/\:\<\=\>\?\@\^\~\\]+)$/.test( id ) && id !== "{}" && id !== "[]" || indexOf([".",",",";"], id) !== -1 || id.substring(0,2) === "/*") )
						id = "'" + redoEscape(id) + "'";
					if( this.args.length === 0 && is_graphic_token(this.id) && priority.indicator !== "")
						return "(" + id + ")";
					return id + (this.args.length > 0 ? "(" + map( this.args,
						function(x) { return x.toString(options, arg_priority); }
					).join(",") + ")" : "");
				} else {
					var priority_op = parseInt(operator.priority);
					var priority_arg = parseInt(priority.priority);
					var cond = priority_op > priority_arg || priority_op === priority_arg && (
						operator.class === "xfx" ||
						operator.class === "xfy" && this.indicator !== priority.indicator ||
						operator.class === "yfx" && this.indicator !== priority.indicator ||
						this.indicator === priority.indicator && operator.class === "yfx" && from === "right" ||
						this.indicator === priority.indicator && operator.class === "xfy" && from === "left" ||
						this.indicator === priority.indicator && operator.class === "xf" && from === "left" ||
						this.indicator === priority.indicator && operator.class === "fx" && from === "right");
					operator.indicator = this.indicator;
					var lpar = cond ? "(" : "";
					var rpar = cond ? ")" : "";
					var space = !(is_graphic_token(this.id) || this.id === "," || this.id === ";")
						|| operator.class.length === 2
						|| operator.class.length === 3 && pl.type.is_number(this.args[1]) && this.args[1].value < 0 ? " " : "";
					if( this.args.length === 0 ) {
						return lpar + this.id + rpar;
					} else if( ["fy","fx"].indexOf( operator.class) !== -1 ) {
						return lpar + id + space + this.args[0].toString( options, operator, "right" ) + rpar;
					} else if( ["yf","xf"].indexOf( operator.class) !== -1 ) {
						return lpar + this.args[0].toString( options, operator, "left" ) + space + id + rpar;
					} else {
						return lpar + this.args[0].toString( options, operator, "left" ) + space + this.id + space + this.args[1].toString( options, operator, "right" ) +  rpar;
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
			return this.head.toString( options, 1200, "left" ) + " :- " + this.body.toString( options, 1200, "right" ) + ".";
		}
	};
	
	// Session
	Session.prototype.toString = function( options ) {
		var str = "";
		for(var prop in this.modules) {
			if(this.modules.hasOwnProperty(prop) && this.modules[prop].is_library)
				str += ":- use_module(library(" + this.modules[prop] + ")).\n";
		}
		str += "\n";
		for(var key in this.modules.user.rules) {
			if(!this.modules.user.rules.hasOwnProperty(key)) continue;
			for(i = 0; i < this.modules.user.rules[key].length; i++) {
				str += this.modules.user.rules[key][i].toString(options);
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
		var term = new Term( this.id, map( this.args, function( arg ) {
			return arg.clone();
		} ) );
		if(this.definition_module)
			term.definition_module = this.definition_module;
		return term;
	};

	// Streams
	Stream.prototype.clone = function() {
		return new Stream( this.stream, this.mode, this.alias, this.type, this.reposition, this.eof_action );
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
		// ground
		if(this.ground)
			return new Term(this.id, this.args);
		// list
		if(this.indicator === "./2") {
			var arr = [];
			var pointer = this;
			while(pointer.indicator === "./2" && !pointer.ground) {
				var app = pointer.args[0].rename(thread);
				arr.push(app);
				pointer = pointer.args[1];
			}
			var list = pointer.rename(thread);
			for(var i = arr.length-1; i >= 0; i--)
				list = new Term(".", [arr[i], list]);
			return list;
		}
		// compound term
		var args = [];
		for(var i = 0; i < this.args.length; i++) {
			var app = this.args[i].rename(thread);
			args.push(app);
		}
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



	// CHECK IF RENAME

	// Variables
	Var.prototype.is_rename = function(obj, links) {
		links = links || {};
		if(!pl.type.is_variable(obj)
		|| links.hasOwnProperty(this.id) && links[this.id] !== obj.id
		|| links.hasOwnProperty(obj.id) && links[obj.id] !== this.id)
			return false;
		links[this.id] = obj.id;
		links[obj.id] = this.id;
		return true;
	};
	
	// Numbers
	Num.prototype.is_rename = function(obj, _links) {
		return this.equals(obj);
	};
	
	// Terms
	Term.prototype.is_rename = function(obj, links) {
		links = links || {};
		if(!pl.type.is_term(obj) || this.indicator !== obj.indicator)
			return false;
		for(var i = 0; i < this.args.length; i++) {
			if(!pl.is_rename(this.args[i], obj.args[i], links))
				return false;
		}
		return true;
	};

	// Streams
	Stream.prototype.is_rename = function(obj, _links) {
		return this.equals(obj);
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
		if(this.ground)
			return [];
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
		// ground atom
		if(this.ground)
			return this;
		// list
		if(this.indicator === "./2") {
			var arr = [];
			var pointer = this;
			while(pointer.indicator === "./2" && !pointer.ground) {
				var app = pointer.args[0].apply(subs);
				arr.push(app);
				pointer = pointer.args[1];
			}
			var list = pointer.apply(subs);
			for(var i = arr.length-1; i >= 0; i--)
				list = new Term(".", [arr[i], list]);
			return list;
		}
		// compound term
		var args = [];
		for(var i = 0; i < this.args.length; i++) {
			var app = this.args[i].apply(subs);
			args.push(app);
		}
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
		while(pl.type.is_term(pointer) && pointer.indicator === ",/2")
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

	// Push to a global stack
	Session.prototype.push_global_stack = function(stack, value) {
		return this.thread.push_global_stack(stack, value);
	};
	Thread.prototype.push_global_stack = function(stack, value) {
		if(!this.__stacks.hasOwnProperty(stack))
			this.__stacks[stack] = [];
		this.__stacks[stack].push(value);
	};

	// Pop all from a global stack
	Session.prototype.flush_global_stack = function(stack, tail) {
		return this.thread.push_global_stack(stack, tail);
	};
	Thread.prototype.flush_global_stack = function(stack, tail) {
		var list = tail || new Term("[]", []);
		if(this.__stacks.hasOwnProperty(stack)) {
			while(this.__stacks[stack].length > 0)
				list = new Term(".", [this.__stacks[stack].pop(), list]);
			delete this.__stacks[stack];
		}
		return list;
	};

	// Set max inferences
	Session.prototype.setMaxInferences = function(max) {
		this.limit = typeof max === "number" && max > 0 ? max : null;
	};
	Thread.prototype.setMaxInferences = function(max) {
		this.session.setMaxInferences(max);
		this.current_limit = this.session.limit;
		this.has_limit = this.session.limit !== null;
	};

	// Format answer
	Session.prototype.format_answer = function(answer, options) {
		return this.thread.format_answer(answer, options);
	};
	Thread.prototype.format_answer = function(answer, options) {
		return pl.format_answer(answer, this, options);
	};

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
		if(this.get_flag("nodejs").indicator === "false/0")
			path = this.absolute_file_name(path);
		return this.file_system.open( path, type, mode );
	};
	Thread.prototype.file_system_open = function( path, type, mode ) {
		return this.session.file_system_open( path, type, mode );
	};

	// Absolute file name
	Session.prototype.absolute_file_name = function(filename) {
		var absolute;
		// node.js
		if(this.get_flag("nodejs").indicator === "true/0") {
			var path = require("path");
			absolute = filename;
			for(var prop in process.env) {
				if(!process.env.hasOwnProperty(prop))
					continue;
				absolute = absolute.replace(new RegExp("\\$" + prop, "g"), process.env[prop]);
			}
			return path.resolve(absolute);
		// browser
		} else {
			var cwd = this.working_directory;
			if(filename[0] === "/")
				absolute = filename;
			else
				absolute = cwd + (cwd[cwd.length-1] === "/" ? filename : "/" + filename);
			absolute = absolute.replace(/\/\.\//g, "/");
			var dirs = absolute.split("/");
			var dirs2 = [];
			for(var i = 0; i < dirs.length; i++) {
				if(dirs[i] !== "..") {
					dirs2.push(dirs[i]);
				} else {
					if(dirs2.length !== 0)
						dirs2.pop();
				}
			}
			absolute = dirs2.join("/").replace(/\/\.$/, "/");
		}
		return absolute;
	};
	Thread.prototype.absolute_file_name = function(path, cwd) {
		return this.session.absolute_file_name(path, cwd);
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
	Session.prototype.add_rule = function(rule, options) {
		return this.thread.add_rule(rule, options);
	};
	Thread.prototype.add_rule = function(rule, options) {
		options = options ? options : {};
		options.from = options.from ? options.from : "$tau-js";
		var module_id, get_module;
		if(pl.type.is_term(rule.head) && rule.head.indicator === ":/2") {
			if(!pl.type.is_atom(rule.head.args[0])) {
				this.throw_warning(pl.error.type("module", rule.head.args[0], "top_level/0"));
				return;
			}
			module_id = rule.head.args[0].id;
			rule.head = rule.head.args[1];
		}
		if(module_id) {
			get_module = this.session.modules[module_id];
			if(!pl.type.is_module(get_module)) {
				get_module = new Module(module_id, {}, "all", {session: this.session});
				this.session.modules[module_id] = get_module;
			}
		} else {
			get_module = this.session.modules[options.context_module];
		}
		get_module.src_predicates[rule.head.indicator] = options.from;
		if(!get_module.rules.hasOwnProperty(rule.head.indicator)) {
			get_module.rules[rule.head.indicator] = [];
		}
		get_module.rules[rule.head.indicator].push(rule);
		if(!get_module.public_predicates.hasOwnProperty(rule.head.indicator))
			get_module.public_predicates[rule.head.indicator] = false;
		// update term indexing
		get_module.add_index_predicate(rule);
		return true;
	};

	// Run a directive
	Session.prototype.run_directive = function(directive, options) {
		return this.thread.run_directive(directive, options);
	};
	Thread.prototype.run_directive = function(directive, options) {
		if(pl.type.is_directive(directive)) {
			if(pl.directive[directive.indicator])
				return pl.directive[directive.indicator](this, directive, options);
			else
				return pl.directive[directive.id + "/*"](this, directive, options);
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
					if( this.__operators[p][name][i].length === arity+1 )
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
	Session.prototype.consult = function(program, options) {
		return this.thread.consult(program, options);
	};
	Thread.prototype.consult = function(program, options) {
		var string = "", success = false;
		var opts = {};
		var callback = typeof options === "function" ? options : function(){};
		options = options === undefined || typeof options === "function" ? {} : options;
		opts.context_module = options.context_module === undefined ? "user" : options.context_module;
		opts.text = options.text === undefined ? true : options.text;
		opts.html = options.html === undefined ? true : options.html;
		opts.url = options.url === undefined ? true : options.url;
		opts.file = options.file === undefined ? true : options.file;
		opts.script = options.script === undefined ? true : options.script;
		opts.success = options.success === undefined ? callback : options.success;
		opts.error = options.error === undefined ? callback : options.error;
		// string
		if(typeof program === "string") {
			string = program;
			// script id
			if(opts.script && this.get_flag("nodejs").indicator === "false/0" && program != "" && document.getElementById(string)) {
				var script = document.getElementById(string);
				var type = script.getAttribute("type");
				if(type !== null && type.replace(/ /g, "").toLowerCase() === "text/prolog") {
					string = script.text;
					success = true;
				}
			}
			// file (node.js)
			if(!success && opts.file && this.get_flag("nodejs").indicator === "true/0") {
				var fs = require("fs");
				var thread = this;
				fs.readFile(program, function(error, data) {
					if(error) {
						opts.file = false;
						thread.consult(program, opts);
					} else {
						parseProgram(thread, data.toString(), opts);
					}
				});
				return;
			}
			// http request
			if(!success && this.get_flag("nodejs").indicator === "false/0" && opts.url && program !== "" && !(/\s/.test(program))) {
				try {
					var xhttp = new XMLHttpRequest();
					var thread = this;
					xhttp.onreadystatechange = function() {
						if(this.readyState == 4) {
							if(this.status == 200) {
								string = xhttp.responseText;
								success = true;
								parseProgram(thread, string, opts);
							} else {
								opts.url = false;
								thread.consult(program, opts);
							}
						}
					}
					xhttp.open("GET", program, true);
					xhttp.send();
					return;
				} catch(ex) {
					opts.error(ex);
					return;
				}
			}
			// text
			if(!success && opts.text) {
				success = true;
			}
		// html
		} else if(opts.html && program.nodeName) {
			switch(program.nodeName.toLowerCase()) {
				case "input":
				case "textarea":
					string = program.value;
					success = true;
					break;
				default:
					string = program.innerHTML;
					success = true;
					break;
			}
		} else {
			opts.error(pl.error.existence("source_sink", new Term(string), "top_level/0"));
		}
		this.warnings = [];
		parseProgram(this, string, opts);
	};

	// Query goal from a string (without ?-)
	Session.prototype.query = function(string, options) {
		return this.thread.query(string, options);
	};
	Thread.prototype.query = function(string, options) {
		this.points = [];
		this.debugger_states = [];
		this.level = new Term("top_level");
		return parseQuery(this, string, options);
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
			while( indexOf( variables, pl.format_variable( this.session.rename, variable.id ) ) !== -1 ) {
				this.session.rename++;
			}
			if( variable.id === "_" ) {
				return new Var( pl.format_variable( this.session.rename, variable.id ) );
			} else {
				this.session.renamed_variables[variable.id] = pl.format_variable( this.session.rename, variable.id );
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
	Session.prototype.is_public_predicate = function(indicator, module_id) {
		module_id = module_id === undefined ? "user" : module_id;
		return pl.type.is_module(this.modules[module_id]) && this.modules[module_id].is_public_predicate(indicator);
	};
	Thread.prototype.is_public_predicate = function(indicator, module_id) {
		return this.session.is_public_predicate(indicator, module_id);
	};
	
	// Check if a predicate is multifile
	Session.prototype.is_multifile_predicate = function(indicator, module_id) {
		module_id = module_id === undefined ? "user" : module_id;
		return pl.type.is_module(this.modules[module_id]) && this.modules[module_id].is_multifile_predicate(indicator);
	};
	Thread.prototype.is_multifile_predicate = function(indicator, module_id) {
		return this.session.is_multifile_predicate(indicator, module_id);
	};

	// Check if a predicate is a meta-predicate
	Session.prototype.is_meta_predicate = function(indicator, module_id) {
		module_id = module_id === undefined ? "user" : module_id;
		if(pl.type.is_module(this.modules[module_id]))
			return this.modules[module_id].is_meta_predicate(indicator);
		return null;
	};
	Thread.prototype.is_meta_predicate = function(indicator, module_id) {
		return this.session.is_meta_predicate(indicator, module_id);
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
	Session.prototype.throw_error = function(error) {
		return this.thread.throw_error(error);
	};
	Thread.prototype.throw_error = function(error) {
		if(pl.type.is_variable(error))
			error = pl.error.instantiation(this.level.indicator);
		var state = new State(
			new Term("throw", [error]),
			new Substitution(),
			null
		);
		state.error = true;
		this.prepend([state]);
	};
	
	// Get the module of a predicate
	Session.prototype.lookup_module = function(atom, context_module) {
		return this.thread.lookup_module(atom, context_module);
	}
	Thread.prototype.lookup_module = function(atom, context_module) {
		var get_module = this.session.modules[context_module];
		if(!pl.type.is_module(get_module))
			get_module = this.session.modules.user;
		if(get_module.rules.hasOwnProperty(atom.indicator) && (
			get_module.exports_predicate(atom.indicator) ||
			get_module.rules.hasOwnProperty(this.level.indicator) ||
			context_module === get_module.id))
				return get_module;
		get_module.modules.system = pl.modules.system;
		get_module.modules.user = this.session.modules.user;
		for(var prop in get_module.modules) {
			if(!this.session.modules.hasOwnProperty(prop))
				continue;
			var get_module = this.session.modules[prop];
			if(get_module.rules.hasOwnProperty(atom.indicator) && (
				get_module.exports_predicate(atom.indicator) ||
				get_module.rules.hasOwnProperty(this.level.indicator) ||
				context_module === get_module.id))
					return get_module;
		}
		return null;
	};

	// Expand a meta-predicate
	Session.prototype.expand_meta_predicate = function(atom, definition_module, context_module) {
		return this.thread.expand_meta_predicate(atom, definition_module, context_module);
	};
	Thread.prototype.expand_meta_predicate = function(atom, definition_module, context_module) {
		var get_module = this.session.modules[definition_module];
		if(!get_module)
			return;
		var meta = get_module.is_meta_predicate(atom.indicator);
		if(!meta)
			return;
		for(var i = 0; i < meta.args.length; i++) {
			if(pl.type.is_integer(meta.args[i]) || pl.type.is_atom(meta.args[i]) && indexOf([":"], meta.args[i].id) !== -1) {
				if(!pl.type.is_term(atom.args[i]) || atom.args[i].indicator !== ":/2") {
					atom.args[i] = new Term(":", [new Term(context_module), atom.args[i]]);
				}
			} else if(pl.type.is_atom(meta.args[i]) && meta.args[i].id === "^") {
				var pointer_last = atom;
				var pointer_index = i;
				var pointer = atom.args[i];
				while(pl.type.is_term(pointer) && pointer.indicator === "^/2") {
					pointer_last = pointer;
					pointer_index = 1;
					pointer = pointer.args[1];
				}
				if(!pl.type.is_term(pointer) || pointer.indicator !== ":/2") {
					pointer_last.args[pointer_index] = new Term(":", [new Term(context_module), pointer]);
				}
			}
		}
	};
	
	// Resolution step
	Session.prototype.step = function() {
		return this.thread.step();
	}
	Thread.prototype.step = function() {
		if(this.points.length === 0) {
			return;
		}
		var asyn = false;
		var point = this.points.pop();
		this.current_point = point;
		if(this.debugger)
			this.debugger_states.push(point);
		var atom = pl.type.is_term(point.goal) ? point.goal.select() : point.goal;
		if(pl.type.is_term(atom) && (atom.indicator !== ":/2" || pl.type.is_term(atom.args[1]))) {
			var context_module = null;
			var states = [];
			if(atom !== null) {
				this.total_steps++;
				var level = point;
				while(level.parent !== null && level.parent.goal.search(atom))
					level = level.parent;
				if(level.parent === null) {
					this.level = new Term("top_level");
				} else {
					this.level = level.parent.goal.select();
					if(this.level.indicator === ":/2")
						this.level = this.level.args[1];
				}
				if(pl.type.is_term(atom) && atom.indicator === ":/2") {
					context_module = atom.args[0];
					atom = atom.args[1];
					if(!pl.type.is_atom(context_module)) {
						this.throw_error(pl.error.type("module", context_module, this.level.indicator));
						return;
					}
					context_module = context_module.id;
				} else {
					if(this.level.definition_module) {
						context_module = this.level.definition_module;
					} else {
						context_module = "user";
					}
				}
				atom.context_module = context_module;
				if(atom.indicator === ",/2") {
					this.prepend([new State(
						point.goal.replace(new Term(",", [
							new Term(":", [new Term(context_module), atom.args[0]]),
							new Term(":", [new Term(context_module), atom.args[1]])])),
						point.substitution,
						point
					)]);
					return;
				}
				this.__call_indicator = atom.indicator;
				var get_module = this.lookup_module(atom, context_module);
				atom.definition_module = pl.type.is_module(get_module) ? get_module.id : "user";
				this.expand_meta_predicate(atom, atom.definition_module, context_module);
				var clauses = null;
				if(get_module && atom.args.length > 0 && atom.args[0].index && get_module.indexed_clauses.hasOwnProperty(atom.indicator) && get_module.indexed_clauses[atom.indicator].hasOwnProperty(atom.args[0].index))
					clauses = get_module.indexed_clauses[atom.indicator][atom.args[0].index];
				else
					clauses = get_module === null ? null : get_module.rules[atom.indicator];
				if(clauses === null) {
					if(!this.session.modules.user.rules.hasOwnProperty(atom.indicator)) {
						if(this.get_flag("unknown").id === "error") {
							this.throw_error( pl.error.existence( "procedure", atom.indicator, this.level.indicator));
						} else if(this.get_flag("unknown").id === "warning") {
							this.throw_warning("unknown procedure " + atom.indicator + " (from " + this.level.indicator + ")");
						}
					}
				} else if(clauses instanceof Function) {
					asyn = clauses(this, point, atom);
				} else {
					// Goal expansion
					if(this.__goal_expansion && atom.indicator === "goal_expansion/2")
						clauses = clauses.concat(pl.builtin.rules["goal_expansion/2"]);
					for(var i = 0; i < clauses.length; i++) {
						this.session.renamed_variables = {};
						var clause = clauses[i].rename(this);
						var occurs_check = this.get_flag("occurs_check").indicator === "true/0";
						var mgu = pl.unify(atom, clause.head, occurs_check);
						if(mgu !== null) {
							var state = new State();
							state.goal = point.goal.replace(clause.body);
							if(state.goal !== null)
								state.goal = state.goal.apply(mgu);
							state.substitution = point.substitution.apply(mgu);
							state.parent = point;
							states.push(state);
						}
					}
					this.prepend(states);
				}
			}
		} else {
			var term = pl.type.is_term(atom) && atom.indicator === ":/2" ? atom.args[1] : atom;
			if(pl.type.is_variable(term))
				this.throw_error(pl.error.instantiation(this.level.indicator));
			else
				this.throw_error(pl.error.type("callable", term, this.level.indicator));
		}
		return asyn;
	};
	
	// Find next computed answer
	Session.prototype.answer = function(options) {
		return this.thread.answer(options);
	};
	Thread.prototype.answer = function(options) {
		var opts = {};
		options = options || function() {};
		if(typeof options === "function") {
			opts = {
				success: options,
				error: options,
				fail: options,
				limit: options
			};
		} else {
			opts.success = options.success === undefined ? function() {} : options.success;
			opts.error = options.error === undefined ? function() {} : options.error;
			opts.fail = options.fail === undefined ? function() {} : options.fail;
			opts.limit = options.limit === undefined ? function() {} : options.limit;
		}
		this.__calls.push(opts);
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
		var answers = max === undefined ? 1000 : max;
		var thread = this;
		if( answers <= 0 ) {
			if(after)
				after();
			return;
		}
		this.answer( function( answer ) {
			callback( answer );
			if( answer !== false ) {
				setTimeout( function() {
					thread.answers( callback, answers-1, after );
				}, 0 );
			} else if(after) {
				after();
			}
		} );
	};

	// Again finding next computed answer
	Session.prototype.again = function(reset_limit) {
		return this.thread.again(reset_limit);
	};
	Thread.prototype.again = function(reset_limit) {
		while(this.__calls.length > 0) {
			this.warnings = [];
			if(reset_limit !== false)
				this.current_limit = this.session.limit;
			while((!this.has_limit || this.current_limit > 0) && this.points.length > 0 && this.head_point().goal !== null && !pl.type.is_error_state(this.head_point())) {
				if(this.has_limit)
					this.current_limit--;
				var t0 = Date.now();
				var asyn = this.step();
				var t1 = Date.now();
				this.cpu_time += t1-t0;
				if(asyn === true)
					return;
			}
			var call = this.__calls.shift();
			// limit of inferences
			if(this.has_limit && this.current_limit <= 0) {
				(function(call) {
					return setTimeout(function() {
						call.limit(null);
					}, 0);
				})(call);
			// no answer
			} else if(this.points.length === 0) {
				(function(call) {
					return setTimeout(function() {
						call.fail(false);
					}, 0);
				})(call);
			// error
			} else if(pl.type.is_error(this.head_point().goal)) {
				var error = this.format_error(this.points.pop());
				this.points = [];
				(function(error, call) {
					return setTimeout(function() {
						call.error(error);
					}, 0);
				})(error, call);
			// computed answer
			} else {
				if(this.debugger)
					this.debugger_states.push(this.head_point());
				var answer = this.format_success(this.points.pop());
				(function(answer, call) {
					return setTimeout(function() {
						call.success(answer);
					}, 0);
				})(answer, call);
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
		var rules = this.modules.user.rules[head.indicator];
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
		return pl.error.instantiation( thread.level.indicator );
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
		for( var _indicator in this.modules.user.rules ) {
			if(!this.modules.user.rules.hasOwnProperty(_indicator)) continue;
			var indicator = this.modules.user.rules[_indicator];
			rules = [];
			str = "\"" + _indicator + "\": [";
			for(var i = 0; i < indicator.length; i++) {
				rules.push(indicator[i].compile());
			}
			str += rules.join();
			str += "]";
			obj.push( str );
		}
		return "{" + obj.join() + "};";
	};

	// Module
	Module.prototype.compile = function() {
		var length = 0;
		var dependencies = 0;
		var str = "var pl;\n";
		str += "(function(pl) {\n";
		// name
		str += "\tvar name = \"" + this.id + "\";\n";
		// predicates
		str += "\tvar predicates = function() {\n";
		str += "\t\treturn {\n";
		for(var prop in this.rules) {
			if(length > 0)
				str += ",\n";
			str += "\t\t\t\"" + prop + "\": ";
			if(typeof this.rules[prop] === "function") {
				str += this.rules[prop];
			} else {
				str += "[\n";
				for(var i = 0; i < this.rules[prop].length; i++) {
					str += "\t\t\t\t" + this.rules[prop][i].compile();
					if(i < this.rules[prop].length-1)
						str += ",";
					str += "\n";
				}
				str += "\t\t\t]";
			}
			length++;
		}
		str += "\n\t\t};\n";
		str += "\t};\n";
		// exports
		str += "\tvar exports = [";
		for(var i = 0; i < this.exports.length; i++) {
			if(i > 0)
				str += ", ";
			str += "\"" + this.exports[i] + "\"";
		}
		str += "];\n";
		// options
		str += "\tvar options = function() {\n";
		str += "\t\treturn {\n";
		// dependencies
		str += "\t\t\tdependencies: [";
		for(var prop in this.modules) {
			if(dependencies > 0)
				str += ", ";
			str += "\"" + prop + "\"";
			dependencies++;
		}
		str += "]\n";
		str += "\t\t};\n";
		str += "};\n";
		// fixed code
		str += "\tif(typeof module !== 'undefined') {\n";
		str += "\t\tmodule.exports = function(p) {\n";
		str += "\t\t\tpl = p;\n";
		str += "\t\t\tnew pl.type.Module(name, predicates(), exports, options());\n";
		str += "\t\t};\n";
		str += "\t} else {\n";
		str += "\t\tnew pl.type.Module(name, predicates(), exports, options());\n";
		str += "\t}\n";
		str += "})(pl);\n";
		return str;
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
	Term.prototype.toJavaScript = function(options) {
		// Atom => String
		if( this.args.length === 0 && this.indicator !== "[]/0" ) {
			return this.toString(options);
		} else if( pl.type.is_list( this ) ) {
			// List => Array
			var all_obj = true;
			var arr = [];
			var obj = {};
			var pointer = this;
			var value;
			while( pointer.indicator === "./2" ) {
				value = pointer.args[0].toJavaScript(options);
				arr.push( value );
				all_obj = all_obj && pl.type.is_term(pointer.args[0]) && pointer.args[0].indicator === "-/2" && pl.type.is_atom(pointer.args[0].args[0]);
				if(all_obj)
					obj[pointer.args[0].args[0].id] = pointer.args[0].args[1].toJavaScript(options);
				pointer = pointer.args[1];
			}
			if( pointer.indicator === "[]/0" )
				return all_obj && arr.length > 0 ? obj : arr;

		}
		return this.toString(options);
	};
	
	
	
	// RULES
	
	// Return singleton variables in the session
	Rule.prototype.singleton_variables = function(include_named) {
		include_named = include_named || false;
		var variables = this.head.variables();
		var count = {};
		var singleton = [];
		if(this.body !== null)
			variables = variables.concat(this.body.variables());
		for(var i = 0; i < variables.length; i++) {
			if(count[variables[i]] === undefined)
				count[variables[i]] = 0;
			count[variables[i]]++;
		}
		for(var key in count) {
			if(!count.hasOwnProperty(key))
				continue;
			if(count[key] === 1) {
				var charcode = codePointAt(key, 1);
				if(!include_named || key === "_")
					if(key === "_" || key[0] === "_" && (charcode === 95 || charcode >= 65 && charcode <= 90))
						continue;
				singleton.push(key);
			}
		}
		return singleton;
	};



	// NODEJS

	var nodejs_flag = typeof process !== 'undefined' && !process.browser

	var nodejs_arguments = nodejs_flag ?
		arrayToList( map(process.argv.slice(1), function(arg) { return new Term( arg ); })) :
		new Term("[]", []);
	
	
	
	// PROLOG

	var pl = {
		
		// Environment
		__env: nodejs_flag ? global : window,
		
		// Modules
		modules: {},
		
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
			fromCodePoint: fromCodePoint,
			// Length of string
			stringLength: stringLength
			
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
			File: TauFile,
			Directory: TauDirectory,
			
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
				return obj instanceof Term
				&& (indexOf([",/2",";/2","->/2"], obj.indicator) === -1
				|| pl.type.is_callable(obj.args[0]) && pl.type.is_callable(obj.args[1]))
				|| obj instanceof Var;
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
				return obj instanceof Term && obj.args.length === 0 && (obj.id.length === 1 || obj.id.length > 0 && obj.id.length <= 2 && codePointAt( obj.id, 0 ) >= 65536);
			},
			
			// Is a in_character
			is_in_character: function( obj ) {
				return obj instanceof Term && (obj.indicator === "end_of_file/0"
				|| obj.id.length === 1
				|| obj.id.length > 0 && obj.id.length <= 2 && codePointAt(obj.id, 0) >= 65536);
			},
			
			// Is a character_code
			is_character_code: function( obj ) {
				return obj instanceof Num && !obj.is_float && obj.value >= 0 && obj.value <= 1114111;
			},
			
			// Is a in_character_code
			is_in_character_code: function( obj ) {
				return obj instanceof Num && !obj.is_float && obj.value >= -1 && obj.value <= 1114111;
			},

			// Is a byte
			is_byte: function( obj ) {
				return obj instanceof Num && !obj.is_float && obj.value >= 0 && obj.value <= 255;
			},

			// Is a in_byte
			is_in_byte: function( obj ) {
				return obj instanceof Num && !obj.is_float && obj.value >= -1 && obj.value <= 255;
			},
			
			// Is an operator
			is_operator: function( obj ) {
				return obj instanceof Term && pl.arithmetic.evaluation[obj.indicator];
			},
			
			// Is a directive
			is_directive: function( obj ) {
				return obj instanceof Term && (pl.directive[obj.indicator] !== undefined || pl.directive[obj.id + "/*"] !== undefined);
			},
			
			// Is a built-in predicate
			is_builtin: function( obj ) {
				return obj instanceof Term && pl.builtin.rules.hasOwnProperty(obj.indicator) && obj.indicator !== "goal_expansion/2";
			},
			
			// Is an error
			is_error: function( obj ) {
				return obj instanceof Term && obj.indicator === "throw/1";
			},

			// Is an error state
			is_error_state: function( obj ) {
				return pl.type.is_state( obj ) && obj.error && obj.error === true;
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
				return pl.type.is_term(obj) && (
					obj.indicator === "end_of_stream/0" ||
					obj.indicator === "past_end_of_stream/0" ||
					obj.indicator === "position/3"
						&& pl.type.is_integer(obj.args[0])
						&& pl.type.is_integer(obj.args[1])
						&& pl.type.is_integer(obj.args[2])
				)
			},

			// Is a stream property
			is_stream_property: function( obj ) {
				return pl.type.is_term( obj ) && (
					obj.indicator === "input/0" || 
					obj.indicator === "output/0" || 
					obj.indicator === "alias/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom( obj.args[0] )) ||
					obj.indicator === "file_name/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom( obj.args[0] )) ||
					obj.indicator === "reposition/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false")) ||
					obj.indicator === "type/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "text" || obj.args[0].id === "binary")) ||
					obj.indicator === "mode/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "read" || obj.args[0].id === "write" || obj.args[0].id === "append")) ||
					obj.indicator === "eof_action/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "error" || obj.args[0].id === "eof_code" || obj.args[0].id === "reset")) ||
					obj.indicator === "end_of_stream/1" && (pl.type.is_variable( obj.args[0] ) || pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "at" || obj.args[0].id === "past" || obj.args[0].id === "not")) ||
					obj.indicator === "position/1"
						&& (pl.type.is_variable(obj.args[0]) || pl.type.is_term(obj.args[0]) && obj.args[0].indicator === "position/3"
							&& (pl.type.is_variable(obj.args[0].args[0]) || pl.type.is_integer(obj.args[0].args[0]))
							&& (pl.type.is_variable(obj.args[0].args[1]) || pl.type.is_integer(obj.args[0].args[1]))
							&& (pl.type.is_variable(obj.args[0].args[2]) || pl.type.is_integer(obj.args[0].args[2])))
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
					obj.indicator === "numbervars/1" && pl.type.is_atom(obj.args[0]) && (obj.args[0].id === "true" || obj.args[0].id === "false") ||
					obj.indicator === "variable_names/1" && pl.type.is_fully_list(obj.args[0])
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

			// Is a module
			is_module: function( obj ) {
				return obj instanceof Module;
			},

			// Is a virtual file
			is_file: function( obj ) {
				return obj instanceof TauFile;
			},

			// Is a virtual directory
			is_directory: function( obj ) {
				return obj instanceof TauDirectory;
			},

			// Is a predicate property
			is_predicate_property: function(obj) {
				return pl.type.is_term(obj) && (
					obj.indicator === "built_in/0" ||
					obj.indicator === "static/0" ||
					obj.indicator === "dynamic/0" ||
					obj.indicator === "native_code/0" ||
					obj.indicator === "multifile/0" ||
					obj.indicator === "meta_predicate/1"
				);
			},

			// Is a meta-argument specifier
			is_meta_argument_specifier: function(obj) {
				return pl.type.is_integer(obj) && obj.value >= 0 ||
					pl.type.is_atom(obj) && indexOf(["+", "-", "?", "*", "^", ":", "//"], obj.id) !== -1;
			},

			// Is a time property
			is_time_property: function( obj ) {
				return pl.type.is_term(obj) && obj.args.length === 1 
				&& (pl.type.is_variable(obj.args[0]) || pl.type.is_integer(obj.args[0]))
				&& indexOf(["year", "month", "day", "hours", "minutes", "seconds", "milliseconds", "weekday"], obj.id) !== -1;
			},
			
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
					fn: function( x, thread ) { return Math.abs(x) <= 1 ? Math.asin(x) : pl.error.evaluation("undefined", thread.__call_indicator); }
				},
				"acos/1": {
					type_args: null,
					type_result: true,
					fn: function( x, thread ) { return Math.abs(x) <= 1 ? Math.acos(x) : pl.error.evaluation("undefined", thread.__call_indicator); }
				},
				"atan/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.atan( x ); }
				},
				"atan2/2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, thread ) { return x === 0 && y === 0 ? pl.error.evaluation("undefined", thread.__call_indicator) : Math.atan2(x, y); }
				},
				"acosh/1": {
					type_args: null,
					type_result: true,
					fn: function( x, thread ) { return x >= 1 ? Math.acosh(x) : pl.error.evaluation("undefined", thread.__call_indicator); }
				},
				"asinh/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.asinh( x ); }
				},
				"atanh/1": {
					type_args: null,
					type_result: true,
					fn: function( x, thread ) { return Math.abs(x) < 1 ? Math.atanh(x) : pl.error.evaluation("undefined", thread.__call_indicator); }
				},
				"cosh/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.cosh( x ); }
				},
				"sinh/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.sinh( x ); }
				},
				"tanh/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.tanh( x ); }
				},
				"exp/1": {
					type_args: null,
					type_result: true,
					fn: function( x, _ ) { return Math.exp( x ); }
				},
				"sqrt/1": {
					type_args: null,
					type_result: true,
					fn: function( x, thread ) { return x >= 0 ? Math.sqrt( x ) : pl.error.evaluation( "undefined", thread.__call_indicator ); }
				},
				"log/1": {
					type_args: null,
					type_result: true,
					fn: function( x, thread ) { return x > 0 ? Math.log( x ) : pl.error.evaluation( "undefined", thread.__call_indicator ); }
				},
				"log/2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, thread ) { return x > 0 && y > 0 ? Math.log(y)/Math.log(x) : pl.error.evaluation( "undefined", thread.__call_indicator ); }
				},
				"log10/1": {
					type_args: null,
					type_result: true,
					fn: function( x, thread ) { return x > 0 ? Math.log(x)/Math.log(10) : pl.error.evaluation( "undefined", thread.__call_indicator ); }
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
					fn: function( x, y, thread ) { return y ? x / y : pl.error.evaluation( "zero_divisor", thread.__call_indicator ); }
				},
				"///2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, thread ) { return y ? Math.trunc( x / y ) : pl.error.evaluation( "zero_divisor", thread.__call_indicator ); }
				},
				"div/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, thread ) { return y ? Math.floor( x / y ) : pl.error.evaluation( "zero_divisor", thread.__call_indicator ); }
				},
				"**/2": {
					type_args: null,
					type_result: true,
					fn: function( x, y, thread ) { return x == 0 && y < 0 ? pl.error.evaluation("zero_divisor", thread.__call_indicator) : Math.pow(x, y); }
				},
				"^/2": {
					type_args: null,
					type_result: null,
					fn: function( x, y, thread ) { return x == 0 && y < 0 ? pl.error.evaluation("zero_divisor", thread.__call_indicator) : Math.pow(x, y); }
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
					fn: function( x, y, thread ) { return y ? x % y : pl.error.evaluation( "zero_divisor", thread.__call_indicator ); }
				},
				"mod/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, thread ) { return y ? x - Math.floor( x / y ) * y : pl.error.evaluation( "zero_divisor", thread.__call_indicator ); }
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
				},
				"gcd/2": {
					type_args: false,
					type_result: false,
					fn: function( x, y, _ ) { return gcd(x, y); }
				}
				
			}
			
		},
		
		// Directives
		directive: {
			
			// dynamic/1
			"dynamic/1": function( thread, atom, options ) {
				var indicators = atom.args[0];
				if(!pl.type.is_list(indicators))
					indicators = arrayToList([indicators]);
				var pointer = indicators;
				while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
					indicator = pointer.args[0];
					if( pl.type.is_variable( indicator ) ) {
						thread.throw_warning( pl.error.instantiation( atom.indicator ) );
					} else if( !pl.type.is_compound( indicator ) || indicator.indicator !== "//2" ) {
						thread.throw_warning( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
					} else if( pl.type.is_variable( indicator.args[0] ) || pl.type.is_variable( indicator.args[1] ) ) {
						thread.throw_warning( pl.error.instantiation( atom.indicator ) );
					} else if( !pl.type.is_atom( indicator.args[0] ) ) {
						thread.throw_warning( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
					} else if( !pl.type.is_integer( indicator.args[1] ) ) {
						thread.throw_warning( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
					} else {
						var key = indicator.args[0].id + "/" + indicator.args[1].value;
						var get_module = thread.session.modules[options.context_module];
						get_module.public_predicates[key] = true;
						if( !get_module.rules[key] )
						get_module.rules[key] = [];
					}
					pointer = pointer.args[1];
				}
				if(pl.type.is_variable(pointer)) {
					thread.throw_warning( pl.error.instantiation( atom.indicator ) );
				} else if(!pl.type.is_term(pointer) || pointer.indicator !== "[]/0") {
					thread.throw_warning( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				}
			},

			// dynamic/[2..]
			"dynamic/*": function( thread, atom ) {
				for(var i = 0; i < atom.args.length; i++) {
					pl.directive["dynamic/1"](thread, new Term("dynamic", [atom.args[i]]));
				}
			},
			
			// multifile/1
			"multifile/1": function( thread, atom, options ) {
				var indicator = atom.args[0];
				if( pl.type.is_variable( indicator ) ) {
					thread.throw_warning( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_compound( indicator ) || indicator.indicator !== "//2" ) {
					thread.throw_warning( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
				} else if( pl.type.is_variable( indicator.args[0] ) || pl.type.is_variable( indicator.args[1] ) ) {
					thread.throw_warning( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( indicator.args[0] ) ) {
					thread.throw_warning( pl.error.type( "atom", indicator.args[0], atom.indicator ) );
				} else if( !pl.type.is_integer( indicator.args[1] ) ) {
					thread.throw_warning( pl.error.type( "integer", indicator.args[1], atom.indicator ) );
				} else {
					var predicate_indicator = atom.args[0].args[0].id + "/" + atom.args[0].args[1].value;
					var get_module = thread.session.modules[options.context_module];
					get_module.multifile_predicates[predicate_indicator] = true;
					if(!get_module.rules.hasOwnProperty(predicate_indicator)) {
						get_module.rules[predicate_indicator] = [];
						get_module.public_predicates[predicate_indicator] = false;
					}
				}
			},

			// meta_predicate/1
			"meta_predicate/1": function(thread, atom, options) {
				var options = options === undefined ? {} : options;
				var head = atom.args[0];
				if( pl.type.is_variable(head) ) {
					thread.throw_warning(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_callable(head)) {
					thread.throw_warning(pl.error.type("callable", head, atom.indicator));
				} else {
					for(var i = 0; i < head.args.length; i++) {
						var arg = head.args[i];
						if(!pl.type.is_meta_argument_specifier(arg)) {
							thread.throw_warning(pl.error.type("meta_argument_specifier", arg, atom.indicator));
							return;
						}
					}
					thread.session.modules[options.context_module].meta_predicates[head.indicator] = head;
				}
			},
			
			// set_prolog_flag
			"set_prolog_flag/2": function( thread, atom ) {
				var flag = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( flag ) || pl.type.is_variable( value ) ) {
					thread.throw_warning( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( flag ) ) {
					thread.throw_warning( pl.error.type( "atom", flag, atom.indicator ) );
				} else if( !pl.type.is_flag( flag ) ) {
					thread.throw_warning( pl.error.domain( "prolog_flag", flag, atom.indicator ) );
				} else if( !pl.type.is_modifiable_flag( flag ) ) {
					thread.throw_warning( pl.error.permission( "modify", "flag", flag, atom.indicator ) );
				} else if( !pl.type.is_value_flag( flag, value ) ) {
					thread.throw_warning( pl.error.domain( "flag_value", new Term( "+", [flag, value] ), atom.indicator ) );
				} else {
					thread.session.flag[flag.id] = value;
				}
			},

			// module/2
			"module/2": function(thread, atom, options) {
				var options = options === undefined ? {} : options;
				options.context_module = options.context_module === undefined ? "user" : options.context_module;
				var module_id = atom.args[0], exports = atom.args[1];
				if(pl.type.is_variable(module_id) || pl.type.is_variable(exports)) {
					thread.throw_warning(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(module_id)) {
					thread.throw_warning(pl.error.type("atom", module_id, atom.indicator));
				} else if(!pl.type.is_list(exports)) {
					thread.throw_warning(pl.error.type("list", exports, atom.indicator));
				} else {
					if(!pl.type.is_module(thread.session.modules[module_id.indicator])) {
						var pointer = exports;
						var indicators = [];
						while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
							var predicate = pointer.args[0];
							if(!pl.type.is_predicate_indicator(predicate)) {
								thread.throw_warning(pl.error.type("predicate_indicator", predicate, atom.indicator));
							} else {
								indicators.push(predicate.args[0].id + "/" + predicate.args[1].value);
							}
							pointer = pointer.args[1];
						}
						if(pl.type.is_variable(pointer)) {
							thread.throw_warning(pl.error.instantiation(atom.indicator));
						} else if(!pl.type.is_empty_list(pointer)) {
							thread.throw_warning(pl.error.type("list", exports, atom.indicator));
						}
						var new_module = new Module(module_id.id, {}, indicators, {
							session: thread.session
						});
						thread.session.modules[module_id.id] = new_module;
						thread.session.modules[options.context_module].modules[module_id.id] = new_module;
						options.context_module = module_id.id;
					} else {
						thread.throw_warning(pl.error.permission("create", "module", module_id, atom.indicator));
					}
				}
			},
			
			// use_module/1
			"use_module/1": function(thread, atom, options) {
				var options = options === undefined ? {} : options;
				options.context_module = options.context_module === undefined ? "user" : options.context_module;
				var module_id = atom.args[0];
				if(pl.type.is_variable(module_id)) {
					thread.throw_warning(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_term(module_id)) {
					thread.throw_warning(pl.error.type("term", module_id, atom.indicator));
				} else {
					if(module_id.indicator === "library/1") {
						var name = module_id.args[0].id;
						var get_module = pl.modules[name];
						if(pl.type.is_module(get_module)) {
							if(!thread.session.modules[options.context_module].modules.hasOwnProperty(name)) {
								thread.session.modules[name] = get_module;
								thread.session.modules[options.context_module].modules[name] = get_module;
								for(var i = 0; i < get_module.dependencies.length; i++) {
									var term = new Term("use_module", [new Term("library", [new Term(get_module.dependencies[i])])]);
									pl.directive["use_module/1"](thread, term, {
										context_module: name
									});
								}
							}
						} else {
							thread.throw_warning(pl.error.existence("module", module_id, atom.indicator));
						}
					} else {
						var name = module_id.id;
						thread.consult(name, {
							context_module: options.context_module,
							text: false,
							success: function() {
								parseProgram(thread, options.string, options);
							},
							error: function() {
								options.error(pl.error.existence("module", module_id, atom.indicator));
							}
						});
						return true;
					}
				}
			},
			
			// char_conversion/2
			"char_conversion/2": function(thread, atom, options) {
				var inchar = atom.args[0], outchar = atom.args[1];
				if(pl.type.is_variable(inchar) || pl.type.is_variable(outchar)) {
					thread.throw_warning(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_character(inchar)) {
					thread.throw_warning(pl.error.type("character", inchar, atom.indicator));
				} else if(!pl.type.is_character(outchar)) {
					thread.throw_warning(pl.error.type("character", outchar, atom.indicator));
				} else {
					if(inchar.id === outchar.id) {
						delete thread.session.__char_conversion[inchar.id];
					} else {
						thread.session.__char_conversion[inchar.id] = outchar.id;
					}
					options.tokens = options.tokenizer.get_tokens(options.current_token);
					options.current_token = 0;
					return true;
				}
			},
			
			// op/3
			"op/3": function( thread, atom ) {
				var priority = atom.args[0], type = atom.args[1], operators = atom.args[2];
				if(pl.type.is_atom(operators))
					operators = new Term(".", [operators, new Term("[]")]);
				if( pl.type.is_variable( priority ) || pl.type.is_variable( type ) || pl.type.is_variable( operators ) ) {
					thread.throw_warning( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_integer( priority ) ) {
					thread.throw_warning( pl.error.type( "integer", priority, atom.indicator ) );
				} else if( !pl.type.is_atom( type ) ) {
					thread.throw_warning( pl.error.type( "atom", type, atom.indicator ) );
				} else if( !pl.type.is_list( operators ) ) {
					thread.throw_warning( pl.error.type( "list", operators, atom.indicator ) );
				} else if( pl.type.is_empty_list( operators ) ) {
					thread.throw_warning( pl.error.permission( "create", "operator", operators, atom.indicator ) );
				} else {
					var pointer = operators;
					while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
						var operator = pointer.args[0];
						pointer = pointer.args[1];
						if( pl.type.is_variable( operator ) ) {
							thread.throw_warning( pl.error.instantiation( atom.indicator ) );
						} else if( !pl.type.is_atom( operator ) ) {
							thread.throw_warning( pl.error.type( "atom", operator, atom.indicator ) );
						} else if( priority.value < 0 || priority.value > 1200 ) {
							thread.throw_warning( pl.error.domain( "operator_priority", priority, atom.indicator ) );
						} else if( operator.id === "," ) {
							thread.throw_error( pl.error.permission( "modify", "operator", operator, atom.indicator ) );
						} else if( operator.id === "{}" ) {
							thread.throw_warning( pl.error.permission( "create", "operator", operator, atom.indicator ) );
						} else if( operator.id === "[]" ) {
							thread.throw_warning( pl.error.permission( "create", "operator", operator, atom.indicator ) );
						} else if( operator.id === "|" && priority.value !== 0 && (priority.value < 1001 || type.id.length !== 3 ) ) {
							thread.throw_warning( pl.error.permission( "create", "operator", operator, atom.indicator ) );
						} else if( ["fy", "fx", "yf", "xf", "xfx", "yfx", "xfy"].indexOf( type.id ) === -1 ) {
							thread.throw_warning( pl.error.domain( "operator_specifier", type, atom.indicator ) );
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
							if(fix.infix && current_class === "postfix" || fix.postfix && current_class === "infix") {
								thread.throw_warning( pl.error.permission( "create", "operator", operator, atom.indicator ) );
							} else {
								if( fix[current_class] ) {
									remove( thread.session.__operators[fix[current_class].priority][operator.id], fix[current_class].type );
									if( thread.session.__operators[fix[current_class].priority][operator.id].length === 0 ) {
										delete thread.session.__operators[fix[current_class].priority][operator.id];
									}
								}
								if( priority.value > 0 ) {
									if( !thread.session.__operators[priority.value] ) thread.session.__operators[priority.value.toString()] = {};
									if( !thread.session.__operators[priority.value][operator.id] ) thread.session.__operators[priority.value][operator.id] = [];
									thread.session.__operators[priority.value][operator.id].push( type.id );
								}
							}
						}
					}
					if(pl.type.is_variable(pointer)) {
						thread.throw_warning( pl.error.instantiation( atom.indicator ) );
						return;
					} else if(!pl.type.is_term(pointer) || pointer.indicator !== "[]/0") {
						thread.throw_warning( pl.error.type( "list", operators, atom.indicator ) );
						return;
					}
				}
			},

			// initialization/1
			"initialization/1": function(thread, atom, options) {
				var goal = atom.args[0];
				options.initialization.push(goal);
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
				value: new Term( "chars" ),
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
				allowed: [new Term( "true" ), new Term( "false" )],
				value: new Term( nodejs_flag ? "true" : "false" ),
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
		unify: function(t1, t2, occurs_check) {
			occurs_check = occurs_check === undefined ? false : occurs_check;
			var left = Array.isArray(t1) ? t1 : [t1];
			var right = Array.isArray(t2) ? t2 : [t2];
			if(left.length !== right.length)
				return null;
			var subs = new Substitution();
			while(left.length > 0) {
				var s = left.pop();
				var t = right.pop();
				if(pl.type.is_variable(s))
					s = s.apply(subs);
				if(pl.type.is_variable(t))
					t = t.apply(subs);
				// same object
				if(s == t)
					continue;
				// compound terms
				if(pl.type.is_term(s) && pl.type.is_term(t)) {
					if(s.indicator !== t.indicator)
						return null;
					for(var i = s.args.length-1; i >= 0; i--) {
						left.push(s.args[i]);
						right.push(t.args[i]);
					}
				// numbers
				} else if(pl.type.is_number(s) && pl.type.is_number(t)) {
					if(s.value !== t.value || s.is_float !== t.is_float)
						return null;
				// variable - term
				} else if(pl.type.is_variable(s)) {
					t = t.apply(subs);
					// x = x
					if(pl.type.is_variable(t) && s.id === t.id)
						continue;
					// occurs check
					if(occurs_check === true && indexOf(t.variables(), s.id) !== -1)
						return null;
					// anonymous variable
					if(s.id !== "_")
						subs.add(s.id, t);
				// term - variable
				} else if(pl.type.is_variable(t)) {
					left.push(t);
					right.push(s);
				// user-defined terms
				} else if(s.unify !== undefined) {
					var user_subs = s.apply(subs).unify(t.apply(subs), occurs_check);
					if(user_subs == null)
						return null;
					for(var i in user_subs.links)
						subs.add(i, user_subs.links[i]);
				} else {
					return null;
				}
			}
			return subs.apply(subs);
		},

		// Is rename
		is_rename: function(obj1, obj2, links) {
			links = links || {};
			if(obj1.is_rename && obj2.is_rename)
				return obj1.is_rename(obj2, links);
			else if(obj1.equals && obj2.equals)
				return obj1.equals(obj2);
			else
				return false;
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
				if(obj.indicator === "^/2" && !type && value !== parseInt(value, 10))
					return pl.error.type( "float", new Num(args[0],false), thread.__call_indicator );
				type = op.type_result === null ? type : op.type_result;
				if( pl.type.is_term( value ) ) {
					return value;
				} else if( value === Number.POSITIVE_INFINITY || value === Number.NEGATIVE_INFINITY ) {
					return pl.error.evaluation( "float_overflow", thread.__call_indicator );
				} else if( type === false && thread.get_flag( "bounded" ).id === "true" && (value > thread.get_flag( "max_integer" ).value || value < thread.get_flag( "min_integer" ).value) ) {
					return pl.error.evaluation( "int_overflow", thread.__call_indicator );
				} else {
					return new Num( value, type );
				}
			} else {
				return pl.error.type( "evaluable", str_indicator(obj.indicator), thread.__call_indicator );
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
			
			// Uninstantation error
			uninstantiation: function( found, indicator ) {
				return new Term( "error", [new Term( "uninstantiation_error", [new Term( found )] ), str_indicator( indicator )] );
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
				var info = new Term( ".", [new Term( "line", [new Num(token.line+1)] ), new Term( ".", [new Term( "column", [new Num(position)] ), new Term( ".", [found, new Term( "[]", [] )] )] )] );
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
		format_variable: function( id, variable ) {
			var charcode = variable && variable.length > 0 ? codePointAt(variable, 1) : 0;
			if(variable === "_" || variable && variable[0] === "_" && (charcode === 95 || charcode >= 65 && charcode <= 90))
				return "__" + id;
			return "_" + id;
		},
		
		// Format of computed answers
		format_answer: function( answer, thread, options ) {
			if( thread instanceof Session )
				thread = thread.thread;
			var options = options ? options : {};
			options.session = thread ? thread.session : undefined;
			if( pl.type.is_error( answer ) ) {
				return "uncaught exception: " + answer.args[0].toString(options);
			} else if( answer === false ) {
				return "false";
			} else if( answer === null ) {
				return "limit exceeded";
			} else {
				var i = 0;
				var str = "";
				if( pl.type.is_substitution( answer ) ) {
					var dom = answer.domain( true );
					for( var link in answer.links ){
						if( !answer.links.hasOwnProperty(link) ) continue;
						if( pl.type.is_variable(answer.links[link]) ) {
							var links = {};
							links[answer.links[link].id] = new Var(link);
							answer = answer.apply( new Substitution(links) );
						}
					}
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
				if( i === 0 ) {
					return "true";
				} else {
					return str;
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

	// Built-in predicates
	pl.builtin = new Module("system", {

		// TERM AND GOAL EXPANSION

		// goal_expansion/2
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



		// SYSTEM PREDICATES ($)

		// '$push_global_stack'/2
		"$push_global_stack/2": function(thread, point, atom) {
			var stack = atom.args[0], value = atom.args[1];
			if(!pl.type.is_variable(stack)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else {
				thread.push_global_stack(stack.id, value);
				thread.success(point);
			}
		},

		// '$flush_global_stack'/3
		"$flush_global_stack/3": function(thread, point, atom) {
			var stack = atom.args[0], list = atom.args[1], tail = atom.args[2];
			if(!pl.type.is_variable(stack)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else {
				var values = thread.flush_global_stack(stack.id, tail);
				thread.prepend([new State(
					point.goal.replace(new Term("=", [list, values])),
					point.substitution,
					point
				)]);
			}
		},

		// '$free_variable_set'/3
		"$free_variable_set/3": function(thread, point, atom) {
			var goal_in = atom.args[0], goal_out = atom.args[1], vars = atom.args[2];
			var bv = [];
			var pointer = goal_in;
			while(pl.type.is_term(pointer) && pointer.indicator === "^/2") {
				bv = bv.concat(pointer.args[0].variables());
				pointer = pointer.args[1];
			}
			var gv = pointer.variables();
			var fv = arrayToList(map(difference(gv, bv), function(v) {
				return new Var(v);
			}));
			thread.prepend([
				new State(
					point.goal.replace(new Term(",", [
						new Term("=", [goal_out, pointer]),
						new Term("=", [vars, fv]) 
					])),
					point.substitution,
					point
				)
			]);
		},

		// '$member'/2
		"$member/2": [
			new pl.type.Rule(new pl.type.Term("$member", [new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("X"),new pl.type.Var("_")])]), null),
			new pl.type.Rule(new pl.type.Term("$member", [new pl.type.Var("X"),new pl.type.Term(".", [new pl.type.Var("_"),new pl.type.Var("Xs")])]), new pl.type.Term("$member", [new pl.type.Var("X"),new pl.type.Var("Xs")]))
		],

		// '$bind_bagof_keys/2'/2
		"$bind_bagof_keys/2": [
			new pl.type.Rule(new pl.type.Term("$bind_bagof_keys", [new pl.type.Term("[]", []),new pl.type.Var("_")]), null),
			new pl.type.Rule(new pl.type.Term("$bind_bagof_keys", [new pl.type.Term(".", [new pl.type.Term("-", [new pl.type.Var("Key"),new pl.type.Var("_")]),new pl.type.Var("Bag")]),new pl.type.Var("Vars")]), new pl.type.Term(",", [new pl.type.Term("term_variables", [new pl.type.Var("Key"),new pl.type.Var("Vars"),new pl.type.Var("_")]),new pl.type.Term("$bind_bagof_keys", [new pl.type.Var("Bag"),new pl.type.Var("Vars")])]))
		],

		// '$findall'/4
		"$findall/4": [
			new pl.type.Rule(new pl.type.Term("$findall", [new pl.type.Var("Template0"),new pl.type.Var("Goal0"),new pl.type.Var("Instances"),new pl.type.Var("Tail")]), new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term("copy_term", [new pl.type.Term("-", [new pl.type.Var("Template0"),new pl.type.Var("Goal0")]),new pl.type.Term("-", [new pl.type.Var("Template1"),new pl.type.Var("Goal1")])]),new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("Goal1")]),new pl.type.Term(",", [new pl.type.Term("copy_term", [new pl.type.Var("Template1"),new pl.type.Var("Template2")]),new pl.type.Term(",", [new pl.type.Term("$push_global_stack", [new pl.type.Var("Var"),new pl.type.Var("Template2")]),new pl.type.Term("false", [])])])])]),new pl.type.Term("$flush_global_stack", [new pl.type.Var("Var"),new pl.type.Var("Instances"),new pl.type.Var("Tail")])]))
		],

		// '$bagof'/3
		"$bagof/3": [
			new pl.type.Rule(new pl.type.Term("$bagof", [new pl.type.Var("Template"),new pl.type.Var("Goal0"),new pl.type.Var("Answer")]), new pl.type.Term(",", [new pl.type.Term("$free_variable_set", [new pl.type.Term("^", [new pl.type.Var("Template"),new pl.type.Var("Goal0")]),new pl.type.Var("Goal1"),new pl.type.Var("FV")]),new pl.type.Term(",", [new pl.type.Term("findall", [new pl.type.Term("-", [new pl.type.Var("FV"),new pl.type.Var("Template")]),new pl.type.Var("Goal1"),new pl.type.Var("Answers"),new pl.type.Term("[]", [])]),new pl.type.Term(",", [new pl.type.Term("$bind_bagof_keys", [new pl.type.Var("Answers"),new pl.type.Var("_")]),new pl.type.Term(",", [new pl.type.Term("keygroup", [new pl.type.Var("Answers"),new pl.type.Var("KeyGroups")]),new pl.type.Term(",", [new pl.type.Term("keysort", [new pl.type.Var("KeyGroups"),new pl.type.Var("KeySorted")]),new pl.type.Term("$member", [new pl.type.Term("-", [new pl.type.Var("FV"),new pl.type.Var("Answer")]),new pl.type.Var("KeySorted")])])])])])]))
		],

		// '$setof'/3
		"$setof/3": [
			new pl.type.Rule(new pl.type.Term("$setof", [new pl.type.Var("Template"),new pl.type.Var("Goal0"),new pl.type.Var("Answer")]), new pl.type.Term(",", [new pl.type.Term("$free_variable_set", [new pl.type.Term("^", [new pl.type.Var("Template"),new pl.type.Var("Goal0")]),new pl.type.Var("Goal1"),new pl.type.Var("FV")]),new pl.type.Term(",", [new pl.type.Term("findall", [new pl.type.Term("-", [new pl.type.Var("FV"),new pl.type.Var("Template")]),new pl.type.Var("Goal1"),new pl.type.Var("Answers"),new pl.type.Term("[]", [])]),new pl.type.Term(",", [new pl.type.Term("$bind_bagof_keys", [new pl.type.Var("Answers"),new pl.type.Var("_")]),new pl.type.Term(",", [new pl.type.Term("keygroup", [new pl.type.Var("Answers"),new pl.type.Var("KeyGroups")]),new pl.type.Term(",", [new pl.type.Term("keysort", [new pl.type.Var("KeyGroups"),new pl.type.Var("KeySorted")]),new pl.type.Term(",", [new pl.type.Term("$member", [new pl.type.Term("-", [new pl.type.Var("FV"),new pl.type.Var("Unsorted")]),new pl.type.Var("KeySorted")]),new pl.type.Term("sort", [new pl.type.Var("Unsorted"),new pl.type.Var("Answer")])])])])])])]))
		],

		// '$if/3'
		"$if/3": [
			new pl.type.Rule(new pl.type.Term("$if", [new pl.type.Var("If"),new pl.type.Var("Then"),new pl.type.Var("Else")]), new pl.type.Term(";", [new pl.type.Term(",", [new pl.type.Term("call", [new pl.type.Var("If")]),new pl.type.Term(",", [new pl.type.Term("$push_global_stack", [new pl.type.Var("Stack"),new pl.type.Var("_")]),new pl.type.Term("call", [new pl.type.Var("Then")])])]),new pl.type.Term(",", [new pl.type.Term("$flush_global_stack", [new pl.type.Var("Stack"),new pl.type.Term("[]", []),new pl.type.Term("[]", [])]),new pl.type.Term("call", [new pl.type.Var("Else")])])]))
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
			var priority = atom.args[0], type = atom.args[1], operators = atom.args[2];
			if(pl.type.is_atom(operators))
				operators = new Term(".", [operators, new Term("[]")]);
			if( pl.type.is_variable( priority ) || pl.type.is_variable( type ) || pl.type.is_variable( operators ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_integer( priority ) ) {
				thread.throw_error( pl.error.type( "integer", priority, atom.indicator ) );
			} else if( !pl.type.is_atom( type ) ) {
				thread.throw_error( pl.error.type( "atom", type, atom.indicator ) );
			} else if( !pl.type.is_list( operators ) ) {
				thread.throw_error( pl.error.type( "list", operators, atom.indicator ) );
			} else if( pl.type.is_empty_list( operators ) ) {
				thread.throw_error( pl.error.permission( "create", "operator", operators, atom.indicator ) );
			} else {
				var pointer = operators;
				while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
					var operator = pointer.args[0];
					pointer = pointer.args[1];
					if( pl.type.is_variable( operator ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
						return;
					} else if( !pl.type.is_atom( operator ) ) {
						thread.throw_error( pl.error.type( "atom", operator, atom.indicator ) );
						return;
					} else if( priority.value < 0 || priority.value > 1200 ) {
						thread.throw_error( pl.error.domain( "operator_priority", priority, atom.indicator ) );
						return;
					} else if( operator.id === "," ) {
						thread.throw_error( pl.error.permission( "modify", "operator", operator, atom.indicator ) );
						return;
					} else if( operator.id === "{}" ) {
						thread.throw_error( pl.error.permission( "create", "operator", operator, atom.indicator ) );
						return;
					} else if( operator.id === "[]" ) {
						thread.throw_error( pl.error.permission( "create", "operator", operator, atom.indicator ) );
						return;
					} else if( operator.id === "|" && priority.value !== 0 && (priority.value < 1001 || type.id.length !== 3 ) ) {
						thread.throw_error( pl.error.permission( "create", "operator", operator, atom.indicator ) );
						return;
					} else if( ["fy", "fx", "yf", "xf", "xfx", "yfx", "xfy"].indexOf( type.id ) === -1 ) {
						thread.throw_error( pl.error.domain( "operator_specifier", type, atom.indicator ) );
						return;
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
						if(fix.infix && current_class === "postfix" || fix.postfix && current_class === "infix") {
							thread.throw_error( pl.error.permission( "create", "operator", operator, atom.indicator ) );
							return;
						} else {
							if( fix[current_class] ) {
								remove( thread.session.__operators[fix[current_class].priority][operator.id], fix[current_class].type );
								if( thread.session.__operators[fix[current_class].priority][operator.id].length === 0 ) {
									delete thread.session.__operators[fix[current_class].priority][operator.id];
								}
							}
							if( priority.value > 0 ) {
								if( !thread.session.__operators[priority.value] ) thread.session.__operators[priority.value.toString()] = {};
								if( !thread.session.__operators[priority.value][operator.id] ) thread.session.__operators[priority.value][operator.id] = [];
								thread.session.__operators[priority.value][operator.id].push( type.id );
							}
						}
					}
				}
				if(pl.type.is_variable(pointer)) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
					return;
				} else if(!pl.type.is_term(pointer) || pointer.indicator !== "[]/0") {
					thread.throw_error( pl.error.type( "list", operators, atom.indicator ) );
					return;
				} else {
					thread.success(point);
				}
			}
		},
		
		// current_op/3
		"current_op/3": function( thread, point, atom ) {
			var priority = atom.args[0], specifier = atom.args[1], operator = atom.args[2];
			var points = [];
			if( !pl.type.is_variable( priority ) && !pl.type.is_integer( priority ) ) {
				thread.throw_error( pl.error.type( "integer", priority, atom.indicator ) );
			} else if( pl.type.is_integer( priority ) && ( priority.value < 0 || priority.value > 1200 ) ) {
				thread.throw_error( pl.error.domain( "operator_priority", priority, atom.indicator ) );
			} else if( !pl.type.is_variable( specifier ) && !pl.type.is_atom( specifier ) ) {
				thread.throw_error( pl.error.type( "atom", specifier, atom.indicator ) );
			} else if( pl.type.is_atom( specifier ) && indexOf( ["fy", "fx", "yf", "xf", "xfx", "yfx", "xfy"], specifier.id ) === -1 ) {
				thread.throw_error( pl.error.domain( "operator_specifier", specifier, atom.indicator ) );
			} else if( !pl.type.is_variable( operator ) && !pl.type.is_atom( operator ) ) {
				thread.throw_error( pl.error.type( "atom", operator, atom.indicator ) );
			} else {
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
			}
		},
	


		// LOGIC AND CONTROL STRUCTURES
	
		// ;/2 (disjunction)
		";/2": function(thread, point, atom) {
			var left = atom.args[0], right = atom.args[1];
			var context_left = left.args[0];
			var free_left = left.indicator === ":/2" ? left.args[1] : left;
			// if then else
			if(pl.type.is_term(free_left) && free_left.indicator === "->/2") {
				var cond = left.indicator === ":/2" ? new Term(":", [context_left, new Term("call", [free_left.args[0]])]) : free_left.args[0];
				var then = left.indicator === ":/2" ? new Term(":", [context_left, free_left.args[1]]) : free_left.args[1];
				var otherwise = right;
				var goal_fst = point.goal.replace(new Term( ",", [cond, new Term(",", [new Term("!"), then])] ) );
				var goal_snd = point.goal.replace(new Term( ",", [new Term("!"), otherwise]));
				thread.prepend([
					new State(goal_fst, point.substitution, point),
					new State(goal_snd, point.substitution, point)
				]);
			// soft-cut
			} else if(pl.type.is_term(free_left) && free_left.indicator === "*->/2") {
				var cond = left.indicator === ":/2" ? new Term(":", [context_left, free_left.args[0]]) : free_left.args[0];
				var then = left.indicator === ":/2" ? new Term(":", [context_left, free_left.args[1]]) : free_left.args[1];
				var otherwise = right;
				thread.prepend([new State(
					point.goal.replace(new Term("$if", [cond, then, otherwise])),
					point.substitution,
					point
				)]);
			// otherwise
			} else {
				thread.prepend([
					new State(point.goal.replace(left), point.substitution, point),
					new State(point.goal.replace(right), point.substitution, point)
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
					if(selected && selected.indicator === ":/2")
						selected = selected.args[1];
					if( selected && selected.id === "call" && selected.search(atom) ) {
						parent_cut = last_cut;
						break;
					}
				}
			}
			var setup_call_cleanup = null;
			for( var i = thread.points.length-1; i >= 0; i-- ) {
				var state = thread.points[i];
				var node = state.parent;
				while( node !== null && node !== parent_cut.parent ) {
					node = node.parent;
				}
				if( node === null && node !== parent_cut.parent )
					states.push( state );
				else if(state.setup_call_cleanup_goal)
					setup_call_cleanup = state.setup_call_cleanup_goal
			}
			thread.points = states.reverse();
			thread.prepend([new State(
				point.goal.replace(setup_call_cleanup),
				point.substitution,
				point
			)]);
		},
		
		// \+ (negation)
		"\\+/1": function( thread, point, atom ) {
			var goal = atom.args[0];
			if( pl.type.is_variable( goal ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_callable( goal ) ) {
				thread.throw_error( pl.error.type( "callable", goal, atom.indicator ) );
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
			var goal = point.goal.replace(new Term(",", [
				new Term("call", [cond]),
				new Term(",", [new Term("!"), then])
			]));
			thread.prepend( [new State( goal, point.substitution, point )] );
		},

		// *->/2 (soft-cut)
		"*->/2": function(thread, point, atom) {
			var cond = atom.args[0], then = atom.args[1];
			var goal = point.goal.replace(new Term(",", [
				new Term("call", [cond]), then]));
			thread.prepend([new State(goal, point.substitution, point)]);
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
			var error = atom.args[0];
			if(pl.type.is_variable(error)) {
				thread.throw_error(pl.error.instantiation(thread.level.indicator));
			} else {
				for(var i = 0; i < thread.points.length; i++) {
					var state = thread.points[i];
					if(state.setup_call_cleanup_catch) {
						thread.points = [new State(
							new Term(",", [
								new Term("catch", [
									state.setup_call_cleanup_catch,
									new Var("_"),
									new Term("throw", [error])
								]),
								new Term("throw", [error])
							]),
							point.substitution,
							point
						)];
						return;
					}
					
				}
				thread.throw_error(error);
			}
		},
		
		// catch/3
		"catch/3": function(thread, point, atom) {
			var goal = atom.args[0], catcher = atom.args[1], recover = atom.args[2];
			var nthread;
			if(!point.catch) {
				nthread = new Thread(thread.session);
				nthread.debugger = thread.debugger;
				nthread.format_success = function(state) { return state.substitution; };
				nthread.format_error = function(state) { return state.goal; };
				nthread.add_goal(goal, true, point);
				point.catch = nthread;
			} else {
				nthread = point.catch;
			}
			var callback = function(answer) {
				if(pl.type.is_error(answer)) {
					var occurs_check = thread.get_flag("occurs_check").indicator === "true/0";
					var state = new State();
					var mgu = pl.unify(answer.args[0], catcher, occurs_check);
					if(mgu !== null) {
						state.substitution = point.substitution.apply(mgu);
						state.goal = point.goal.replace(recover).apply(mgu);
						state.parent = point;
						thread.prepend([state]);
					} else {
						thread.throw_error(answer.args[0]);
					}
				} else if(answer !== false && answer !== null) {
					var state = answer === null ? [] : new State(
						point.goal.apply(answer).replace(null),
						point.substitution.apply(answer),
						point
					);
					thread.prepend([state, point]);
				} else if(answer === null) {
					thread.prepend([point]);
					if(thread.has_limit)
						thread.current_limit = 0;
				}
				thread.again(answer !== null);
			};
			nthread.answer(callback);
			return true;
		},

		// call_cleanup/2
		"call_cleanup/2": function(thread, point, atom) {
			var call = atom.args[0], cleanup = atom.args[1];
			if(pl.type.is_variable(call) || pl.type.is_variable(cleanup)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(call)) {
				thread.throw_error(pl.error.type("callable", call, atom.indicator));
			} else if(!pl.type.is_callable(cleanup)) {
				thread.throw_error(pl.error.type("callable", cleanup, atom.indicator));
			} else {
				var nthread, callback;
				if(point.hasOwnProperty("setup_call_cleanup_thread")) {
					nthread = point.setup_call_cleanup_thread;
					callback = point.setup_call_cleanup_callback;
				} else {
					var goal = new Term("call", [call]);
					nthread = new Thread(thread.session);
					nthread.add_goal(goal, true, point);
					callback = function(answer) {
						if(answer === null) {
							var state = new State(
								point.goal,
								point.substitution,
								point
							);
							state.setup_call_cleanup_thread = nthread;
							state.setup_call_cleanup_callback = callback;
							thread.prepend([state]);
						} else if(answer === false) {
							var cleanup_and_fail = new Term(",", [
								new Term("call", [cleanup]),
								new Term("fail")
							]);
							var state = new State(
								point.goal.replace(cleanup_and_fail),
								point.substitution,
								point
							);
							thread.prepend([state]);
						} else if(pl.type.is_error(answer)) {
							var cleanup_and_throw = new Term(",", [
								new Term("call", [cleanup]),
								answer
							]);
							var state = new State(
								point.goal.replace(cleanup_and_throw),
								point.substitution,
								point
							);
							thread.prepend([state]);
						} else {
							if(nthread.points.length === 0) {
								var state = new State(
									point.goal.replace(
										new Term("call", [cleanup])
									).apply(answer),
									point.substitution.apply(answer),
									point
								);
								thread.prepend([state]);
							} else {
								var state1 = new State(
									point.goal.apply(answer).replace(null),
									point.substitution.apply(answer),
									point
								);
								var state2 = new State(
									point.goal,
									point.substitution,
									point
								);
								state2.setup_call_cleanup_thread = nthread;
								state2.setup_call_cleanup_callback = callback;
								state2.setup_call_cleanup_goal = cleanup.apply(answer);
								state2.setup_call_cleanup_catch = cleanup;
								thread.prepend([state1, state2]);
							}
						}
						thread.again();
					}
				}
				nthread.answer(callback);
				return true;
			}
		},

		// setup_call_cleanup/3
		"setup_call_cleanup/3": function(thread, point, atom) {
			var setup = atom.args[0], call = atom.args[1], cleanup = atom.args[2];
			if(pl.type.is_variable(setup) || pl.type.is_variable(call) || pl.type.is_variable(cleanup)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(setup)) {
				thread.throw_error(pl.error.type("callable", setup, atom.indicator));
			} else if(!pl.type.is_callable(call)) {
				thread.throw_error(pl.error.type("callable", call, atom.indicator));
			} else if(!pl.type.is_callable(cleanup)) {
				thread.throw_error(pl.error.type("callable", cleanup, atom.indicator));
			} else {
				thread.prepend([new State(
					point.goal.replace(new Term(",", [
						new Term("once", [setup]),
						new Term("call_cleanup", [call, cleanup])
					])),
					point.substitution,
					point
				)]);
			}
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
		/*
		subsumes_term(General, Specific) :-
			\+ \+ (
			term_variables(Specific, Vars1),
			unify_with_occurs_check(General, Specific),
			term_variables(Vars1, Vars2),
			Vars1 == Vars2
		).
		*/
		"subsumes_term/2": function( thread, point, atom ) {
			var general = atom.args[0], specific = atom.args[1];
			var vars1 = thread.next_free_variable();
			var vars2 = thread.next_free_variable();
			thread.prepend([new State(
				point.goal.replace(new Term("\\+", [
					new Term("\\+", [
						new Term(",", [
							new Term("term_variables", [specific, vars1]),
							new Term(",", [
								new Term("unify_with_occurs_check", [general, specific]),
								new Term(",", [
									new Term("term_variables", [vars1, vars2]),
									new Term("==", [vars1, vars2])
								])
							])
						])
					])
				])),
				point.substitution,
				point
			)]);
		},
		
		// ALL SOLUTIONS

		// findall/3
		"findall/3": function(thread, point, atom) {
			var template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
			var tail = new Term("[]", []);
			thread.prepend([new State(
				point.goal.replace(new Term("findall", [template, goal, instances, tail])),
				point.substitution,
				point
			)]);
		},

		// findall/4
		"findall/4": function(thread, point, atom) {
			var template = atom.args[0], goal = atom.args[1], instances = atom.args[2], tail = atom.args[3];
			var proper_goal = goal;
			if(pl.type.is_term(goal) && goal.indicator === ":/2")
				proper_goal = goal.args[1];
			if(pl.type.is_variable(proper_goal)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(proper_goal)) {
				thread.throw_error(pl.error.type("callable", goal, atom.indicator));
			} else if(!pl.type.is_variable(instances) && !pl.type.is_list(instances)) {
				thread.throw_error(pl.error.type("list", instances, atom.indicator));
			} else if(!pl.type.is_variable(tail) && !pl.type.is_list(tail)) {
				thread.throw_error(pl.error.type("list", tail, atom.indicator));
			} else {
				thread.prepend([new State(
					point.goal.replace(new Term("$findall", [template, goal, instances, tail])),
					point.substitution,
					point
				)]);
			}
		},
		
		// bagof/3
		"bagof/3": function(thread, point, atom) {
			var template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
			if(pl.type.is_variable(goal)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(goal)) {
				thread.throw_error(pl.error.type("callable", goal, atom.indicator));
			} else if(!pl.type.is_variable(instances) && !pl.type.is_list(instances)) {
				thread.throw_error( pl.error.type("list", instances, atom.indicator));
			} else {
				thread.prepend([new State(
					point.goal.replace(new Term("$bagof", [template, goal, instances])),
					point.substitution,
					point
				)]);
			}
		},

		// setof/3
		"setof/3": function(thread, point, atom) {
			var template = atom.args[0], goal = atom.args[1], instances = atom.args[2];
			if(pl.type.is_variable(goal)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(goal)) {
				thread.throw_error(pl.error.type("callable", goal, atom.indicator));
			} else if(!pl.type.is_variable(instances) && !pl.type.is_list(instances)) {
				thread.throw_error( pl.error.type("list", instances, atom.indicator));
			} else {
				thread.prepend([new State(
					point.goal.replace(new Term("$setof", [template, goal, instances])),
					point.substitution,
					point
				)]);
			}
		},
		
		// TERM CREATION AND DECOMPOSITION
		
		// functor/3
		"functor/3": function( thread, point, atom ) {
			var subs;
			var term = atom.args[0], name = atom.args[1], arity = atom.args[2];
			if( pl.type.is_variable( term ) && (pl.type.is_variable( name ) || pl.type.is_variable( arity )) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( arity ) && !pl.type.is_integer( arity ) ) {
				thread.throw_error( pl.error.type( "integer", atom.args[2], atom.indicator ) );
			} else if( !pl.type.is_variable( name ) && !pl.type.is_atomic( name ) ) {
				thread.throw_error( pl.error.type( "atomic", atom.args[1], atom.indicator ) );
			} else if( pl.type.is_variable( term ) && !pl.type.is_atom( name ) && pl.type.is_integer( arity ) && arity.value > 0 ) {
				thread.throw_error( pl.error.type( "atom", atom.args[1], atom.indicator ) );
			} else if( pl.type.is_variable( term ) && pl.type.is_integer( arity ) && arity.value < 0 ) {
				thread.throw_error( pl.error.domain( "not_less_than_zero", atom.args[2], atom.indicator ) );
			} else if( pl.type.is_variable( term ) ) {
				if( atom.args[2].value >= 0 ) {
					var args = [];
					for( var i = 0; i < arity.value; i++ )
						args.push( thread.next_free_variable() );
					var functor = pl.type.is_number( name ) ? name : new Term( name.id, args );
					thread.prepend( [new State( point.goal.replace( new Term( "=", [term, functor] ) ), point.substitution, point )] );
				}
			} else {
				var id = pl.type.is_number( term ) ? term : new Term( term.id, [] );
				var length = pl.type.is_number( term ) ? new Num( 0, false ) : new Num( term.args.length, false );
				var goal = new Term( ",", [new Term( "=", [id, name] ), new Term( "=", [length, arity] )] );
				thread.prepend( [new State( point.goal.replace( goal ), point.substitution, point )] );
			}
		},
		
		// arg/3
		"arg/3": function( thread, point, atom ) {
			if( pl.type.is_variable( atom.args[0] ) || pl.type.is_variable( atom.args[1] ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_integer( atom.args[0] ) ) {
				thread.throw_error( pl.error.type( "integer", atom.args[0], atom.indicator ) );
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
			} else if( pl.type.is_variable( atom.args[0] ) && pl.type.is_empty_list( atom.args[1] ) ) {
				thread.throw_error( pl.error.domain( "non_empty_list", atom.args[1], atom.indicator ) );
			} else if( !pl.type.is_variable( atom.args[0] ) ) {
				if( pl.type.is_term( atom.args[0] ) && atom.args[0].args.length > 0 ) {
					list = new Term( "[]" );
					for( var i = atom.args[0].args.length - 1; i >= 0; i-- ) {
						list = new Term( ".", [atom.args[0].args[i], list] );
					}
					list = new Term( ".", [new Term( atom.args[0].id ), list] );
				} else {
					list = new Term( ".", [atom.args[0], new Term( "[]" )] );
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
		"copy_term/2": function(thread, point, atom) {
			var original_term = atom.args[0], renamed_term = atom.args[1];
			thread.session.renamed_variables = {};
			var new_term = original_term.rename(thread);
			thread.session.renamed_variables = {};
			thread.prepend([
				new State(
					point.goal.replace(new Term("=", [renamed_term, new_term])),
					point.substitution,
					point)
				]
			);
		},
		
		// term_variables/2
		"term_variables/2": [
			new pl.type.Rule(new pl.type.Term("term_variables", [new pl.type.Var("Term"),new pl.type.Var("Vars")]), new pl.type.Term("term_variables", [new pl.type.Var("Term"),new pl.type.Var("Vars"),new pl.type.Term("[]", [])]))
		],

		// term_variables/3
		"term_variables/3": function(thread, point, atom) {
			var term = atom.args[0], vars = atom.args[1], tail = atom.args[2];
			if( !pl.type.is_fully_list( vars ) ) {
				thread.throw_error( pl.error.type( "list", vars, atom.indicator ) );
			} else {
				var list = arrayToList(map(nub(term.variables()), function(v) {
					return new Var(v);
				}), tail);
				thread.prepend([new State(
					point.goal.replace(new Term("=", [vars, list])),
					point.substitution,
					point
				)]);
			}
		},

		// numbervars/3
		"numbervars/3": function(thread, point, atom) {
			var term = atom.args[0], start = atom.args[1], end = atom.args[2];
			if(pl.type.is_variable(start)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_integer(start)) {
				thread.throw_error(pl.error.type("integer", start, atom.indicator));
			} else if(!pl.type.is_variable(end) && !pl.type.is_integer(end)) {
				thread.throw_error(pl.error.type("integer", end, atom.indicator));
			} else {
				var variables = nub(term.variables());
				var value = start.value;
				var unif_body = new Term("true");
				for(var i = 0; i < variables.length; i++) {
					unif_body = new Term(",", [
						new Term("=", [
							new Var(variables[i]),
							new Term("$VAR", [new Num(value, false)])]),
							unif_body]);
					value++;
				}
				var unif_end = new Term("=", [end, new Num(value, false)]);
				if(pl.type.is_variable(end) || end.value === value) {
					thread.prepend([new State(
						point.goal.replace(new Term(",", [unif_body, unif_end])),
						point.substitution,
						point
					)]);
				}
			}
		},
		
		// CLAUSE RETRIEVAL AND INFORMATION
		
		// clause/2
		"clause/2": function(thread, point, atom) {
			var head = atom.args[0], body = atom.args[1];
			var module_id = "user";
			if(pl.type.is_term(head) && head.indicator === ":/2") {
				if(!pl.type.is_atom(head.args[0])) {
					thread.throw_error(pl.error.type("module", head.args[0], atom.indicator));
					return;
				}
				module_id = head.args[0].id;
				head = head.args[1];
			}
			var get_module = thread.session.modules[module_id];
			if(pl.type.is_variable(head)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(head)) {
				thread.throw_error(pl.error.type("callable", head, atom.indicator));
			} else if(!pl.type.is_variable(body) && !pl.type.is_callable(body)) {
				thread.throw_error(pl.error.type("callable", body, atom.indicator));
			} else if(head.indicator === ",/2" || thread.session.modules.system.rules.hasOwnProperty(head.indicator)) {
				thread.throw_error(pl.error.permission("access", "private_procedure", str_indicator(head.indicator), atom.indicator));
			} else if(pl.type.is_module(get_module) && get_module.rules[head.indicator]) {
				if(get_module.is_public_predicate(head.indicator)) {
					var states = [];
					if(typeof get_module.rules[head.indicator] === "function") {
						thread.throw_error(pl.error.permission("modify", "static_procedure", str_indicator(head.indicator), atom.indicator));
						return;
					}
					for(var i = 0; i < get_module.rules[head.indicator].length; i++) {
						var rule = get_module.rules[head.indicator][i];
						thread.session.renamed_variables = {};
						rule = rule.rename(thread);
						if(rule.body === null)
							rule.body = new Term("true");
						var goal = new Term(",", [
							new Term("=", [rule.head, head]),
							new Term("=", [rule.body, body])
						]);
						states.push(new State(point.goal.replace(goal), point.substitution, point));
					}
					thread.prepend(states);
				} else {
					thread.throw_error(pl.error.permission("access", "private_procedure", str_indicator(head.indicator), atom.indicator));
				}
			}
		},
		
		// current_predicate/1
		"current_predicate/1": function(thread, point, atom) {
			var indicator = atom.args[0];
			var module_id;
			if(pl.type.is_term(indicator) && indicator.indicator === ":/2") {
				if(!pl.type.is_atom(indicator.args[0])) {
					thread.throw_error(pl.error.type("module", indicator.args[0], atom.indicator));
					return;
				}
				module_id = indicator.args[0].id;
				indicator = indicator.args[1];
			} else {
				module_id = "user";
			}
			if(!pl.type.is_variable(indicator) && (!pl.type.is_compound(indicator) || indicator.indicator !== "//2")) {
				thread.throw_error(pl.error.type( "predicate_indicator", indicator, atom.indicator));
			} else if(!pl.type.is_variable( indicator ) && !pl.type.is_variable(indicator.args[0]) && !pl.type.is_atom(indicator.args[0])) {
				thread.throw_error(pl.error.type( "atom", indicator.args[0], atom.indicator));
			} else if(!pl.type.is_variable(indicator) && !pl.type.is_variable(indicator.args[1]) && !pl.type.is_integer(indicator.args[1])) {
				thread.throw_error(pl.error.type("integer", indicator.args[1], atom.indicator));
			} else if(!pl.type.is_variable(indicator) && pl.type.is_integer(indicator.args[1]) && indicator.args[1].value < 0) {
				thread.throw_error(pl.error.domain("not_less_than_zero", indicator.args[1], atom.indicator));
			} else {
				var states = [];
				var get_module = thread.session.modules[module_id];
				if(pl.type.is_module(get_module)) {
					for(var prop in get_module.rules) {
						if(!get_module.rules.hasOwnProperty(prop))
							continue;
						var predicate = str_indicator(prop);
						var goal = new Term("=", [predicate, indicator]);
						states.push(new State(point.goal.replace(goal), point.substitution, point));
					}
					thread.prepend(states);
				}
			}
		},

		// current_module/1
		"current_module/1": function(thread, point, atom) {
			var module_id = atom.args[0];
			if(!pl.type.is_variable(module_id) && !pl.type.is_atom(module_id)) {
				thread.throw_error(pl.error.type("atom", module_id, atom.indicator));
			} else {
				if(pl.type.is_variable(module_id)) {
					var states = [];
					for(var prop in thread.session.modules) {
						if(!thread.session.modules.hasOwnProperty(prop))
							continue;
						states.push(new State(
							point.goal.replace(new Term("=", [module_id, new Term(prop)])),
							point.substitution,
							point
						));
					}
					thread.prepend(states);
				} else {
					if(thread.session.modules.hasOwnProperty(module_id.id))
						thread.success(point);
				}
			}
		},

		// predicate_property/2
		"predicate_property/2": function(thread, point, atom) {
			var head = atom.args[0], property = atom.args[1];
			var module_id;
			if(pl.type.is_term(head) && head.indicator === ":/2") {
				if(!pl.type.is_atom(head.args[0])) {
					thread.throw_error(pl.error.type("module", head.args[0], atom.indicator));
					return;
				}
				module_id = head.args[0].id;
				head = head.args[1];
			}
			if(!pl.type.is_variable(head) && !pl.type.is_callable(head)) {
				thread.throw_error(pl.error.type("callable", head, atom.indicator));
			} else if(!pl.type.is_variable(property) && !pl.type.is_predicate_property(property)) {
				thread.throw_error(pl.error.domain("predicate_property", property, atom.indicator));
			} else {
				var get_module = module_id ? thread.session.modules[module_id] : thread.session.modules.user;
				var points = [];
				// all predicates
				if(pl.type.is_variable(head)) {
					// built-in predicates (built_in + static + native_code + meta_predicate?)
					if(!module_id) {
						for(var prop in pl.builtin.rules) {
							if(!pl.builtin.rules.hasOwnProperty(prop))
								continue;
							var indicator = str_indicator(prop);
							var args = [];
							for(var i = 0; i < indicator.args[1].value; i++)
								args.push(thread.next_free_variable());
							var unif_head = new Term(indicator.args[0].id, args);
							var current_properties = [
								new Term("static"),
								new Term("built_in"),
								new Term("native_code")
							];
							if(pl.builtin.meta_predicates.hasOwnProperty(prop))
								current_properties.push(new Term("meta_predicate", [
									pl.builtin.meta_predicates[prop]
								]));
							// all predicates, one property / all properties
							for(var i = 0; i < current_properties.length; i++) {
								if(pl.type.is_variable(property) || current_properties[i].indicator === property.indicator) {
									points.push(new State(
										point.goal.replace(new Term(",", [
											new Term("=", [head, unif_head]),
											new Term("=", [property, current_properties[i]])
										])),
										point.substitution,
										point
									));
								}
							}
						}
					}
					// user-defined predicates
					if(pl.type.is_module(get_module)) {
						for(var prop in get_module.rules) {
							if(!get_module.rules.hasOwnProperty(prop))
								continue;
							var indicator = str_indicator(prop);
							var args = [];
							for(var i = 0; i < indicator.args[1].value; i++)
								args.push(thread.next_free_variable());
							var unif_head = new Term(indicator.args[0].id, args);
							var current_properties = [];
							if(thread.is_public_predicate(prop, module_id))
								current_properties.push(new Term("dynamic"));
							else
								current_properties.push(new Term("static"));
							if(get_module.rules[prop] instanceof Function)
								current_properties.push(new Term("native_code"));
							if(thread.is_multifile_predicate(prop, module_id))
								current_properties.push(new Term("multifile"));
							if(get_module.meta_predicates.hasOwnProperty(prop))
								current_properties.push(new Term("meta_predicate", [
									get_module.meta_predicates[prop]
								]));
							// all predicates, one property / all properties
							for(var i = 0; i < current_properties.length; i++) {
								if(pl.type.is_variable(property) || current_properties[i].indicator === property.indicator) {
									points.push(new State(
										point.goal.replace(new Term(",", [
											new Term("=", [head, unif_head]),
											new Term("=", [property, current_properties[i]])
										])),
										point.substitution,
										point
									));
								}
							}
						}
					}
				// one predicate
				} else {
					var builtin = !module_id && pl.type.is_builtin(head);
					var predicate = builtin ? pl.builtin.rules[head.indicator] : get_module.rules[head.indicator];
					get_module = builtin ? pl.builtin : get_module;
					if(predicate) {
						var current_properties;
						if(builtin) {
							current_properties = [
								new Term("static"),
								new Term("built_in"),
								new Term("native_code")
							];
						} else {
							current_properties = [];
							if(thread.is_public_predicate(head.indicator, module_id))
								current_properties.push(new Term("dynamic"));
							else
								current_properties.push(new Term("static"));
							if(predicate instanceof Function)
								current_properties.push(new Term("native_code"));
							if(thread.is_multifile_predicate(head.indicator, module_id))
								current_properties.push(new Term("multifile"));
						}
						if(get_module.meta_predicates.hasOwnProperty(head.indicator))
							current_properties.push(new Term("meta_predicate", [
								get_module.meta_predicates[head.indicator]
							]));
						var args = [];
						for(var i = 0; i < head.args.length; i++)
							args.push(thread.next_free_variable());
						var unif_head = new Term(head.id, args);
						// one predicate, one property / all properties
						for(var i = 0; i < current_properties.length; i++) {
							if(pl.type.is_variable(property) || current_properties[i].indicator === property.indicator) {
								points.push(new State(
									point.goal.replace(new Term(",", [
										new Term("=", [head, unif_head]),
										new Term("=", [property, current_properties[i]])
									])),
									point.substitution,
									point
								));
							}
						}
					}
				}
				thread.prepend(points);
			}
		},

		// listing/0
		"listing/0": function( thread, point, atom ) {
			var context_module = atom.context_module ? atom.context_module : "user";
			var rules = {};
			if(pl.type.is_module(thread.session.modules[context_module])) {
				rules = thread.session.modules[context_module].rules;
			}
			var str = "";
			for(var indicator in rules) {
				if(!rules.hasOwnProperty(indicator)) continue;
				var predicate = rules[indicator];
				str += "% " + indicator + "\n";
				if(predicate instanceof Array) {
					for(var i = 0; i < predicate.length; i++)
						str += predicate[i].toString( {session: thread.session} ) + "\n";
				} else {
					str += "/*\n" + predicate.toString() + "\n*/";
				}
				str += "\n";
			}
			thread.prepend( [new State(
				point.goal.replace(new Term("write", [new Term(str, [])])),
				point.substitution,
				point
			)] );
		},

		// listing/1
		"listing/1": function( thread, point, atom ) {
			var indicator = atom.args[0];
			var context_module = "user";
			if(indicator.indicator === ":/2") {
				context_module = indicator.args[0].id;
				indicator = indicator.args[1];
			}
			if(pl.type.is_variable(indicator)) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if(!pl.type.is_predicate_indicator(indicator)) {
				thread.throw_error( pl.error.type( "predicate_indicator", indicator, atom.indicator ) );
			} else {
				var rules = {};
				if(pl.type.is_module(thread.session.modules[context_module])) {
					rules = thread.session.modules[context_module].rules;
				}
				var str = "";
				var str_indicator = indicator.args[0].id + "/" + indicator.args[1].value;
				if(rules.hasOwnProperty(str_indicator)) {
					var predicate = rules[str_indicator];
					if(predicate instanceof Array) {
						for(var i = 0; i < predicate.length; i++)
							str += predicate[i].toString( {session: thread.session} ) + "\n";
					} else {
						str += "/*\n" + predicate.toString() + "\n*/";
					}
					str += "\n";
				}
				thread.prepend( [new State(
					point.goal.replace(new Term("write", [new Term(str, [])])),
					point.substitution,
					point
				)] );
			}
		},

		// LIST OPERATIONS

		// sort/2
		"sort/2": function( thread, point, atom ) {
			var list = atom.args[0], expected = atom.args[1];
			if( pl.type.is_variable( list ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( expected ) && !pl.type.is_fully_list( expected ) ) {
				thread.throw_error( pl.error.type( "list", expected, atom.indicator ) );
			} else {
				var arr = [];
				var pointer = list;
				while( pointer.indicator === "./2" ) {
					arr.push( pointer.args[0] );
					pointer = pointer.args[1];
				}
				if( pl.type.is_variable( pointer ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_empty_list( pointer ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else {
					var sorted_arr = arr.sort( pl.compare );
					for( var i = sorted_arr.length-1; i > 0; i-- ) {
						if( sorted_arr[i].equals(sorted_arr[i-1]) )
							sorted_arr.splice(i,1);
					}
					var sorted_list = new Term( "[]" );
					for( var i = sorted_arr.length-1; i >= 0; i-- ) {
						sorted_list = new Term( ".", [sorted_arr[i], sorted_list] );
					}
					thread.prepend( [new State( point.goal.replace( new Term( "=", [sorted_list, expected] ) ), point.substitution, point )] );
				}
			}
		},

		// keysort/2
		"keysort/2": function( thread, point, atom ) {
			var list = atom.args[0], expected = atom.args[1];
			if( pl.type.is_variable( list ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( expected ) && !pl.type.is_fully_list( expected ) ) {
				thread.throw_error( pl.error.type( "list", expected, atom.indicator ) );
			} else {
				var arr = [];
				var elem;
				var pointer = list;
				while( pointer.indicator === "./2" ) {
					elem = pointer.args[0];
					if( pl.type.is_variable( elem ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
						return;
					} else if( !pl.type.is_term( elem ) || elem.indicator !== "-/2" ) {
						thread.throw_error( pl.error.type( "pair", elem, atom.indicator ) );
						return;
					}
					elem.args[0].pair = elem.args[1];
					arr.push( elem.args[0] );
					pointer = pointer.args[1];
				}
				if( pl.type.is_variable( pointer ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_empty_list( pointer ) ) {
					thread.throw_error( pl.error.type( "list", list, atom.indicator ) );
				} else {
					if(!pl.type.is_variable(expected)) {
						var pointer = expected;
						while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
							var head = pointer.args[0];
							if(!pl.type.is_variable(head) && (!pl.type.is_term(head) || head.indicator !== "-/2")) {
								thread.throw_error( pl.error.type( "pair", head, atom.indicator ) );
								return;
							}
							pointer = pointer.args[1];
						}
						if(!pl.type.is_variable(pointer) && !pl.type.is_empty_list(pointer)) {
							thread.throw_error( pl.error.type( "list", expected, atom.indicator ) );
							return;
						}
					}
					var sorted_arr = arr.sort( pl.compare );
					var sorted_list = new pl.type.Term( "[]" );
					for( var i = sorted_arr.length - 1; i >= 0; i-- ) {
						sorted_list = new pl.type.Term( ".", [new pl.type.Term( "-", [sorted_arr[i], sorted_arr[i].pair] ), sorted_list] );
						delete sorted_arr[i].pair;
					}
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [sorted_list, expected] ) ), point.substitution, point )] );
				}
			}
		},

		// keygroup
		"keygroup/2": function(thread, point, atom) {
			var list = atom.args[0], expected = atom.args[1];
			if(pl.type.is_variable(list)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_variable(expected) && !pl.type.is_fully_list(expected)) {
				thread.throw_error(pl.error.type("list", expected, atom.indicator));
			} else {
				var keys = [];
				var values = [];
				var pointer = list
				while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
					var elem = pointer.args[0];
					if(pl.type.is_variable(elem)) {
						thread.throw_error(pl.error.instantiation(atom.indicator));
						return;
					} else if(!pl.type.is_term(elem) || elem.indicator !== "-/2") {
						thread.throw_error(pl.error.type("pair", elem, atom.indicator));
						return;
					}
					var key = elem.args[0], value = elem.args[1];
					var index = -1;
					for(var i = 0; i < keys.length; i++) {
						if(pl.compare(key, keys[i]) === 0) {
							index = i;
							break;
						}
					}
					if(index === -1) {
						index = keys.length;
						keys.push(key);
						values.push([]);
					}
					values[index].push(value);
					pointer = pointer.args[1];
				}
				if(pl.type.is_variable(pointer)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_empty_list(pointer)) {
					thread.throw_error(pl.error.type("list", list, atom.indicator));
				} else {
					if(!pl.type.is_variable(expected)) {
						var pointer = expected;
						while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
							var head = pointer.args[0];
							if(!pl.type.is_variable(head) && (!pl.type.is_term(head) || head.indicator !== "-/2")) {
								thread.throw_error(pl.error.type("pair", head, atom.indicator));
								return;
							}
							pointer = pointer.args[1];
						}
						if(!pl.type.is_variable(pointer) && !pl.type.is_empty_list(pointer)) {
							thread.throw_error(pl.error.type("list", expected, atom.indicator));
							return;
						}
					}
					group = new Term("[]", []);
					for(var i = keys.length-1; i >= 0; i--)
						group = new Term(".", [new Term("-", [keys[i], arrayToList(values[i])]), group]);
					thread.prepend([
						new State(
							point.goal.replace(new pl.type.Term("=", [expected, group])),
							point.substitution,
							point
						)
					]);
				}
			}
		},
		
		// CLAUSE CREATION AND DESTRUCTION
		
		// asserta/1
		"asserta/1": function(thread, point, atom) {
			var clause = atom.args[0];
			var module_id = "user";
			if(pl.type.is_term(clause) && clause.indicator === ":/2") {
				module_id = clause.args[0].id;
				clause = clause.args[1];
			}
			if(pl.type.is_variable(clause)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(clause)) {
				thread.throw_error(pl.error.type("callable", clause, atom.indicator));
			} else {
				var head, body, get_module;
				if(clause.indicator === ":-/2") {
					head = clause.args[0];
					body = body_conversion(clause.args[1]);
				} else {
					head = clause;
					body = null;
				}
				if(pl.type.is_variable(head)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_callable(head)) {
					thread.throw_error(pl.error.type("callable", head, atom.indicator));
				} else if(body !== null && !pl.type.is_callable(body)) {
					thread.throw_error( pl.error.type("callable", body, atom.indicator));
				} else if((!pl.type.is_module(thread.session.modules[module_id])
				|| thread.is_public_predicate(head.indicator, module_id))
				&& head.indicator !== ",/2"
				&& !thread.session.modules.system.rules.hasOwnProperty(head.indicator)) {
					if(!pl.type.is_module(thread.session.modules[module_id])) {
						get_module = new Module(module_id, {}, "all", {session: thread.session});
						thread.session.modules[module_id] = get_module;
					} else {
						get_module = thread.session.modules[module_id];
					}
					if(get_module.rules[head.indicator] === undefined)
						get_module.rules[head.indicator] = [];
					get_module.public_predicates[head.indicator] = true;
					get_module.rules[head.indicator] = [new Rule(head, body, true)].concat(get_module.rules[head.indicator]);
					get_module.update_indices_predicate(head.indicator);
					thread.success(point);
				} else {
					thread.throw_error(pl.error.permission("modify", "static_procedure", str_indicator(head.indicator), atom.indicator));
				}
			}
		},
		
		// assertz/1
		"assertz/1": function(thread, point, atom) {
			var clause = atom.args[0];
			var module_id = "user";
			if(pl.type.is_term(clause) && clause.indicator === ":/2") {
				module_id = clause.args[0].id;
				clause = clause.args[1];
			}
			if(pl.type.is_variable(clause)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(clause)) {
				thread.throw_error(pl.error.type("callable", clause, atom.indicator));
			} else {
				var head, body, get_module;
				if(clause.indicator === ":-/2") {
					head = clause.args[0];
					body = body_conversion(clause.args[1]);
				} else {
					head = clause;
					body = null;
				}
				if(pl.type.is_variable(head)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_callable(head)) {
					thread.throw_error(pl.error.type("callable", head, atom.indicator));
				} else if(body !== null && !pl.type.is_callable(body)) {
					thread.throw_error( pl.error.type("callable", body, atom.indicator));
				} else if((!pl.type.is_module(thread.session.modules[module_id])
				|| thread.is_public_predicate(head.indicator, module_id))
				&& head.indicator !== ",/2"
				&& !thread.session.modules.system.rules.hasOwnProperty(head.indicator)) {
					if(!pl.type.is_module(thread.session.modules[module_id])) {
						get_module = new Module(module_id, {}, "all", {session: thread.session});
						thread.session.modules[module_id] = get_module;
					} else {
						get_module = thread.session.modules[module_id];
					}
					if(get_module.rules[head.indicator] === undefined)
						get_module.rules[head.indicator] = [];
					get_module.public_predicates[head.indicator] = true;
					get_module.rules[head.indicator].push(new Rule(head, body, true));
					get_module.update_indices_predicate(head.indicator);
					thread.success(point);
				} else {
					thread.throw_error(pl.error.permission("modify", "static_procedure", str_indicator(head.indicator), atom.indicator));
				}
			}
		},
		
		// retract/1
		"retract/1": function(thread, point, atom) {
			var clause = atom.args[0];
			if(pl.type.is_variable(clause)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(clause)) {
				thread.throw_error(pl.error.type("callable", clause, atom.indicator));
			} else {
				var head, body, module_atom, module_id;
				if(clause.indicator === ":/2") {
					module_atom = clause.args[0];
					clause = clause.args[1];
					if(!pl.type.is_atom(module_atom)) {
						thread.throw_error(pl.error.type("module", module_atom, atom.indicator));
						return;
					}
				} else {
					module_atom = new Term("user");
				}
				if(clause.indicator === ":-/2") {
					head = clause.args[0];
					body = clause.args[1];
				} else {
					head = clause;
					body = new Term("true");
				}
				if(pl.type.is_variable(head)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
					return;
				} else if(!pl.type.is_callable(head)) {
					thread.throw_error(pl.error.type("callable", head, atom.indicator));
					return;
				}
				module_id = module_atom.id;
				var get_module = thread.session.modules[module_id];
				if(!pl.type.is_module(get_module))
					return;
				if(!point.retract) {
					if(thread.is_public_predicate(head.indicator, module_id)
					&& head.indicator !== ",/2"
					&& !thread.session.modules.system.rules.hasOwnProperty(head.indicator)) {
						if(get_module.rules[head.indicator] !== undefined) {
							var states = [];
							if(typeof get_module.rules[head.indicator] === "function") {
								thread.throw_error(pl.error.permission("modify", "static_procedure", str_indicator(head.indicator), atom.indicator));
								return;
							}
							for(var i = 0; i < get_module.rules[head.indicator].length; i++) {
								thread.session.renamed_variables = {};
								var orule = get_module.rules[head.indicator][i];
								var rule = orule.rename(thread);
								if(rule.body === null)
									rule.body = new Term("true", []);
								var occurs_check = thread.get_flag("occurs_check").indicator === "true/0";
								var mgu = pl.unify(new Term(",", [head, body]), new Term(",", [rule.head, rule.body]), occurs_check);
								if(mgu !== null) {
									var state = new State(
										point.goal.replace(new Term(",", [
											new Term(":", [
												module_atom,
												new Term("retract", [new Term(":-", [head, body])]),
											]),
											new Term(",", [
												new Term("=", [head, rule.head]),
												new Term("=", [body, rule.body])
											])
										])), point.substitution, point);
									state.retract = orule;
									states.push(state);
								}
							}
							thread.prepend(states);
						}
					} else {
						thread.throw_error(pl.error.permission("modify", "static_procedure", str_indicator(head.indicator), atom.indicator));
					}
				} else {
					retract(thread, point, head.indicator, point.retract, get_module);
				}
			}
		},
		
		// retractall/1
		"retractall/1": function(thread, point, atom) {
			var head = atom.args[0];
			var context_module = "user";
			if(pl.type.is_term(head) && head.indicator === ":/2") {
				context_module = head.args[0].id;
				head = head.args[1];
			}
			if(pl.type.is_variable(head)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_callable(head)) {
				thread.throw_error(pl.error.type("callable", head, atom.indicator));
			} else if(!thread.is_public_predicate(head.indicator, context_module)
			|| head.indicator === ",/2"
			|| thread.session.modules.system.rules.hasOwnProperty(head.indicator)) {
				thread.throw_error(pl.error.permission("modify", "static_procedure", str_indicator(head.indicator), atom.indicator));
			} else {
				thread.prepend([
					new State(point.goal.replace(new Term(",", [
						new Term(":", [
							new Term(context_module),
							new Term("retract", [new pl.type.Term(":-", [head, new Var("_")])])
						]),
						new Term("fail", [])
					])), point.substitution, point),
					new State(point.goal.replace(null), point.substitution, point)
				]);
			}
		},

		// abolish/1
		"abolish/1": function(thread, point, atom) {
			var predicate = atom.args[0];
			var module_id;
			if(pl.type.is_term(predicate) && predicate.indicator === ":/2") {
				if(!pl.type.is_atom(predicate.args[0])) {
					thread.throw_error(pl.error.type("module", predicate.args[0], atom.indicator));
					return;
				}
				module_id = predicate.args[0].id;
				predicate = predicate.args[1];
			} else {
				module_id = "user";
			}
			if(pl.type.is_variable(predicate) || pl.type.is_term(predicate) && predicate.indicator === "//2"
			&& (pl.type.is_variable(predicate.args[0]) || pl.type.is_variable(predicate.args[1]))) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_term(predicate) || predicate.indicator !== "//2") {
				thread.throw_error(pl.error.type("predicate_indicator", predicate, atom.indicator));
			} else if(!pl.type.is_atom(predicate.args[0])) {
				thread.throw_error(pl.error.type("atom", predicate.args[0], atom.indicator));
			} else if(!pl.type.is_integer(predicate.args[1])) {
				thread.throw_error(pl.error.type("integer", predicate.args[1], atom.indicator));
			} else if(predicate.args[1].value < 0) {
				thread.throw_error(pl.error.domain("not_less_than_zero", predicate.args[1], atom.indicator));
			} else if(pl.type.is_number(thread.get_flag("max_arity")) && predicate.args[1].value > thread.get_flag("max_arity").value) {
				thread.throw_error(pl.error.representation("max_arity", atom.indicator));
			} else {
				var get_module = thread.session.modules[module_id];
				if(pl.type.is_module(get_module)) {
					var indicator = predicate.args[0].id + "/" + predicate.args[1].value;
					if(thread.is_public_predicate(indicator, module_id)
					&& indicator !== ",/2"
					&& !thread.session.modules.system.rules.hasOwnProperty(indicator)) {
						delete get_module.rules[indicator];
						delete get_module.indexed_clauses[indicator];
						delete get_module.non_indexable_clauses[indicator];
						delete get_module.public_predicates[indicator];
						delete get_module.multifile_predicates[indicator];
						thread.success(point);
					} else {
						thread.throw_error(pl.error.permission("modify", "static_procedure", atom.args[0], atom.indicator));
					}
				} else {
					thread.success(point);
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
			} else if( !pl.type.is_atom( atom1 ) ) {
				thread.throw_error( pl.error.type( "atom", atom1, atom.indicator ) );
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
			} else if( !pl.type.is_variable( subatom ) && !pl.type.is_atom( subatom ) ) {
				thread.throw_error( pl.error.type( "atom", subatom, atom.indicator ) );
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
					if(!pl.type.is_variable(list)) {
						var pointer = list;
						while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
							if(!pl.type.is_character(pointer.args[0]) && !pl.type.is_variable(pointer.args[0])) {
								thread.throw_error(pl.error.type("character", pointer.args[0], atom.indicator));
								return;
							}
							pointer = pointer.args[1];
						}
					}
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
					if(!pl.type.is_variable(list)) {
						var pointer = list;
						while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
							if(!pl.type.is_character_code(pointer.args[0]) && !pl.type.is_variable(pointer.args[0])) {
								thread.throw_error(pl.error.type("integer", pointer.args[0], atom.indicator));
								return;
							}
							pointer = pointer.args[1];
						}
					}
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
								thread.throw_error( pl.error.representation( "character_code", atom.indicator ) );
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
		
		// atomic_concat/3
		"atomic_concat/3": function( thread, point, atom ) {
			var atomic1 = atom.args[0], atomic2 = atom.args[1], concat = atom.args[2];
			if( pl.type.is_variable( atomic1 ) || pl.type.is_variable( atomic2 ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_atomic( atomic1 ) ) {
				thread.throw_error( pl.error.type( "atomic", atomic1, atom.indicator ) );
			} else if( !pl.type.is_atomic( atomic2 ) ) {
				thread.throw_error( pl.error.type( "atomic", atomic2, atom.indicator ) );
			} else if( !pl.type.is_variable( concat ) && !pl.type.is_atom( concat ) ) {
				thread.throw_error( pl.error.type( "atom", concat, atom.indicator ) );
			} else {
				var id = "";
				if( pl.type.is_atom( atomic1 ) ) {
					id += atomic1.id;
				} else {
					id += "" + atomic1.value;
				}
				if( pl.type.is_atom( atomic2 ) ) {
					id += atomic2.id;
				} else {
					id += "" + atomic2.value;
				}
				var atom = new Term(id, []);
				thread.prepend( [new State( point.goal.replace( new Term( "=", [atom, concat] ) ), point.substitution, point )] );
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
			} else if( !pl.type.is_atom( separator ) && !pl.type.is_number( separator ) ) {
				thread.throw_error( pl.error.type( "atomic", separator, atom.indicator ) );
			} else if( !pl.type.is_variable( concat ) && !pl.type.is_atom( concat ) ) {
				thread.throw_error( pl.error.type( "atom", concat, atom.indicator ) );
			} else {
				var id = "";
				var pointer = list;
				while( pl.type.is_term( pointer ) && pointer.indicator === "./2" ) {
					if( pl.type.is_variable( pointer.args[0] ) ) {
						thread.throw_error( pl.error.instantiation( atom.indicator ) );
						return;
					} else if( !pl.type.is_atom( pointer.args[0] ) && !pl.type.is_number( pointer.args[0] ) ) {
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
				thread.throw_error( pl.error.domain( "order", order, atom.indicator ) );
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
				thread.prepend( [new State( point.goal.replace( new Term( "=", [atom.args[0], op], atom.indicator ) ), point.substitution, point )] );
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
					if( lower.value <= upper.value ) {
						var states = [new State( point.goal.replace( new Term( "=", [bet, lower] ) ), point.substitution, point )];
						states.push( new State( point.goal.replace( new Term( "between", [new Num( lower.value+1, false ), upper, bet] ) ), point.substitution, point ) );
						thread.prepend( states );
					}
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
			var callable = atom.args[0];
			if(pl.type.is_term(callable)) {
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
			if(!pl.type.is_variable(stream)
			&& (!pl.type.is_stream(stream) || !thread.get_stream_by_alias(stream.alias)
										   && !thread.get_stream_by_alias(stream.id))
			&& (!pl.type.is_atom(stream) || !thread.get_stream_by_alias(stream.id))) {
				thread.throw_error( pl.error.domain("stream", stream, atom.indicator) );
			} else {
				if(pl.type.is_atom(stream))
					stream = thread.get_stream_by_alias(stream.id);
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
			if(!pl.type.is_variable(stream)
			&& (!pl.type.is_stream(stream) || !thread.get_stream_by_alias(stream.alias)
										   && !thread.get_stream_by_alias(stream.id))
			&& (!pl.type.is_atom(stream) || !thread.get_stream_by_alias(stream.id))) {
				thread.throw_error( pl.error.domain("stream", stream, atom.indicator) );
			} else {
				if(pl.type.is_atom(stream))
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
			} else if( !pl.type.is_stream( input ) && !pl.type.is_atom( input ) ) {
				thread.throw_error( pl.error.domain( "stream_or_alias", input, atom.indicator ) );
			} else if( !pl.type.is_stream( stream ) || !thread.get_stream_by_alias(input.alias)
													&& !thread.get_stream_by_alias(input.id) ) {
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
			} else if( !pl.type.is_stream( output ) && !pl.type.is_atom( output ) ) {
				thread.throw_error( pl.error.domain( "stream_or_alias", output, atom.indicator ) );
			} else if( !pl.type.is_stream( stream ) || !thread.get_stream_by_alias(output.alias)
													&& !thread.get_stream_by_alias(output.id) ) {
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
			if( pl.type.is_variable( dest ) || pl.type.is_variable( mode ) || pl.type.is_variable( options ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( mode ) && !pl.type.is_atom( mode ) ) {
				thread.throw_error( pl.error.type( "atom", mode, atom.indicator ) );
			} else if( !pl.type.is_list( options ) ) {
				thread.throw_error( pl.error.type( "list", options, atom.indicator ) );
			} else if( !pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.uninstantiation( stream, atom.indicator ) );
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
					if(stream2 === thread.session.standard_input || stream2 === thread.session.standard_output || stream2 === thread.session.standard_error) {
						thread.success( point );
						return;
					} else if( stream2 === thread.session.current_input ) {
						thread.session.current_input = thread.session.standard_input;
					} else if( stream2 === thread.session.current_output ) {
						thread.session.current_output = thread.session.standard_output;
					}
					if( stream2.alias !== null && stream2.alias !== undefined )
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
		"flush_output/0": [
			new Rule(new Term("flush_output", []), new Term(",", [new Term("current_output", [new Var("S")]),new Term("flush_output", [new Var("S")])]))
		],

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
			} else if( stream2.input === true ) {
				thread.throw_error( pl.error.permission( "output", "stream", stream, atom.indicator ) );
			} else {
				stream2.stream.flush();
				thread.success( point );
			}
		},

		// stream_property/2
		"stream_property/2": function( thread, point, atom ) {
			var stream = atom.args[0], property = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( !pl.type.is_variable( stream ) && !pl.type.is_stream( stream ) ) {
				thread.throw_error( pl.error.domain( "stream", stream, atom.indicator ) );
			} else if( !pl.type.is_variable( stream ) && (!pl.type.is_stream( stream2 ) || stream2.stream === null) ) {
				thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
			} else if( !pl.type.is_variable( property ) && !pl.type.is_stream_property( property ) ) {
				thread.throw_error( pl.error.domain( "stream_property", property, atom.indicator ) );
			} else {
				var streams = [];
				var states = [];
				var propvar = pl.type.is_variable(property);
				if( !pl.type.is_variable( stream ) )
					streams.push( stream2 );
				else
					for( var key in thread.session.streams )
						streams.push( thread.session.streams[key] );
				for( var i = 0; i < streams.length; i++ ) {
					var properties = [];
					// file_name/1
					if( (propvar || property.indicator === "file_name/1") && streams[i].filename )
						properties.push( new Term( "file_name", [new Term(streams[i].file_name, [])] ) );
					// mode/1
					if(propvar || property.indicator === "mode/1")
						properties.push( new Term( "mode", [new Term(streams[i].mode, [])] ) );
					// input/0 or output/0
					if(propvar || property.indicator === "input/0" || property.indicator === "output/0")
						properties.push( new Term( streams[i].input ? "input" : "output", [] ) );
					// alias/1
					if( (propvar || property.indicator === "alias/1") && streams[i].alias )
						properties.push( new Term( "alias", [new Term(streams[i].alias, [])] ) );
					// position/1
					if(propvar || property.indicator === "position/1")
						properties.push( new Term( "position", [
							new Term("position", [
								new Num(streams[i].char_count, false),
								new Num(streams[i].line_count, false),
								new Num(streams[i].line_position, false)
							])
						] ) );
					// end_of_stream/1
					if(propvar || property.indicator === "end_of_stream/1")
						properties.push( new Term( "end_of_stream", [new Term(
							streams[i].position === "end_of_stream" || streams[i].stream.eof && streams[i].stream.eof(streams[i].position) ? "at" :
							streams[i].position === "past_end_of_stream" ? "past" :
							"not", [])] ) );
					// eof_action/1
					if(propvar || property.indicator === "eof_action/1")	
						properties.push( new Term( "eof_action", [new Term(streams[i].eof_action, [])] ) );
					// reposition/1
					if(propvar || property.indicator === "reposition/1")
						properties.push( new Term( "reposition", [new Term(streams[i].reposition ? "true" : "false", [])] ) );
					// type/1
					if(propvar || property.indicator === "type/1")
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

		// stream_position_data
		"stream_position_data/3": function(thread, point, atom) {
			var field = atom.args[0], position = atom.args[1], value = atom.args[2];
			if(pl.type.is_variable(position)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_term(position) || position.indicator !== "position/3") {
				thread.throw_error(pl.error.domain("stream_position", position, atom.indicator));
			} else if(!pl.type.is_variable(field) && !pl.type.is_atom(field)) {
				thread.throw_error(pl.error.type("atom", field, atom.indicator));
			} else if(!pl.type.is_variable(value) && !pl.type.is_integer(value)) {
				thread.throw_error(pl.error.type("integer", value, atom.indicator));
			} else {
				var fields = ["char_count", "line_count", "line_position"];
				var states = [];
				var data_pos = {char_count: 0, line_count: 1, line_position: 2};
				if(pl.type.is_variable(field)) {
					for(var i = 0; i < fields.length; i++) {
						states.push(new State(point.goal.replace(
							new Term(",", [
								new Term("=", [new Term(fields[i]), field]),
								new Term("=", [value, position.args[data_pos[fields[i]]]])
							])
						), point.substitution, point));
					}
				} else if(data_pos.hasOwnProperty(field.id)) {
					states.push(new State(point.goal.replace(
						new Term("=", [value, position.args[data_pos[field.id]]])
					), point.substitution, point));
				}
				thread.prepend(states);
			}
		},

		// at_end_of_stream/0
		"at_end_of_stream/0": [
			new Rule(new Term("at_end_of_stream", []), new pl.type.Term(",", [new Term("current_input", [new Var("S")]),new Term(",", [new Term("stream_property", [new Var("S"),new Term("end_of_stream", [new Var("E")])]),new Term(",", [new Term("!", []),new Term(";", [new Term("=", [new Var("E"),new Term("at", [])]),new Term("=", [new Var("E"),new Term("past", [])])])])])]))
		],

		// at_end_of_stream/1
		"at_end_of_stream/1": function( thread, point, atom ) {
			var stream = atom.args[0];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_stream( stream2 ) || stream2.stream === null ) {
				thread.throw_error( pl.error.existence( "stream", stream, atom.indicator ) );
			} else {
				var e = thread.next_free_variable();
				thread.prepend( [new State(
					point.goal.replace(
						new Term(",", [new Term("stream_property", [stream2,new Term("end_of_stream", [e])]),
						new Term(",", [new Term("!", []),new Term(";", [new Term("=", [e,new Term("at", [])]),
						new Term("=", [e,new Term("past", [])])])])])
					),
					point.substitution,
					point
				)] );
			}
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
				if( position.indicator === "position/3" ) {
					stream2.position = position.args[0].value;
					stream2.char_count = position.args[0].value;
					stream2.line_count = position.args[1].value;
					stream2.line_position = position.args[2].value;
				} else {
					stream2.position = position.id;
				}
				thread.success( point );
			}
		},



		//  CHARACTER INPUT OUTPUT
		
		// get_char/1
		"get_char/1": [
			new Rule(new Term("get_char", [new Var("C")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("get_char", [new Var("S"),new Var("C")])]))
		],

		// get_char/2
		"get_char/2": function( thread, point, atom ) {
			var stream = atom.args[0], char = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( char ) && !pl.type.is_in_character( char ) ) {
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
				} else if( stream2.position === "past_end_of_stream" ) {
					stream_char = "end_of_file";
					stream2.position = "past_end_of_stream";
				} else {
					stream_char = stream2.stream.get( 1, stream2.position );
					if( stream_char === null ) {
						thread.throw_error( pl.error.representation( "character", atom.indicator ) );
						return;
					} else if(stream_char === "end_of_stream") {
						stream_char = "end_of_file";
						stream2.position = "past_end_of_stream";
					} else {
						stream2.position++;
						stream2.char_count++;
						stream2.line_position++;
						if(stream_char === "\n") {
							stream2.line_count++;
							stream2.line_position = 0;
						}
					}
				}
				thread.prepend( [new State(
					point.goal.replace( new Term( "=", [new Term(stream_char,[]), char] ) ),
					point.substitution,
					point
				)] );
			}
		},

		// get_code/1
		"get_code/1": [
			new Rule(new Term("get_code", [new Var("C")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("get_code", [new Var("S"),new Var("C")])]))
		],

		// get_code/2
		"get_code/2": function( thread, point, atom ) {
			var stream = atom.args[0], code = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( code ) && !pl.type.is_integer( code ) ) {
				thread.throw_error( pl.error.type( "integer", code, atom.indicator ) );
			} else if( pl.type.is_integer( code ) && !pl.type.is_in_character_code( code ) ) {
				thread.throw_error( pl.error.representation( "in_character_code", atom.indicator ) );
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
				} else if( stream2.position === "past_end_of_stream" ) {
					stream_code = -1;
					stream2.position = "past_end_of_stream";
				} else {
					stream_code = stream2.stream.get( 1, stream2.position );
					if( stream_code === null ) {
						thread.throw_error( pl.error.representation( "character", atom.indicator ) );
						return;
					} else if(stream_code === "end_of_stream") {
						stream_code = -1;
						stream2.position = "past_end_of_stream";
					} else {
						stream_code = codePointAt( stream_code, 0 );
						stream2.position++;
						stream2.char_count++;
						stream2.line_position++;
						if(stream_code === 10) {
							stream2.line_count++;
							stream2.line_position = 0;
						}
					}
				}
				thread.prepend( [new State(
					point.goal.replace( new Term( "=", [new Num(stream_code, false), code] ) ),
					point.substitution,
					point
				)] );
			}
		},

		// peek_char/1
		"peek_char/1": [
			new Rule(new Term("peek_char", [new Var("C")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("peek_char", [new Var("S"),new Var("C")])]))
		],

		// peek_char/2
		"peek_char/2": function( thread, point, atom ) {
			var stream = atom.args[0], char = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( char ) && !pl.type.is_in_character( char ) ) {
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
				} else if( stream2.position === "past_end_of_stream" ) {
					stream_char = "end_of_file";
					stream2.position = "past_end_of_stream";
				} else {
					stream_char = stream2.stream.get( 1, stream2.position );
					if( stream_char === null ) {
						thread.throw_error( pl.error.representation( "character", atom.indicator ) );
						return;
					} else if(stream_char === "end_of_stream") {
						stream_char = "end_of_file";
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
		"peek_code/1": [
			new Rule(new Term("peek_code", [new Var("C")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("peek_code", [new Var("S"),new Var("C")])]))
		],

		// peek_code/2
		"peek_code/2": function( thread, point, atom ) {
			var stream = atom.args[0], code = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( code ) && !pl.type.is_integer( code ) ) {
				thread.throw_error( pl.error.type( "integer", code, atom.indicator ) );
			} else if( pl.type.is_integer( code ) && !pl.type.is_in_character_code( code ) ) {
				thread.throw_error( pl.error.representation( "in_character_code", atom.indicator ) );
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
					} else if(stream_code === "end_of_stream") {
						stream_code = -1;
					} else {
						stream_code = codePointAt( stream_code, 0 );
					}
				}
				thread.prepend( [new State(
					point.goal.replace( new Term( "=", [new Num(stream_code, false), code] ) ),
					point.substitution,
					point
				)] );
			}
		},

		// put_char/1
		"put_char/1": [
			new Rule(new Term("put_char", [new Var("C")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_char", [new Var("S"),new Var("C")])]))
		],

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
					stream2.char_count++;
					stream2.line_position++;
					if(char.id === "\n") {
						stream2.line_count++;
						stream2.line_position = 0;
					}
					thread.success( point );
				}
			}
		},

		// put_code/1
		"put_code/1": [
			new Rule(new Term("put_code", [new Var("C")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_code", [new Var("S"),new Var("C")])]))
		],

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
				if( stream2.stream.put( fromCodePoint( code.value ), stream2.position ) ) {
					if(typeof stream2.position === "number")
						stream2.position++;
					stream2.char_count++;
					stream2.line_position++;
					if(code.value === 10) {
						stream2.line_count++;
						stream2.line_position = 0;
					}
					thread.success( point );
				}
			}
		},

		// nl/0
		"nl/0": [
			new Rule(new Term("nl"), new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_char", [new Var("S"),new Term("\n")])]))
		],

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
		"get_byte/1": [
			new Rule(new Term("get_byte", [new Var("B")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("get_byte", [new Var("S"),new Var("B")])]))
		],

		// get_byte/2
		"get_byte/2": function( thread, point, atom ) {
			var stream = atom.args[0], byte = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( byte ) && !pl.type.is_in_byte( byte ) ) {
				thread.throw_error( pl.error.type( "in_byte", byte, atom.indicator ) );
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
					stream_byte = -1;
					stream2.position = "past_end_of_stream";
				} else {
					stream_byte = stream2.stream.get_byte( stream2.position );
					if( stream_byte === null ) {
						thread.throw_error( pl.error.representation( "byte", atom.indicator ) );
						return;
					} else if(stream_byte === "end_of_stream") {
						stream_byte = -1;
						stream2.position = "past_end_of_stream";
					} else {
						stream2.position++;
						stream2.char_count++;
						stream2.line_position++;
					}
				}
				thread.prepend( [new State(
					point.goal.replace( new Term( "=", [new Num(stream_byte,false), byte] ) ),
					point.substitution,
					point
				)] );
			}
		},
		
		// peek_byte/1
		"peek_byte/1": [
			new Rule(new Term("peek_byte", [new Var("B")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("peek_byte", [new Var("S"),new Var("B")])]))
		],

		// peek_byte/2
		"peek_byte/2": function( thread, point, atom ) {
			var stream = atom.args[0], byte = atom.args[1];
			var stream2 = pl.type.is_stream( stream ) ? stream : thread.get_stream_by_alias( stream.id );
			if( pl.type.is_variable( stream ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_variable( byte ) && !pl.type.is_in_byte( byte ) ) {
				thread.throw_error( pl.error.type( "in_byte", byte, atom.indicator ) );
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
					stream_byte = -1;
					stream2.position = "past_end_of_stream";
				} else {
					stream_byte = stream2.stream.get_byte( stream2.position );
					if( stream_byte === null ) {
						thread.throw_error( pl.error.representation( "byte", atom.indicator ) );
						return;
					} else if(stream_byte === "end_of_stream") {
						stream_byte = -1;
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
		"put_byte/1": [
			new Rule(new Term("put_byte", [new Var("B")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("put_byte", [new Var("S"),new Var("B")])]))
		],

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
					stream2.char_count++;
					stream2.line_position++;
					thread.success( point );
				}
			}
		},



		// TERM INPUT/OUTPUT

		// read/1
		"read/1": [
			new Rule(new Term("read", [new Var("T")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("read_term", [new Var("S"),new Var("T"),new Term("[]")])]))
		],

		// read/2
		"read/2": [
			new Rule(new Term("read", [new Var("S"), new Var("T")]), new Term("read_term", [new Var("S"),new Var("T"),new Term("[]")]))
		],

		// read_term/2
		"read_term/2": [
			new Rule(new Term("read_term", [new Var("T"),new Var("O")]), new Term(",", [new Term("current_input", [new Var("S")]),new Term("read_term", [new Var("S"),new Var("T"),new Var("O")])]))
		],

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
			} else if( stream2.position === "past_end_of_stream" && stream2.eof_action === "eof_code" ) {
				expr = {
					value: new Term("end_of_file", []),
					type: SUCCESS,
					len: -1
				};
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
					var lexical_error = false;
					// Get term
					while( last_token === null || lexical_error || last_token.name !== "atom" || last_token.value !== "." || tokens.length > 0 && expr.type === ERROR ) {
						char = stream2.stream.get( 1, stream2.position );
						while(char !== null && char !== "." && char !== "end_of_stream" && char !== "past_end_of_stream") {
							stream2.position++;
							text += char;
							char = stream2.stream.get( 1, stream2.position );
						}
						if( char === null ) {
							thread.throw_error( pl.error.representation( "character", atom.indicator ) );
							return;
						} else if( char === "end_of_stream" || char === "past_end_of_stream" ) {
							if(tokens === null || tokens.length === 0) {
								stream2.position = "past_end_of_stream";
								expr = {
									value: new Term("end_of_file", []),
									type: SUCCESS,
									len: -1
								};
								break;
							} else if(expr) {
								thread.throw_error( pl.error.syntax( last_token, "unexpected end of file", false ) );
								return;
							} else {
								thread.throw_error( pl.error.syntax( last_token, "token not found", true ) );
								return;
							}
						} else if(char === ".") {
							stream2.position++;
							text += char;
						}
						tokenizer = new Tokenizer( thread );
						tokenizer.new_text( text );
						tokens = tokenizer.get_tokens();
						num_token = tokens !== null && tokens.length > 1 ? tokens[tokens.length-2] : null;
						last_token = tokens !== null && tokens.length > 0 ? tokens[tokens.length-1] : null;
						if(tokens === null)
							continue;
						lexical_error = false;
						for(var i = 0; i < tokens.length && !lexical_error; i++)
							lexical_error = tokens[i].name === "lexical";
						if(lexical_error)
							continue;
						expr = parseExpr(thread, tokens, 0, thread.__get_max_priority(), false);
						if(num_token && num_token.name === "number" && !num_token.float && !num_token.blank && last_token.value === ".") {
							var next_char = stream2.stream.get(1, stream2.position);
							if(next_char >= '0' && next_char <= '9') {
								stream2.position++;
								text += next_char;
								last_token = null;
								continue;
							}
						}
					}
					if(last_token) {
						if(last_token.line_position === last_token.len)
							stream2.line_position += last_token.line_position;
						else
							stream2.line_position = last_token.line_position;
						stream2.line_count += last_token.line_count;
						stream2.char_count += last_token.len;
					}
					// Succeed analyzing term
					if( expr.type === SUCCESS && (expr.len === -1 || expr.len === tokens.length-1 && last_token.value === "." )) {
						thread.session.renamed_variables = {};
						expr = expr.value.rename( thread );
						var eq = new Term( "=", [term, expr] );
						// Variables
						if( obj_options.variables ) {
							var vars = arrayToList( map( nub( expr.variables() ), function(v) { return new Var(v); } ) );
							eq = new Term( ",", [eq, new Term( "=", [obj_options.variables, vars] )] )
						}
						// Variable names
						if( obj_options.variable_names ) {
							var vars = nub(expr.variables());
							var plvars = [];
							for(var i = 0; i < vars.length; i++) {
								var v = vars[i];
								for( var prop in thread.session.renamed_variables ) {
									if( thread.session.renamed_variables.hasOwnProperty( prop ) ) {
										if( thread.session.renamed_variables[ prop ] === v ) {
											plvars.push(new Term( "=", [new Term( prop, []), new Var(v)] ));
											break;
										}
									}
								}
							}
							plvars = arrayToList(plvars);
							eq = new Term( ",", [eq, new Term( "=", [obj_options.variable_names, plvars] )] );
						}
						// Singletons
						if( obj_options.singletons ) {
							var vars = nub(new Rule(expr, null).singleton_variables(true));
							var plvars = [];
							for(var i = 0; i < vars.length; i++) {
								var v = vars[i];
								for( var prop in thread.session.renamed_variables ) {
									if( thread.session.renamed_variables.hasOwnProperty( prop ) ) {
										if( thread.session.renamed_variables[ prop ] === v ) {
											plvars.push(new Term( "=", [new Term( prop, []), new Var(v)] ));
											break;
										}
									}
								}
							}
							plvars = arrayToList(plvars);
							eq = new Term( ",", [eq, new Term( "=", [obj_options.singletons, plvars] )] );
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
		"write/1": [
			new Rule(new Term("write", [new Var("T")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("write", [new Var("S"),new Var("T")])]))
		],
		
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
		"writeq/1": [
			new Rule(new Term("writeq", [new Var("T")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("writeq", [new Var("S"),new Var("T")])]))
		],
		
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
		"write_canonical/1": [
			new Rule(new Term("write_canonical", [new Var("T")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("write_canonical", [new Var("S"),new Var("T")])]))
		],
		
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
		"write_term/2": [
			new Rule(new Term("write_term", [new Var("T"),new Var("O")]), new Term(",", [new Term("current_output", [new Var("S")]),new Term("write_term", [new Var("S"),new Var("T"),new Var("O")])]))
		],
		
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
					if(property.indicator === "variable_names/1")
						obj_options[property.id] = property.args[0];
					else
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
					var nl = (text.match(/\n/g) || []).length;
					stream2.line_count += nl;
					if(nl > 0)
						stream2.line_position = text.length - text.lastIndexOf("\n") - 1;
					else
						stream2.line_position += text.length;
					stream2.char_count += text.length;
					thread.success( point );
				}
			}
		},
		
		// IMPLEMENTATION DEFINED HOOKS
		
		// halt/0
		"halt/0": function( thread, point, _ ) {
			if( thread.get_flag("nodejs").indicator === "true/0" )
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
				if( thread.get_flag("nodejs").indicator === "true/0" )
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
			} else if( !pl.type.is_modifiable_flag( flag ) ) {
				thread.throw_error( pl.error.permission( "modify", "flag", flag, atom.indicator ) );
			} else if( !pl.type.is_value_flag( flag, value ) ) {
				thread.throw_error( pl.error.domain( "flag_value", new Term( "+", [flag, value] ), atom.indicator ) );
			} else {
				thread.session.flag[flag.id] = value;
				thread.success( point );
			}
		},



		// LOAD PROLOG SOURCE FILES

		// consult/1
		"consult/1": function(thread, point, atom) {
			var src = atom.args[0];
			var context_module = "user";
			if(src.indicator === ":/2") {
				context_module = src.args[0].id;
				src = src.args[1];
			}
			if(pl.type.is_variable(src)) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if(!pl.type.is_atom(src)) {
				thread.throw_error( pl.error.type( "atom", src, atom.indicator ) );
			} else {
				if(thread.consult(src.id, {
					context_module: context_module,
					text: false,
					html: false,
					success: function() {
						thread.success(point);
						thread.again();
					},
					error: function(err) {
						thread.throw_error(err);
						thread.again();
					}
				}));
				return true;
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

		// time_property
		"time_property/2": function(thread, point, atom) {
			var time = atom.args[0], property = atom.args[1];
			if(pl.type.is_variable(time)) {
				thread.throw_error(pl.error.instantiation(atom.indicator));
			} else if(!pl.type.is_variable(time) && !pl.type.is_number(time)) {
				thread.throw_error(pl.error.type("number", time, atom.indicator));
			} else if(!pl.type.is_variable(property) && !pl.type.is_time_property(property)) {
				thread.throw_error(pl.error.domain("time_property", property, atom.indicator));
			} else {
				var props;
				if(pl.type.is_variable(property)) {
					props = ["year", "month", "day", "hours", "minutes", "seconds", "milliseconds", "weekday"];
				} else {
					props = [property.id];
				}
				var date = new Date(time.value);
				var value;
				var states = [];
				for(var i = 0; i < props.length; i++) {
					switch(props[i]) {
						case "year":
							value = new Term("year", [new Num(date.getFullYear(), false)]);
							break;
						case "month":
							value = new Term("month", [new Num(date.getMonth(), false)]);
							break;
						case "day":
							value = new Term("day", [new Num(date.getDate(), false)]);
							break;
						case "hours":
							value = new Term("hours", [new Num(date.getHours(), false)]);
							break;
						case "minutes":
							value = new Term("minutes", [new Num(date.getMinutes(), false)]);
							break;
						case "seconds":
							value = new Term("seconds", [new Num(date.getSeconds(), false)]);
							break;
						case "milliseconds":
							value = new Term("milliseconds", [new Num(date.getMilliseconds(), false)]);
							break;
						case "weekday":
							value = new Term("weekday", [new Num(date.getDay(), false)]);
							break;
					}
					states.push(new State(
						point.goal.replace( new Term( "=", [property, value] ) ), 
						point.substitution,
						point
					));
				}
				thread.prepend(states);
			}
		},

		// time_year/2
		"time_year/2": function( thread, point, atom ) {
			var time = atom.args[0], year = atom.args[1];
			if(pl.type.is_variable(time)) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if(!pl.type.is_number(time)) {
				thread.throw_error( pl.error.type( "number", time, atom.indicator ) );
			} else if(!pl.type.is_variable(year) && !pl.type.is_integer(year)) {
				thread.throw_error( pl.error.type( "integer", year, atom.indicator ) );
			} else {
				var value = new Num(new Date(time.value).getFullYear(), false);
				thread.prepend( [new State(
					point.goal.replace( new Term( "=", [year, value] ) ), 
					point.substitution,
					point
				)] );
			}
		},

		// time_month/2
		"time_month/2": function( thread, point, atom ) {
			var time = atom.args[0], month = atom.args[1];
			if(pl.type.is_variable(time)) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if(!pl.type.is_number(time)) {
				thread.throw_error( pl.error.type( "number", time, atom.indicator ) );
			} else if(!pl.type.is_variable(month) && !pl.type.is_integer(month)) {
				thread.throw_error( pl.error.type( "integer", month, atom.indicator ) );
			} else {
				var value = new Num(new Date(time.value).getMonth(), false);
				thread.prepend( [new State(
					point.goal.replace( new Term( "=", [month, value] ) ), 
					point.substitution,
					point
				)] );
			}
		},



		// GRAMMARS

		// phrase/3
		"phrase/3": function( thread, point, atom ) {
			var grbody = atom.args[0], s0 = atom.args[1], s = atom.args[2];
			var context_module = "user";
			if(grbody.indicator === ":/2") {
				context_module = grbody.args[0].id;
				grbody = grbody.args[1];
			}
			if( pl.type.is_variable( grbody ) ) {
				thread.throw_error( pl.error.instantiation( atom.indicator ) );
			} else if( !pl.type.is_callable( grbody ) ) {
				thread.throw_error( pl.error.type( "callable", grbody, atom.indicator ) );
			} else {
				var goal = body_to_dcg( grbody.clone(), s0, thread );
				goal.value = new Term(":", [new Term(context_module), new Term("call", [goal.value])]);
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
			msg += "Copyright (C) 2017 - 2022 José Antonio Riaza Valverde\n\n";
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

	}, "all", {
		meta_predicates: {
			// '$findall'(?, 0, -, ?)
			"$findall/4": new Term("$findall", [new Term("?"), new Num(0, false), new Term("-"), new Term("?")]),
			// '$bagof'(?, ^, -)
			"$bagof/3": new Term("$bagof", [new Term("?"), new Term("^"), new Term("-")]),
			// '$setof'(?, ^, -)
			"$setof/3": new Term("$setof", [new Term("?"), new Term("^"), new Term("-")]),
			// '$if'(0, 0, 0)
			"$if/3": new Term("$if", [new Num(0, false), new Num(0, false), new Num(0, false)]),
			// (0;0)
			";/2": new Term(";", [new Num(0, false), new Num(0, false)]),
			// (0->0)
			"->/2": new Term("->", [new Num(0, false), new Num(0, false)]),
			// (0->0)
			"*->/2": new Term("*->", [new Num(0, false), new Num(0, false)]),
			// (\+0)
			"\\+/1": new Term("\\+", [new Num(0, false)]),
			// abolish(:)
			"abolish/1": new Term("abolish", [new Term(":")]),
			// asserta(:)
			"asserta/1": new Term("asserta", [new Term(":")]),
			// assertz(:)
			"assertz/1": new Term("assertz", [new Term(":")]),
			// bagof(?, ^, -)
			"bagof/3": new Term("bagof", [new Term("?"), new Term("^"), new Term("-")]),
			// call(0)
			"call/1": new Term("call", [new Num(0, false)]),
			// call(1, ?)
			"call/2": new Term("call", [new Num(1, false), new Term("?")]),
			// call(2, ?, ?)
			"call/3": new Term("call", [new Num(2, false), new Term("?"), new Term("?")]),
			// call(3, ?, ?, ?)
			"call/4": new Term("call", [new Num(3, false), new Term("?"), new Term("?"), new Term("?")]),
			// call(4, ?, ?, ?, ?)
			"call/5": new Term("call", [new Num(4, false), new Term("?"), new Term("?"), new Term("?"), new Term("?")]),
			// call(5, ?, ?, ?, ?, ?)
			"call/6": new Term("call", [new Num(5, false), new Term("?"), new Term("?"), new Term("?"), new Term("?"), new Term("?")]),
			// call(6, ?, ?, ?, ?, ?, ?)
			"call/7": new Term("call", [new Num(6, false), new Term("?"), new Term("?"), new Term("?"), new Term("?"), new Term("?"), new Term("?")]),
			// call(7, ?, ?, ?, ?, ?, ?, ?)
			"call/8": new Term("call", [new Num(6, false), new Term("?"), new Term("?"), new Term("?"), new Term("?"), new Term("?"), new Term("?"), new Term("?")]),
			// call_cleanup(0, 0)
			"call_cleanup/2": new Term("call_cleanup", [new Num(0, false), new Num(0, false)]),
			// catch(0, ?, 0)
			"catch/3": new Term("catch", [new Num(0, false), new Term("?"), new Num(0, false)]),
			// consult(:)
			"consult/1": new Term("consult", [new Term(":")]),
			// clause(:, ?)
			"clause/2": new Term("clause", [new Term(":"), new Term("?")]),
			// current_predicate(?, :)
			"current_predicate/2": new Term("current_predicate", [new Term("?"), new Term(":")]),
			// findall(?, 0, -)
			"findall/3": new Term("findall", [new Term("?"), new Num(0, false), new Term("-")]),
			// findall(?, 0, -, ?)
			"findall/4": new Term("findall", [new Term("?"), new Num(0, false), new Term("-"), new Term("?")]),
			// forall(0, 0)
			"forall/2": new Term("forall", [new Num(0, false), new Num(0, false)]),
			// listing(:)
			"listing/1": new Term("listing", [new Term(":")]),
			// once(0)
			"once/1": new Term("once", [new Num(0, false)]),
			// phrase(:, ?)
			"phrase/2": new Term("phrase", [new Term(":"),new Term("?")]),
			// phrase(:, ?, ?)
			"phrase/3": new Term("phrase", [new Term(":"),new Term("?"), new Term("?")]),
			// retract(:)
			"retract/1": new Term("retract", [new Term(":")]),
			// retractall(:)
			"retractall/1": new Term("retractall", [new Term(":")]),
			// setup_call_cleanup(0, 0, 0)
			"setup_call_cleanup/3": new Term("setup_call_cleanup", [new Num(0, false), new Num(0, false), new Num(0, false)]),
			// setof(?, ^, -)
			"setof/3": new Term("setof", [new Term("?"), new Term("^"), new Term("-")])
		}
	});

	if( typeof module !== 'undefined' ) {
		module.exports = pl;
	} else {
		window.pl = pl;
	}
	
})();

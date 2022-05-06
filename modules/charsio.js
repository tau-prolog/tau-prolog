var pl;
(function(pl) {
	var name = "charsio";
	var predicates = function() {
		return {

            // write_term_to_chars/3
            "write_term_to_chars/3": function(thread, point, atom) {
                var term = atom.args[0], options = atom.args[1], chars = atom.args[2];
                if(!pl.type.is_variable(chars) && !pl.type.is_list(chars)) {
                    thread.throw_error(pl.error.type("list", chars, atom.indicator));
                } else if(pl.type.is_variable(options)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else {
                    // check chars
                    if(!pl.type.is_variable(chars)) {
                        var pointer = chars;
                        while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
                            var char = pointer.args[0];
                            if(!pl.type.is_character(char)) {
                                thread.throw_error(pl.error.type("character", char, atom.indicator));
                                return;
                            }
                            pointer = pointer.args[1];
                        }
                        if(!pl.type.is_variable(pointer) && !pl.type.is_empty_list(pointer)) {
                            thread.throw_error(pl.error.type("list", chars, atom.indicator));
                            return;
                        }
                    }
                    // get options
                    var obj_options = {};
                    var pointer = options;
                    var property;
                    while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
                        property = pointer.args[0];
                        if(pl.type.is_variable(property)) {
                            thread.throw_error( pl.error.instantiation( atom.indicator ) );
                            return;
                        } else if(!pl.type.is_write_option(property)) {
                            thread.throw_error( pl.error.domain("write_option", property, atom.indicator));
                            return;
                        }
                        obj_options[property.id] = property.args[0].id === "true";
                        pointer = pointer.args[1];
                    }
                    if(pointer.indicator !== "[]/0") {
                        if(pl.type.is_variable(pointer))
                            thread.throw_error(pl.error.instantiation(atom.indicator));
                        else
                            thread.throw_error(pl.error.type("list", options, atom.indicator));
                        return;
                    } else {
                        obj_options.session = thread.session;
                        var text = term.toString( obj_options );
                        var list = new pl.type.Term("[]", []);
                        for(var i = pl.utils.stringLength(text)-1; i >= 0; i--)
                            list = new pl.type.Term(".", [new pl.type.Term(pl.utils.fromCodePoint(pl.utils.codePointAt(text, i)), []), list]);
                        thread.prepend([new pl.type.State(
                            point.goal.replace(new pl.type.Term("=", [chars, list])),
                            point.substitution,
                            point
                        )]);
                    }
                }
            },

            // fabricate_var_name/3
            "fabricate_var_name/3": [
                new pl.type.Rule(new pl.type.Term("fabricate_var_name", [new pl.type.Var("VarType"),new pl.type.Var("VarName"),new pl.type.Var("N")]), new pl.type.Term(",", [new pl.type.Term("char_code", [new pl.type.Term("A", []),new pl.type.Var("AC")]),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("LN"),new pl.type.Term("+", [new pl.type.Term("mod", [new pl.type.Var("N"),new pl.type.Num(26, false)]),new pl.type.Var("AC")])]),new pl.type.Term(",", [new pl.type.Term("char_code", [new pl.type.Var("LC"),new pl.type.Var("LN")]),new pl.type.Term(",", [new pl.type.Term("is", [new pl.type.Var("NN"),new pl.type.Term("//", [new pl.type.Var("N"),new pl.type.Num(26, false)])]),new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("=:=", [new pl.type.Var("NN"),new pl.type.Num(0, false)]),new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("==", [new pl.type.Var("VarType"),new pl.type.Term("fabricated", [])]),new pl.type.Term("atom_chars", [new pl.type.Var("VarName"),new pl.type.Term(".", [new pl.type.Term("_", []),new pl.type.Term(".", [new pl.type.Var("LC"),new pl.type.Term("[]", [])])])])]),new pl.type.Term("->", [new pl.type.Term("==", [new pl.type.Var("VarType"),new pl.type.Term("numbervars", [])]),new pl.type.Term("atom_chars", [new pl.type.Var("VarName"),new pl.type.Term(".", [new pl.type.Var("LC"),new pl.type.Term("[]", [])])])])])]),new pl.type.Term(",", [new pl.type.Term("number_chars", [new pl.type.Var("NN"),new pl.type.Var("NNChars")]),new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term("==", [new pl.type.Var("VarType"),new pl.type.Term("fabricated", [])]),new pl.type.Term("atom_chars", [new pl.type.Var("VarName"),new pl.type.Term(".", [new pl.type.Term("_", []),new pl.type.Term(".", [new pl.type.Var("LC"),new pl.type.Var("NNChars")])])])]),new pl.type.Term("->", [new pl.type.Term("==", [new pl.type.Var("VarType"),new pl.type.Term("numbervars", [])]),new pl.type.Term("atom_chars", [new pl.type.Var("VarName"),new pl.type.Term(".", [new pl.type.Var("LC"),new pl.type.Var("NNChars")])])])])])])])])])]))
            ]

        };
    };
	var exports = ["write_term_to_chars/3"];
	if(typeof module !== 'undefined') {
		module.exports = function(p) {
			pl = p;
			new pl.type.Module(name, predicates(), exports);
		};
	} else {
		new pl.type.Module(name, predicates(), exports);
	}
})(pl);
var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
            
            // OPERATING SYSTEM INTERACTION

			// shell/1
			"shell/1": function( thread, point, atom ) {
				var command = atom.args[0];
				thread.prepend( [new pl.type.State(
					point.goal.replace( new pl.type.Term("shell", [command, new pl.type.Num(0, false)]) ),
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
					if(pl.flag["nodejs"].value.indicator === "true/0") {
						const { exec } = require('child_process');
						exec( command.id, function() {} ).on( 'exit', function(code) {
							thread.prepend( [new pl.type.State(
								point.goal.replace( new pl.type.Term("=", [status, new pl.type.Num(code, false)]) ),
								point.substitution,
								point
							)] );
							thread.again();
						} );
						return true;
					} else {
						try {
							eval( command.id );
							thread.prepend( [new pl.type.State(
								point.goal.replace( new pl.type.Term("=", [status, new pl.type.Num(0, false)]) ),
								point.substitution,
								point
							)] );
						} catch( error ) {
							thread.prepend( [new pl.type.State(
								point.goal.replace( new pl.type.Term("=", [status, new pl.type.Num(1, false)]) ),
								point.substitution,
								point
							)] );
						}
					}
				}
            },
            
            // cd/1
            "cd/1": function(thread, point, atom) {
                var path = atom.args[0];
                if(pl.type.is_variable(path)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_atom(path)) {
                    thread.throw_error(pl.error.type("atom", path, atom.indicator));
                } else {
                    if(thread.session.flag.nodejs.indicator === "true/0") {
                        try {
                            process.chdir(path.id);
                            thread.success(point);
                        } catch(err) {
                            thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                        }
                    } else {
                        var working_directory = pl.utils.cd(thread.session.working_directory, path.id);
                        if(working_directory === null) {
                            thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                        } else {
                            thread.session.working_directory = working_directory;
                            thread.success(point);
                        }
                    }
                }
            },

            // ls/0
            "ls/0": function(thread, point, atom) {
                thread.prepend([new pl.type.State(
					point.goal.replace( new pl.type.Term("ls", [new pl.type.Term(".", [])]) ),
					point.substitution,
					point
				)]);
            },

            // ls/1
            "ls/1": function(thread, point, atom) {
                var path = atom.args[0];
                if(pl.type.is_variable(path)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_atom(path)) {
                    thread.throw_error(pl.error.type("atom", path, atom.indicator));
                } else {
                    if(thread.session.flag.nodejs.indicator === "true/0") {
                        var fs = require('fs');
                        fs.readdir(path.id, function(_err, items) {
                            var listing = items.join(" ");
                            thread.prepend([new pl.type.State(
                                point.goal.replace( new pl.type.Term("write", [new pl.type.Term(listing, [])]) ),
                                point.substitution,
                                point
                            )]);
                            thread.again();
                        });
                        return true;
                    } else {
                        var directory = pl.utils.cd(thread.session.working_directory, path.id);
                        var fs = thread.session.file_system;
                        var list = [];
                        for(var file in fs.files) {
                            if(file.substr(0, directory.length) === directory) {
                                var rest = file.substr(directory.length).split("/");
                                if(list.indexOf(rest[0]) === -1)
                                    list.push(rest[0]);
                            }
                        }
                        var listing = list.join(" ");
                        thread.prepend([new pl.type.State(
                            point.goal.replace( new pl.type.Term("write", [new pl.type.Term(listing, [])]) ),
                            point.substitution,
                            point
                        )]);
                    }
                }
            },

            // pwd/0
            "pwd/0": function(thread, point, atom) {
                var wd;
                if(thread.session.flag.nodejs.indicator === "true/0") {
                    wd = process.cwd();
                } else {
                    wd = thread.session.working_directory;
                }
                thread.prepend([new pl.type.State(
                    point.goal.replace( new pl.type.Term("write", [new pl.type.Term(wd, [])]) ),
                    point.substitution,
                    point
                )]);
            },

            // cwd/1
            "cwd/1": function(thread, point, atom) {
                var path = atom.args[0];
                if(!pl.type.is_variable(path) && !pl.type.is_atom(path)) {
                    thread.throw_error(pl.error.type("atom", path, atom.indicator));
                } else {
                    var wd;
                    if(thread.session.flag.nodejs.indicator === "true/0") {
                        wd = process.cwd();
                    } else {
                        wd = thread.session.working_directory;
                    }
                    thread.prepend([new pl.type.State(
                        point.goal.replace( new pl.type.Term("=", [path, new pl.type.Term(wd, [])]) ),
                        point.substitution,
                        point
                    )]);
                }
            },

            // rm/1
            "rm/1": function(thread, point, atom) {
                var path = atom.args[0];
                if(pl.type.is_variable(path)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_atom(path)) {
                    thread.throw_error(pl.error.type("atom", path, atom.indicator));
                } else {
                    if(thread.session.flag.nodejs.indicator === "true/0") {
                        var fs = require('fs');
                        fs.stat(path.id, function(error, stat) {
                            if(error) {
                                thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                                thread.again();
                            } else {
                                if(stat.isDirectory()) {
                                    fs.rmdir(path.id, function(error) {
                                        if(error)
                                            thread.throw_error(pl.error.permission("delete", "source_sink", path, atom.indicator));
                                        else
                                            thread.success( point );
                                        thread.again();
                                    });
                                } else {
                                    fs.unlink(path.id, function(error) {
                                        if(error)
                                            thread.throw_error(pl.error.permission("delete", "source_sink", path, atom.indicator));
                                        else
                                            thread.success( point );
                                        thread.again();
                                    });
                                }
                            }
                        });
                        return true;
                    } else {
                        var file = pl.utils.cd(thread.session.working_directory, path.id);
                        if(thread.session.file_system.files.hasOwnProperty(file)) {
                            thread.session.file_system.files[file] = undefined;
                            thread.success(point);
                        } else {
                            thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                        }
                    }
                }
            }
		
		};
		
	};
	
    var exports = ["shell/1", "shell/2", "cd/1", "ls/0", "ls/1", "pwd/0", "cwd/1", "rm/1"];


	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "shell", predicates(), exports );
		};
	} else {
		new pl.type.Module( "shell", predicates(), exports );
	}

})( pl );

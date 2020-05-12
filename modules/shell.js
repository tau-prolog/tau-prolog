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
					if(thread.get_flag("nodejs").indicator === "true/0") {
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
                    if(thread.get_flag("nodejs").indicator === "true/0") {
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
                    if(thread.get_flag("nodejs").indicator === "true/0") {
                        var fs = require('fs');
                        fs.readdir(path.id, function(error, items) {
                            if(error) {
                                thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                            } else {
                                var listing = items.join(" ");
                                thread.prepend([new pl.type.State(
                                    point.goal.replace( new pl.type.Term("write", [new pl.type.Term(listing, [])]) ),
                                    point.substitution,
                                    point
                                )]);
                            }
                            thread.again();
                        });
                        return true;
                    } else {
                        var file = pl.utils.cd(thread.session.working_directory, path.id);
                        var dirs = file.replace(/\/$/, "").split("/");
                        var dir = thread.session.file_system.files;
                        for(var i = 1; i < dirs.length; i++) {
                            if(!dir.hasOwnProperty(dirs[i])) {
                                thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                                return;
                            }
                            dir = dir[dirs[i]];
                        }
                        var list = [];
                        for(var prop in dir)
                            list.push(prop);
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
                if(thread.get_flag("nodejs").indicator === "true/0") {
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
                    if(thread.get_flag("nodejs").indicator === "true/0") {
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
                    if(thread.get_flag("nodejs").indicator === "true/0") {
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
                        var dirs = file.replace(/\/$/, "").split("/");
                        var dir = thread.session.file_system.files;
                        var name = dirs[dirs.length-1];
                        for(var i = 1; i < dirs.length-1; i++) {
                            if(dir.hasOwnProperty(dirs[i]))
                                dir = dir[dirs[i]];
                            else {
                                thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                                return;
                            }
                        }
                        if(dir[name]) {
                            if(dir[name].path !== file && !is_empty(dir[name])) {
                                thread.throw_error(pl.error.permission("delete", "source_sink", path, atom.indicator));
                                return;
                            }
                            delete dir[name];
                            thread.success(point);
                        } else {
                            thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
                        }
                    }
                }
            },

            // mkdir/1
            "mkdir/1": function(thread, point, atom) {
                var path = atom.args[0];
                if(pl.type.is_variable(path)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_atom(path)) {
                    thread.throw_error(pl.error.type("atom", path, atom.indicator));
                } else {
                    if(thread.get_flag("nodejs").indicator === "true/0") {
                        var fs = require('fs');
                        fs.stat(path.id, function(error, stat) {
                            if(!error && (stat.isDirectory() || stat.isFile())) {
                                thread.throw_error(pl.error.permission("create", "directory", path, atom.indicator));
                                thread.again();
                            } else {
                                fs.mkdir(path.id, function(error) { 
                                    if(error)
                                        thread.throw_error(pl.error.existence("directory", path, atom.indicator));
                                    else
                                        thread.success(point);
                                    thread.again();
                                });
                            }
                        });
                        return true;
                    } else {
                        var absolute = pl.utils.cd(thread.session.working_directory, path.id);
                        var dirs = absolute.replace(/\/$/, "").split("/");
                        var dir = thread.session.file_system.files;
                        var name = dirs[dirs.length-1];
                        for(var i = 1; i < dirs.length-1; i++) {
                            if(dir.hasOwnProperty(dirs[i]))
                                dir = dir[dirs[i]];
                            else {
                                thread.throw_error(pl.error.existence("directory", path, atom.indicator));
                                return;
                            }
                        }
                        if(dir[name]) {
                            thread.throw_error(pl.error.permission("create", "directory", path, atom.indicator));
                        } else {
                            dir[name] = {};
                            thread.success(point);
                        }
                    }
                }
            },

            // mv/2
            "mv/2": function(thread, point, atom) {
                var old_path = atom.args[0], new_path = atom.args[1];
                if(pl.type.is_variable(old_path) || pl.type.is_variable(new_path)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_atom(old_path)) {
                    thread.throw_error(pl.error.type("atom", old_path, atom.indicator));
                } else if(!pl.type.is_atom(new_path)) {
                    thread.throw_error(pl.error.type("atom", new_path, atom.indicator));
                } else {
                    if(thread.get_flag("nodejs").indicator === "true/0") {
                        var fs = require('fs');
                        fs.stat(old_path.id, function(error, stat) {
                            if(error || !stat.isFile()) {
                                thread.throw_error(pl.error.existence("source_sink", old_path, atom.indicator));
                                thread.again();
                            } else {
                                fs.rename(old_path.id, new_path.id, function(error) { 
                                    if(error)
                                        thread.throw_error(pl.error.existence("source_sink", new_path, atom.indicator));
                                    else
                                        thread.success(point);
                                    thread.again();
                                });
                            }
                        });
                        return true;
                    } else {
                        var old_file = thread.file_system_open(old_path.id, "text", "read");
                        if(old_file) {
                            var new_file = thread.file_system_open(new_path.id, "text", "write");
                            if(new_file) {
                                new_file.text = old_file.text;
                                var absolute = pl.utils.cd(thread.session.working_directory, old_path.id);
                                var dirs = absolute.replace(/\/$/, "").split("/");
                                var dir = thread.session.file_system.files;
                                var name = dirs[dirs.length-1];
                                for(var i = 1; i < dirs.length-1; i++)
                                    dir = dir[dirs[i]];
                                delete dir[name];
                                thread.success(point);
                            } else {
                                thread.throw_error(pl.error.existence("source_sink", new_path, atom.indicator));
                            }
                        } else {
                            thread.throw_error(pl.error.existence("source_sink", old_path, atom.indicator));
                        }
                    }
                }
            }
		
		};
		
	};
	
    var exports = ["shell/1", "shell/2", "cd/1", "ls/0", "ls/1", "pwd/0", "cwd/1", "rm/1", "mv/2", "mkdir/1"];

    function is_empty(obj) {
        for(var prop in obj)
            if(obj.hasOwnProperty(prop))
                return false;
        return true;
    }

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "shell", predicates(), exports );
		};
	} else {
		new pl.type.Module( "shell", predicates(), exports );
	}

})( pl );

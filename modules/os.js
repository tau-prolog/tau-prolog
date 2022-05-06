var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// OPERATING SYSTEM INTERACTION

			// sleep/1
			"sleep/1": function( thread, point, atom ) {
				var time = atom.args[0];
				if( pl.type.is_variable( time ) ) {
					thread.throw_error( pl.error.instantiation( thread.level ) );
				} else if( !pl.type.is_integer( time ) ) {
					thread.throw_error( pl.error.type( "integer", time, thread.level ) );
				} else {
					setTimeout( function() {
						thread.success( point );
						thread.again();
					}, time.value );
					return true;
				}
			},

			// set_interval/3
			"set_interval/3": function(thread, point, atom) {
				var time = atom.args[0], goal = atom.args[1], varid = atom.args[2];
				if(pl.type.is_variable(time) || pl.type.is_variable(goal) || !pl.type.is_variable(varid)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_integer(time)) {
					thread.throw_error(pl.error.type("integer", time, atom.indicator));
				} else if(!pl.type.is_callable(goal)) {
					thread.throw_error(pl.error.type("callable", goal, atom.indicator));
				} else {
					var id = setInterval((function(goal, thread) {
						return function() {
							var nthread = new pl.type.Thread(thread.session);
							nthread.add_goal(goal);
							nthread.answer();
						};
					})(goal, thread), time.value);
					tau_last_interval_id++;
					tau_intervals[tau_last_interval_id] = id;
					thread.prepend([new pl.type.State(
						point.goal.replace(new pl.type.Term("=", [
							varid,
							new pl.type.Num(tau_last_interval_id, false)
						])),
						point.substitution,
						point
					)]);
				}
			},

			// set_timeout/3
			"set_timeout/3": function(thread, point, atom) {
				var time = atom.args[0], goal = atom.args[1], varid = atom.args[2];
				if(pl.type.is_variable(time) || pl.type.is_variable(goal) || !pl.type.is_variable(varid)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_integer(time)) {
					thread.throw_error(pl.error.type("integer", time, atom.indicator));
				} else if(!pl.type.is_callable(goal)) {
					thread.throw_error(pl.error.type("callable", goal, atom.indicator));
				} else {
					var id = setTimeout((function(goal, thread) {
						return function() {
							var nthread = new pl.type.Thread(thread.session);
							nthread.add_goal(goal);
							nthread.answer();
						};
					})(goal, thread), time.value);
					tau_last_timeout_id++;
					tau_timeouts[tau_last_timeout_id] = id;
					thread.prepend([new pl.type.State(
						point.goal.replace(new pl.type.Term("=", [
							varid,
							new pl.type.Num(tau_last_timeout_id, false)
						])),
						point.substitution,
						point
					)]);
				}
			},

			// clear_interval/1
			"clear_interval/1": function(thread, point, atom) {
				var id = atom.args[0];
				if(pl.type.is_variable(id)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_integer(id)) {
					thread.throw_error(pl.error.type("integer", id, atom.indicator));
				} else {
					if(tau_intervals[id.value]) {
						clearInterval(tau_intervals[id.value]);
						thread.success(point);
						delete tau_intervals[id.value];
					}
				}
			},

			// clear_timeout/1
			"clear_timeout/1": function(thread, point, atom) {
				var id = atom.args[0];
				if(pl.type.is_variable(id)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_integer(id)) {
					thread.throw_error(pl.error.type("integer", id, atom.indicator));
				} else {
					if(tau_timeouts[id.value]) {
						clearInterval(tau_timeouts[id.value]);
						thread.success(point);
						delete tau_timeouts[id.value];
					}
				}
			},

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

			// directory_files/2
			"directory_files/2": function(thread, point, atom) {
				var path = atom.args[0], entries = atom.args[1];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else if(!pl.type.is_variable(entries) && !pl.type.is_list(entries)) {
					thread.throw_error(pl.error.type("list", entries, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.readdir(absolute, function(error, items) {
							if(error) {
								thread.throw_error(pl.error.existence("directory", path, atom.indicator));
							} else {
								var listing = new pl.type.Term("[]", []);
								for(var i = items.length-1; i >= 0; i--)
									listing = new pl.type.Term(".", [new pl.type.Term(items[i], []), listing]);
								thread.prepend([new pl.type.State(
									point.goal.replace(new pl.type.Term("=", [entries, listing])),
									point.substitution,
									point
								)]);
							}
							thread.again();
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_directory(file)) {
							var items = [];
							for(var prop in file.files)
								items.push(prop);
							var listing = new pl.type.Term("[]", []);
							for(var i = items.length-1; i >= 0; i--)
								listing = new pl.type.Term(".", [new pl.type.Term(items[i], []), listing]);
							thread.prepend([new pl.type.State(
								point.goal.replace(new pl.type.Term("=", [entries, listing])),
								point.substitution,
								point
							)]);
						} else {
							thread.throw_error(pl.error.existence("directory", path, atom.indicator));
						}
					}
				}
			},

			// working_directory/2
			"working_directory/2": function(thread, point, atom) {
				var oldcwd = atom.args[0], newcwd = atom.args[1];
				if(pl.type.is_variable(newcwd) && (!pl.type.is_variable(oldcwd) || oldcwd.id !== newcwd.id)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_variable(oldcwd) && !pl.type.is_atom(oldcwd)) {
					thread.throw_error(pl.error.type("atom", oldcwd, atom.indicator));
				} else if(!pl.type.is_variable(newcwd) && !pl.type.is_atom(newcwd)) {
					thread.throw_error(pl.error.type("atom", newcwd, atom.indicator));
				} else {
					var wd;
					if(thread.get_flag("nodejs").indicator === "true/0") {
						wd = process.cwd();
						if(!pl.type.is_variable(newcwd))
							try {
								process.chdir(thread.absolute_file_name(newcwd.id));
							} catch(ex) {
								thread.throw_error(pl.error.existence("directory", newcwd, atom.indicator));
								return;
							}
					} else {
						wd = thread.session.working_directory;
						if(!pl.type.is_variable(newcwd)) {
							thread.session.working_directory = thread.absolute_file_name(newcwd.id);
						}
					}
					thread.prepend([new pl.type.State(
						point.goal.replace(new pl.type.Term("=", [oldcwd, new pl.type.Term(wd, [])])),
						point.substitution,
						point
					)]);
				}
			},

			// delete_file/1
			"delete_file/1": function(thread, point, atom) {
				var path = atom.args[0];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error && stat.isFile()) {
								fs.unlink(absolute, function(error) {
									if(error)
										thread.throw_error(pl.error.permission("delete", "source_sink", path, atom.indicator));
									else
										thread.success(point);
									thread.again();
								});
							} else {
								thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
								thread.again();
							}
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_file(file)) {
							file.parent.remove(file.name);
							thread.success(point);
						} else {
							thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
						}
					}
				}
			},

			// delete_directory/1
			"delete_directory/1": function(thread, point, atom) {
				var path = atom.args[0];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error && stat.isDirectory()) {
								fs.rmdir(absolute, function(error) {
									if(error)
										thread.throw_error(pl.error.permission("delete", "directory", path, atom.indicator));
									else
										thread.success(point);
									thread.again();
								});
							} else {
								thread.throw_error(pl.error.existence("directory", path, atom.indicator));
								thread.again();
							}
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_directory(file)) {
							if(file !== thread.session.file_system.files && file.empty()) {
								file.parent.remove(file.name);
								thread.success(point);
							} else {
								thread.throw_error(pl.error.permission("delete", "directory", path, atom.indicator));
							}
						} else {
							thread.throw_error(pl.error.existence("directory", path, atom.indicator));
						}
					}
				}
			},

			// make_directory/1
			"make_directory/1": function(thread, point, atom) {
				var path = atom.args[0];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error && (stat.isDirectory() || stat.isFile())) {
								thread.throw_error(pl.error.permission("create", "directory", path, atom.indicator));
								thread.again();
							} else {
								fs.mkdir(absolute, function(error) { 
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
						var dirs = absolute.replace(/\/$/, "").split("/");
						var dir = thread.session.file_system.files;
						var name = dirs[dirs.length-1];
						for(var i = 1; i < dirs.length-1; i++) {
							dir = dir.lookup(dirs[i]);
							if(!pl.type.is_directory(dir)) {
								thread.throw_error(pl.error.existence("directory", path, atom.indicator));
								return;
							}
						}
						if(dir.lookup(name)) {
							thread.throw_error(pl.error.permission("create", "directory", path, atom.indicator));
						} else {
							dir.push(name, new pl.type.Directory(name, dir));
							thread.success(point);
						}
					}
				}
			},

			// copy_file/2
			"copy_file/2": function(thread, point, atom) {
				var old_path = atom.args[0], new_path = atom.args[1];
				if(pl.type.is_variable(old_path) || pl.type.is_variable(new_path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(old_path)) {
					thread.throw_error(pl.error.type("atom", old_path, atom.indicator));
				} else if(!pl.type.is_atom(new_path)) {
					thread.throw_error(pl.error.type("atom", new_path, atom.indicator));
				} else {
					var old_absolute = thread.absolute_file_name(old_path.id);
					var new_absolute = thread.absolute_file_name(new_path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(old_absolute, function(error, stat) {
							if(error || !stat.isFile()) {
								thread.throw_error(pl.error.existence("source_sink", old_path, atom.indicator));
								thread.again();
							} else {
								fs.copyFile(old_absolute, new_absolute, function(error) { 
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
						var old_file = thread.file_system_open(old_absolute, "text", "read");
						if(old_file) {
							var new_file = thread.file_system_open(new_absolute, "text", "write");
							if(new_file) {
								new_file.text = old_file.text;
								thread.success(point);
							} else {
								thread.throw_error(pl.error.existence("source_sink", new_path, atom.indicator));
							}
						} else {
							thread.throw_error(pl.error.existence("source_sink", old_path, atom.indicator));
						}
					}
				}
			},

			// rename_file/2
			"rename_file/2": function(thread, point, atom) {
				var old_path = atom.args[0], new_path = atom.args[1];
				if(pl.type.is_variable(old_path) || pl.type.is_variable(new_path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(old_path)) {
					thread.throw_error(pl.error.type("atom", old_path, atom.indicator));
				} else if(!pl.type.is_atom(new_path)) {
					thread.throw_error(pl.error.type("atom", new_path, atom.indicator));
				} else {
					var old_absolute = thread.absolute_file_name(old_path.id);
					var new_absolute = thread.absolute_file_name(new_path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(old_absolute, function(error, stat) {
							if(error || !stat.isFile()) {
								thread.throw_error(pl.error.existence("source_sink", old_path, atom.indicator));
								thread.again();
							} else {
								fs.rename(old_absolute, new_absolute, function(error) { 
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
						var old_file = thread.file_system_open(old_absolute, "text", "read");
						if(old_file) {
							var new_file = thread.file_system_open(new_absolute, "text", "write");
							if(new_file) {
								new_file.text = old_file.text;
								var dirs = old_absolute.replace(/\/$/, "").split("/");
								var dir = thread.session.file_system.files;
								var name = dirs[dirs.length-1];
								for(var i = 1; i < dirs.length-1; i++)
									dir = dir.lookup(dirs[i]);
								dir.remove(name);
								thread.success(point);
							} else {
								thread.throw_error(pl.error.existence("source_sink", new_path, atom.indicator));
							}
						} else {
							thread.throw_error(pl.error.existence("source_sink", old_path, atom.indicator));
						}
					}
				}
			},
			
			// exists_file/1
			"exists_file/1": function(thread, point, atom) {
				var path = atom.args[0];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error && stat.isFile())
								thread.success(point);
							thread.again();
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_file(file))
							thread.success(point);
					}
				}
			},

			// exists_directory/1
			"exists_directory/1": function(thread, point, atom) {
				var path = atom.args[0];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error && stat.isDirectory())
								thread.success(point);
							thread.again();
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_directory(file))
							thread.success(point);
					}
				}
			},

			// same_file/2
			"same_file/2": function(thread, point, atom) {
				var fst_path = atom.args[0], snd_path = atom.args[1];
				if(pl.type.is_variable(fst_path) || pl.type.is_variable(snd_path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(fst_path)) {
					thread.throw_error(pl.error.type("atom", fst_path, atom.indicator));
				} else if(!pl.type.is_atom(snd_path)) {
					thread.throw_error(pl.error.type("atom", snd_path, atom.indicator));
				} else {
					if(fst_path.id === snd_path.id) {
						thread.success(point);
					} else {
						var fst_absolute = thread.absolute_file_name(fst_path.id);
						var snd_absolute = thread.absolute_file_name(snd_path.id);
						if(thread.get_flag("nodejs").indicator === "true/0") {
							var fs = require('fs');
							fs.stat(fst_absolute, function(error, fst_stat) {
								if(!error)
									fs.stat(snd_absolute, function(error, snd_stat) {
										if(!error && fst_stat.dev === snd_stat.dev && fst_stat.ino === snd_stat.ino)
											thread.success(point);
										thread.again();
									});
								else
									thread.again();
							});
							return true;
						} else {
							var fst_file = thread.session.file_system.get(fst_absolute);
							var snd_file = thread.session.file_system.get(snd_absolute);
							if(fst_file && snd_file && fst_file === snd_file)
								thread.success(point);
						}
					}
				}
			},

			// absolute_file_name/2
			"absolute_file_name/2": function(thread, point, atom) {
				var filename = atom.args[0], absolute = atom.args[1];
				if(pl.type.is_variable(filename)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(filename)) {
					thread.throw_error(pl.error.type("atom", filename, atom.indicator));
				} else if(!pl.type.is_variable(absolute) && !pl.type.is_atom(absolute)) {
					thread.throw_error(pl.error.type("atom", absolute, atom.indicator));
				} else {
					var absolute_filename = thread.absolute_file_name(filename.id);
					thread.prepend([new pl.type.State(
						point.goal.replace(new pl.type.Term("=", [
							absolute,
							new pl.type.Term(absolute_filename, [])])),
						point.substitution,
						point
					)]);
				}
			},

			// is_absolute_file_name/1
			"is_absolute_file_name/1": function(thread, point, atom) {
				var filename = atom.args[0];
				if(pl.type.is_variable(filename)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(filename)) {
					thread.throw_error(pl.error.type("atom", filename, atom.indicator));
				} else {
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var path = require('path');
						var expand = filename.id;
						for(var prop in process.env) {
							if(!process.env.hasOwnProperty(prop))
								continue;
							expand = expand.replace(new RegExp("\\$" + prop, "g"), process.env[prop]);
						}
						if(path.isAbsolute(expand))
							thread.success(point);
					} else {
						if(filename.id.length > 0 && filename.id[0] === "/")
							thread.success(point);
					}
				}
			},

			// size_file/2
			"size_file/2": function(thread, point, atom) {
				var path = atom.args[0], size = atom.args[1];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else if(!pl.type.is_variable(size) && !pl.type.is_integer(size)) {
					thread.throw_error(pl.error.type("integer", size, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error) {
								var filesize = stat.size;
								thread.prepend([new pl.type.State(
									point.goal.replace(new pl.type.Term("=", [size, new pl.type.Num(filesize, false)])),
									point.substitution,
									point
								)]);
							} else {
								thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
							}
							thread.again();
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_file(file) || pl.type.is_directory(file)) {
							var filesize = file.size();
							thread.prepend([new pl.type.State(
								point.goal.replace(new pl.type.Term("=", [size, new pl.type.Num(filesize, false)])),
								point.substitution,
								point
							)]);
						} else {
							thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
						}
					}
				}
			},

			// time_file/2
			"time_file/2": function(thread, point, atom) {
				var path = atom.args[0], time = atom.args[1];
				if(pl.type.is_variable(path)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else if(!pl.type.is_variable(time) && !pl.type.is_number(time)) {
					thread.throw_error(pl.error.type("number", time, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						fs.stat(absolute, function(error, stat) {
							if(!error) {
								var mtime = stat.mtime / 1000;
								thread.prepend([new pl.type.State(
									point.goal.replace(new pl.type.Term("=", [time, new pl.type.Num(mtime)])),
									point.substitution,
									point
								)]);
							} else {
								thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
							}
							thread.again();
						});
						return true;
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_file(file) || pl.type.is_directory(file)) {
							var mtime = file.modified;
							thread.prepend([new pl.type.State(
								point.goal.replace(new pl.type.Term("=", [time, new pl.type.Num(mtime)])),
								point.substitution,
								point
							)]);
						} else {
							thread.throw_error(pl.error.existence("source_sink", path, atom.indicator));
						}
					}
				}
			},
			
			// file_permission/2
			"file_permission/2": function(thread, point, atom) {
				var path = atom.args[0];
				var permission = atom.args[1];
				if(pl.type.is_variable(path) || pl.type.is_variable(permission)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(path)) {
					thread.throw_error(pl.error.type("atom", path, atom.indicator));
				} else if(!pl.type.is_atom(permission)) {
					thread.throw_error(pl.error.type("atom", permission, atom.indicator));
				} else {
					var absolute = thread.absolute_file_name(path.id);
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var fs = require('fs');
						if(permission.indicator === "read/0") {
							fs.access(absolute, fs.constants.R_OK, function(error, stat) {
								if(!error)
									thread.success(point);
								thread.again();
							});
							return true;
						} else if(permission.indicator === "write/0") {
							fs.access(absolute, fs.constants.W_OK, function(error, stat) {
								if(!error)
									thread.success(point);
								thread.again();
							});
							return true;
						} else if(permission.indicator === "execute/0") {
							fs.access(absolute, fs.constants.X_OK, function(error, stat) {
								if(!error)
									thread.success(point);
								thread.again();
							});
							return true;
						} else {
							thread.throw_error(pl.error.domain( "file_permission", permission, atom.indicator ));
						}
					} else {
						var file = thread.session.file_system.get(absolute);
						if(pl.type.is_file(file))
							thread.success(point);
					}
				}
			},

			// getenv/2
			"getenv/2": function(thread, point, atom) {
				var name = atom.args[0], value = atom.args[1];
				if(!pl.type.is_variable(name) && !pl.type.is_atom(name)) {
					thread.throw_error(pl.error.type("atom", name, atom.indicator));
				} else if(!pl.type.is_variable(value) && !pl.type.is_atom(value)) {
					thread.throw_error(pl.error.type("atom", value, atom.indicator));
				} else {
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var points = [];
						if(pl.type.is_variable(name)) {
							for(var prop in process.env) {
								if(!process.env.hasOwnProperty(prop))
									continue;
								var getname = new pl.type.Term(prop, []);
								var getvalue = new pl.type.Term(process.env[prop], []);
								points.push(new pl.type.State(
									point.goal.replace(new pl.type.Term(",", [
										new pl.type.Term("=", [name, getname]),
										new pl.type.Term("=", [value, getvalue])
									])),
									point.substitution,
									point
								));
							}
						} else {
							var getvalue = process.env[name.id] ? new pl.type.Term(process.env[name.id].toString(), []) : null;
							if(getvalue !== null) {
								points.push(new pl.type.State(
									point.goal.replace(new pl.type.Term("=", [value, getvalue])),
									point.substitution,
									point
								));
							}
						}
						thread.prepend(points);
					}
				}
			},

			// setenv/2
			"setenv/2": function(thread, point, atom) {
				var name = atom.args[0], value = atom.args[1];
				if(pl.type.is_variable(name) || pl.type.is_variable(value)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(name) && !pl.type.is_integer(name)) {
					thread.throw_error(pl.error.type("atom_or_integer", name, atom.indicator));
				} else if(!pl.type.is_atom(value) && !pl.type.is_integer(value)) {
					thread.throw_error(pl.error.type("atom_or_integer", value, atom.indicator));
				} else {
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var getname = pl.type.is_atom(name) ? name.id : name.value.toString();
						var getvalue = pl.type.is_atom(value) ? value.id : value.value.toString();
						process.env[getname] = getvalue;
						thread.success(point);
					}
				}
			},

			// unsetenv/1
			"unsetenv/1": function(thread, point, atom) {
				var name = atom.args[0];
				if(pl.type.is_variable(name)) {
					thread.throw_error(pl.error.instantiation(atom.indicator));
				} else if(!pl.type.is_atom(name) && !pl.type.is_integer(name)) {
					thread.throw_error(pl.error.type("atom_or_integer", name, atom.indicator));
				} else {
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var getname = pl.type.is_atom(name) ? name.id : name.value.toString();
						delete process.env[getname];
						thread.success(point);
					}
				}
			},

			// pid/1
			"pid/1": function(thread, point, atom) {
				var pid = atom.args[0];
				if(!pl.type.is_variable(pid) && !pl.type.is_integer(pid)) {
					thread.throw_error(pl.error.type("integer", pid, atom.indicator));
				} else {
					var points = [];
					if(thread.get_flag("nodejs").indicator === "true/0") {
						var process = require('process');
						points.push(new pl.type.State(
							point.goal.replace( new pl.type.Term("=", [pid, new pl.type.Num(process.pid, false)]) ),
							point.substitution,
							point
						));
					} else {
						points.push(new pl.type.State(
							point.goal.replace( new pl.type.Term("=", [pid, new pl.type.Num(0, false)]) ),
							point.substitution,
							point
						));
					}
					thread.prepend(points);
				}
			}
		
		};
		
	};
	
	var exports = ["sleep/1", "set_interval/3", "set_timeout/3", "clear_interval/1", "clear_timeout/1", "shell/1", "shell/2", "directory_files/2", "working_directory/2", "delete_file/1", "delete_directory/1", "rename_file/2", "copy_file/2", "make_directory/1", "exists_file/1", "exists_directory/1", "same_file/2", "absolute_file_name/2", "is_absolute_file_name/1", "size_file/2", "file_permission/2", "time_file/2", "getenv/2", "setenv/2", "unsetenv/1", "pid/1"];

	var options = function() {
		return {
			meta_predicates: {
				// set_interval(+, 0, -)
				"set_interval/3": new pl.type.Term("set_interval", [new pl.type.Term("+"), new pl.type.Num(0), new pl.type.Term("-")]),
				// set_timeout(+, 0, -)
				"set_timeout/3": new pl.type.Term("set_timeout", [new pl.type.Term("+"), new pl.type.Num(0), new pl.type.Term("-")])
			}
		};
	};

	var tau_last_interval_id = 0;
	var tau_last_timeout_id = 0;
	var tau_intervals = {};
	var tau_timeouts = {};

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "os", predicates(), exports, options() );
		};
	} else {
		new pl.type.Module( "os", predicates(), exports, options() );
	}

})( pl );

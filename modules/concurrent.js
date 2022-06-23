var pl;
(function(pl) {

    var FUTURE_PENDING = 0, FUTURE_FULFILLED = 1, FUTURE_FAILED = 2, FUTURE_REJECTED = 3;

    var predicates = function() {
        
        return {

            // future/3
            "future/3": function(thread, point, atom) {
                var resolve = atom.args[0], goal = atom.args[1], var_future = atom.args[2];
                if(!pl.type.is_variable(var_future)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_callable(goal)) {
                    thread.throw_error(pl.error.type("callable", goal, atom.indicator));
                } else {
                    var future = new pl.type.Future();
                    var nthread = new pl.type.Thread(thread.session);
                    var template = thread.next_free_variable();
                    thread.session.renamed_variables = {};
                    var future_goal = new pl.type.Term(",", [new pl.type.Term("call", [goal.rename(thread)]), new pl.type.Term("=", [template, resolve.rename(thread)])]);
                    nthread.add_goal(future_goal);
                    var handlers = {
                        success: function(answer) {
                            future.done(answer.links[template.id], FUTURE_FULFILLED);
                        },
                        error: function(error) {
                            future.done(error.args[0], FUTURE_REJECTED);
                        },
                        fail: function() {
                            future.done(null, FUTURE_FAILED);
                        },
                        limit: function() {
                            nthread.answer(handlers);
                        }
                    };
                    nthread.answer(handlers);
                    thread.prepend([new pl.type.State(
                        point.goal.replace(new pl.type.Term("=", [
                            var_future,
                            future
                        ])),
                        point.substitution,
                        point
                    )]);
                }
            },

            // future_done/1
            "future_done/1": function(thread, point, atom) {
                var future = atom.args[0];
                if(pl.type.is_variable(future)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_future_object(future)) {
                    thread.throw_error(pl.error.type("future", future, atom.indicator));
                } else {
                    if(future.state !== FUTURE_PENDING) {
                        thread.success(point);
                    }
                }
            },

            // await/2
            "await/2": function(thread, point, atom) {
                var future = atom.args[0], value = atom.args[1];
                if(pl.type.is_variable(future)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_future_object(future)) {
                    thread.throw_error(pl.error.type("future", future, atom.indicator));
                } else {
                    future.then(
                        function(answer) {
                            thread.prepend([new pl.type.State(
                                point.goal.replace(new pl.type.Term("=", [
                                    value,
                                    answer
                                ])),
                                point.substitution,
                                point
                            )]);
                            thread.again();
                        },
                        function(error) {
                            thread.throw_error(error);
                            thread.again();
                        },
                        function() {
                            thread.again();
                        }
                    );
                    return true;
                }
            },

            // future_all/2
            "future_all/2": function(thread, point, atom) {
                var futures = atom.args[0], all = atom.args[1];
                if(pl.type.is_variable(futures) || !pl.type.is_variable(all)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_list(futures)) {
                    thread.throw_error(pl.error.type("list", futures, atom.indicator));
                } else {
                    var arr_futures = [];
                    var pointer = futures;
                    while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
                        var head_future = pointer.args[0];
                        if(pl.type.is_variable(head_future)) {
                            thread.throw_error(pl.error.instantiation(atom.indicator));
                            return;
                        } else if(!pl.type.is_future_object(head_future)) {
                            thread.throw_error(pl.error.type("future", head_future, atom.indicator));
                            return;
                        }
                        arr_futures.push(head_future);
                        pointer = pointer.args[1];
                    }
                    if(pl.type.is_variable(pointer)) {
                        thread.throw_error(pl.error.instantiation(atom.indicator));
                        return;
                    } else if(!pl.type.is_empty_list(pointer)) {
                        thread.throw_error(pl.error.type("list", futures, atom.indicator));
                        return;
                    }
                    var future = new pl.type.Future();
                    future.expected = arr_futures.length;
                    var templates = [];
                    for(var i = 0; i < arr_futures.length; i++) {
                        arr_futures[i].then(
                            (function(i) {
                                return function(answer) {
                                    templates[i] = answer;
                                    future.expected--;
                                    if(future.state === FUTURE_PENDING && future.expected === 0) {
                                        var list = new pl.type.Term("[]", []);
                                        for(var j = templates.length-1; j >= 0; j--)
                                            list = new pl.type.Term(".", [templates[j], list]);
                                        future.done(list, FUTURE_FULFILLED);
                                    }
                                };
                            })(i),
                            function(error) {
                                future.expected--;
                                future.done(error, FUTURE_REJECTED);
                            },
                            function() {
                                future.expected--;
                                future.done(null, FUTURE_FAILED);
                            }
                        );
                    }
                    thread.prepend([new pl.type.State(
                        point.goal.replace(new pl.type.Term("=", [
                            all,
                            future
                        ])),
                        point.substitution,
                        point
                    )]);
                }
            },

            // future_any/2
            "future_any/2": function(thread, point, atom) {
                var futures = atom.args[0], any = atom.args[1];
                if(pl.type.is_variable(futures) || !pl.type.is_variable(any)) {
                    thread.throw_error(pl.error.instantiation(atom.indicator));
                } else if(!pl.type.is_list(futures)) {
                    thread.throw_error(pl.error.type("list", futures, atom.indicator));
                } else {
                    var arr_futures = [];
                    var pointer = futures;
                    while(pl.type.is_term(pointer) && pointer.indicator === "./2") {
                        var head_future = pointer.args[0];
                        if(pl.type.is_variable(head_future)) {
                            thread.throw_error(pl.error.instantiation(atom.indicator));
                            return;
                        } else if(!pl.type.is_future_object(head_future)) {
                            thread.throw_error(pl.error.type("future", head_future, atom.indicator));
                            return;
                        }
                        arr_futures.push(head_future);
                        pointer = pointer.args[1];
                    }
                    if(pl.type.is_variable(pointer)) {
                        thread.throw_error(pl.error.instantiation(atom.indicator));
                        return;
                    } else if(!pl.type.is_empty_list(pointer)) {
                        thread.throw_error(pl.error.type("list", futures, atom.indicator));
                        return;
                    }
                    var future = new pl.type.Future();
                    future.expected = arr_futures.length;
                    var templates = [];
                    for(var i = 0; i < arr_futures.length; i++) {
                        arr_futures[i].then(
                            function(answer) {
                                future.expected--;
                                if(future.state === FUTURE_PENDING) {
                                    future.done(answer, FUTURE_FULFILLED);
                                }
                            },
                            function(error) {
                                future.expected--;
                                future.done(error, FUTURE_REJECTED);
                            },
                            function() {
                                future.expected--;
                                if(future.expected === 0)
                                    future.done(null, FUTURE_FAILED);
                            }
                        );
                    }
                    thread.prepend([new pl.type.State(
                        point.goal.replace(new pl.type.Term("=", [
                            any,
                            future
                        ])),
                        point.substitution,
                        point
                    )]);
                }
            }

        };

    };

    var exports = ["future/3", "await/2", "future_done/1", "future_all/2", "future_any/2"];

    var extend = function(pl) {

        // Is a Future object
        pl.type.is_future_object = function(obj) {
            return obj instanceof pl.type.Future;
        };

        // Ordering relation
        pl.type.order.push(pl.type.Future);

        // DOM Prolog object
        pl.type.Future = function() {
            this.value = null;
            this.state = FUTURE_PENDING;
            this.tasks = [];
        };

        pl.type.Future.prototype.done = function(value, state) {
            this.value = value;
            this.state = state;
            if(state === FUTURE_FULFILLED) {
                while(this.tasks.length > 0) {
                    var task = this.tasks.shift();
                    task.resolve(this.value);
                }
            } else if(state === FUTURE_REJECTED) {
                while(this.tasks.length > 0) {
                    var task = this.tasks.shift();
                    task.reject(this.value);
                }
            } else if(state === FUTURE_FAILED) {
                while(this.tasks.length > 0) {
                    var task = this.tasks.shift();
                    task.fail();
                }
            }
        }

        pl.type.Future.prototype.then = function(resolve, reject, fail) {
            if(this.state === FUTURE_FULFILLED)
                resolve(this.value);
            else if(this.state === FUTURE_REJECTED)
                reject(this.value);
            else if(this.state === FUTURE_FAILED)
                fail();
            else
                this.tasks.push({
                    resolve: resolve,
                    reject: reject,
                    fail: fail
                });
        };

        // toString
        pl.type.Future.prototype.toString = function(options) {
            if(this.value !== null)
                return "<future>(" + this.value.toString(options) + ")";
            return "<future>(pending)";
        };

        // clone
        pl.type.Future.prototype.clone = function() {
            var p = new pl.type.Future();
            p.state = this.state;
            p.value = this.value;
        };

        // equals
        pl.type.Future.prototype.equals = function(obj) {
            return obj === this;
        };

        // rename
        pl.type.Future.prototype.rename = function(_) {
            return this;
        };

        // get variables
        pl.type.Future.prototype.variables = function() {
            return [];
        };

        // apply substitutions
        pl.type.Future.prototype.apply = function(_) {
            return this;
        };

        // unify
        pl.type.Future.prototype.unify = function(obj, _) {
            if(obj === this)
                return new pl.type.Substitution();
            return null;
        };

        // interpret
        pl.type.Future.prototype.interpret = function(indicator) {
            return pl.error.instantiation(indicator);
        };

        // compare
        pl.type.Future.prototype.compare = function(obj) {
            if(this === obj) {
                return 0;
            } else if(this < obj) {
                return -1;
            } else {
                return 1;
            }
        };

        // to javascript
        pl.type.Future.prototype.toJavaScript = function() {
            if(!Promise)
                return null;
            var future = this;
            return new Promise(function(resolve, reject) {
                future.then(
                    function(answer) {
                        resolve(answer.toJavaScript());
                    },
                    function(error) {
                        reject(error.toJavaScript());
                    },
                    function() {
                        reject(false);
                    }
                );
            });
        };
        
        // from javascript
        pl.fromJavaScript.test.promise = function(obj) {
            return Promise && obj instanceof Promise;
        };
        pl.fromJavaScript.conversion.promise = function(obj) {
            var future = new pl.type.Future();
            obj.then(function(value) {
                future.done(pl.fromJavaScript.apply(value), FUTURE_FULFILLED);
            }).catch(function(error) {
                future.done(pl.fromJavaScript.apply(error), FUTURE_REJECTED);
            });
            return future;
        };

    };

    var options = function() {
        return {
            meta_predicates: {
                // future(?, 0, -)
                "future/3": new pl.type.Term("future", [new pl.type.Term("?"), new pl.type.Num(0, false), new pl.type.Term("-")])
            }
        };
    };

    if(typeof module !== 'undefined') {
        module.exports = function(p) {
            pl = p;
            extend(pl);
            new pl.type.Module("concurrent", predicates(), exports, options());
        };
    } else {
        extend(pl);
        new pl.type.Module("concurrent", predicates(), exports, options());
    }

})(pl);
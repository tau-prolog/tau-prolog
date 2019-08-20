var pl;
(function( pl ) {

	var predicates = function() {
		
		return {
			
			// EVENTS
			
			// bind/4
			"bind/4": function( thread, point, atom ) {
				var elem = atom.args[0], type = atom.args[1], event = atom.args[2], goal = atom.args[3];
				if( pl.type.is_variable( elem ) || pl.type.is_variable( type ) && pl.type.is_variable( goal ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( elem ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", elem, atom.indicator ) );
				} else if( !pl.type.is_atom( type ) ) {
					thread.throw_error( pl.error.type( "atom", type, atom.indicator ) );
				} else if( !pl.type.is_variable( event ) && !pl.type.is_dom_event_object( event ) ) {
					thread.throw_error( pl.error.type( "DOMEventObject", type, atom.indicator ) );
				} else if( !pl.type.is_variable( goal ) ) {
					var thread_ = new pl.type.Thread( thread.session );
					var eventObject = new pl.type.DOMEvent( type.id );
					var links = {};
					if( pl.type.is_variable( event ) )
						links[event.id] = eventObject;
					var subs = new pl.type.Substitution( links );
					var handler = function( e ) {
						eventObject.event = e;
						thread_.add_goal( goal.apply( subs ) );
						thread_.answer( thread.__calls[0] );
					};
					events.add( elem.object, type.id, handler );
					elem.object.tau_events = elem.object.tau_events === undefined ? {} : elem.object.tau_events;
					if( elem.object.tau_events[type.id] === undefined )
						elem.object.tau_events[type.id] = [];
					elem.object.tau_events[type.id].push( {goal: goal, fn: handler} );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [eventObject, event] ) ), point.substitution, point )] );
				} else {
					var event = elem.object.tau_events ? elem.object.tau_events[type.id] : undefined;
					if( event !== undefined ) {
						var states = [];
						for( var i = 0; i < event.length; i++ )
							states.push( new pl.type.State( point.goal.replace( new pl.type.Term( "=", [goal, event[i].goal.rename(thread)] ) ), point.substitution, point ) );
						thread.prepend( states );
					}
				}
			},
			
			// unbind/2
			"unbind/2": function( thread, point, atom ) {
				var elem = atom.args[0], type = atom.args[1];
				if( pl.type.is_variable( elem ) || pl.type.is_variable( type ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( elem ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", elem, atom.indicator ) );
				} else if( !pl.type.is_atom( type ) ) {
					thread.throw_error( pl.error.type( "atom", type, atom.indicator ) );
				} else if( !pl.type.is_variable( goal ) ) {
					if( elem.object.tau_events && elem.object.tau_events[type.id] ) {
						var event = elem.object.tau_events[type.id];
						for( var i = 0; i < event.length; i++ ) {
							events.remove( elem.object, type.id, event[i].fn );
						}
						delete elem.object.tau_events[type.id];
					}
					thread.success( point );
				}
			},
			
			// unbind/3
			"unbind/3": function( thread, point, atom ) {
				var elem = atom.args[0], type = atom.args[1], goal = atom.args[2];
				if( pl.type.is_variable( elem ) || pl.type.is_variable( type ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( elem ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", elem, atom.indicator ) );
				} else if( !pl.type.is_atom( type ) ) {
					thread.throw_error( pl.error.type( "atom", type, atom.indicator ) );
				} else if( !pl.type.is_variable( goal ) && !pl.type.is_term( goal ) ) {
					thread.throw_error( pl.error.type( "term", goal, atom.indicator ) );
				} else if( !pl.type.is_variable( goal ) ) {
					if( elem.object.tau_events && elem.object.tau_events[type.id] ) {
						var event = elem.object.tau_events[type.id];
						var newevents = [];
						for( var i = 0; i < event.length; i++ ) {
							if( pl.unify( event[i].goal, goal ) !== null ) {
								events.remove( elem.object, type.id, event[i].fn );
							} else {
								newevents.push( event[i] );
							}
						}
						elem.object.tau_events[type.id] = newevents;
					}
					thread.success( point );
				}
			},
			
			// event_property/3
			"event_property/3": function( thread, point, atom ) {
				var event = atom.args[0], prop = atom.args[1], val = atom.args[2]
				if( pl.type.is_variable( event ) || pl.type.is_variable( prop ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_event_object( event ) ) {
					thread.throw_error( pl.error.type( "DOMEventObject", event, atom.indicator ) );
				} else if( !pl.type.is_atom( prop ) ) {
					thread.throw_error( pl.error.type( "atom", prop, atom.indicator ) );
				} else if( !pl.type.is_variable( val ) && !pl.type.is_atomic( val ) ) {
					thread.throw_error( pl.error.type( "atomic", val, atom.indicator ) );
				} else {
					if( event.event !== null && event.event[prop.id] ) {
						var value = event.event[prop.id];
						value = isNaN(value) ? new pl.type.Term( value, [] ) : new pl.type.Num( value );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, val] ) ), point.substitution, point )] );
					}
				}
			},
			
			// prevent_default/1
			"prevent_default/1": function( thread, point, atom ) {
				var event = atom.args[0];
				if( pl.type.is_variable( event ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_event_object( event ) ) {
					thread.throw_error( pl.error.type( "eventObject", event, atom.indicator ) );
				} else {
					if( event.event !== null ) {
						event.event.preventDefault();
						thread.success( point );
					}
				}
			},
			
			// EFFECTS
			
			// hide/1
			"hide/1": function( thread, point, atom ) {
				var element = atom.args[0];
				if( pl.type.is_variable( element ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else {
					var state = document.defaultView.getComputedStyle( element.object, "" ).display;
					if( state !== undefined && state !== "none" )
						element.object.tau_display = state;
					element.object.style.display = "none";
					thread.success( point );
				}
			},
			
			// show/1
			"show/1": function( thread, point, atom ) {
				var element = atom.args[0];
				if( pl.type.is_variable( element ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else {
					element.object.style.display = element.object.tau_display !== undefined ? element.object.tau_display : "block";
					thread.success( point );
				}
			},
			
			// toggle/1
			"toggle/1": [
				new pl.type.Rule(new pl.type.Term("toggle", [new pl.type.Var("X")]), new pl.type.Term(";", [new pl.type.Term("->", [new pl.type.Term(",", [new pl.type.Term("style", [new pl.type.Var("X"),new pl.type.Term("display", []),new pl.type.Var("Y")]),new pl.type.Term("=", [new pl.type.Var("Y"),new pl.type.Term("none", [])])]),new pl.type.Term("show", [new pl.type.Var("X")])]),new pl.type.Term("hide", [new pl.type.Var("X")])]))
			],
			
			// DOM MANIPULATION

			// document/1
			"document/1": function( session, point, atom) {
				var doc = atom.args[0];
				var newdoc = new pl.type.DOM( document );
				session.prepend( [new pl.type.State(
					point.goal.replace(new pl.type.Term("=", [doc, newdoc])),
					point.substitution,
					point
				)] );
			},

			// head/1
			"head/1": function( session, point, atom) {
				var head = atom.args[0];
				var newhead = new pl.type.DOM( document.head );
				session.prepend( [new pl.type.State(
					point.goal.replace(new pl.type.Term("=", [head, newhead])),
					point.substitution,
					point
				)] );
			},

			// body/1
			"body/1": function( session, point, atom) {
				var body = atom.args[0];
				var newbody = new pl.type.DOM( document.body );
				session.prepend( [new pl.type.State(
					point.goal.replace(new pl.type.Term("=", [body, newbody])),
					point.substitution,
					point
				)] );
			},
			
			// get_by_id/2
			"get_by_id/2": function( session, point, atom ) {
				var id = atom.args[0], object = atom.args[1];
				if( pl.type.is_variable( id ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( id ) ) {
					session.throw_error( pl.error.type( "atom", id, atom.indicator ) );
				} else if( !pl.type.is_variable( object ) && !pl.type.is_dom_object( object ) ) {
					session.throw_error( pl.error.type( "HTMLObject", object, atom.indicator ) );
				} else {
					var element = document.getElementById( id.id );
					if( element ) {
						var html = new pl.type.DOM( element );
						session.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [html, object] ) ), point.substitution, point )] );
					}
				}
			},

			// get_by_class/2
			"get_by_class/2": [
				new pl.type.Rule(new pl.type.Term("get_by_class", [new pl.type.Var("Class"),new pl.type.Var("Html")]), new pl.type.Term(",", [new pl.type.Term("document", [new pl.type.Var("D")]),new pl.type.Term("get_by_class", [new pl.type.Var("D"),new pl.type.Var("Class"),new pl.type.Var("Html")])]))
			],
			
			// get_by_class/3
			"get_by_class/3": function( session, point, atom ) {
				var parent = atom.args[0], name = atom.args[1], object = atom.args[2];
				if( pl.type.is_variable( parent ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( parent ) ) {
					session.throw_error( pl.error.type( "HTMLObject", parent, atom.indicator ) );
				} else if( pl.type.is_variable( name ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) ) {
					session.throw_error( pl.error.type( "atom", name, atom.indicator ) );
				} else if( !pl.type.is_variable( object ) && !pl.type.is_dom_object( object ) ) {
					session.throw_error( pl.error.type( "HTMLObject", object, atom.indicator ) );
				} else {
					var elements = parent.object.getElementsByClassName( name.id );
					if( elements ) {
						var states = [];
						for( var i = 0; i < elements.length; i++ ) {
							var html = new pl.type.DOM( elements[i] );
							states.push( new pl.type.State( point.goal.replace( new pl.type.Term( "=", [html, object] ) ), point.substitution, point ) );
						}
						session.prepend( states );
					}
				}
			},

			// get_by_tag/2
			"get_by_tag/2": [
				new pl.type.Rule(new pl.type.Term("get_by_tag", [new pl.type.Var("Tag"),new pl.type.Var("Html")]), new pl.type.Term(",", [new pl.type.Term("document", [new pl.type.Var("D")]),new pl.type.Term("get_by_tag", [new pl.type.Var("D"),new pl.type.Var("Tag"),new pl.type.Var("Html")])]))
			],
			
			// get_by_tag/3
			"get_by_tag/3": function( session, point, atom ) {
				var parent = atom.args[0], tag = atom.args[1], object = atom.args[2];
				if( pl.type.is_variable( parent ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( parent ) ) {
					session.throw_error( pl.error.type( "HTMLObject", parent, atom.indicator ) );
				} else if( pl.type.is_variable( tag ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( tag ) ) {
					session.throw_error( pl.error.type( "atom", tag, atom.indicator ) );
				} else if( !pl.type.is_variable( object ) && !pl.type.is_dom_object( object ) ) {
					session.throw_error( pl.error.type( "HTMLObject", object, atom.indicator ) );
				} else {
					var elements = parent.object.getElementsByTagName( tag.id );
					if( elements ) {
						var states = [];
						for( var i = 0; i < elements.length; i++ ) {
							var html = new pl.type.DOM( elements[i] );
							states.push( new pl.type.State( point.goal.replace( new pl.type.Term( "=", [html, object] ) ), point.substitution, point ) );
						}
						session.prepend( states );
					}
				}
			},

			// get_by_name/2
			"get_by_name/2": function( session, point, atom ) {
				var name = atom.args[0], object = atom.args[1];
				if( pl.type.is_variable( name ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_atom( name ) ) {
					session.throw_error( pl.error.type( "atom", name, atom.indicator ) );
				} else if( !pl.type.is_variable( object ) && !pl.type.is_dom_object( object ) ) {
					session.throw_error( pl.error.type( "HTMLObject", object, atom.indicator ) );
				} else {
					var elements = document.getElementsByName( name.id );
					if( elements ) {
						var states = [];
						for( var i = 0; i < elements.length; i++ ) {
							var html = new pl.type.DOM( elements[i] );
							states.push( new pl.type.State( point.goal.replace( new pl.type.Term( "=", [html, object] ) ), point.substitution, point ) );
						}
						session.prepend( states );
					}
				}
			},

			// get_style/3
			"get_style/3": function( session, point, atom ) {
				var html = atom.args[0], property = atom.args[1], value = atom.args[2];
				if( pl.type.is_variable( html ) || pl.type.is_variable( property ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", selector, atom.indicator ) );
				} else if( !pl.type.is_atom( property ) ) {
					session.throw_error( pl.error.type( "atom", property, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					var style = document.defaultView.getComputedStyle( html.object, "" )[property.id] || "";
					if( style === '' && html.object.style[property.id] )
						style = html.object.style[property.id];
					var html_value = styleToProlog( style );
					session.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, html_value] ) ), point.substitution, point )] );
				}
			},

			// set_style/3
			"set_style/3": function( session, point, atom ) {
				var html = atom.args[0], property = atom.args[1], value = atom.args[2];
				var styleValue = styleFromProlog( value );
				var ground = pl.type.is_ground( value );
				if( pl.type.is_variable( html ) || pl.type.is_variable( property ) || !ground ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", selector, atom.indicator ) );
				} else if( !pl.type.is_atom( property ) ) {
					session.throw_error( pl.error.type( "atom", property, atom.indicator ) );
				} else if( styleValue === false ) {
					session.throw_error( pl.error.domain( "style_value", value, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					html.object.style[property.id] = styleValue;
					session.success( point );
				}
			},
			
			// style/3
			"style/3": function( session, point, atom ) {
				var html = atom.args[0], property = atom.args[1], value = atom.args[2];
				var styleValue = styleFromProlog( value );
				var ground = pl.type.is_ground( value );
				if( pl.type.is_variable( html ) || pl.type.is_variable( property ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", html, atom.indicator ) );
				} else if( !pl.type.is_atom( property ) ) {
					session.throw_error( pl.error.type( "atom", property, atom.indicator ) );
				} else if( !pl.type.is_variable( value ) && ground && styleValue === false ) {
					session.throw_error( pl.error.domain( "style_value", value, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					if( !ground ) {
						var style = document.defaultView.getComputedStyle( html.object, "" )[property.id] || "";
						if( style === '' && html.object.style[property.id] )
							style = html.object.style[property.id];
						var html_value = styleToProlog( style );
						session.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [value, html_value] ) ), point.substitution, point )] );
					} else {
						html.object.style[property.id] = styleValue;
						session.success( point );
					}
				}
			},

			// get_attr/3
			"get_attr/3": function( session, point, atom ) {
				var html = atom.args[0], attr = atom.args[1], value = atom.args[2];
				if( pl.type.is_variable( html ) || pl.type.is_variable( attr ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", selector, atom.indicator ) );
				} else if( !pl.type.is_atom( attr ) ) {
					session.throw_error( pl.error.type( "atom", attr, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					var html_value = attr.id === "value" ? new pl.type.Term(html.object.value) : styleToProlog(html.object.getAttribute(attr.id));
					if( html_value !== null && html_value !== undefined )
						session.prepend( [new pl.type.State(
							point.goal.replace( new pl.type.Term( "=", [value, html_value] ) ),
							point.substitution, point
						)] );
				}
			},

			// set_attr/3
			"set_attr/3": function( session, point, atom ) {
				var html = atom.args[0], attr = atom.args[1], value = atom.args[2];
				var styleValue = styleFromProlog( value );
				var ground = pl.type.is_ground( value );
				if( pl.type.is_variable( html ) || pl.type.is_variable( attr ) || !ground ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", selector, atom.indicator ) );
				} else if( !pl.type.is_atom( attr ) ) {
					session.throw_error( pl.error.type( "atom", attr, atom.indicator ) );
				} else if( styleValue === false ) {
					session.throw_error( pl.error.domain( "attribute_value", value, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					if( attr.id === "value" ) {
						html.object.value = styleValue;
					} else {
						html.object.setAttribute( attr.id, styleValue );
					}
					session.success( point );
				}
			},
			
			// attr/3
			"attr/3": function( session, point, atom ) {
				var html = atom.args[0], attr = atom.args[1], value = atom.args[2];
				var styleValue = styleFromProlog( value );
				var ground = pl.type.is_ground( value );
				if( pl.type.is_variable( html ) || pl.type.is_variable( attr ) ) {
					session.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", selector, atom.indicator ) );
				} else if( !pl.type.is_atom( attr ) ) {
					session.throw_error( pl.error.type( "atom", attr, atom.indicator ) );
				} else if( !pl.type.is_variable( value ) && ground && styleValue === false ) {
					session.throw_error( pl.error.domain( "attribute_value", value, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					if( !ground ) {
						var html_value = attr.id === "value" ? new pl.type.Term(html.object.value) : styleToProlog(html.object.getAttribute(attr.id));
						if( html_value !== null && html_value !== undefined )
							session.prepend( [new pl.type.State(
								point.goal.replace( new pl.type.Term( "=", [value, html_value] ) ),
								point.substitution, point
							)] );
					} else {
						if( attr.id === "value" ) {
							html.object.value = styleValue;
						} else {
							html.object.setAttribute( attr.id, styleValue );
						}
						session.success( point );
					}
				}
			},

			// get_html/2
			"get_html/2": function( thread, point, atom ) {
				var html = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( html ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", html, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					var inner = new pl.type.Term( html.object.innerHTML );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [inner, value] ) ), point.substitution, point )] );
				}
			},

			// set_html/2
			"set_html/2": function( thread, point, atom ) {
				var html = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( html ) || pl.type.is_variable( value ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( html ) ) {
					session.throw_error( pl.error.type( "HTMLObject", html, atom.indicator ) );
				} else {
					if( html.object === document ) return;
					if( pl.type.is_atom( value ) )
						html.object.innerHTML = value.id;
					else
						html.object.innerHTML = value.toString();
					thread.success( point );
				}
			},
			
			// html/2
			"html/2": function( thread, point, atom ) {
				var html = atom.args[0], value = atom.args[1];
				if( pl.type.is_variable( html ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else {
					if( html.object === document ) return;
					if( pl.type.is_variable( value ) ) {
						var inner = new pl.type.Term( html.object.innerHTML );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [inner, value] ) ), point.substitution, point )] );
					} else {
						if( pl.type.is_atom( value ) )
							html.object.innerHTML = value.id;
						else
							html.object.innerHTML = value.toString();
						thread.success( point );
					}
				}
			},
			
			// create/2
			"create/2": function( thread, point, atom ) {
				var tag = atom.args[0], element = atom.args[1];
				if( pl.type.is_variable( tag ) && pl.type.is_variable( element ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( tag ) && !pl.type.is_atom( tag ) ) {
					thread.throw_error( pl.error.type( "atom", tag, atom.indicator ) );
				} else if( !pl.type.is_variable( element ) && !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else if( pl.type.is_variable( element ) ) {
					var node = new pl.type.DOM( document.createElement( tag.id ) );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [element, node] ) ), point.substitution, point )] );
				} else if( pl.type.is_variable( element ) ) {
					var node = new pl.type.DOM( document.createElement( tag.id.toLowerCase() ) );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [element, node] ) ), point.substitution, point )] );
				} else if( pl.type.is_variable( tag ) ) {
					var type = new pl.type.Term( element.object.nodeName.toLowerCase() );
					thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [type, tag] ) ), point.substitution, point )] );
				}
			},
			
			// parent_of/2
			"parent_of/2": function( thread, point, atom ) {
				var child = atom.args[0], parent = atom.args[1];
				if( pl.type.is_variable( parent ) && pl.type.is_variable( child ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( parent ) && !pl.type.is_dom_object( parent ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", parent, atom.indicator ) );
				} else if( !pl.type.is_variable( child ) && !pl.type.is_dom_object( child ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", child, atom.indicator ) );
				} else if( pl.type.is_variable( child ) ) {
					var children = parent.object.children;
					var states = [];
					for( var i = 0; i < children.length; i++ ) {
						states.push( new pl.type.State( point.goal.replace( new pl.type.Term( "=", [child, new pl.type.DOM( children[i] )] ) ), point.substitution, point ) );
					}
					thread.prepend( states );
				} else {
					if( child.object.parentNode ) {
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [parent, new pl.type.DOM( child.object.parentNode )] ) ), point.substitution, point )] );
					}
				}
			},
			
			// sibling/2
			"sibling/2": function( thread, point, atom ) {
				var left = atom.args[0], right = atom.args[1];
				if( pl.type.is_variable( left ) && pl.type.is_variable( right ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_variable( left ) && !pl.type.is_dom_object( left ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", left, atom.indicator ) );
				} else if( !pl.type.is_variable( right ) && !pl.type.is_dom_object( right ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", right, atom.indicator ) );
				} else {
					if( pl.type.is_variable( left ) && right.object.previousElementSibling ) {
						var elem = new pl.type.DOM( right.object.previousElementSibling );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [left, elem] ) ), point.substitution, point )] );
					} else if( !pl.type.is_variable( left ) &&  left.object.nextElementSibling ) {
						var elem = new pl.type.DOM( left.object.nextElementSibling );
						thread.prepend( [new pl.type.State( point.goal.replace( new pl.type.Term( "=", [right, elem] ) ), point.substitution, point )] );
					}
				}
			},
			
			// remove/1
			"remove/1": function( thread, point, atom ) {
				var element = atom.args[0];
				if( pl.type.is_variable( element ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else {
					if( element.object.parentNode ) {
						element.object.parentNode.removeChild( element.object );
						thread.success( point );
					}
				}
			},
			
			// insert_after/2
			"insert_after/2": function( thread, point, atom ) {
				var element = atom.args[0], reference = atom.args[1];
				if( pl.type.is_variable( element ) || pl.type.is_variable( reference ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else if( !pl.type.is_dom_object( reference ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", reference, atom.indicator ) );
				} else {
					if( reference.object.parentNode ) {
						reference.object.parentNode.insertBefore(element.object, reference.object.nextSibling);
						thread.success( point );
					}
				}
			},
			
			// insert_before/2
			"insert_before/2": function( thread, point, atom ) {
				var element = atom.args[0], reference = atom.args[1];
				if( pl.type.is_variable( element ) || pl.type.is_variable( reference ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else if( !pl.type.is_dom_object( reference ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", reference, atom.indicator ) );
				} else {
					if( reference.object.parentNode ) {
						reference.object.parentNode.insertBefore(element.object, reference.object);
						thread.success( point );
					}
				}
			},
			
			// prepend_child/2
			"prepend_child/2": function( thread, point, atom ) {
				var parent = atom.args[0], child = atom.args[1];
				if( pl.type.is_variable( parent ) || pl.type.is_variable( child ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( parent ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", parent, atom.indicator ) );
				} else if( !pl.type.is_dom_object( child ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", child, atom.indicator ) );
				} else {
					if( parent.object.firstChild )
						parent.object.insertBefore( child.object, parent.object.firstChild );
					else
						parent.object.appendChild( child.object );
					thread.success( point );
				}
			},
			
			// append_child/2
			"append_child/2": function( thread, point, atom ) {
				var parent = atom.args[0], child = atom.args[1];
				if( pl.type.is_variable( parent ) || pl.type.is_variable( child ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( parent ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", parent, atom.indicator ) );
				} else if( !pl.type.is_dom_object( child ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", child, atom.indicator ) );
				} else {
					parent.object.appendChild( child.object );
					thread.success( point );
				}
			},
			
			// add_class/2
			"add_class/2": function( thread, point, atom ) {
				var element = atom.args[0], name = atom.args[1];
				if( pl.type.is_variable( element ) || pl.type.is_variable( name ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else if( !pl.type.is_atom( name ) ) {
					thread.throw_error( pl.error.type( "atom", name, atom.indicator ) );
				} else {
					var arr = element.object.className.split(" ");
					if( arr.indexOf( name.id ) === -1 ) {
						element.object.className += " " + name.id;
					}
					thread.success( point );
				}
			},
			
			// remove_class/2
			"remove_class/2": function( thread, point, atom ) {
				var element = atom.args[0], name = atom.args[1];
				if( pl.type.is_variable( element ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else if( !pl.type.is_atom( name ) && !pl.type.is_variable( name ) ) {
					thread.throw_error( pl.error.type( "atom", name, atom.indicator ) );
				} else {
					var arr = element.object.className.split(" ");
					if( pl.type.is_variable( name ) ) {
						var states = [];
						for( var i = 0; i < arr.length; i++ ) {
							states.push( new pl.type.State( point.goal.replace(
								new pl.type.Term( ",", [
									new pl.type.Term( "=", [name, new pl.type.Term( arr[i], [] )] ),
									new pl.type.Term( "remove_class", [element, name] )
								] )
							), point.substitution, point ) );
						}
						thread.prepend( states );
					} else {
						var newclasses = "";
						for( var i = 0; i < arr.length; i++ )
							if( arr[i] !== name.id )
								newclasses += arr[i] + " ";
						element.object.className = newclasses;
						thread.success( point );
					}
				}
			},
			
			// has_class/2
			"hasClass/2": function( thread, point, atom ) {
				var element = atom.args[0], name = atom.args[1];
				if( pl.type.is_variable( element ) ) {
					thread.throw_error( pl.error.instantiation( atom.indicator ) );
				} else if( !pl.type.is_dom_object( element ) ) {
					thread.throw_error( pl.error.type( "HTMLObject", element, atom.indicator ) );
				} else if( !pl.type.is_atom( name ) && !pl.type.is_variable( name ) ) {
					thread.throw_error( pl.error.type( "atom", name, atom.indicator ) );
				} else {
					var arr = element.object.className.split(" ");
					if( pl.type.is_variable( name ) ) {
						var states = [];
						for( var i = 0; i < arr.length; i++ )
							states.push( new pl.type.State( point.goal.replace( new pl.type.Term( "=", [name, new pl.type.Term( arr[i], [] )] ) ), point.substitution, point ) );
						thread.prepend( states );
					} else {
						if( arr.indexOf( name.id ) !== -1 )
							thread.success( point );
					}
				}
			}
			
		};
	};
	
	var exports = ["document/1", "head/1", "body/1", "show/1", "hide/1", "toggle/1", "create/2", "get_by_id/2", "get_by_tag/2", "get_by_tag/3", "get_by_class/2", "get_by_class/3", "get_by_name/2", "attr/3", "set_attr/3", "get_attr/3", "style/3", "set_style/3", "get_style/3", "html/2", "set_html/2", "get_html/2", "parent_of/2", "insert_after/2", "insert_before/2", "append_child/2", "prepend_child/2", "sibling/2", "remove/1", "add_class/2", "remove_class/2", "has_class/2", "bind/4", "unbind/2", "unbind/3", "event_property/3", "prevent_default/1"];
	
	
	
	// DOM HTML OBJECTS
	
	// Get value of style from Prolog object
	function styleFromProlog( obj ) {
		if( obj === undefined || obj === null )
			return false;
		else if( pl.type.is_number( obj ) )
			return obj.value;
		else if( pl.type.is_term( obj ) && obj.args.length === 0 )
			return obj.id;
		else if( pl.type.is_term( obj ) && obj.indicator === "px/1" && pl.type.is_number( obj.args[0] ) )
			return obj.args[0].value.toString() + "px";
		else if( pl.type.is_term( obj ) && obj.indicator === "%/1" && pl.type.is_number( obj.args[0] ) )
			return obj.args[0].value.toString() + "%";
		else if( pl.type.is_term( obj ) && obj.indicator === "url/1" && pl.type.is_atom( obj.args[0] ) )
			return "url(\"" + obj.args[0].id + "\")";
		else if( pl.type.is_term( obj ) && obj.indicator === "rgb/3" && pl.type.is_integer( obj.args[0] ) && pl.type.is_integer( obj.args[1] ) && pl.type.is_integer( obj.args[2] ) )
			return obj.toString();
		return false;
	}
	
	// Get value of style to Prolog object
	function styleToProlog( str ) {
		if( str === undefined || str === null )
			return
		else if( /^-?[0-9]*\.?[0-9]*\s*px\s*$/.test( str ) )
			return new pl.type.Term( "px", [new pl.type.Num( parseInt( str ) )] );
		else if( /^-?[0-9]*\.?[0-9]*\s*\%\s*$/.test( str ) )
			return new pl.type.Term( "%", [new pl.type.Num( parseFloat( str ) )] );
		else if( /^url\(["'].*["']\)$/.test( str ) )
			return new pl.type.Term( "url", [new pl.type.Term( str.substring(5, str.length-2) )] );
		else if( /^rgb\(\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\)\s*$/.test( str ) ) {
			var rgb = str.replace("rgb","").replace("(","").replace(")","").split(",");
			return new pl.type.Term( "rgb", [new pl.type.Num(parseInt(rgb[0]), false), new pl.type.Num(parseInt(rgb[1]), false), new pl.type.Num(parseInt(rgb[2]), false)] );
		}
		return new pl.type.Term( str.toString(), [] );
	}
	
	// Is a DOM object
	pl.type.is_dom_object = function( obj ) {
		return obj instanceof pl.type.DOM;
	};

	// Ordering relation
	pl.type.order.push( pl.type.DOM );

	// DOM Prolog object
	pl.type.DOM = function( object ) {
		this.object = object;
	}

	// toString
	pl.type.DOM.prototype.toString = function() {
		return "<html>(" + (this.object.id !== "" && this.object.id !== undefined ? "#" + this.object.id : this.object.nodeName.toLowerCase().replace("#", "")) + ")";
	};

	// clone
	pl.type.DOM.prototype.clone = function() {
		return new pl.type.DOM( this.object );
	};

	// equals
	pl.type.DOM.prototype.equals = function( obj ) {
		return pl.type.is_dom_object( obj ) && this.object === obj.object;
	};

	// rename
	pl.type.DOM.prototype.rename = function( _ ) {
		return this;
	};

	// get variables
	pl.type.DOM.prototype.variables = function() {
		return [];
	};

	// apply substitutions
	pl.type.DOM.prototype.apply = function( _ ) {
		return this;
	};

	// unify
	pl.type.DOM.prototype.unify = function( obj, _ ) {
		if( pl.type.is_dom_object( obj ) && this.object === obj.object ) {
			return new pl.type.State( obj, new pl.type.Substitution() );
		}
		return null;
	};

	// interpret
	pl.type.DOM.prototype.interpret = function( indicator ) {
		return pl.error.instantiation( indicator );
	};

	// compare
	pl.type.DOM.prototype.compare = function( obj ) {
		if( this.object === obj.object ) {
			return 0;
		} else if( this.object < obj.object ) {
			return -1;
		} else if( this.object > obj.object ) {
			return 1;
		}
	};
	
	// to javascript
	pl.type.DOM.prototype.toJavaScript = function() {
		return this.object;
	};
	
	// from javascript
	pl.fromJavaScript.test.dom = function( obj ) {
		return obj instanceof HTMLElement;
	};
	pl.fromJavaScript.conversion.dom = function( obj ) {
		return new pl.type.DOM( obj );
	};
	
	// from javascript (collection)
	pl.fromJavaScript.test.dom_collection = function( obj ) {
		return obj instanceof HTMLCollection;
	};
	pl.fromJavaScript.conversion.dom_collection = function( obj ) {
		var arr = Array.prototype.slice.call( obj, 0 );
		return pl.fromJavaScript.apply( arr );
	};

	// Streamable
	pl.type.DOM.prototype.stream = function( options, mode ) {
		if( mode === "write" )
			if( this.object instanceof HTMLInputElement )
				this.object.value = "";
			else
				this.object.innerHTML = "";
		return {
			object: this.object,
			get: function( length, position ) {
				var text;
				if( this.object instanceof HTMLInputElement )
					text = this.object.value.substring( position, position+length );
				else
					text = this.object.innerHTML;
				if( position >= text.length )
					return "end_of_html";
				return text.substring( position, position+length );
			},
			put: function( text, position ) {
				if( position === "end_of_file" ) {
					if( this.object instanceof HTMLInputElement )
						this.object.value += text;
					else
						this.object.innerHTML += text;
					return true;
				} else if( position === "past_end_of_file" ) {
					return null;
				} else {
					if( this.object instanceof HTMLInputElement )
						this.object.value = this.object.value.substring(0, position) + text + this.object.value.substring(position+text.length);
					else
						this.object.innerHTML = this.object.innerHTML.substring(0, position) + text + this.object.innerHTML.substring(position+text.length);
					return true;
				}
			},
			get_byte: function( position ) {
				if( position === "end_of_stream" )
					return -1;
				var index = Math.floor(position/2);
				var text;
				if( this.object instanceof HTMLInputElement )
					text = this.object.value.substring( position, position+length );
				else
					text = this.object.innerHTML;
				if( text.length <= index )
					return -1;
				var code = pl.utils.codePointAt( text[Math.floor(position/2)], 0 );
				if( position % 2 === 0 )
					return code & 0xff;
				else
					return code / 256 >>> 0;
			},
			put_byte: function( byte, position ) {
				var text;
				if( this.object instanceof HTMLInputElement )
					text = this.object.value;
				else
					text = this.object.innerHTML;
				var index = position === "end_of_stream" ? text.length : Math.floor(position/2);
				if( text.length < index )
					return null;
				var code = text.length === index ? -1 : pl.utils.codePointAt( text[Math.floor(position/2)], 0 );
				if( position % 2 === 0 ) {
					code = code / 256 >>> 0;
					code = ((code & 0xff) << 8) | (byte & 0xff);
				} else {
					code = code & 0xff;
					code = ((byte & 0xff) << 8) | (code & 0xff);
				}
				if( text.length === index )
					text += pl.utils.fromCodePoint( code );
				else 
					text = text.substring( 0, index ) + pl.utils.fromCodePoint( code ) + text.substring( index+1 );
				if( this.object instanceof HTMLInputElement )
					this.object.value = text;
				else
					this.object.innerHTML = text;
				return true;
			},
			flush: function() {
				return true;
			},
			close: function() {
				return true;
			}
		};
	};
	
	
	
	// DOM EVENT OBJECTS
	
	// Is a DOM Event object
	pl.type.is_dom_event_object = function( obj ) {
		return obj instanceof pl.type.DOMEvent;
	};

	// Ordering relation
	pl.type.order.push( pl.type.DOMEvent );

	// DOM Event Prolog object
	pl.type.DOMEvent = function( type, event, epoch ) {
		this.type = type;
		this.event = event || null;
		this.epoch = epoch || (new Date).getTime();
	}

	// toString
	pl.type.DOMEvent.prototype.toString = function() {
		return "<event>(" + this.type.toLowerCase() + ")";
	};

	// clone
	pl.type.DOMEvent.prototype.clone = function() {
		return new pl.type.DOMEvent( this.type, this.event, this.epoch );
	};

	// equals
	pl.type.DOMEvent.prototype.equals = function( obj ) {
		return pl.type.is_dom_event_object( obj ) && this.type === obj.type && this.epoch === obj.epoch;
	};

	// rename
	pl.type.DOMEvent.prototype.rename = function( _ ) {
		return this;
	};

	// get variables
	pl.type.DOMEvent.prototype.variables = function() {
		return [];
	};

	// apply substitutions
	pl.type.DOMEvent.prototype.apply = function( _ ) {
		return this;
	};

	// unify
	pl.type.DOMEvent.prototype.unify = function( obj, _ ) {
		if( pl.type.is_dom_event_object( obj ) && this.type === obj.type && this.epoch === obj.epoch ) {
			return new pl.type.State( obj, new pl.type.Substitution() );
		}
		return null;
	};

	// interpret
	pl.type.DOMEvent.prototype.interpret = function( indicator ) {
		return pl.error.instantiation( indicator );
	};

	// compare
	pl.type.DOMEvent.prototype.compare = function( obj ) {
		if( this.epoch === obj.epoch ) {
			return 0;
		} else if( this.epoch < obj.epoch ) {
			return -1;
		} else if( this.epoch > obj.epoch ) {
			return 1;
		}
	};
	
	// to javascript
	pl.type.DOMEvent.prototype.toJavaScript = function() {
		return this.event;
	};
	
	// from javascript
	pl.fromJavaScript.test.event = function( obj ) {
		return obj instanceof Event;
	};
	pl.fromJavaScript.conversion.event = function( obj ) {
		return new pl.type.DOMEvent( obj.type, obj );
	};
	
	
	// EVENT HANDLING
	var events = (function() {

		var tau_fn_event = {};


		var add = function(element, evt, fn) {

			if(element.addEventListener !== undefined) {
				element.addEventListener(evt, fn);
				return true;
			}

			else if(element.attachEvent !== undefined) {
				element.attachEvent("on" + evt, fn);
				return true;
			}

			var prop = element["on" + evt];
			var fns = [];
			if(prop) {
				if(prop.tau_fn_event === tau_fn_event) {
					if(prop.fns.indexOf(fn) === -1 )
						prop.fns.push(fn);
					return true;
				} else {
					fns.push(prop);
				}
			}

			fns.push(fn);
			element["on" + evt] = function(e) {
				for(var i = 0; i < fns.length; i++)
					fns[i].call(element, e, element);	
			};
			element["on" + evt].fns = fns;
			element["on" + evt].tau_fn_event = tau_fn_event;
			return true;
		};

		var remove = function(element, evt, fn) {

			if(element.removeEventListener) {
				element.removeEventListener(evt, fn);
				return true;
			}

			else if(element.detachEvent) {
				element.detachEvent("on" + evt, fn);
				return true;
			}

			else if(element["on" + evt]) {
				var f = element["on" + evt];
				if(f === fn)
					element["on" + evt] = undefined;
				else if(f.tau_fn_event === tau_fn_event) {
					for(var i = 0; i < f.fns.length; i++) {
						if(f.fns[i] === fn) {
							f.fns.splice(i, 1);
							break;
						}
					}
					return true;
				}
				else
					return false;
			}

			return true;
		};

		return {
			add: add,
			remove: remove
		};
	})();

	

	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			new pl.type.Module( "dom", predicates(), exports );
		};
	} else {
		new pl.type.Module( "dom", predicates(), exports );
	}
	

})( pl );

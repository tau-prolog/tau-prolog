var pl;
(function( pl ) {

	var width = function( ctx, font, margin, padding ) {
		ctx.font = font;
		ctx.textAlign = "center";
		return Math.max( ctx.measureText( this.text_goal ).width, ctx.measureText( this.text_substitution ).width ) + ( 2 * margin ) + ( 2 * padding );
	};

	var contains = function( obj, arr ) {
			for( var i = 0; i < arr.length; i++ )
				if( arr[i] === obj )
					return true;
		return false;
	};

	var get_states_by_level = function( parent ) {
		var levels = [];
		var i = 0;
		do {
			var level = get_states_at_level( [parent], i );
			if( level.length != 0 ) 
				levels.push( level );
			i++;
		} while( level.length != 0 );
		for( i = 0; i < levels.length; i++ )
			for( var j = 0; j < levels[i].length; j++ )
				levels[i][j].id_canvas_level = j;
		return levels;
	};

	var get_states_at_level = function( nodes, level ) {
		if(level == 0)
			return nodes;
		var children = [];
		for(var i = 0; i < nodes.length; i++)
			children = children.concat( get_states_at_level( nodes[i].children, level-1 ) );
		return children;
	};
	
	var set_preorder_id = function( node, id ) {
		node.preorder_id = id;
		for( var i = 0; i < node.children.length; i++ ) {
			id++;
			id = set_preorder_id( node.children[i], id );
		}
		return id++;
	};
	
	var clear_substitution = function( subs ) {
		var links = {};
		for( var x in subs.links ) {
			if(!subs.links.hasOwnProperty(x)) continue;
			var link = subs.links[x];
			if( !pl.type.is_variable(link) || link.id != x )
				links[x] = link;
		}
		return new pl.type.Substitution( links );
	};
	
	var set_style = function( obj, name, value ) {
		if( typeof obj[name] === "undefined" )
			obj[name] = value;
	};

	var init = function( thread, max, canvas, styles ) {
		styles = styles ? styles : {};
		set_style( styles, "font-size", 14 );
		set_style( styles, "font-family", "Monospace, Courier New" );
		set_style( styles, "font", styles["font-size"] + "px " + styles["font-family"] );
		set_style( styles, "border-width", 2 );
		set_style( styles, "border-color", "#43207a" );
		set_style( styles, "padding", 5 );
		set_style( styles, "margin-x", 10 );
		set_style( styles, "margin-y", 20 );
		set_style( styles, "node", {} );
		set_style( styles, "order", {} );
		set_style( styles, "state", {} );
		set_style( styles, "answer", {} );
		set_style( styles, "error", {} );
		set_style( styles["order"], "radius", 15 );
		set_style( styles["order"], "background-color", "#43207a" );
		set_style( styles["order"], "border-width", 4 );
		set_style( styles["order"], "border-color", "#43207a" );
		set_style( styles["order"], "font-color", "#ffffff" );
		set_style( styles["state"], "background-color", "#e0ccfd" );
		set_style( styles["state"], "border-width", 4 );
		set_style( styles["state"], "border-color", "#43207a" );
		set_style( styles["state"], "font-color", "#43207a" );
		set_style( styles["answer"], "background-color", "#a7e3a7" );
		set_style( styles["answer"], "border-width", 4 );
		set_style( styles["answer"], "border-color", "#0b6a0d" );
		set_style( styles["answer"], "font-color", "#0b6a0d" );
		set_style( styles["error"], "background-color", "#ecc2c2" );
		set_style( styles["error"], "border-width", 4 );
		set_style( styles["error"], "border-color", "#881717" );
		set_style( styles["error"], "font-color", "#881717" );
		// Get parent node
		parent = thread.points[0];
		if(typeof parent === "undefined")
			return;
		// Get answer states
		var answers = [];
		var format_success = thread.session.format_success;
		var format_error = thread.session.format_error;
		var deb = thread.session.thread.debugger;
		var id = function(x) { return x };
		var after = function() {
			thread.session.format_success = format_success;
			thread.session.format_error = format_error;
			thread.debugger = deb;
			answers = thread.debugger_states;
			// Set pointers to child nodes
			for(var i = 0; i < answers.length; i++) {
				var state = answers[i];
				var child = null;
				while( state != null ) {
					state.text_goal = state.goal === null ? "□" : state.goal.toString();
					state.text_substitution = clear_substitution(state.substitution).toString();
					state.width = width;
					if(state.children == null)
						state.children = [];
					if( child != null && !contains(child, state.children) )
						state.children.push( child );
					child = state;
					state = state.parent;
				}
			}
			// Set preorder id
			set_preorder_id( parent, 0 );
			// Get nodes by level
			var levels = get_states_by_level( parent );
			draw( levels, canvas, styles );
		};
		thread.session.format_success = id;
		thread.session.format_error = id;
		thread.session.thread.debugger = true;
		thread.session.answers( function(x) { x.status = "answer"; }, max, after );
	};

	var draw = function( tree, canvas_id, styles ) {
		// Graph configuration
		var padding = styles["padding"];
		var margin_x = styles["margin-x"];
		var margin_y = styles["margin-y"];
		var font_size = styles["font-size"];
		var radius = styles["order"]["radius"];
		// Get container, canvas and context
		var src = document.getElementById(src);
		var canvas = typeof canvas_id === "string" ? document.getElementById(canvas_id) : canvas_id;
		var ctx = canvas.getContext("2d");
		// Maximum width of each level
		var maxwidth = tree.map(function(a){return a.reduce(function(b,c){return b + c.width(ctx, styles["font"], margin_x, padding)}, 0)}, 0);
		// Width of the image
		var width = maxwidth.reduce(function(a,b){return Math.max(a,b)}, 0);
		// Get height of state blocks, levels and image
		var unifier_height = (padding * 3) + ((font_size + 2) * 2);
		var level_height = (radius * 2) + (margin_y * 2) + unifier_height;
		var height = level_height * tree.length - (2 * radius);
		// Set canvas properties
		ctx.height = height;
		ctx.width = width;
		canvas.height = ctx.height;
		canvas.width = ctx.width;
		
		// CLEAR CANVAS
		ctx.clearRect(0, 0, width, height);
		
		// DRAW LINES
		
		// Relative centers to other levels
		var relative = [];
		// Height
		var offset_y = margin_y;
		var offset_z = margin_y;
		// Iterate levels
		for (var i = 0; i < tree.length; i++) {
			// Push centers for following levels
			relative.push([]);
			// Get offset
			var offset_x = (width - maxwidth[i]) / 2;
			// Iterate states
			for (var j = 0; j < tree[i].length; j++) {
				// Get initial offset
				offset_z = offset_y;
				// Set relative center
				var center = offset_x + (tree[i][j].width(ctx, styles["font"], margin_x, padding) / 2);
				relative[i].push(center);
				if (i > 0) {
					// Draw lines
					ctx.lineWidth = styles["border-width"];
					ctx.strokeStyle = styles["border-color"];
					ctx.beginPath();
					ctx.moveTo(relative[i-1][tree[i][j].parent.id_canvas_level], offset_y - unifier_height / 2);
					ctx.lineTo(center, offset_y + 2 * (margin_y + radius));
					ctx.closePath();
					ctx.stroke();
					// Draw rule
					offset_z += 2 * (margin_y + radius)
				}
				offset_z += unifier_height;
				// Update offset
				offset_x += tree[i][j].width(ctx, styles["font"], margin_x, padding);
			}
			// Update offset
			offset_y = offset_z;
		}
		
		// DRAW BLOCKS
		
		// Relative centers to other levels
		relative = [];
		// Height
		offset_y = margin_y;
		offset_z = margin_y;
		// Iterate levels
		for (i = 0; i < tree.length; i++) {
			// Push centers for following levels
			relative.push([]);
			// Get offset
			offset_x = (width - maxwidth[i]) / 2;
			// Iterate states
			for (j = 0; j < tree[i].length; j++) {
				// Get initial offset
				offset_z = offset_y;
				// Set relative center
				center = offset_x + (tree[i][j].width(ctx, styles["font"], margin_x, padding) / 2);
				relative[i].push(center);
				if (i > 0) {
					var x1 = Math.abs(relative[i-1][tree[i][j].parent.id_canvas_level] - center);
					var y1 = 2 * (radius + margin_y) + unifier_height / 2;
					var y3 = margin_y + radius;
					var alpha = Math.atan(y1 / x1);
					var y4 = y3 + radius * Math.sin(alpha);
					var x3 = y3 / (Math.tan(alpha));
					var x4 = x3 - radius * Math.cos(alpha);
					if (relative[i-1][tree[i][j].parent.id_canvas_level] < center) {
						x3 = -x3;
						x4 = -x4;
					}
					// Draw rule
					offset_z += margin_y + radius;
							ctx.fillStyle = styles["order"]["background-color"];
							ctx.strokeStyle = styles["order"]["border-color"];
							ctx.lineWidth = styles["order"]["border-width"];
							ctx.beginPath();
							ctx.arc(center + x3, offset_z, radius, 0, 2 * Math.PI);
							ctx.closePath();
							ctx.stroke();
							ctx.fill();
						ctx.fillStyle = styles["order"]["font-color"];
						ctx.font = "bold " + styles["font"];
						ctx.textAlign = "center"; 
						ctx.fillText(tree[i][j].preorder_id, center + x3, offset_z + font_size / 2 - 1);
					offset_z += radius + margin_y;
				}
				// Draw state
				var type_state;
				if(tree[i][j].status && tree[i][j].status === "answer") // Answer
					type_state = "answer";
				else if(tree[i][j].children.length == 0 && tree[i][j].goal !== null) // Error
					type_state = "error";
				else
					type_state = "state";
				ctx.fillStyle = styles[type_state]["background-color"];
				ctx.strokeStyle = styles[type_state]["border-color"];
				ctx.lineWidth = styles[type_state]["border-width"];
				ctx.beginPath();
				ctx.rect(center - tree[i][j].width(ctx, styles["font"], 0, padding) / 2, offset_z, tree[i][j].width(ctx, styles["font"], 0, padding), unifier_height);
				ctx.closePath();
				ctx.stroke();
				ctx.fill();
				ctx.fillStyle = styles[type_state]["font-color"];
				ctx.font = styles["font"];
				ctx.textAlign = "center"; 
				ctx.fillText(tree[i][j].text_goal, center, offset_z + padding + font_size);
				ctx.fillText(tree[i][j].text_substitution, center, offset_z + (2 * font_size) + (2 * padding));
				offset_z += unifier_height;
				// Update offset
				offset_x += tree[i][j].width(ctx, styles["font"], margin_x, padding);
			}
			// Update offset
			offset_y = offset_z;
		}
		// Add click event
		var click_draw = function(){ window.open(canvas.toDataURL(), '_blank'); };
		if (canvas.addEventListener) {
			if(canvas.click_draw)
				canvas.removeEventListener("click", canvas.click_draw);
			canvas.addEventListener("click", click_draw, false);
			canvas.style.cursor = "pointer";
		} else {
			if (canvas.attachEvent) {
				if(canvas.click_draw)
					canvas.detachEvent("click", canvas.click_draw);
				canvas.attachEvent("click", click_draw);
				canvas.style.cursor = "pointer";
			}
		}
		canvas.click_draw = click_draw;
	};
	


	if( typeof module !== 'undefined' ) {
		module.exports = function( p ) {
			pl = p;
			pl.type.Thread.prototype.draw = function( max, canvas, styles ) { return init( this, max, canvas, styles ); };
			pl.type.Session.prototype.draw = function( max, canvas, styles ) { return this.thread.draw( max, canvas, styles ); };
		};
	} else {
		pl.type.Thread.prototype.draw = function( max, canvas, styles ) { return init( this, max, canvas, styles ); };
		pl.type.Session.prototype.draw = function( max, canvas, styles ) { return this.thread.draw( max, canvas, styles ); };
	}

})( pl );

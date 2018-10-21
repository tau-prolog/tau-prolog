(function() {

	var width = function( ctx, margin, padding ) {
		ctx.font = "14px Roboto Mono, Monospace, Courier New";
		ctx.textAlign = "center";
		return Math.max( ctx.measureText( this.text_goal ).width, ctx.measureText( this.text_substitution ).width ) + ( 2 * margin ) + ( 2 * padding );
	};

	var contains = function( obj, arr ) {
			for( var i = 0; i < arr.length; i++ )
				if( arr[i] === obj )
					return true;
		return false;
	};

	function get_states_by_level( parent ) {
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
	}

	function get_states_at_level( nodes, level ) {
		if(level == 0)
			return nodes;
		var children = [];
		for(var i = 0; i < nodes.length; i++)
			children = children.concat( get_states_at_level( nodes[i].children, level-1 ) );
		return children;
	}

	var tau_prolog_derivation = {
		
		draw: function( canvas, session, max ) {
			// Get parent node
			parent = session.thread.points[0];
			// Get answer states
			var answers = [];
			var format_success = session.format_success;
			var format_error = session.format_error;
			var id = function(x) { return x };
			session.format_success = id;
			session.format_error = id;
			session.answers( function(x) { return answers.push(x); }, max );
			session.format_success = format_success;
			session.format_error = format_error;
			// Set pointers to child nodes
			for(var i = 0; i < answers.length-1; i++) {
				var state = answers[i];
				var child = null;
				while( state != null ) {
					state.text_goal = state.goal === null ? "□" : state.goal.toString();
					state.text_substitution = state.substitution.toString();
					state.width = width;
					if(state.children == null)
						state.children = [];
					if( child != null && !contains(child, state.children) )
						state.children.push( child );
					child = state;
					state = state.parent;
				}
			}
			// Get nodes by level
			var levels = get_states_by_level( parent );
			this.drawTree( canvas, levels );
		},
		
		// Dibujar arbol
		drawTree: function( canvas, tree ) {
			// Graph configuration
			var padding = 5;
			var margin_x = 10;
			var margin_y = 20;
			var radius = 18;
			var font_size = 14;
			// Get container, canvas and context
			var src = document.getElementById(src);
			var canvas = document.getElementById(canvas);
			var ctx = canvas.getContext("2d");
			// Maximum width of each level
			var maxwidth = tree.map(function(a){return a.reduce(function(b,c){return b + c.width(ctx, margin_x, padding)}, 0)}, 0);
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
					var center = offset_x + (tree[i][j].width(ctx, margin_x, padding) / 2);
					relative[i].push(center);
					if (i > 0) {
						// Draw lines
						ctx.lineWidth = 2;
						ctx.strokeStyle = "#43207a";
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
					offset_x += tree[i][j].width(ctx, margin_x, padding);
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
					center = offset_x + (tree[i][j].width(ctx, margin_x, padding) / 2);
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
								ctx.fillStyle = "#43207a";
								ctx.strokeStyle = "#43207a";
								ctx.lineWidth = 4;
								ctx.beginPath();
								ctx.arc(center + x3, offset_z, radius, 0, 2 * Math.PI);
								ctx.closePath();
								ctx.stroke();
								ctx.fill();
							ctx.fillStyle = "#ffffff";
							ctx.font = "bold 14px Roboto Mono, Monospace, Courier New";
							ctx.textAlign = "center"; 
							ctx.fillText(j, center + x3, offset_z + font_size / 2 - 1);
						offset_z += radius + margin_y;
					}
					// Draw unifier
					ctx.fillStyle = "#e0ccfd";
					ctx.strokeStyle = "#43207a";
					ctx.lineWidth = 4;
					ctx.strokeStyle = "#43207a";
					ctx.beginPath();
					ctx.rect(center - tree[i][j].width(ctx, 0, padding) / 2, offset_z, tree[i][j].width(ctx, 0, padding), unifier_height);
					ctx.closePath();
					ctx.stroke();
					ctx.fill();
					ctx.fillStyle = "#43207a";
					ctx.font = "14px Roboto Mono, Monospace, Courier New";
					ctx.textAlign = "center"; 
					ctx.fillText(tree[i][j].text_goal, center, offset_z + padding + font_size);
					ctx.fillText(tree[i][j].text_substitution, center, offset_z + (2 * font_size) + (2 * padding));
					offset_z += unifier_height;
					// Update offset
					offset_x += tree[i][j].width(ctx, margin_x, padding);
				}
				// Update offset
				offset_y = offset_z;
			}
			// Add click event
			if (canvas.addEventListener) {
				canvas.addEventListener("click", function(){ window.open(canvas.toDataURL(), '_blank'); }, false);
				canvas.style.cursor = "pointer";
			} else {
				if (canvas.attachEvent) {
					canvas.attachEvent("click", function(){ window.open(canvas.toDataURL(), '_blank'); });
					canvas.style.cursor = "pointer";
				}
			}
		}
		
	};
	
	window.tau_prolog_derivation = tau_prolog_derivation;

}());

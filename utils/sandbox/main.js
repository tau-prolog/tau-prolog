var try_program = "";
var try_goal = "";
var try_goals = [""];
var try_stack = 0;
var session = null;
var code = null;
var query = null;
var MODE_PROGRAM = 1;
var MODE_DERIVATION = 2;
var mode = MODE_PROGRAM;
var reset = 0;
var styles = {};

window.addEventListener("load", function() {
	var value = document.getElementById("program").innerHTML.replace(/&lt;/g,"<").replace(/&gt;/g,">");
	document.getElementById("program").innerHTML = "";
	code = CodeMirror(document.getElementById("program"), {
		value: value,
		lineNumbers: true,
		theme: "tau",
		placeholder: "Your program here...",
		mode: "prolog"
	});
	query = CodeMirror(document.getElementById("query"), {
		lineNumbers: false,
		theme: "tauout",
		placeholder: "Type a Prolog goal in here and press ENTER",
		mode: "prolog"
	});
	code.setSize("100%", "100%");
	code.on("change", function(instance, change) {
		document.getElementById("save-program").value = instance.getValue();
	});
	query.setSize("100%", query.defaultTextHeight() + 2 * 4);
	query.on("beforeChange", function(instance, change) {
		var newtext = change.text.join("").replace(/\n/g, "");
		change.update(change.from, change.to, [newtext]);
		return true;
	});
	query.on("keyHandled", try_tau_prolog)
});

function try_tau_prolog( cm, msg, e ) {
	// Down
	if( e.keyCode === 40 ) {
		try_stack++;
		if( try_stack >= try_goals.length ) try_stack = 0;
		query.setValue(try_goals[try_stack]);
	// Up
	} else if( e.keyCode === 38 ) {
		try_stack--;
		if( try_stack < 0 ) try_stack = try_goals.length - 1;
		query.setValue(try_goals[try_stack]);
	// Enter
	} else if( e.keyCode === 13 ) {
		try {
			var raw_program = code.getValue();
			var raw_goal = query.getValue();
			if( try_program !== raw_program || try_goal !== raw_goal || reset || mode == MODE_DERIVATION ) {
				if( mode == MODE_PROGRAM ) {
					new_block(raw_goal);
					try_goals.push( raw_goal );
					try_stack = try_goals.length - 1;
					reset = 0;
				} else if( mode == MODE_DERIVATION ) {
					reset = 1;
				}
				if( session == null )
					session = pl.create(parseInt(document.getElementById("limit").value));
				session.limit = parseInt(document.getElementById("limit").value);
				var q = session.query( raw_goal );
				if( q !== true ) {
					try_answer( 'error parsing query: ' + q.args[0], true );
					return;
				}
			}
		} catch( ex ) {
			try_answer( 'javascript error: ' + ex.toString() + '<div class="report-github"><a href="https://github.com/jariazavalverde/tau-prolog/issues" target="_blank">Report error on GitHub</a></div>', true );
			return;
		}
		if( mode == MODE_PROGRAM )
			session.answer( try_answer );
		else if( mode == MODE_DERIVATION ) {
			var max_answers = parseInt(document.getElementById("max_answers").value);
			session.draw(max_answers, "tau-canvas", styles);
		}
	}
	try_program = raw_program;
	try_goal = raw_goal;
}

function escapeHtml(unsafe) {
	return unsafe.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#039;");
}

function new_block(last) {
	document.getElementById( "output" ).innerHTML = "<div class=\"goal\"></div>" + document.getElementById( "output" ).innerHTML;
	document.getElementById( "output" ).innerHTML = "<div class=\"last\">?- " + last + "</div>" + document.getElementById( "output" ).innerHTML;
}

function try_answer( answer, format ) {
	var elem = document.getElementsByClassName( "goal" )[0];
	var inner = elem.innerHTML;
	if( inner !== "" ) {
		elem.innerHTML = "<div class=\"sep\"></div>" + inner;
	}
	elem.innerHTML = "<div class=\"answer\">" + (format ? answer : escapeHtml(pl.format_answer( answer, session )) ) + "</div>" + elem.innerHTML;	
}

function toggle(id) {
	if( id == "save")
		document.getElementById("help").style.display = 'none';
	else
		document.getElementById("save").style.display = 'none';
	var elem = document.getElementById( id );
	if( elem.style.display == 'none' ) {
		elem.style.display = 'block';
	} else {
		elem.style.display = 'none';
	}
}

function add(text) {
	code.setValue(text + "\n" + code.getValue());
}

function addClassName( elem, name ) {
	var arr = elem.className.split(" ");
	if( arr.indexOf(name) == -1 )
		elem.className += " " + name;
}

function removeClassName( elem, name ) {
	elem.className = elem.className.replace(name, "");
} 

function show_program_tab() {
	removeClassName(document.getElementById( "derivation-tab" ), "selected-section");
	addClassName(document.getElementById( "program-tab" ), "selected-section");
	document.getElementById( "canvas-container" ).style.display = "none";
	document.getElementById( "program-container" ).style.display = "block";
	document.getElementById( "output" ).style.display = "block";
	document.getElementById( "max_answers-container" ).style.display = "none";
	document.getElementById( "tree-options" ).style.display = "none";
	mode = MODE_PROGRAM;
}

function show_derivation_tab() {
	removeClassName(document.getElementById( "program-tab" ), "selected-section");
	addClassName(document.getElementById( "derivation-tab" ), "selected-section");
	document.getElementById( "program-container" ).style.display = "none";
	document.getElementById( "canvas-container" ).style.display = "block";
	document.getElementById( "output" ).style.display = "none";
	document.getElementById( "max_answers-container" ).style.display = "inline";
	document.getElementById( "tree-options" ).style.display = "block";
	mode = MODE_DERIVATION;
}

function set_theme( theme ) {
	var xhttp = new XMLHttpRequest();
	xhttp.onreadystatechange = function() {
		if (this.readyState == 4 && this.status == 200) {
			styles = JSON.parse( this.responseText );
			try_tau_prolog( query, null, {keyCode: 13} );
		}
	};
	xhttp.open("GET", "../utils/draw-derivation-trees/themes/" + theme + ".json", true);
	xhttp.send();
}

function reconsult() {
	document.getElementById("reconsult").value = "Reconsult program";
	var raw_program = code.getValue();
	if( session == null )
		session = pl.create(parseInt(document.getElementById("limit").value));
	var c = session.consult( raw_program );
	reset = 1;
	new_block("consult");
	if( c !== true )
		try_answer( 'error parsing program: ' + c.args[0], true );
	else
		try_answer( 'parsing program: ok!', true );
	var warnings = session.getWarnings();
	for( var i = warnings.length-1; i >= 0; i-- )
		try_answer( 'warning parsing program: ' + warnings[i].toString(), true );
}

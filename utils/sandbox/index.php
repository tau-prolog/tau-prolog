<?php

$_time_core = filemtime("../code/core-latest.js");
$_time_lists = filemtime("../code/lists-latest.js");
$_time_random = filemtime("../code/random-latest.js");
$_time_statistics = filemtime("../code/statistics-latest.js");
$_time_dom = filemtime("../code/dom-latest.js");
$_time_js = filemtime("../code/js-latest.js");
$_time_draw = filemtime("../utils/draw-derivation-trees/draw-derivation-trees.js");

function genid() {
	$letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
	$len = strlen($letters);
	$id = "";
	for($i = 0; $i < 8; $i++){
		$id .= $letters[rand(0,$len)];
	}
	return $id;
}

function genuniqid($link) {
	$exists = true;
	$id = "";
	while( $exists ) {
		$id = genid();
		$exists = $link->query("SELECT id FROM code WHERE link='$id'")->num_rows > 0;
	}
	return $id;
}

$program = ":-use_module(library(lists)).\n";
$title = "";
$description = "Test your Prolog online with Tau Prolog code editor.";
$date = "";

if( isset($_GET["id"]) ) {
	
	require( "mysql.php" );
	$id = $_link->escape_string($_GET["id"]);
	$query = $_link->query("SELECT * FROM code WHERE link='$id'");
	while( $row = $query->fetch_assoc() ) {
		$program = $row["code"];
		$title = $row["name"];
		$description = $row["description"];
		$date = $row["date"];
		if ($title == "") $title = $id;
		if ($description == "") $description = $id;
		$description .= " - Tau Prolog Sandbox";
	}
	
} elseif( isset($_POST["submit"]) ) {
	
	require( "mysql.php" );
	if( isset($_POST["title"]) ) $title = $_link->escape_string($_POST["title"]); else $title = "";
	if( isset($_POST["description"]) ) $description = $_link->escape_string($_POST["description"]); else $description = "";
	$program = $_link->escape_string($_POST["program"]);
	$id = genuniqid($_link);
	$query = $_link->query("INSERT INTO code(link, olink, code, name, description) VALUES ('$id', '$id', '$program', '$title', '$description')");
	header("location: $id");

}

?>
<!DOCTYPE html>
<html lang="en" onClick="hide_dialogs();">
	<head>
		<title><?php if( $title != "") echo $title . " - "; ?>Tau Prolog Sandbox</title>
		<meta name="description" content="<?php echo $description; ?>" />
		<meta name="author" content="José Antonio Riaza Valverde" />
		<meta name="keywords" content="Prolog, JavaScript, Interpreter, Logic, Programming">
		<meta charset="UTF-8" />
		<link href="https://fonts.googleapis.com/css?family=Source+Sans+Pro" rel="stylesheet">
		<link href="https://fonts.googleapis.com/css?family=Inconsolata" rel="stylesheet">
		<link href="/content/img/favicon.ico" type="image/x-icon" rel="icon" />
		<link rel="StyleSheet" href="main.css" type="text/css" media="ALL" />
		<!-- Tau Prolog modules -->
		<script type="text/javascript" src="/code/core-latest.js?update=<?php echo $_time_core; ?>"></script>
		<script type="text/javascript" src="/code/lists-latest.js?update=<?php echo $_time_lists; ?>"></script>
		<script type="text/javascript" src="/code/random-latest.js?update=<?php echo $_time_random; ?>"></script>
		<script type="text/javascript" src="/code/statistics-latest.js?update=<?php echo $_time_statistics; ?>"></script>
		<script type="text/javascript" src="/code/dom-latest.js?update=<?php echo $_time_dom; ?>"></script>
		<script type="text/javascript" src="/code/js-latest.js?update=<?php echo $_js_statistics; ?>"></script>
		<!-- Tau Prolog utils -->
		<script type="text/javascript" src="/utils/draw-derivation-trees/draw-derivation-trees.js?update=<?php echo $_time_draw; ?>"></script>
		<!-- Codemirror -->
		<script src="codemirror/lib/codemirror.js"></script>
		<link rel="stylesheet" href="codemirror/lib/codemirror.css">
		<link rel="stylesheet" href="codemirror/theme/tau.css">
		<link rel="stylesheet" href="codemirror/theme/tauout.css">
		<!--script src="codemirror/mode/javascript/javascript.js"></script-->
		<script src="codemirror/addon/mode/simple.js"></script>
		<script src="codemirror/mode/prolog/prolog.js"></script>
		<script src="codemirror/addon/placeholder/placeholder.js"></script>
		<script type="text/javascript" src="main.js"></script>
	</head>
	<body>
		<div id="save" onClick="event.stopPropagation();" style="display:none;">
			<form action="./" method="post">
				<input id="save-title" name="title" type="input" placeholder="Title (optional)" />
				<textarea id="save-program" style="display:none!important;" name="program"><?php echo $program; ?></textarea>
				<textarea id="save-descr" name="description" type="input" placeholder="Description (optional)"></textarea>
				<p id="save-legal">The code you may store and/or share through this service is your responsibility. We do not guarantee the completeness, truthfulness, accuracy or reliability of the code you post here. Use this sandbox at your own risk.</p>
				<input id="save-submit" name="submit" type="submit" value="Save" />
			</form>
		</div>
		<div id="help" onClick="event.stopPropagation();" style="display:none;">
			Look at <a href="http://tau-prolog.org/documentation" title="Tau Prolog: Documentation" target="_blank">built-in predicates and modules</a> supported by Tau Prolog.
			<ul>
				<li><a href="http://tau-prolog.org/documentation#lists"  title="Tau Prolog: Documentation # lists module" target="_blank">lists</a> <span title="Add to your program" onClick="add(':-use_module(library(lists)).');">:-use_module(library(lists)).</span></li>
				<li><a href="http://tau-prolog.org/documentation#random"  title="Tau Prolog: Documentation # random module" target="_blank">random</a> <span title="Add to your program" onClick="add(':-use_module(library(random)).');">:-use_module(library(random)).</span></li>
				<li><a href="http://tau-prolog.org/documentation#statistics"  title="Tau Prolog: Documentation # statistics module" target="_blank">statistics</a> <span title="Add to your program" onClick="add(':-use_module(library(statistics)).');">:-use_module(library(statistics)).</span></li>
				<li><a href="http://tau-prolog.org/documentation#dom"  title="Tau Prolog: Documentation # dom module" target="_blank">dom</a> <span title="Add to your program" onClick="add(':-use_module(library(dom)).');">:-use_module(library(dom)).</span></li>
				<li><a href="http://tau-prolog.org/documentation#js"  title="Tau Prolog: Documentation # js module" target="_blank">js</a> <span title="Add to your program" onClick="add(':-use_module(library(js)).');">:-use_module(library(js)).</span></li>
			</ul>
			<div class="help-bar"></div>
			<p class="help-par">Latest updates</p>
			<ul class="help-list-version">
				<li>core.js <?php echo "[" . $_time_core . "] " . date ("(d-m-Y H:i)", $_time_core); ?></li>
				<li>lists.js <?php echo "[" . $_time_lists . "] " . date ("(d-m-Y H:i)", $_time_lists); ?></li>
				<li>random.js <?php echo "[" . $_time_random . "] " . date ("(d-m-Y H:i)", $_time_random); ?></li>
				<li>statistics.js <?php echo "[" . $_time_statistics . "] " . date ("(d-m-Y H:i)", $_time_statistics); ?></li>
				<li>dom.js <?php echo "[" . $_time_dom . "] " . date ("(d-m-Y H:i)", $_time_dom); ?></li>
				<li>js.js <?php echo "[" . $_time_js . "] " . date ("(d-m-Y H:i)", $_time_js); ?></li>
			</ul>
		</div>
<?php if (!isset($_GET["toolbar"]) || $_GET["toolbar"] != "hidden") { ?>
		<div id="toolbar">
			<ul>
					<li><a id="home" href="http://tau-prolog.org" title="Tau Prolog"></a></li>
					<li><a href="http://tau-prolog.org/sandbox" title="New sandbox" />New</a></li>
					<li><input type="button" value="Save" onClick="toggle('save');" title="Permanent link" /></li>
					<li><input type="button" value="Help" onClick="toggle('help');" title="Help" /></li>
					<li id="taupl-version-container"><span id="taupl-version"></span></li>
			</ul>
		</div>
<?php } ?>
		<div id="sections">
			<ul>
					<li><input type="button" id="program-tab" onClick="show_program_tab();" value="Program editor" class="selected-section" /></li>
					<li><input type="button" id="derivation-tab" onClick="show_derivation_tab();" value="Derivation tree visualization" /></li>
					<li><input type="button" id="transformations-tab" onClick="show_transformations_tab();" value="Transformations" /></li>
			</ul>
		</div>
		<div id="content">
			<div id="runable">
				<div id="program-container">
					<div>
						<div class="header">Program</div>
						<input type="button" id="reconsult" onClick="reconsult();" value="Consult program" />
						<div id="program"><?php echo htmlspecialchars($program); ?></div>
					</div>
				</div>
				<div id="canvas-container">
						<div class="canvas-note">Only queries executed under this tab will be drawn.</div>
						<canvas id="tau-canvas"></canvas>
				</div>
				<div id="transformations-container">
					<div></div>
				</div>
				<div id="query-container">
					<div>
						<div class="header">Query
							<span id="limit-container" title="Maximum number of inferences">limit: <input id="limit" type="text" value="10000" /></span>
							<span id="max_answers-container" title="Maximum number of answers">max-answers: <input id="max_answers" type="text" value="10" /></span>
							<span style="margin-left: 20px;" id="quoted-container" title="Quoted"><input type="checkbox" id="quoted" name="quoted" checked="checked"> <label for="quoted">quoted</label></span>
							<span style="margin-left: 10px;" id="ignore_ops-container" title="Ignore operators"><input type="checkbox" id="ignore_ops" name="ignore_ops"> <label for="ignore_ops">ignore_ops</label></span>
							<span style="margin-left: 10px;" id="numbervars-container" title="Numbervars"><input type="checkbox" id="numbervars" name="numbervars"> <label for="numbervars">numbervars</label></span>
						</div>
						<div id="query"></div>
						<div id="output"></div>
						<div id="tree-options">
							<span>Theme:</span>
<ul>
<?php
foreach(scandir("../utils/draw-derivation-trees/themes/") as $f)
	if($f != "." && $f != "..") {
		$name = str_replace(".json", "", $f);
		echo "<li><span onClick=\"set_theme('$name')\" class=\"theme\" id=\"theme-$name\" />$name</span></li>\n";
	}
?>
</ul>
						</div>
					</div>
				</div>
			</div>
		</div>
	</body>
</html>

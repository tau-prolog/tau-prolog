var pl;
(function(pl) {

	// Extend Tau Prolog prototypes
	var extend = function(pl) {

		// Consult a program from a string
		pl.type.Session.prototype.asyncConsult = function(program, options) {
			return this.thread.asyncConsult(program, options);
		};

		pl.type.Thread.prototype.asyncConsult = function(program, options) {
			var thread = this;
			return new Promise(function(resolve, reject) {
				var opts = {};
				options = options === undefined ? {} : options;
				opts.context_module = options.context_module;
				opts.text = options.text;
				opts.html = options.html;
				opts.url = options.url;
				opts.file = options.file;
				opts.script = options.script;
				opts.success = resolve;
				opts.error = reject;
				thread.consult(program, opts);
			});
		};

		// Query goal from a string (without ?-)
		pl.type.Session.prototype.asyncQuery = function(string) {
			return this.thread.asyncQuery(string);
		};

		pl.type.Thread.prototype.asyncQuery = function(string) {
			var thread = this;
			return new Promise(function(resolve, reject) {
				thread.query(string, {
					success: resolve,
					error: reject
				});
			});
		};

		// Find next computed answer
		pl.type.Session.prototype.asyncAnswer = function() {
			return this.thread.asyncAnswer();
		};

		pl.type.Thread.prototype.asyncAnswer = function() {
			var thread = this;
			return new Promise(function(resolve, reject) {
				thread.answer({
					success: resolve,
					fail: resolve,
					error: reject,
					limit: reject
				});
			});
		};

		// Find all computed answers (asynchronous generator function)
		pl.type.Session.prototype.asyncAnswers = function() {
			return this.thread.asyncAnswers();
		};
		pl.type.Thread.prototype.asyncAnswers = async function*() {
			while(true) {
				var answer = await this.asyncAnswer();
				if(answer !== false)
					yield answer;
				else
					return;
			}
		};

	}

	if(typeof module !== 'undefined') {
		module.exports = function(p) {
			pl = p;
			extend(pl);
		};
	} else {
		extend(pl);
	}

})(pl);
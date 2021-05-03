var pl;
(function(pl) {

	// Extend Tau Prolog prototypes
	var extend = function(pl) {

		// Consult a program from a string
		pl.type.Session.prototype.promiseConsult = function(program, options) {
			return this.thread.promiseConsult(program, options);
		};

		pl.type.Thread.prototype.promiseConsult = function(program, options) {
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
		pl.type.Session.prototype.promiseQuery = function(string) {
			return this.thread.promiseQuery(string);
		};

		pl.type.Thread.prototype.promiseQuery = function(string) {
			var thread = this;
			return new Promise(function(resolve, reject) {
				thread.query(string, {
					success: resolve,
					error: reject
				});
			});
		};

		// Find next computed answer
		pl.type.Session.prototype.promiseAnswer = function() {
			return this.thread.promiseAnswer();
		};

		pl.type.Thread.prototype.promiseAnswer = function() {
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
		pl.type.Session.prototype.promiseAnswers = function() {
			return this.thread.promiseAnswers();
		};
		pl.type.Thread.prototype.promiseAnswers = async function*() {
			while(true) {
				var answer = await this.promiseAnswer();
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
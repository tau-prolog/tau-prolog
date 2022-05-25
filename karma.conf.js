/* eslint-env node */

module.exports = function (config) {
  config.set({
    browsers: [
      'FirefoxHeadless',
      // Consider Chromium and Google Chrome to be similar enough.
      // Pick whichever is most likely to be locally installed and kept up-to-date
      // (Chromium on Linux, Chrome on macOS/Windows).
      // You can override this via CHROME_BIN or CHROMIUM_BIN.
      // For example, if your OS defaults to Chrome but you prefer Chromium,
      // then set something like CHROME_BIN=/usr/bin/chromium.
      (process.platform === 'linux') ? 'ChromiumHeadless' : 'ChromeHeadless'
    ],
    frameworks: ['qunit'],
    files: [
      'modules/core.js',
      'test/utils.js',
      { pattern: 'test/setup.js', included: false, served: false },
      'test/*.js'
    ],
    autoWatch: false,
    singleRun: true,
    preprocessors: {
      'modules/*.js': ['coverage']
    },
    reporters: ['dots', 'coverage'],
    coverageReporter: {
      reporters: [
        { type: 'lcov', dir: '.nyc_output/' },
        // Karma uses subdirs by default to account for multiple browsers.
        // For the JSON file, it's important we disable 'subdir' so that
        // the 'nyc report' command can pick this up when combining code
        // coverage with the Node.js test run.
        { type: 'json', dir: '.nyc_output/', subdir: '.' }
      ]
    }
  });
};

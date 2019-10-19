CodeMirror.defineSimpleMode("prolog", {
  start: [
      {regex: /(?:"([^"]|""|\\")*"|`([^`]|``|\\`)*`)/, token: 'string'},
      {regex: /(?:0o[0-7]+|0x[0-9a-f]+|0b[01]+|0'(?:''|\\[abfnrtv\\'"`]|\\x?\d+\\|.)|\d+(?:\.\d+(?:e[+-]?\d+)?)?)/, token: 'number'},
      {regex: /\s*(?:(?:%.*)|(?:\s+))\s*/, token: 'comment'},
      {regex: /\/\*/, token: 'comment', next: 'comment'},
      {regex: /(?:[A-Z_][a-zA-Z0-9_]*)/, token: 'variable'},
      {regex: /([a-z][0-9a-zA-Z_]*)/, token: 'atom'},
      {regex: /'/, token: 'atom', next: 'atom'},
      {regex: /([#\$\&\*\+\-\.\/\:\<\=\>\?@\^\~\\]+)/, token: 'graphic'},
      {regex: /\./, token: 'point'},
      {regex: /\!/, token: 'cut'},
      {regex: /[,;\(\)]/, token: 'paren'},
      {regex: /\s+/, token: null},
      {regex: /./, token: 'error'}
  ],
  
  comment: [
    {regex: /.*?\*\//, token: 'comment', next: 'start'},
    {regex: /.*/, token: 'comment'}
  ],

  atom: [
    {regex: /'/, token: 'atom', next: 'start'},
    {regex: /[^']|(?:\\(?:x?\d+)?\\)|(?:'')|(?:\\')/, token: 'atom'}
  ],
});
// This file is a copy of https://github.com/codemirror/legacy-modes/blob/main/mode/mllike.js
// but edited to highlight Jane Street OCaml extensions.
//
// MIT License
//
// Copyright (C) 2018-2021 by Marijn Haverbeke <marijn@haverbeke.berlin> and others
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

(function () {
  function mlLike(parserConfig) {
    var words = {
      as: 'keyword',
      do: 'keyword',
      else: 'keyword',
      end: 'keyword',
      exception: 'keyword',
      fun: 'keyword',
      functor: 'keyword',
      if: 'keyword',
      in: 'keyword',
      include: 'keyword',
      let: 'keyword',
      of: 'keyword',
      open: 'keyword',
      rec: 'keyword',
      struct: 'keyword',
      then: 'keyword',
      type: 'keyword',
      val: 'keyword',
      while: 'keyword',
      with: 'keyword',
    };

    var ppxes = {
      map: 'keyword',
      bind: 'keyword',
      sub: 'keyword',
      arr: 'keyword',
      tydi: 'keyword',
      pattern_map: 'keyword',
      pattern_bind: 'keyword',
      message: 'keyword',
      string: 'keyword',
      css: 'keyword',
      html: 'keyword',
      demo: 'keyword',
      sexp: 'keyword',
      compare: 'keyword',
      sexp_of: 'keyword',
      of_sexp: 'keyword',
      equal: 'keyword',
      expect_test: 'keyword',
      quick_test: 'keyword',
      datafetcher: 'keyword',
      bench: 'keyword',
      bench_fun: 'keyword',
      expect: 'keyword',
      test: 'keyword',
      test_eq: 'keyword',
    };

    var extraWords = parserConfig.extraWords || {};
    for (var prop in extraWords) {
      if (extraWords.hasOwnProperty(prop)) {
        words[prop] = parserConfig.extraWords[prop];
      }
    }
    var hintWords = [];
    for (var k in words) {
      hintWords.push(k);
    }

    function tokenBase(stream, state) {
      var ch = stream.next();

      if (ch === '"') {
        state.tokenize = tokenString;
        return state.tokenize(stream, state);
      }
      if (ch === '\'') {
        if (stream.match(/\\?.'/)) {
          // A character literal
          return 'string';
        } else {
          // A type parameter
          stream.match(/[a-z_]+/);
          return 'type';
        }
      }
      if (ch === '{') {
        if (stream.eat('|')) {
          state.longString = true;
          state.tokenize = tokenLongString;
          return state.tokenize(stream, state);
        }
      }
      if (ch === '(') {
        if (stream.match(/^\*(?!\))/)) {
          state.commentLevel++;
          state.tokenize = tokenComment;
          return state.tokenize(stream, state);
        }
      }
      if (ch === '~' || ch === '?') {
        stream.eatWhile(/\w/);
        return 'variableName.special';
      }
      if (ch === '`') {
        stream.eatWhile(/\w/);
        return "quote";
      }
      if (ch === '/' && parserConfig.slashComments && stream.eat('/')) {
        stream.skipToEnd();
        return 'comment';
      }
      if (/\d/.test(ch)) {
        if (ch === '0' && stream.eat(/[bB]/)) {
          stream.eatWhile(/[01]/);
        }
        if (ch === '0' && stream.eat(/[xX]/)) {
          stream.eatWhile(/[0-9a-fA-F]/);
        }
        if (ch === '0' && stream.eat(/[oO]/)) {
          stream.eatWhile(/[0-7]/);
        } else {
          stream.eatWhile(/[\d_]/);
          if (stream.eat('.')) {
            stream.eatWhile(/[\d]/);
          }
          if (stream.eat(/[eE]/)) {
            stream.eatWhile(/[\d\-+]/);
          }
        }
        return 'number';
      }

      if (/[+\-*&=<>!?|@\.~:]/.test(ch)) {
        return 'operator';
      }

      if (/[\w\xa1-\uffff]/.test(ch) || ch === '%') {
        stream.eatWhile(/[\w\xa1-\uffff]/);
        var cur = stream.current();
        if (cur === '%') {
          return 'operator';
        }

        function variable() {
          if (cur[0].toUpperCase() === cur[0] && cur[0].toLowerCase() !== cur[0]) {
            // We want to color uppercase variables (e.g. constructors/modules) differently
            // from lowercase variables.
            return 'quote';
          }
          return 'variable';
        }

        if (cur[0] === '%') {
          const ppx = cur.slice(1);
          return ppxes.hasOwnProperty(ppx) ? ppxes[ppx] : variable();
        }
        return words.hasOwnProperty(cur) ? words[cur] : variable();
      }
      return null;
    }

    function tokenString(stream, state) {
      var next,
        end = false,
        escaped = false;
      while ((next = stream.next()) != null) {
        if (next === '"' && !escaped) {
          end = true;
          break;
        }
        escaped = !escaped && next === '\\';
      }
      if (end && !escaped) {
        state.tokenize = tokenBase;
      }
      return 'string';
    }

    function tokenComment(stream, state) {
      var prev, next;
      while (state.commentLevel > 0 && (next = stream.next()) != null) {
        if (prev === '(' && next === '*') state.commentLevel++;
        if (prev === '*' && next === ')') state.commentLevel--;
        if (prev === 'C' && next === 'R') {
          stream.backUp(2);
          state.tokenize = tokenCommentCR;
          return 'comment';
        }
        prev = next;
      }
      if (state.commentLevel <= 0) {
        state.tokenize = tokenBase;
      }
      return 'comment';
    }

    function tokenCommentCR(stream, state) {
      state.tokenize = tokenComment;
      if (stream.match(/CR(-soon|-someday)? [a-z\-_]{2,}( for [a-z\-_]{2,})?:/)) {
        return 'atom';
      } else {
        stream.match('CR');
        return state.tokenize(stream, state);
      }
    }

    function tokenLongString(stream, state) {
      var prev, next;
      while (state.longString && (next = stream.next()) != null) {
        if (prev === '|' && next === '}') state.longString = false;
        prev = next;
      }
      if (!state.longString) {
        state.tokenize = tokenBase;
      }
      return 'string';
    }

    return {
      startState: function () {
        return { tokenize: tokenBase, commentLevel: 0, longString: false };
      },
      token: function (stream, state) {
        if (stream.eatSpace()) return null;
        return state.tokenize(stream, state);
      },

      languageData: {
        autocomplete: hintWords,
        commentTokens: {
          line: parserConfig.slashComments ? '//' : undefined,
          block: { open: '(*', close: '*)' },
        },
      },
    };
  }

  globalThis.codemirror_ocaml__stream_parser = mlLike({
    name: 'ocaml',
    extraWords: {
      and: 'keyword',
      assert: 'keyword',
      begin: 'keyword',
      class: 'keyword',
      constraint: 'keyword',
      done: 'keyword',
      downto: 'keyword',
      external: 'keyword',
      function: 'keyword',
      initializer: 'keyword',
      lazy: 'keyword',
      match: 'keyword',
      method: 'keyword',
      module: 'keyword',
      mutable: 'keyword',
      new: 'keyword',
      nonrec: 'keyword',
      object: 'keyword',
      private: 'keyword',
      sig: 'keyword',
      to: 'keyword',
      try: 'keyword',
      virtual: 'keyword',
      when: 'keyword',

      // OCaml Extensions
      local_: 'keyword',
      global_: 'keyword',
      exclave_: 'keyword',
      kind_abbrev_: 'keyword',
      kind_of_: 'keyword',
      once_: 'keyword',
      stack_: 'keyword',
      unique_: 'keyword',

      // builtins
      raise: 'builtin',
      failwith: 'builtin',
      true: 'builtin',
      false: 'builtin',

      // Pervasives builtins
      asr: 'operatorKeyword',
      land: 'operatorKeyword',
      lor: 'operatorKeyword',
      lsl: 'operatorKeyword',
      lsr: 'operatorKeyword',
      lxor: 'operatorKeyword',
      mod: 'operatorKeyword',
      or: 'operatorKeyword',

      // More Pervasives
      raise_notrace: 'builtin',
      trace: 'builtin',
      exit: 'builtin',
      print_string: 'builtin',
      print_endline: 'builtin',

      int: 'type',
      float: 'type',
      bool: 'type',
      char: 'type',
      string: 'type',
      unit: 'type',

      // Modules
      List: 'builtin',
    },
  });
})();

"use strict";

globalThis.codemirror_emacs__emacs = {};

var _blockCursor = globalThis.codemirror_emacs__block_cursor;

var _state = codemirror.State;

var _view = codemirror.View;

var commands = _interopRequireWildcard(codemirror.Commands);

var _autocomplete = codemirror.Autocomplete;

var _search = codemirror.Search;

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function _getRequireWildcardCache(nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || _typeof(obj) !== "object" && typeof obj !== "function") { return { "default": obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj["default"] = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }

function _iterableToArrayLimit(arr, i) { var _i = arr == null ? null : typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"]; if (_i == null) return; var _arr = []; var _n = true; var _d = false; var _s, _e; try { for (_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

function _createForOfIteratorHelper(o, allowArrayLike) { var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"]; if (!it) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() { }; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e2) { throw _e2; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = it.call(o); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e3) { didErr = true; err = _e3; }, f: function f() { try { if (!normalCompletion && it["return"] != null) it["return"](); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var emacsStyle = _view.EditorView.theme({
  ".cm-emacsMode .cm-cursorLayer:not(.cm-vimCursorLayer)": {
    display: "none"
  },
  ".cm-vim-panel": {
    padding: "5px 10px",
    backgroundColor: "#fffa8f",
    fontFamily: "monospace"
  },
  ".cm-vim-panel input": {
    border: "none",
    outline: "none",
    backgroundColor: "#fffa8f"
  }
});

var emacsPlugin = _view.ViewPlugin.fromClass( /*#__PURE__*/function () {
  function _class2(view) {
    _classCallCheck(this, _class2);

    _defineProperty(this, "status", "");

    this.view = view;
    this.em = new EmacsHandler(view);
    this.blockCursor = new _blockCursor.BlockCursorPlugin(view, this.em);
    this.view.scrollDOM.classList.add("cm-emacsMode");
    /*this.em.on("dialog", () => {
      view.dispatch({
        effects: showEmacsPanel.of(!!this.cm.state.dialog)
      })
    });*/
  }

  _createClass(_class2, [{
    key: "update",
    value: function update(_update) {
      if (_update.docChanged) {
        this.em.$emacsMark = null;
        this.em.updateMarksOnChange(_update.changes);
      }
      /*if (update.selectionSet) {
        this.em.onSelectionChange()
      }
      if (update.viewportChanged) {
        // scroll
      }
      if (this.em.curOp && !this.em.curOp.isVimOp) {
        this.em.onBeforeEndOperation();
      }/**/


      this.blockCursor.update(_update);
    }
  }, {
    key: "destroy",
    value: function destroy() {
      this.view.scrollDOM.classList.remove("cm-emacsMode");
      this.blockCursor.destroy();
    }
  }]);

  return _class2;
}(), {
  eventHandlers: {
    keydown: function keydown(e, view) {
      var result = this.em.handleKeyboard(e);
      if (result) this.blockCursor.scheduleRedraw();
      return !!result;
    },
    mousedown: function mousedown() {
      this.em.$emacsMark = null;
    }
  }
});

var showVimPanel = _state.StateEffect.define();

var vimPanelState = _state.StateField.define({
  create: function create() {
    return false;
  },
  update: function update(value, tr) {
    var _iterator = _createForOfIteratorHelper(tr.effects),
      _step;

    try {
      for (_iterator.s(); !(_step = _iterator.n()).done;) {
        var e = _step.value;
        if (e.is(showVimPanel)) value = e.value;
      }
    } catch (err) {
      _iterator.e(err);
    } finally {
      _iterator.f();
    }

    return value;
  },
  provide: function provide(f) {
    return _view.showPanel.from(f, function (on) {
      return on ? createVimPanel : null;
    });
  }
});

function createVimPanel(view) {
  var dom = document.createElement("div");
  dom.className = "cm-vim-panel";
  return {
    top: false,
    dom: dom
  };
}

function emacs() {
  var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  return [emacsStyle, emacsPlugin, _blockCursor.hideNativeSelection, vimPanelState];
}
globalThis.codemirror_emacs__emacs = emacs;

var specialKey = {
  Return: 'Return',
  Escape: 'Esc',
  Insert: 'Ins',
  ArrowLeft: 'Left',
  ArrowRight: 'Right',
  ArrowUp: 'Up',
  ArrowDown: 'Down',
  Enter: 'Return',
  Divide: '/',
  Slash: '/',
  Multiply: '*',
  Subtract: '-',
  Minus: "-",
  Equal: '='
};
var ignoredKeys = {
  Shift: 1,
  Alt: 1,
  Command: 1,
  Control: 1,
  CapsLock: 1
};
var commandKeyBinding = {};

var EmacsHandler = /*#__PURE__*/function () {
  function EmacsHandler(view) {
    _classCallCheck(this, EmacsHandler);

    _defineProperty(this, "$data", {
      count: 0,
      keyChain: "",
      lastCommand: ""
    });

    _defineProperty(this, "$emacsMarkRing", []);

    _defineProperty(this, "$emacsMark", null);

    this.view = view;
  } // commands


  _createClass(EmacsHandler, [{
    key: "handleKeyboard",
    value: function handleKeyboard(e) {
      var keyData = EmacsHandler.getKey(e);
      var result = this.findCommand(keyData);
      if (/Up|Down/.test(keyData === null || keyData === void 0 ? void 0 : keyData[0]) && (0, _autocomplete.completionStatus)(this.view.state)) return;

      if (result && result.command) {
        var commandResult = EmacsHandler.execCommand(result.command, this, result.args, result.count);
        if (commandResult === false) return;
      }

      return result;
    }
  }, {
    key: "findCommand",
    value: function findCommand(_ref) {
      var _ref2 = _slicedToArray(_ref, 3),
        key = _ref2[0],
        modifier = _ref2[1],
        text = _ref2[2];

      // if keyCode == -1 a non-printable key was pressed, such as just
      // control. Handling those is currently not supported in this handler
      if (!key) return undefined;
      var editor = this;
      var data = this.$data; // editor._signal("changeStatus");
      // insertstring data.count times

      if (!modifier && key.length == 1) {
        editor.pushEmacsMark();

        if (data.count) {
          var str = new Array(data.count + 1).join(text);
          data.count = null;
          return {
            command: "insertstring",
            args: str
          };
        }
      } // CTRL + number / universalArgument for setting data.count


      if (modifier == "C-" || data.count) {
        var count = parseInt(key[key.length - 1]);

        if (typeof count === 'number' && !isNaN(count)) {
          data.count = Math.max(data.count || 0, 0);
          data.count = 10 * data.count + count;
          return {
            command: "null"
          };
        }
      } // this.commandKeyBinding maps key specs like "c-p" (for CTRL + P) to
      // command objects, for lookup key needs to include the modifier


      if (modifier) key = modifier + key; // Key combos like CTRL+X H build up the data.keyChain

      if (data.keyChain) key = data.keyChain += " " + key; // Key combo prefixes get stored as "null" (String!) in this
      // this.commandKeyBinding. When encountered no command is invoked but we
      // buld up data.keyChain

      var command = commandKeyBinding[key];
      data.keyChain = command == "null" ? key : ""; // there really is no command

      if (!command) return undefined; // we pass b/c of key combo or universalArgument

      if (command === "null") return {
        command: "null"
      };

      if (command === "universalArgument") {
        // if no number pressed emacs repeats action 4 times.
        // minus sign is needed to allow next keypress to replace it
        data.count = -4;
        return {
          command: "null"
        };
      } // lookup command
      // TODO extract special handling of markmode
      // TODO special case command.command is really unnecessary, remove


      var args;

      if (typeof command !== "string") {
        args = command.args;
        if (command.command) command = command.command;
      }

      if (command === "insertstring" || command === commands.splitLine || command === commands.toggleComment) {
        editor.pushEmacsMark();
      }

      if (typeof command === "string") {
        command = EmacsHandler.commands[command];
        if (!command) return undefined;
      }

      if (!command.readOnly && !command.keepLastCommand) {
        data.lastCommand = null;
      }

      var count = data.count || 1;
      if (data.count) data.count = 0;
      return {
        command: command,
        args: args,
        count: count
      };
    }
  }, {
    key: "showCommandLine",
    value: function showCommandLine(text) {
      console.error("TODO");
    } // mark

  }, {
    key: "updateMarksOnChange",
    value: function updateMarksOnChange(change) {
      var _this = this;

      if (this.$emacsMark) {
        this.$emacsMark = this.updateMark(this.$emacsMark, change);
      }

      this.$emacsMarkRing = this.$emacsMarkRing.map(function (x) {
        return _this.updateMark(x, change);
      }).filter(Boolean);
    }
  }, {
    key: "updateMark",
    value: function updateMark(mark, change) {
      if (!mark) return;
      var updated = mark.map(function (x) {
        return change.mapPos(x, 1, _state.MapMode.TrackDel);
      }).filter(function (x) {
        return x != null;
      });
      return updated.length == 0 ? null : updated;
    }
  }, {
    key: "emacsMark",
    value: function emacsMark() {
      return this.$emacsMark;
    }
  }, {
    key: "setEmacsMark",
    value: function setEmacsMark(p) {
      // to deactivate pass in a falsy value
      this.$emacsMark = p;
    }
  }, {
    key: "pushEmacsMark",
    value: function pushEmacsMark(p, activate) {
      var prevMark = this.$emacsMark;
      if (prevMark) pushUnique(this.$emacsMarkRing, prevMark);
      if (!p || activate) this.setEmacsMark(p); else pushUnique(this.$emacsMarkRing, p);
    }
  }, {
    key: "popEmacsMark",
    value: function popEmacsMark() {
      var mark = this.emacsMark();

      if (mark) {
        this.setEmacsMark(null);
        return mark;
      }

      return this.$emacsMarkRing.pop();
    }
  }, {
    key: "getLastEmacsMark",
    value: function getLastEmacsMark() {
      return this.$emacsMark || this.$emacsMarkRing.slice(-1)[0];
    }
  }, {
    key: "getCopyText",
    value: function getCopyText() {
      var state = this.view.state;
      return state.selection.ranges.map(function (r) {
        return state.sliceDoc(r.from, r.to);
      }).join("\n");
    }
  }, {
    key: "clearSelection",
    value: function clearSelection() {
      var view = this.view;
      var selection = view.state.selection;
      var isEmpty = !selection.ranges.some(function (r) {
        return r.from != r.to;
      });
      if (isEmpty) return false;
      var newRanges = selection.ranges.map(function (x) {
        return _state.EditorSelection.range(x.head, x.head);
      });
      view.dispatch({
        selection: _state.EditorSelection.create(newRanges, selection.mainIndex)
      });
      return true;
    }
  }, {
    key: "onPaste",
    value: function onPaste(text) {
      var view = this.view;
      var selection = view.state.selection;
      var linesToInsert;

      if (selection.ranges.length > 1) {
        var lines = text.split("\n");

        if (lines.length == selection.ranges.length) {
          linesToInsert = lines;
        }
      }

      var i = 0;
      var specs = view.state.changeByRange(function (range) {
        var toInsert = linesToInsert ? linesToInsert[i] : text;
        i++;
        return {
          changes: {
            from: range.from,
            to: range.to,
            insert: toInsert
          },
          range: _state.EditorSelection.cursor(range.from + toInsert.length)
        };
      });
      view.dispatch(specs);
    }
  }, {
    key: "selectionToEmacsMark",
    value: function selectionToEmacsMark() {
      var selection = this.view.state.selection;
      return selection.ranges.map(function (x) {
        return x.head;
      });
    }
  }], [{
    key: "bindKey",
    value: function bindKey(keyGroup, command) {
      keyGroup.split("|").forEach(function (binding) {
        var chain = "";
        var parts = binding.split(/\s+/);
        parts.forEach(function (keyGroup, index) {
          var modifiers = keyGroup.split(/-(?=.)/);
          var key = modifiers.pop();

          if (modifiers.length) {
            chain += modifiers.sort().join("-") + "-";
          }

          chain += key;

          if (index === parts.length - 1) {
            commandKeyBinding[chain] = command;
          } else {
            commandKeyBinding[chain] = "null";
            chain += " ";
          }
        });
      });
    }
  }, {
    key: "getKey",
    value: function getKey(e) {
      var code = e.code;
      var key = e.key;
      if (ignoredKeys[key]) return ['', '', ''];

      if (code.length > 1) {
        if (code[0] == "N") code = code.replace(/^Numpad/, "");
        if (code[0] == "K") code = code.replace(/^Key/, "");
      }

      code = specialKey[code] || code;
      if (code.length == 1) code = code.toLowerCase();
      var modifier = '';

      if (e.ctrlKey) {
        modifier += 'C-';
      }

      if (e.metaKey) {
        modifier += 'CMD-';
      }

      if (e.altKey) {
        modifier += 'M-';
      }

      if (e.shiftKey) {
        modifier += 'S-';
      }

      return [code, modifier, key];
    }
  }, {
    key: "addCommands",
    value: function addCommands(commands) {
      Object.keys(commands).forEach(function (name) {
        var command = commands[name];

        if (typeof command == "function") {
          command = {
            exec: command
          };
        }

        EmacsHandler.commands[name] = command;
      });
    }
  }, {
    key: "execCommand",
    value: function execCommand(command, handler, args) {
      var count = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1;
      var commandResult = undefined;
      if (count < 0) count = -count;

      if (typeof command === "function") {
        for (var i = 0; i < count; i++) {
          command(handler.view);
        }
      } else if (command === "null") {// waith for next key in the chain
      } else if (command.exec) {
        if (count > 1 && command.handlesCount) {
          if (!args) args = {};
          if (_typeof(args) === 'object') args.count = count;
          count = 1;
        }

        for (var i = 0; i < count; i++) {
          commandResult = command.exec(handler, args || {});
        }
      } else {
        throw new Error("missformed command");
      }

      return commandResult;
    }
  }]);

  return EmacsHandler;
}();

globalThis.codemirror_emacs__emacs.EmacsHandler = EmacsHandler;

_defineProperty(EmacsHandler, "commands", {});

function pushUnique(array, item) {
  if (array.length && array[array.length - 1] + "" == item + "") return;
  array.push(item);
}

var emacsKeys = {
  // movement
  "Up|C-p": {
    command: "goOrSelect",
    args: [commands.cursorLineUp, commands.selectLineUp]
  },
  "Down|C-n": {
    command: "goOrSelect",
    args: [commands.cursorLineDown, commands.selectLineDown]
  },
  "Left|C-b": {
    command: "goOrSelect",
    args: [commands.cursorCharBackward, commands.selectCharBackward]
  },
  "Right|C-f": {
    command: "goOrSelect",
    args: [commands.cursorCharForward, commands.selectCharForward]
  },
  "C-Left|M-b": {
    command: "goOrSelect",
    args: [commands.cursorGroupLeft, commands.selectGroupLeft]
  },
  "C-Right|M-f": {
    command: "goOrSelect",
    args: [commands.cursorGroupRight, commands.selectGroupRight]
  },
  "Home|C-a": {
    command: "goOrSelect",
    args: [commands.cursorLineStart, commands.selectLineStart]
  },
  "End|C-e": {
    command: "goOrSelect",
    args: [commands.cursorLineEnd, commands.selectLineEnd]
  },
  "C-Home|S-M-,": {
    command: "goOrSelect",
    args: [commands.cursorDocStart, commands.selectDocStart]
  },
  "C-End|S-M-.": {
    command: "goOrSelect",
    args: [commands.cursorDocEnd, commands.selectDocEnd]
  },
  // selection
  "S-Up|S-C-p": commands.selectLineUp,
  "S-Down|S-C-n": commands.selectLineDown,
  "S-Left|S-C-b": commands.selectCharBackward,
  "S-Right|S-C-f": commands.selectCharForward,
  "S-C-Left|S-M-b": commands.selectGroupBackward,
  "S-C-Right|S-M-f": commands.selectGroupForward,
  "S-Home|S-C-a": commands.selectLineStart,
  "S-End|S-C-e": commands.selectLineEnd,
  "S-C-Home": commands.selectDocStart,
  "S-C-End": commands.selectDocEnd,
  "C-l": "recenterTopBottom",
  "M-s": "centerSelection",
  "M-g": "gotoline",
  "C-x C-p|C-x h": commands.selectAll,
  "PageDown|C-v|C-Down": {
    command: "goOrSelect",
    args: [commands.cursorPageDown, commands.selectPageDown]
  },
  "PageUp|M-v|C-Up": {
    command: "goOrSelect",
    args: [commands.cursorPageUp, commands.selectPageDown]
  },
  "S-C-Down": commands.selectPageDown,
  "S-C-Up": commands.selectPageUp,
  // TODO use iSearch
  "C-s": _search.openSearchPanel,
  // "iSearch",
  "C-r": _search.openSearchPanel,
  // "iSearchBackwards",
  "M-C-s": "findnext",
  "M-C-r": "findprevious",
  "S-M-5": "replace",
  // basic editing
  "Backspace": commands.deleteCharBackward,
  "Delete|C-d": commands.deleteCharForward,
  "Return|C-m": {
    command: "insertstring",
    args: "\n"
  },
  // "newline"
  "C-o": commands.splitLine,
  "M-d|C-Delete": {
    command: "killWord",
    args: "right"
  },
  "C-Backspace|M-Backspace|M-Delete": {
    command: "killWord",
    args: "left"
  },
  "C-k": "killLine",
  "M-h": "selectParagraph",
  "M-@|M-S-2": "markWord",
  "C-y|S-Delete": "yank",
  "M-y": "yankRotate",
  "C-g": "keyboardQuit",
  "C-w|C-S-w": "killRegion",
  "M-w": "killRingSave",
  "C-Space": "setMark",
  "C-x C-x": "exchangePointAndMark",
  "C-t": commands.transposeChars,
  "M-u": {
    command: "changeCase",
    args: {
      dir: 1
    }
  },
  "M-l": {
    command: "changeCase",
    args: {
      dir: -1
    }
  },
  "C-x C-u": {
    command: "changeCase",
    args: {
      dir: 1,
      region: true
    }
  },
  "C-x C-l": {
    command: "changeCase",
    args: {
      dir: 1,
      region: true
    }
  },
  "M-/": _autocomplete.startCompletion,
  "C-u": "universalArgument",
  "M-;": commands.toggleComment,
  "C-/|C-x u|S-C--|C-z": commands.undo,
  "S-C-/|S-C-x u|C--|S-C-z": commands.redo,
  // infinite undo?
  // vertical editing
  "C-x r": "selectRectangularRegion",
  "M-x": {
    command: "focusCommandLine",
    args: "M-x "
  },
  // todo
  // "C-x C-t" "M-t" "M-c" "F11" "C-M- "M-q"
  "Esc": "unsetTransientMark"
};
globalThis.codemirror_emacs__emacs.emacsKeys = emacsKeys;

for (var i in emacsKeys) {
  EmacsHandler.bindKey(i, emacsKeys[i]);
}

EmacsHandler.addCommands({
  unsetTransientMark: function unsetTransientMark(handler) {
    handler.setEmacsMark(null);
    return false;
  },
  markWord: function markWord(handler, args) { },
  selectParagraph: function selectParagraph(handler, args) {
    var view = handler.view;
    var head = view.state.selection.ranges[0].head;
    var doc = view.state.doc;
    var startLine = doc.lineAt(head);
    var start = -1;
    var end = -1;
    var line = startLine;

    while (/\S/.test(line.text) && line.from > 0) {
      start = line.from;
      line = view.state.doc.lineAt(line.from - 1);
    }

    if (start == -1) {
      while (!/\S/.test(line.text) && line.to < doc.length) {
        start = line.from;
        line = view.state.doc.lineAt(line.to + 1);
      }
    } else {
      line = startLine;
    }

    while (/\S/.test(line.text) && line.to < doc.length) {
      end = line.to;
      line = view.state.doc.lineAt(line.to + 1);
    }

    if (end == -1) {
      end = startLine.to;
    }

    var newRanges = [_state.EditorSelection.range(start, end)];
    view.dispatch({
      selection: _state.EditorSelection.create(newRanges)
    });
  },
  goOrSelect: {
    exec: function exec(handler, args) {
      var command = handler.emacsMark() ? args[1] : args[0];
      command(handler.view);
    }
  },
  changeCase: function changeCase(handler, args) {
    var view = handler.view;

    if (!args.region) {
      handler.clearSelection();
      commands.selectGroupForward(view);
    }

    var specs = view.state.changeByRange(function (range) {
      var toInsert = view.state.sliceDoc(range.from, range.to);
      toInsert = args.dir == 1 ? toInsert.toUpperCase() : toInsert.toLowerCase();
      return {
        changes: {
          from: range.from,
          to: range.to,
          insert: toInsert
        },
        range: _state.EditorSelection.cursor(range.from + toInsert.length)
      };
    });
    view.dispatch(specs);
  },
  centerSelection: function centerSelection(handler) {
    handler.view.dispatch({
      scrollIntoView: true
    });
  },
  recenterTopBottom: function recenterTopBottom(handler) {
    var view = handler.view;
    var scrollTop = view.scrollDOM.scrollTop;
    view.dispatch({
      scrollIntoView: true
    });

    try {
      // force synchronous measurment
      view.measure(true);
    } catch (e) { }

    if (scrollTop != view.scrollDOM.scrollTop) return;
    var base = view.scrollDOM.getBoundingClientRect();
    var cursor = view.coordsAtPos(view.state.selection.main.head);
    if (!cursor) return;
    var lineHeight = cursor.bottom - cursor.top;
    var screenHeight = base.height;
    var cursorTop = cursor.top - base.top;

    if (Math.abs(cursorTop) < lineHeight / 4) {
      scrollTop += cursorTop + lineHeight - screenHeight + 2;
    } else if (Math.abs(cursorTop - screenHeight * 0.5) < lineHeight / 4) {
      scrollTop += cursorTop - 2;
    } else {
      scrollTop += cursorTop - screenHeight * 0.5;
    }

    view.scrollDOM.scrollTop = scrollTop;
  },
  selectRectangularRegion: function selectRectangularRegion(handler) {
    var view = handler.view;
    var ranges = view.state.selection.ranges;
    var newRanges = [];

    if (ranges.length > 1) {
      newRanges.push(_state.EditorSelection.range(ranges[0].from, ranges[ranges.length - 1].to));
    } else {
      var doc = view.state.doc;
      var startLine = doc.lineAt(ranges[0].from);
      var endLine = doc.lineAt(ranges[0].to);
      var startCollumn = ranges[0].from - startLine.from;
      var endCollumn = ranges[0].to - endLine.from;

      while (startLine.from < endLine.to) {
        newRanges.push(_state.EditorSelection.range(startLine.from + startCollumn, startLine.from + endCollumn));
        if (startLine.to + 1 >= doc.length) break;
        startLine = doc.lineAt(startLine.to + 1);
      }
    }

    view.dispatch({
      selection: _state.EditorSelection.create(newRanges)
    });
  },
  setMark: {
    exec: function exec(handler, args) {
      var view = handler.view;
      var ranges = view.state.selection.ranges; // Sets mark-mode and clears current selection.
      // When mark is set, keyboard cursor movement commands become
      // selection modification commands. That is,
      // "goto" commands become "select" commands.
      // Any insertion or mouse click resets mark-mode.
      // setMark twice in a row at the same place resets markmode.
      // in multi select mode, ea selection is handled individually

      if (args && args.count) {
        var newMark = handler.selectionToEmacsMark();
        var mark = handler.popEmacsMark();

        if (mark) {
          var newRanges = mark.map(function (p) {
            return _state.EditorSelection.cursor(p, p);
          });
          view.dispatch({
            selection: _state.EditorSelection.create(newRanges)
          });
          handler.$emacsMarkRing.unshift(newMark);
        }

        return;
      }

      var mark = handler.emacsMark();
      var rangePositions = ranges.map(function (r) {
        return r.head;
      });
      var transientMarkModeActive = true;
      var hasNoSelection = ranges.every(function (range) {
        return range.from == range.to;
      }); // if transientMarkModeActive then mark behavior is a little
      // different. Deactivate the mark when setMark is run with active
      // mark

      if (transientMarkModeActive && (mark || !hasNoSelection)) {
        handler.clearSelection();
        if (mark) handler.pushEmacsMark(null);
        return;
      }

      if (!mark) {
        handler.pushEmacsMark(rangePositions);
        handler.setEmacsMark(rangePositions);
        return;
      } // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    },
    readOnly: true,
    handlesCount: true
  },
  exchangePointAndMark: {
    exec: function exec(handler, args) {
      var view = handler.view;
      var selection = view.state.selection;
      var isEmpty = !selection.ranges.some(function (r) {
        return r.from != r.to;
      });

      if (!args.count && !isEmpty) {
        // just invert selection
        var newRanges = selection.ranges.map(function (x) {
          return _state.EditorSelection.range(x.head, x.anchor);
        });
        view.dispatch({
          selection: _state.EditorSelection.create(newRanges, selection.mainIndex)
        });
        return;
      }

      var markRing = handler.$emacsMarkRing;
      var lastMark = markRing[markRing.length - 1];
      if (!lastMark) return;

      if (args.count) {
        // replace mark and point
        markRing[markRing.length - 1] = handler.selectionToEmacsMark();
        handler.clearSelection();
        var newRanges = lastMark.map(function (x) {
          return _state.EditorSelection.range(x, x);
        });
        view.dispatch({
          selection: _state.EditorSelection.create(newRanges, selection.mainIndex)
        });
      } else {
        // create selection to last mark
        var n = Math.min(lastMark.length, selection.ranges.length);
        newRanges = [];

        for (var i = 0; i < n; i++) {
          newRanges.push(_state.EditorSelection.range(selection.ranges[i].head, lastMark[i]));
        }
      }
    },
    readOnly: true,
    handlesCount: true
  },
  killWord: {
    exec: function exec(handler, dir) {
      var view = handler.view;
      var selection = view.state.selection;
      var newRanges = selection.ranges.map(function (x) {
        return _state.EditorSelection.range(x.head, x.head);
      });
      view.dispatch({
        selection: _state.EditorSelection.create(newRanges, selection.mainIndex)
      });
      if (dir == "left") commands.selectGroupBackward(view); else commands.selectGroupForward(view);
      selection = view.state.selection;
      selection.ranges.forEach(function (r) {
        var text = view.state.sliceDoc(r.from, r.to);
        killRing.add(text);
      });
      view.dispatch(view.state.replaceSelection(""));
    }
  },
  killLine: {
    exec: function exec(handler) {
      handler.pushEmacsMark(null); // don't delete the selection if it's before the cursor

      handler.clearSelection();
      var view = handler.view;
      var state = view.state;
      var text = [];
      var changes = state.selection.ranges.map(function (range) {
        var from = range.head;
        var lineObject = state.doc.lineAt(from);
        var to = lineObject.to;
        var line = state.sliceDoc(from, to); // remove EOL if only whitespace remains after the cursor

        if (/^\s*$/.test(line) && to < state.doc.length - 1) {
          to += 1;
          text.push(line + "\n");
        } else {
          text.push(line);
        }

        return {
          from: from,
          to: to,
          insert: ""
        };
      });

      if (handler.$data.lastCommand == "killLine") {
        killRing.append(text.join("\n"));
      } else {
        killRing.add(text.join("\n"));
      }

      handler.$data.lastCommand = "killLine";
      view.dispatch({
        changes: changes
      });
    },
    keepLastCommand: true
  },
  yank: {
    exec: function exec(handler) {
      handler.onPaste(killRing.get());
      handler.$data.lastCommand = "yank";
    },
    keepLastCommand: true
  },
  yankRotate: {
    exec: function exec(handler) {
      if (handler.$data.lastCommand != "yank") return;
      commands.undo(handler.view);
      handler.$emacsMarkRing.pop(); // also undo recording mark

      handler.onPaste(killRing.rotate());
      handler.$data.lastCommand = "yank";
    },
    keepLastCommand: true
  },
  killRegion: {
    exec: function exec(handler) {
      killRing.add(handler.getCopyText());
      var view = handler.view;
      view.dispatch(view.state.replaceSelection(""));
      handler.setEmacsMark(null);
    }
  },
  killRingSave: {
    exec: function exec(handler) {
      var text = handler.getCopyText();
      killRing.add(text);
      handler.clearSelection();
      navigator.clipboard.writeText(text);
    },
    readOnly: true
  },
  keyboardQuit: function keyboardQuit(handler) {
    var view = handler.view;
    var selection = view.state.selection;
    var isEmpty = !selection.ranges.some(function (r) {
      return r.from != r.to;
    });

    if (selection.ranges.length > 1 && !isEmpty) {
      var newRanges = selection.ranges.map(function (x) {
        return _state.EditorSelection.range(x.head, x.head);
      });
      view.dispatch({
        selection: _state.EditorSelection.create(newRanges, selection.mainIndex)
      });
    } else {
      commands.simplifySelection(handler.view);
    }

    handler.setEmacsMark(null);
    handler.$data.count = null;
  },
  focusCommandLine: function focusCommandLine(handler, arg) {
    handler.showCommandLine(arg);
  }
});
var killRing = {
  $data: [],
  add: function add(str) {
    str && this.$data.push(str);
    if (this.$data.length > 30) this.$data.shift();
  },
  append: function append(str) {
    var idx = this.$data.length - 1;
    var text = this.$data[idx] || "";
    if (str) text += str;
    if (text) this.$data[idx] = text;
  },
  get: function get(n) {
    n = n || 1;
    return this.$data.slice(this.$data.length - n, this.$data.length).reverse().join('\n');
  },
  pop: function pop() {
    if (this.$data.length > 1) this.$data.pop();
    return this.get();
  },
  rotate: function rotate() {
    var last = this.$data.pop();
    if (last) this.$data.unshift(last);
    return this.get();
  }
};

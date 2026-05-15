globalThis.codemirror_vim__cm_adapter__CodeMirror = void 0;

var _state = codemirror.State;

var _language = codemirror.Language;

var _view = codemirror.View;

var _search = codemirror.Search;

var _commands = codemirror.Commands;

function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _indexFromPos(doc, pos) {
  var ch = pos.ch;
  var lineNumber = pos.line + 1;

  if (lineNumber < 1) {
    lineNumber = 1;
    ch = 0;
  }

  if (lineNumber > doc.lines) {
    lineNumber = doc.lines;
    ch = Number.MAX_VALUE;
  }

  var line = doc.line(lineNumber);
  return Math.min(line.from + Math.max(0, ch), line.to);
}

function _posFromIndex(doc, offset) {
  var line = doc.lineAt(offset);
  return {
    line: line.number - 1,
    ch: offset - line.from
  };
}

var Pos = /*#__PURE__*/_createClass(function Pos(line, ch) {
  _classCallCheck(this, Pos);

  this.line = line;
  this.ch = ch;
});

;

function _on(emitter, type, f) {
  if (emitter.addEventListener) {
    emitter.addEventListener(type, f, false);
  } else {
    var map = emitter._handlers || (emitter._handlers = {});
    map[type] = (map[type] || []).concat(f);
  }
}

;

function _off(emitter, type, f) {
  if (emitter.removeEventListener) {
    emitter.removeEventListener(type, f, false);
  } else {
    var map = emitter._handlers,
        arr = map && map[type];

    if (arr) {
      var index = arr.indexOf(f);

      if (index > -1) {
        map[type] = arr.slice(0, index).concat(arr.slice(index + 1));
      }
    }
  }
}

function _signal(emitter, type) {
  var _emitter$_handlers;

  var handlers = (_emitter$_handlers = emitter._handlers) === null || _emitter$_handlers === void 0 ? void 0 : _emitter$_handlers[type];
  if (!handlers) return;

  for (var _len = arguments.length, args = new Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
    args[_key - 2] = arguments[_key];
  }

  for (var i = 0; i < handlers.length; ++i) {
    handlers[i].apply(handlers, args);
  }
}

function signalTo(handlers) {
  if (!handlers) return;

  for (var _len2 = arguments.length, args = new Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
    args[_key2 - 1] = arguments[_key2];
  }

  for (var i = 0; i < handlers.length; ++i) {
    handlers[i].apply(handlers, args);
  }
}

var wordChar;

try {
  wordChar = new RegExp("[\\w\\p{Alphabetic}\\p{Number}_]", "u");
} catch (_) {
  wordChar = /[\w]/;
}

// workaround for missing api for merging transactions
function dispatchChange(cm, transaction) {
  var view = cm.cm6;
  if (view.state.readOnly) return;
  var type = "input.type.compose";

  if (cm.curOp) {
    if (!cm.curOp.lastChange) type = "input.type.compose.start";
  }

  if (transaction.annotations) {
    try {
      transaction.annotations.some(function (note) {
        if (note.value == "input") note.value = type;
      });
    } catch (e) {
      console.error(e);
    }
  } else {
    transaction.userEvent = type;
  }

  return view.dispatch(transaction);
}

function runHistoryCommand(cm, revert) {
  var _cm$curOp;

  if (cm.curOp) {
    cm.curOp.$changeStart = undefined;
  }

  (revert ? _commands.undo : _commands.redo)(cm.cm6);
  var changeStartIndex = (_cm$curOp = cm.curOp) === null || _cm$curOp === void 0 ? void 0 : _cm$curOp.$changeStart; // vim mode expects the changed text to be either selected or cursor placed at the start

  if (changeStartIndex != null) {
    cm.cm6.dispatch({
      selection: {
        anchor: changeStartIndex
      }
    });
  }
}

var keys = {
  Left: function Left(cm) {
    return (0, _view.runScopeHandlers)(cm.cm6, {
      key: "Left"
    }, "editor");
  },
  Right: function Right(cm) {
    return (0, _view.runScopeHandlers)(cm.cm6, {
      key: "Right"
    }, "editor");
  },
  Up: function Up(cm) {
    return (0, _view.runScopeHandlers)(cm.cm6, {
      key: "Up"
    }, "editor");
  },
  Down: function Down(cm) {
    return (0, _view.runScopeHandlers)(cm.cm6, {
      key: "Down"
    }, "editor");
  },
  Backspace: function Backspace(cm) {
    return (0, _view.runScopeHandlers)(cm.cm6, {
      key: "Backspace"
    }, "editor");
  },
  Delete: function Delete(cm) {
    return (0, _view.runScopeHandlers)(cm.cm6, {
      key: "Delete"
    }, "editor");
  }
};

var CodeMirror = /*#__PURE__*/function () {
  function CodeMirror(cm6) {
    _classCallCheck(this, CodeMirror);

    _defineProperty(this, "state", {});

    _defineProperty(this, "marks", Object.create(null));

    _defineProperty(this, "$mid", 0);

    _defineProperty(this, "options", {});

    _defineProperty(this, "_handlers", {});

    _defineProperty(this, "$lastChangeEndOffset", 0);

    _defineProperty(this, "virtualSelection", null);

    this.cm6 = cm6;
    this.onChange = this.onChange.bind(this);
    this.onSelectionChange = this.onSelectionChange.bind(this);
  }

  _createClass(CodeMirror, [{
    key: "openDialog",
    value: // --------------------------
    // --------------------------
    function openDialog(template, callback, options) {
      return _openDialog(this, template, callback, options);
    }
  }, {
    key: "openNotification",
    value: function openNotification(template, options) {
      return _openNotification(this, template, options);
    }
  }, {
    key: "on",
    value: function on(type, f) {
      _on(this, type, f);
    }
  }, {
    key: "off",
    value: function off(type, f) {
      _off(this, type, f);
    }
  }, {
    key: "signal",
    value: function signal(type, e, handlers) {
      _signal(this, type, e, handlers);
    }
  }, {
    key: "indexFromPos",
    value: function indexFromPos(pos) {
      return _indexFromPos(this.cm6.state.doc, pos);
    }
  }, {
    key: "posFromIndex",
    value: function posFromIndex(offset) {
      return _posFromIndex(this.cm6.state.doc, offset);
    }
  }, {
    key: "foldCode",
    value: function foldCode(pos) {
      var view = this.cm6;
      var ranges = view.state.selection.ranges;
      var doc = this.cm6.state.doc;

      var index = _indexFromPos(doc, pos);

      var tmpRanges = _state.EditorSelection.create([_state.EditorSelection.range(index, index)], 0).ranges;

      view.state.selection.ranges = tmpRanges;
      (0, _language.foldCode)(view);
      view.state.selection.ranges = ranges;
    }
  }, {
    key: "firstLine",
    value: function firstLine() {
      return 0;
    }
  }, {
    key: "lastLine",
    value: function lastLine() {
      return this.cm6.state.doc.lines - 1;
    }
  }, {
    key: "lineCount",
    value: function lineCount() {
      return this.cm6.state.doc.lines;
    }
  }, {
    key: "setCursor",
    value: function setCursor(line, ch) {
      if (_typeof(line) === 'object') {
        ch = line.ch;
        line = line.line;
      }

      var offset = _indexFromPos(this.cm6.state.doc, {
        line: line,
        ch: ch || 0
      });

      this.cm6.dispatch({
        selection: {
          anchor: offset
        }
      }, {
        scrollIntoView: !this.curOp
      });
      if (this.curOp && !this.curOp.isVimOp) this.onBeforeEndOperation();
    }
  }, {
    key: "getCursor",
    value: function getCursor(p) {
      var sel = this.cm6.state.selection.main;
      var offset = p == "head" || !p ? sel.head : p == "anchor" ? sel.anchor : p == "start" ? sel.from : p == "end" ? sel.to : null;
      if (offset == null) throw new Error("Invalid cursor type");
      return this.posFromIndex(offset);
    }
  }, {
    key: "listSelections",
    value: function listSelections() {
      var doc = this.cm6.state.doc;
      return this.cm6.state.selection.ranges.map(function (r) {
        return {
          anchor: _posFromIndex(doc, r.anchor),
          head: _posFromIndex(doc, r.head)
        };
      });
    }
  }, {
    key: "setSelections",
    value: function setSelections(p, primIndex) {
      var doc = this.cm6.state.doc;
      var ranges = p.map(function (x) {
        var head = _indexFromPos(doc, x.head);

        var anchor = _indexFromPos(doc, x.anchor); // workaround for codemirror bug, see https://github.com/replit/codemirror-vim/issues/169


        if (head == anchor) return _state.EditorSelection.cursor(head, 1);
        return _state.EditorSelection.range(anchor, head);
      });
      this.cm6.dispatch({
        selection: _state.EditorSelection.create(ranges, primIndex)
      });
    }
  }, {
    key: "setSelection",
    value: function setSelection(anchor, head, options) {
      this.setSelections([{
        anchor: anchor,
        head: head
      }], 0);

      if (options && options.origin == '*mouse') {
        this.onBeforeEndOperation();
      }
    }
  }, {
    key: "getLine",
    value: function getLine(row) {
      var doc = this.cm6.state.doc;
      if (row < 0 || row >= doc.lines) return "";
      return this.cm6.state.doc.line(row + 1).text;
    }
  }, {
    key: "getLineHandle",
    value: function getLineHandle(row) {
      if (!this.$lineHandleChanges) this.$lineHandleChanges = [];
      return {
        row: row,
        index: this.indexFromPos(new Pos(row, 0))
      };
    }
  }, {
    key: "getLineNumber",
    value: function getLineNumber(handle) {
      var updates = this.$lineHandleChanges;
      if (!updates) return null;
      var offset = handle.index;

      for (var i = 0; i < updates.length; i++) {
        offset = updates[i].changes.mapPos(offset, 1, _state.MapMode.TrackAfter);
        if (offset == null) return null;
      }

      var pos = this.posFromIndex(offset);
      return pos.ch == 0 ? pos.line : null;
    }
  }, {
    key: "releaseLineHandles",
    value: function releaseLineHandles() {
      this.$lineHandleChanges = undefined;
    }
  }, {
    key: "getRange",
    value: function getRange(s, e) {
      var doc = this.cm6.state.doc;
      return this.cm6.state.sliceDoc(_indexFromPos(doc, s), _indexFromPos(doc, e));
    }
  }, {
    key: "replaceRange",
    value: function replaceRange(text, s, e, source) {
      if (!e) e = s;
      var doc = this.cm6.state.doc;

      var from = _indexFromPos(doc, s);

      var to = _indexFromPos(doc, e);

      dispatchChange(this, {
        changes: {
          from: from,
          to: to,
          insert: text
        }
      });
    }
  }, {
    key: "replaceSelection",
    value: function replaceSelection(text) {
      dispatchChange(this, this.cm6.state.replaceSelection(text));
    }
  }, {
    key: "replaceSelections",
    value: function replaceSelections(replacements) {
      var ranges = this.cm6.state.selection.ranges;
      var changes = ranges.map(function (r, i) {
        return {
          from: r.from,
          to: r.to,
          insert: replacements[i] || ""
        };
      });
      dispatchChange(this, {
        changes: changes
      });
    }
  }, {
    key: "getSelection",
    value: function getSelection() {
      return this.getSelections().join("\n");
    }
  }, {
    key: "getSelections",
    value: function getSelections() {
      var cm = this.cm6;
      return cm.state.selection.ranges.map(function (r) {
        return cm.state.sliceDoc(r.from, r.to);
      });
    }
  }, {
    key: "somethingSelected",
    value: function somethingSelected() {
      return this.cm6.state.selection.ranges.some(function (r) {
        return !r.empty;
      });
    }
  }, {
    key: "getInputField",
    value: function getInputField() {
      return this.cm6.contentDOM;
    }
  }, {
    key: "clipPos",
    value: function clipPos(p) {
      var doc = this.cm6.state.doc;
      var ch = p.ch;
      var lineNumber = p.line + 1;

      if (lineNumber < 1) {
        lineNumber = 1;
        ch = 0;
      }

      if (lineNumber > doc.lines) {
        lineNumber = doc.lines;
        ch = Number.MAX_VALUE;
      }

      var line = doc.line(lineNumber);
      ch = Math.min(Math.max(0, ch), line.to - line.from);
      return new Pos(lineNumber - 1, ch);
    }
  }, {
    key: "getValue",
    value: function getValue() {
      return this.cm6.state.doc.toString();
    }
  }, {
    key: "setValue",
    value: function setValue(text) {
      var cm = this.cm6;
      return cm.dispatch({
        changes: {
          from: 0,
          to: cm.state.doc.length,
          insert: text
        },
        selection: _state.EditorSelection.range(0, 0)
      });
    }
  }, {
    key: "focus",
    value: function focus() {
      return this.cm6.focus();
    }
  }, {
    key: "blur",
    value: function blur() {
      return this.cm6.contentDOM.blur();
    }
  }, {
    key: "defaultTextHeight",
    value: function defaultTextHeight() {
      return this.cm6.defaultLineHeight;
    }
  }, {
    key: "findMatchingBracket",
    value: function findMatchingBracket(pos, _options) {
      var state = this.cm6.state;

      var offset = _indexFromPos(state.doc, pos);

      var m = (0, _language.matchBrackets)(state, offset + 1, -1);

      if (m && m.end) {
        return {
          to: _posFromIndex(state.doc, m.end.from)
        };
      }

      m = (0, _language.matchBrackets)(state, offset, 1);

      if (m && m.end) {
        return {
          to: _posFromIndex(state.doc, m.end.from)
        };
      }

      return {
        to: undefined
      };
    }
  }, {
    key: "scanForBracket",
    value: function scanForBracket(pos, dir, style, config) {
      return _scanForBracket(this, pos, dir, style, config);
    }
  }, {
    key: "indentLine",
    value: function indentLine(line, more) {
      // todo how to indent only one line instead of selection
      if (more) this.indentMore();else this.indentLess();
    }
  }, {
    key: "indentMore",
    value: function indentMore() {
      (0, _commands.indentMore)(this.cm6);
    }
  }, {
    key: "indentLess",
    value: function indentLess() {
      (0, _commands.indentLess)(this.cm6);
    }
  }, {
    key: "execCommand",
    value: function execCommand(name) {
      if (name == "indentAuto") CodeMirror.commands.indentAuto(this);else if (name == "goLineLeft") (0, _commands.cursorLineBoundaryBackward)(this.cm6);else if (name == "goLineRight") {
        (0, _commands.cursorLineBoundaryForward)(this.cm6);
        var state = this.cm6.state;
        var cur = state.selection.main.head;

        if (cur < state.doc.length && state.sliceDoc(cur, cur + 1) !== "\n") {
          (0, _commands.cursorCharBackward)(this.cm6);
        }
      } else console.log(name + " is not implemented");
    }
  }, {
    key: "setBookmark",
    value: function setBookmark(cursor, options) {
      var assoc = options !== null && options !== void 0 && options.insertLeft ? 1 : -1;
      var offset = this.indexFromPos(cursor);
      var bm = new Marker(this, offset, assoc);
      return bm;
    }
  }, {
    key: "addOverlay",
    value: function addOverlay(_ref) {
      var query = _ref.query;
      var cm6Query = new _search.SearchQuery({
        regexp: true,
        search: query.source,
        caseSensitive: !/i/.test(query.flags)
      });

      if (cm6Query.valid) {
        cm6Query.forVim = true;
        this.cm6Query = cm6Query;

        var effect = _search.setSearchQuery.of(cm6Query);

        this.cm6.dispatch({
          effects: effect
        });
        return cm6Query;
      }
    }
  }, {
    key: "removeOverlay",
    value: function removeOverlay(overlay) {
      if (!this.cm6Query) return;
      this.cm6Query.forVim = false;

      var effect = _search.setSearchQuery.of(this.cm6Query);

      this.cm6.dispatch({
        effects: effect
      });
    }
  }, {
    key: "getSearchCursor",
    value: function getSearchCursor(query, pos) {
      var cm = this;
      var last = null;
      var lastCM5Result = null;
      var afterEmptyMatch = false;
      if (pos.ch == undefined) pos.ch = Number.MAX_VALUE;

      var firstOffset = _indexFromPos(cm.cm6.state.doc, pos);

      var source = query.source.replace(/(\\.|{(?:\d+(?:,\d*)?|,\d+)})|[{}]/g, function (a, b) {
        if (!b) return "\\" + a;
        return b;
      });

      function rCursor(doc) {
        var from = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
        var to = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : doc.length;
        return new _search.RegExpCursor(doc, source, {
          ignoreCase: query.ignoreCase
        }, from, to);
      }

      function nextMatch(from) {
        var doc = cm.cm6.state.doc;
        if (from > doc.length) return null;
        var res = rCursor(doc, from).next();
        return res.done ? null : res.value;
      }

      var ChunkSize = 10000;

      function prevMatchInRange(from, to) {
        var doc = cm.cm6.state.doc;

        for (var size = 1;; size++) {
          var start = Math.max(from, to - size * ChunkSize);
          var cursor = rCursor(doc, start, to),
              range = null;

          while (!cursor.next().done) {
            range = cursor.value;
          }

          if (range && (start == from || range.from > start + 10)) return range;
          if (start == from) return null;
        }
      }

      return Object.defineProperties({
        findNext: function findNext() {
          return this.find(false);
        },
        findPrevious: function findPrevious() {
          return this.find(true);
        },
        find: function find(back) {
          var doc = cm.cm6.state.doc;

          if (back) {
            var endAt = last ? afterEmptyMatch ? last.to - 1 : last.from : firstOffset;
            last = prevMatchInRange(0, endAt);
          } else {
            var startFrom = last ? afterEmptyMatch ? last.to + 1 : last.to : firstOffset;
            last = nextMatch(startFrom);
          }

          lastCM5Result = last && {
            from: _posFromIndex(doc, last.from),
            to: _posFromIndex(doc, last.to),
            match: last.match
          };
          afterEmptyMatch = last ? last.from == last.to : false;
          return last && last.match;
        },
        from: function from() {
          var _lastCM5Result;

          return (_lastCM5Result = lastCM5Result) === null || _lastCM5Result === void 0 ? void 0 : _lastCM5Result.from;
        },
        to: function to() {
          var _lastCM5Result2;

          return (_lastCM5Result2 = lastCM5Result) === null || _lastCM5Result2 === void 0 ? void 0 : _lastCM5Result2.to;
        },
        replace: function replace(text) {
          if (last) {
            dispatchChange(cm, {
              changes: {
                from: last.from,
                to: last.to,
                insert: text
              }
            });
            last.to = last.from + text.length;

            if (lastCM5Result) {
              lastCM5Result.to = _posFromIndex(cm.cm6.state.doc, last.to);
            }
          }
        }
      }, {
        match: {
          get: function get() {
            return lastCM5Result && lastCM5Result.match;
          },
          configurable: true,
          enumerable: true
        }
      });
    }
  }, {
    key: "findPosV",
    value: function findPosV(start, amount, unit, goalColumn) {
      var cm6 = this.cm6;
      var doc = cm6.state.doc;
      var pixels = unit == 'page' ? cm6.dom.clientHeight : 0;

      var startOffset = _indexFromPos(doc, start);

      var range = _state.EditorSelection.cursor(startOffset, 1, undefined, goalColumn);

      var count = Math.round(Math.abs(amount));

      for (var i = 0; i < count; i++) {
        if (unit == 'page') {
          range = cm6.moveVertically(range, amount > 0, pixels);
        } else if (unit == 'line') {
          range = cm6.moveVertically(range, amount > 0);
        }
      }

      var pos = _posFromIndex(doc, range.head); // set hitside to true if there was no place to move and cursor was clipped to the edge
      // of document. Needed for gj/gk


      if (amount < 0 && range.head == 0 && goalColumn != 0 && start.line == 0 && start.ch != 0 || amount > 0 && range.head == doc.length && pos.ch != goalColumn && start.line == pos.line) {
        pos.hitSide = true;
      }

      return pos;
    }
  }, {
    key: "charCoords",
    value: function charCoords(pos, mode) {
      var rect = this.cm6.contentDOM.getBoundingClientRect();

      var offset = _indexFromPos(this.cm6.state.doc, pos);

      var coords = this.cm6.coordsAtPos(offset);
      var d = -rect.top;
      return {
        left: ((coords === null || coords === void 0 ? void 0 : coords.left) || 0) - rect.left,
        top: ((coords === null || coords === void 0 ? void 0 : coords.top) || 0) + d,
        bottom: ((coords === null || coords === void 0 ? void 0 : coords.bottom) || 0) + d
      };
    }
  }, {
    key: "coordsChar",
    value: function coordsChar(coords, mode) {
      var rect = this.cm6.contentDOM.getBoundingClientRect();
      var offset = this.cm6.posAtCoords({
        x: coords.left + rect.left,
        y: coords.top + rect.top
      }) || 0;
      return _posFromIndex(this.cm6.state.doc, offset);
    }
  }, {
    key: "getScrollInfo",
    value: function getScrollInfo() {
      var scroller = this.cm6.scrollDOM;
      return {
        left: scroller.scrollLeft,
        top: scroller.scrollTop,
        height: scroller.scrollHeight,
        width: scroller.scrollWidth,
        clientHeight: scroller.clientHeight,
        clientWidth: scroller.clientWidth
      };
    }
  }, {
    key: "scrollTo",
    value: function scrollTo(x, y) {
      if (x != null) this.cm6.scrollDOM.scrollLeft = x;
      if (y != null) this.cm6.scrollDOM.scrollTop = y;
    }
  }, {
    key: "scrollIntoView",
    value: function scrollIntoView(pos, margin) {
      if (pos) {
        var offset = this.indexFromPos(pos);
        this.cm6.dispatch({
          effects: _view.EditorView.scrollIntoView(offset)
        });
      } else {
        this.cm6.dispatch({
          scrollIntoView: true,
          userEvent: "scroll"
        });
      }
    }
  }, {
    key: "getWrapperElement",
    value: function getWrapperElement() {
      return this.cm6.dom;
    }
  }, {
    key: "getMode",
    value: // for tests
    function getMode() {
      return {
        name: this.getOption("mode")
      };
    }
  }, {
    key: "setSize",
    value: function setSize(w, h) {
      this.cm6.dom.style.width = w + 4 + "px";
      this.cm6.dom.style.height = h + "px";
      this.refresh();
    }
  }, {
    key: "refresh",
    value: function refresh() {
      this.cm6.measure();
    } // event listeners

  }, {
    key: "destroy",
    value: function destroy() {
      this.removeOverlay();
    }
  }, {
    key: "getLastEditEnd",
    value: function getLastEditEnd() {
      return this.posFromIndex(this.$lastChangeEndOffset);
    }
  }, {
    key: "onChange",
    value: function onChange(update) {
      var _this = this;

      if (this.$lineHandleChanges) {
        this.$lineHandleChanges.push(update);
      }

      for (var i in this.marks) {
        var m = this.marks[i];
        m.update(update.changes);
      }

      if (this.virtualSelection) {
        this.virtualSelection.ranges = this.virtualSelection.ranges.map(function (range) {
          return range.map(update.changes);
        });
      }

      var curOp = this.curOp = this.curOp || {};
      update.changes.iterChanges(function (fromA, toA, fromB, toB, text) {
        if (curOp.$changeStart == null || curOp.$changeStart > fromB) curOp.$changeStart = fromB;
        _this.$lastChangeEndOffset = toB;
        var change = {
          text: text.toJSON()
        };

        if (!curOp.lastChange) {
          curOp.lastChange = curOp.change = change;
        } else {
          curOp.lastChange.next = curOp.lastChange = change;
        }
      }, true);
      if (!curOp.changeHandlers) curOp.changeHandlers = this._handlers["change"] && this._handlers["change"].slice();
    }
  }, {
    key: "onSelectionChange",
    value: function onSelectionChange() {
      var curOp = this.curOp = this.curOp || {};
      if (!curOp.cursorActivityHandlers) curOp.cursorActivityHandlers = this._handlers["cursorActivity"] && this._handlers["cursorActivity"].slice();
      this.curOp.cursorActivity = true;
    }
  }, {
    key: "operation",
    value: function operation(fn, force) {
      if (!this.curOp) this.curOp = {
        $d: 0
      };
      this.curOp.$d++;

      try {
        var result = fn();
      } finally {
        if (this.curOp) {
          this.curOp.$d--;
          if (!this.curOp.$d) this.onBeforeEndOperation();
        }
      }

      return result;
    }
  }, {
    key: "onBeforeEndOperation",
    value: function onBeforeEndOperation() {
      var op = this.curOp;
      var scrollIntoView = false;

      if (op) {
        if (op.change) {
          signalTo(op.changeHandlers, this, op.change);
        }

        if (op && op.cursorActivity) {
          signalTo(op.cursorActivityHandlers, this, null);
          if (op.isVimOp) scrollIntoView = true;
        }

        this.curOp = null;
      }

      if (scrollIntoView) this.scrollIntoView();
    }
  }, {
    key: "moveH",
    value: function moveH(increment, unit) {
      if (unit == 'char') {
        // todo
        var cur = this.getCursor();
        this.setCursor(cur.line, cur.ch + increment);
      }
    }
  }, {
    key: "setOption",
    value: function setOption(name, val) {
      switch (name) {
        case "keyMap":
          this.state.keyMap = val;
          break;

        case "textwidth":
          this.state.textwidth = val;
          break;
        // TODO cm6 doesn't provide any method to reconfigure these

        case "tabSize":
        case "indentWithTabs":
          break;
      }
    }
  }, {
    key: "getOption",
    value: function getOption(name) {
      switch (name) {
        case "firstLineNumber":
          return 1;

        case "tabSize":
          return this.cm6.state.tabSize || 4;

        case "readOnly":
          return this.cm6.state.readOnly;

        case "indentWithTabs":
          return this.cm6.state.facet(_language.indentUnit) == "\t";
        // TODO

        case "indentUnit":
          return this.cm6.state.facet(_language.indentUnit).length || 2;

        case "textwidth":
          return this.state.textwidth;
        // for tests

        case "keyMap":
          return this.state.keyMap || "vim";
      }
    }
  }, {
    key: "toggleOverwrite",
    value: function toggleOverwrite(on) {
      this.state.overwrite = on;
    }
  }, {
    key: "getTokenTypeAt",
    value: function getTokenTypeAt(pos) {
      var _node$type;

      // only comment|string are needed
      var offset = this.indexFromPos(pos);
      var tree = (0, _language.ensureSyntaxTree)(this.cm6.state, offset);
      var node = tree === null || tree === void 0 ? void 0 : tree.resolve(offset);
      var type = (node === null || node === void 0 ? void 0 : (_node$type = node.type) === null || _node$type === void 0 ? void 0 : _node$type.name) || "";
      if (/comment/i.test(type)) return "comment";
      if (/string/i.test(type)) return "string";
      return "";
    }
  }, {
    key: "overWriteSelection",
    value: function overWriteSelection(text) {
      var doc = this.cm6.state.doc;
      var sel = this.cm6.state.selection;
      var ranges = sel.ranges.map(function (x) {
        if (x.empty) {
          var ch = x.to < doc.length ? doc.sliceString(x.from, x.to + 1) : "";
          if (ch && !/\n/.test(ch)) return _state.EditorSelection.range(x.from, x.to + 1);
        }

        return x;
      });
      this.cm6.dispatch({
        selection: _state.EditorSelection.create(ranges, sel.mainIndex)
      });
      this.replaceSelection(text);
    }
    /*** multiselect ****/

  }, {
    key: "isInMultiSelectMode",
    value: function isInMultiSelectMode() {
      return this.cm6.state.selection.ranges.length > 1;
    }
  }, {
    key: "virtualSelectionMode",
    value: function virtualSelectionMode() {
      return !!this.virtualSelection;
    }
  }, {
    key: "forEachSelection",
    value: function forEachSelection(command) {
      var selection = this.cm6.state.selection;
      this.virtualSelection = _state.EditorSelection.create(selection.ranges, selection.mainIndex);

      for (var i = 0; i < this.virtualSelection.ranges.length; i++) {
        var range = this.virtualSelection.ranges[i];
        if (!range) continue;
        this.cm6.dispatch({
          selection: _state.EditorSelection.create([range])
        });
        command();
        this.virtualSelection.ranges[i] = this.cm6.state.selection.ranges[0];
      }

      this.cm6.dispatch({
        selection: this.virtualSelection
      });
      this.virtualSelection = null;
    }
  }, {
    key: "hardWrap",
    value: function hardWrap(options) {
      return _hardWrap(this, options);
    }
  }]);

  return CodeMirror;
}();

globalThis.codemirror_vim__cm_adapter__CodeMirror = CodeMirror;

_defineProperty(CodeMirror, "isMac", typeof navigator != "undefined" && /Mac/.test(navigator.platform));

_defineProperty(CodeMirror, "Pos", Pos);

_defineProperty(CodeMirror, "StringStream", _language.StringStream);

_defineProperty(CodeMirror, "commands", {
  cursorCharLeft: function cursorCharLeft(cm) {
    (0, _commands.cursorCharLeft)(cm.cm6);
  },
  redo: function redo(cm) {
    runHistoryCommand(cm, false);
  },
  undo: function undo(cm) {
    runHistoryCommand(cm, true);
  },
  newlineAndIndent: function newlineAndIndent(cm) {
    (0, _commands.insertNewlineAndIndent)({
      state: cm.cm6.state,
      dispatch: function dispatch(tr) {
        return dispatchChange(cm, tr);
      }
    });
  },
  indentAuto: function indentAuto(cm) {
    (0, _commands.indentSelection)(cm.cm6);
  },
  newlineAndIndentContinueComment: undefined,
  save: undefined
});

_defineProperty(CodeMirror, "isWordChar", function (ch) {
  return wordChar.test(ch);
});

_defineProperty(CodeMirror, "keys", keys);

_defineProperty(CodeMirror, "addClass", function (el, str) {});

_defineProperty(CodeMirror, "rmClass", function (el, str) {});

_defineProperty(CodeMirror, "e_preventDefault", function (e) {
  e.preventDefault();
});

_defineProperty(CodeMirror, "e_stop", function (e) {
  var _e$stopPropagation, _e$preventDefault;

  e === null || e === void 0 ? void 0 : (_e$stopPropagation = e.stopPropagation) === null || _e$stopPropagation === void 0 ? void 0 : _e$stopPropagation.call(e);
  e === null || e === void 0 ? void 0 : (_e$preventDefault = e.preventDefault) === null || _e$preventDefault === void 0 ? void 0 : _e$preventDefault.call(e);
});

_defineProperty(CodeMirror, "lookupKey", function lookupKey(key, map, handle) {
  var result = CodeMirror.keys[key];
  if (!result && /^Arrow/.test(key)) result = CodeMirror.keys[key.slice(5)];
  if (result) handle(result);
});

_defineProperty(CodeMirror, "on", _on);

_defineProperty(CodeMirror, "off", _off);

_defineProperty(CodeMirror, "signal", _signal);

_defineProperty(CodeMirror, "findMatchingTag", findMatchingTag);

_defineProperty(CodeMirror, "findEnclosingTag", findEnclosingTag);

_defineProperty(CodeMirror, "keyName", undefined);

;

/************* dialog *************/
function dialogDiv(cm, template, bottom) {
  var dialog = document.createElement("div");
  dialog.appendChild(template);
  return dialog;
}

function closeNotification(cm, newVal) {
  if (cm.state.currentNotificationClose) cm.state.currentNotificationClose();
  cm.state.currentNotificationClose = newVal;
}

function _openNotification(cm, template, options) {
  closeNotification(cm, close);
  var dialog = dialogDiv(cm, template, options && options.bottom);
  var closed = false;
  var doneTimer;
  var duration = options && typeof options.duration !== "undefined" ? options.duration : 5000;

  function close() {
    if (closed) return;
    closed = true;
    clearTimeout(doneTimer);
    dialog.remove();
    hideDialog(cm, dialog);
  }

  dialog.onclick = function (e) {
    e.preventDefault();
    close();
  };

  showDialog(cm, dialog);
  if (duration) doneTimer = setTimeout(close, duration);
  return close;
}

function showDialog(cm, dialog) {
  var oldDialog = cm.state.dialog;
  cm.state.dialog = dialog;
  dialog.style.flex = "1";

  if (dialog && oldDialog !== dialog) {
    if (oldDialog && oldDialog.contains(document.activeElement)) cm.focus();

    if (oldDialog && oldDialog.parentElement) {
      oldDialog.parentElement.replaceChild(dialog, oldDialog);
    } else if (oldDialog) {
      oldDialog.remove();
    }

    CodeMirror.signal(cm, "dialog");
  }
}

function hideDialog(cm, dialog) {
  if (cm.state.dialog == dialog) {
    cm.state.dialog = null;
    CodeMirror.signal(cm, "dialog");
  }
}

function _openDialog(me, template, callback, options) {
  if (!options) options = {};
  closeNotification(me, undefined);
  var dialog = dialogDiv(me, template, options.bottom);
  var closed = false;
  showDialog(me, dialog);

  function close(newVal) {
    if (typeof newVal == 'string') {
      inp.value = newVal;
    } else {
      if (closed) return;
      closed = true;
      hideDialog(me, dialog);
      if (!me.state.dialog) me.focus();
      if (options.onClose) options.onClose(dialog);
    }
  }

  var inp = dialog.getElementsByTagName("input")[0];

  if (inp) {
    if (options.value) {
      inp.value = options.value;
      if (options.selectValueOnOpen !== false) inp.select();
    }

    if (options.onInput) CodeMirror.on(inp, "input", function (e) {
      options.onInput(e, inp.value, close);
    });
    if (options.onKeyUp) CodeMirror.on(inp, "keyup", function (e) {
      options.onKeyUp(e, inp.value, close);
    });
    CodeMirror.on(inp, "keydown", function (e) {
      if (options && options.onKeyDown && options.onKeyDown(e, inp.value, close)) {
        return;
      }

      if (e.keyCode == 13) callback && callback(inp.value);

      if (e.keyCode == 27 || options.closeOnEnter !== false && e.keyCode == 13) {
        inp.blur();
        CodeMirror.e_stop(e);
        close();
      }
    });
    if (options.closeOnBlur !== false) CodeMirror.on(inp, "blur", function () {
      setTimeout(function () {
        if (document.activeElement === inp) return;
        close();
      });
    });
    inp.focus();
  }

  return close;
}

var matching = {
  "(": ")>",
  ")": "(<",
  "[": "]>",
  "]": "[<",
  "{": "}>",
  "}": "{<",
  "<": ">>",
  ">": "<<"
};

function bracketRegex(config) {
  return config && config.bracketRegex || /[(){}[\]]/;
}

function _scanForBracket(cm, where, dir, style, config) {
  var maxScanLen = config && config.maxScanLineLength || 10000;
  var maxScanLines = config && config.maxScanLines || 1000;
  var stack = [];
  var re = bracketRegex(config);
  var lineEnd = dir > 0 ? Math.min(where.line + maxScanLines, cm.lastLine() + 1) : Math.max(cm.firstLine() - 1, where.line - maxScanLines);

  for (var lineNo = where.line; lineNo != lineEnd; lineNo += dir) {
    var line = cm.getLine(lineNo);
    if (!line) continue;
    var pos = dir > 0 ? 0 : line.length - 1,
        end = dir > 0 ? line.length : -1;
    if (line.length > maxScanLen) continue;
    if (lineNo == where.line) pos = where.ch - (dir < 0 ? 1 : 0);

    for (; pos != end; pos += dir) {
      var ch = line.charAt(pos);

      if (re.test(ch)
      /*&& (style === undefined ||
          (cm.getTokenTypeAt(new Pos(lineNo, pos + 1)) || "") == (style || ""))*/
      ) {
        var match = matching[ch];
        if (match && match.charAt(1) == ">" == dir > 0) stack.push(ch);else if (!stack.length) return {
          pos: new Pos(lineNo, pos),
          ch: ch
        };else stack.pop();
      }
    }
  }

  return lineNo - dir == (dir > 0 ? cm.lastLine() : cm.firstLine()) ? false : null;
}

function findMatchingTag(cm, pos) {
  return null;
}

function findEnclosingTag(cm, pos) {
  var state = cm.cm6.state;
  var offset = cm.indexFromPos(pos);

  if (offset < state.doc.length) {
    var text = state.sliceDoc(offset, offset + 1);
    if (text == "<") offset++;
  }

  var tree = (0, _language.ensureSyntaxTree)(state, offset);
  var node = (tree === null || tree === void 0 ? void 0 : tree.resolve(offset)) || null;

  while (node) {
    var _node$firstChild, _node$lastChild;

    if (((_node$firstChild = node.firstChild) === null || _node$firstChild === void 0 ? void 0 : _node$firstChild.type.name) == 'OpenTag' && ((_node$lastChild = node.lastChild) === null || _node$lastChild === void 0 ? void 0 : _node$lastChild.type.name) == 'CloseTag') {
      return {
        open: convertRange(state.doc, node.firstChild),
        close: convertRange(state.doc, node.lastChild)
      };
    }

    node = node.parent;
  }
}

function convertRange(doc, cm6Range) {
  return {
    from: _posFromIndex(doc, cm6Range.from),
    to: _posFromIndex(doc, cm6Range.to)
  };
}

var Marker = /*#__PURE__*/function () {
  function Marker(cm, offset, assoc) {
    _classCallCheck(this, Marker);

    this.cm = cm;
    this.id = cm.$mid++;
    this.offset = offset;
    this.assoc = assoc;
    cm.marks[this.id] = this;
  }

  _createClass(Marker, [{
    key: "clear",
    value: function clear() {
      delete this.cm.marks[this.id];
    }
  }, {
    key: "find",
    value: function find() {
      if (this.offset == null) return null;
      return this.cm.posFromIndex(this.offset);
    }
  }, {
    key: "update",
    value: function update(change) {
      if (this.offset != null) this.offset = change.mapPos(this.offset, this.assoc, _state.MapMode.TrackDel);
    }
  }]);

  return Marker;
}();

function _hardWrap(cm, options) {
  var max = options.column || cm.getOption('textwidth') || 80;
  var allowMerge = options.allowMerge != false;
  var row = Math.min(options.from, options.to);
  var endRow = Math.max(options.from, options.to);

  while (row <= endRow) {
    var line = cm.getLine(row);

    if (line.length > max) {
      var space = findSpace(line, max, 5);

      if (space) {
        var _$exec;

        var indentation = (_$exec = /^\s*/.exec(line)) === null || _$exec === void 0 ? void 0 : _$exec[0];
        cm.replaceRange("\n" + indentation, new Pos(row, space.start), new Pos(row, space.end));
      }

      endRow++;
    } else if (allowMerge && /\S/.test(line) && row != endRow) {
      var nextLine = cm.getLine(row + 1);

      if (nextLine && /\S/.test(nextLine)) {
        var trimmedLine = line.replace(/\s+$/, "");
        var trimmedNextLine = nextLine.replace(/^\s+/, "");
        var mergedLine = trimmedLine + " " + trimmedNextLine;
        var space = findSpace(mergedLine, max, 5);

        if (space && space.start > trimmedLine.length || mergedLine.length < max) {
          cm.replaceRange(" ", new Pos(row, trimmedLine.length), new Pos(row + 1, nextLine.length - trimmedNextLine.length));
          row--;
          endRow--;
        } else if (trimmedLine.length < line.length) {
          cm.replaceRange("", new Pos(row, trimmedLine.length), new Pos(row, line.length));
        }
      }
    }

    row++;
  }

  return row;

  function findSpace(line, max, min) {
    if (line.length < max) return;
    var before = line.slice(0, max);
    var after = line.slice(max);
    var spaceAfter = /^(?:(\s+)|(\S+)(\s+))/.exec(after);
    var spaceBefore = /(?:(\s+)|(\s+)(\S+))$/.exec(before);
    var start = 0;
    var end = 0;

    if (spaceBefore && !spaceBefore[2]) {
      start = max - spaceBefore[1].length;
      end = max;
    }

    if (spaceAfter && !spaceAfter[2]) {
      if (!start) start = max;
      end = max + spaceAfter[1].length;
    }

    if (start) {
      return {
        start: start,
        end: end
      };
    }

    if (spaceBefore && spaceBefore[2] && spaceBefore.index > min) {
      return {
        start: spaceBefore.index,
        end: spaceBefore.index + spaceBefore[2].length
      };
    }

    if (spaceAfter && spaceAfter[2]) {
      start = max + spaceAfter[2].length;
      return {
        start: start,
        end: start + spaceAfter[3].length
      };
    }
  }
}



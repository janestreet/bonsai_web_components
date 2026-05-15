globalThis.codemirror_vim__vim = vim;

var _cm_adapter = { Codemirror : globalThis.codemirror_vim__cm_adapter__CodeMirror }

var _blockCursor = globalThis.codemirror_vim__block_cursor;

var _state = codemirror.State;

var _view = codemirror.View;

var _search = codemirror.Search;

function _createForOfIteratorHelper(o, allowArrayLike) { var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"]; if (!it) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = it.call(o); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it["return"] != null) it["return"](); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var FIREFOX_LINUX = typeof navigator != "undefined" && /linux/i.test(navigator.platform) && / Gecko\/\d+/.exec(navigator.userAgent);
var Vim = (0, globalThis.codemirror_vim__initVim)(globalThis.codemirror_vim__cm_adapter__CodeMirror);
globalThis.codemirror_vim__Vim = Vim;
var HighlightMargin = 250;

var vimStyle = _view.EditorView.baseTheme({
  ".cm-vimMode .cm-cursorLayer:not(.cm-vimCursorLayer)": {
    display: "none"
  },
  ".cm-vim-panel": {
    padding: "0px 10px",
    fontFamily: "monospace",
    minHeight: "1.3em",
    display: 'flex'
  },
  ".cm-vim-panel input": {
    border: "none",
    outline: "none",
    backgroundColor: "inherit"
  },
  "&light .cm-searchMatch": {
    backgroundColor: "#ffff0054"
  },
  "&dark .cm-searchMatch": {
    backgroundColor: "#00ffff8a"
  }
});

var vimPlugin = function(use_system_clipboard) { return _view.ViewPlugin.fromClass( /*#__PURE__*/function () {
  function _class2(view) {
    var _this = this;

    _classCallCheck(this, _class2);

    _defineProperty(this, "status", "");

    _defineProperty(this, "query", null);

    _defineProperty(this, "decorations", _view.Decoration.none);

    _defineProperty(this, "waitForCopy", false);

    _defineProperty(this, "lastKeydown", '');

    _defineProperty(this, "useNextTextInput", false);

    _defineProperty(this, "compositionText", '');

    this.view = view;
    var cm = this.cm = new globalThis.codemirror_vim__cm_adapter__CodeMirror(view);
    Vim.enterVimMode(this.cm, use_system_clipboard);
    this.view.cm = this.cm;
    this.cm.state.vimPlugin = this;
    this.blockCursor = new _blockCursor.BlockCursorPlugin(view, cm);
    this.updateClass();
    this.cm.on("vim-command-done", function () {
      if (cm.state.vim) cm.state.vim.status = "";

      _this.blockCursor.scheduleRedraw();

      _this.updateStatus();
    });
    this.cm.on("vim-mode-change", function (e) {
      if (!cm.state.vim) return;
      cm.state.vim.mode = e.mode;

      if (e.subMode) {
        cm.state.vim.mode += " block";
      }

      cm.state.vim.status = "";

      _this.blockCursor.scheduleRedraw();

      _this.updateClass();

      _this.updateStatus();
    });
    this.cm.on("dialog", function () {
      if (_this.cm.state.statusbar) {
        _this.updateStatus();
      } else {
        view.dispatch({
          effects: showVimPanel.of(!!_this.cm.state.dialog)
        });
      }
    });
    this.dom = document.createElement("span");
    this.spacer = document.createElement("span");
    this.spacer.style.flex = "1";
    this.statusButton = document.createElement("span");

    this.statusButton.onclick = function (e) {
      Vim.handleKey(_this.cm, "<Esc>", "user");

      _this.cm.focus();
    };

    this.statusButton.style.cssText = "cursor: pointer";
  }

  _createClass(_class2, [{
    key: "update",
    value: function update(_update) {
      if ((_update.viewportChanged || _update.docChanged) && this.query) {
        this.highlight(this.query);
      }

      if (_update.docChanged) {
        this.cm.onChange(_update);
      }

      if (_update.selectionSet) {
        this.cm.onSelectionChange();
      }

      if (_update.viewportChanged) {// scroll
      }

      if (this.cm.curOp && !this.cm.curOp.isVimOp) {
        this.cm.onBeforeEndOperation();
      }

      if (_update.transactions) {
        var _iterator = _createForOfIteratorHelper(_update.transactions),
            _step;

        try {
          for (_iterator.s(); !(_step = _iterator.n()).done;) {
            var tr = _step.value;

            var _iterator2 = _createForOfIteratorHelper(tr.effects),
                _step2;

            try {
              for (_iterator2.s(); !(_step2 = _iterator2.n()).done;) {
                var effect = _step2.value;

                if (effect.is(_search.setSearchQuery)) {
                  var _effect$value;

                  var forVim = (_effect$value = effect.value) === null || _effect$value === void 0 ? void 0 : _effect$value.forVim;

                  if (!forVim) {
                    this.highlight(null);
                  } else {
                    var query = effect.value.create();
                    this.highlight(query);
                  }
                }
              }
            } catch (err) {
              _iterator2.e(err);
            } finally {
              _iterator2.f();
            }
          }
        } catch (err) {
          _iterator.e(err);
        } finally {
          _iterator.f();
        }
      }

      this.blockCursor.update(_update);
    }
  }, {
    key: "updateClass",
    value: function updateClass() {
      var state = this.cm.state;
      if (!state.vim || state.vim.insertMode && !state.overwrite) this.view.scrollDOM.classList.remove("cm-vimMode");else this.view.scrollDOM.classList.add("cm-vimMode");
    }
  }, {
    key: "updateStatus",
    value: function updateStatus() {
      var dom = this.cm.state.statusbar;
      var vim = this.cm.state.vim;
      if (!dom || !vim) return;
      var dialog = this.cm.state.dialog;

      if (dialog) {
        if (dialog.parentElement != dom) {
          dom.textContent = "";
          dom.appendChild(dialog);
        }
      } else {
        dom.textContent = "";
        var status = (vim.mode || "normal").toUpperCase();
        if (vim.insertModeReturn) status += "(C-O)";
        this.statusButton.textContent = "--".concat(status, "--");
        dom.appendChild(this.statusButton);
        dom.appendChild(this.spacer);
      }

      this.dom.textContent = vim.status;
      dom.appendChild(this.dom);
    }
  }, {
    key: "destroy",
    value: function destroy() {
      Vim.leaveVimMode(this.cm);
      this.updateClass();
      this.blockCursor.destroy();
      delete this.view.cm;
    }
  }, {
    key: "highlight",
    value: function highlight(query) {
      this.query = query;
      if (!query) return this.decorations = _view.Decoration.none;
      var view = this.view;
      var builder = new _state.RangeSetBuilder();

      for (var i = 0, ranges = view.visibleRanges, l = ranges.length; i < l; i++) {
        var _ranges$i = ranges[i],
            from = _ranges$i.from,
            to = _ranges$i.to;

        while (i < l - 1 && to > ranges[i + 1].from - 2 * HighlightMargin) {
          to = ranges[++i].to;
        }

        query.highlight(view.state, from, to, function (from, to) {
          builder.add(from, to, matchMark);
        });
      }

      return this.decorations = builder.finish();
    }
  }, {
    key: "handleKey",
    value: function handleKey(e, view) {
      var cm = this.cm;
      var vim = cm.state.vim;
      if (!vim) return;
      var key = Vim.vimKeyFromEvent(e, vim);

      globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'inputEvent', {
        type: "handleKey",
        key: key
      });

      if (!key) return; // clear search highlight

      if (key == "<Esc>" && !vim.insertMode && !vim.visualMode && this.query
      /* && !cm.inMultiSelectMode*/
      ) {
        var searchState = vim.searchState_;

        if (searchState) {
          cm.removeOverlay(searchState.getOverlay());
          searchState.setOverlay(null);
        }
      }

      var isCopy = key === "<C-c>" && !globalThis.codemirror_vim__cm_adapter__CodeMirror.isMac;

      if (isCopy && cm.somethingSelected()) {
        this.waitForCopy = true;
        return true;
      }

      vim.status = (vim.status || "") + key;
      var result = Vim.multiSelectHandleKey(cm, key, "user");
      vim = Vim.maybeInitVimState_(cm); // the object can change if there is an exception in handleKey
      // insert mode

      if (!result && vim.insertMode && cm.state.overwrite) {
        if (e.key && e.key.length == 1 && !/\n/.test(e.key)) {
          result = true;
          cm.overWriteSelection(e.key);
        } else if (e.key == "Backspace") {
          result = true;

          globalThis.codemirror_vim__cm_adapter__CodeMirror.commands.cursorCharLeft(cm);
        }
      }

      if (result) {
        globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'vim-keypress', key);

        e.preventDefault();
        e.stopPropagation();
        this.blockCursor.scheduleRedraw();
      }

      this.updateStatus();
      return !!result;
    }
  }]);

  return _class2;
}(), {
  eventHandlers: {
    copy: function copy(e, view) {
      var _this2 = this;

      if (!this.waitForCopy) return;
      this.waitForCopy = false;
      Promise.resolve().then(function () {
        var cm = _this2.cm;
        var vim = cm.state.vim;
        if (!vim) return;

        if (vim.insertMode) {
          cm.setSelection(cm.getCursor(), cm.getCursor());
        } else {
          cm.operation(function () {
            if (cm.curOp) cm.curOp.isVimOp = true;
            Vim.handleKey(cm, '<Esc>', 'user');
          });
        }
      });
    },
    compositionstart: function compositionstart(e, view) {
      this.useNextTextInput = true;

      globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'inputEvent', e);
    },
    compositionupdate: function compositionupdate(e, view) {
      globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'inputEvent', e);
    },
    compositionend: function compositionend(e, view) {
      globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'inputEvent', e);
    },
    keypress: function keypress(e, view) {
      globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'inputEvent', e);

      if (this.lastKeydown == "Dead") this.handleKey(e, view);
    },
    keydown: function keydown(e, view) {
      globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(this.cm, 'inputEvent', e);

      this.lastKeydown = e.key;

      if (this.lastKeydown == "Unidentified" || this.lastKeydown == "Process" || this.lastKeydown == "Dead") {
        this.useNextTextInput = true;
      } else {
        this.useNextTextInput = false;
        this.handleKey(e, view);
      }
    }
  },
  provide: function provide() {
    return [_view.EditorView.inputHandler.of(function (view, from, to, text) {
      var _cm$state, _cm$curOp;

      var cm = getCM(view);
      if (!cm) return false;
      var vim = (_cm$state = cm.state) === null || _cm$state === void 0 ? void 0 : _cm$state.vim;
      var vimPlugin = cm.state.vimPlugin;

      if (vim && !vim.insertMode && !((_cm$curOp = cm.curOp) !== null && _cm$curOp !== void 0 && _cm$curOp.isVimOp)) {
        if (text === "\0\0") {
          return true;
        }

        globalThis.codemirror_vim__cm_adapter__CodeMirror.signal(cm, 'inputEvent', {
          type: "text",
          text: text,
          from: from,
          to: to
        });

        if (text.length == 1 && vimPlugin.useNextTextInput) {
          if (vim.expectLiteralNext && view.composing) {
            vimPlugin.compositionText = text;
            return false;
          }

          if (vimPlugin.compositionText) {
            var toRemove = vimPlugin.compositionText;
            vimPlugin.compositionText = '';
            var head = view.state.selection.main.head;
            var textInDoc = view.state.sliceDoc(head - toRemove.length, head);

            if (toRemove === textInDoc) {
              var pos = cm.getCursor();
              cm.replaceRange('', cm.posFromIndex(head - toRemove.length), pos);
            }
          }

          vimPlugin.handleKey({
            key: text,
            preventDefault: function preventDefault() {},
            stopPropagation: function stopPropagation() {}
          });
          forceEndComposition(view);
          return true;
        }
      }

      return false;
    })];
  },
  decorations: function decorations(v) {
    return v.decorations;
  }
})};
/**
 * removes contenteditable element and adds it back to end
 * IME composition in normal mode
 * this method works on all browsers except for Firefox on Linux
 * where we need to reset textContent of editor 
 * (which doesn't work on other browsers)
 */


function forceEndComposition(view) {
  var parent = view.scrollDOM.parentElement;
  if (!parent) return;

  if (FIREFOX_LINUX) {
    view.contentDOM.textContent = "\0\0";
    view.contentDOM.dispatchEvent(new CustomEvent("compositionend"));
    return;
  }

  var sibling = view.scrollDOM.nextSibling;
  var selection = window.getSelection();
  var savedSelection = selection && {
    anchorNode: selection.anchorNode,
    anchorOffset: selection.anchorOffset,
    focusNode: selection.focusNode,
    focusOffset: selection.focusOffset
  };
  view.scrollDOM.remove();
  parent.insertBefore(view.scrollDOM, sibling);

  try {
    if (savedSelection && selection) {
      selection.setPosition(savedSelection.anchorNode, savedSelection.anchorOffset);

      if (savedSelection.focusNode) {
        selection.extend(savedSelection.focusNode, savedSelection.focusOffset);
      }
    }
  } catch (e) {
    console.error(e);
  }

  view.focus();
  view.contentDOM.dispatchEvent(new CustomEvent("compositionend"));
}

var matchMark = _view.Decoration.mark({
  "class": "cm-searchMatch"
});

var showVimPanel = _state.StateEffect.define();

var vimPanelState = _state.StateField.define({
  create: function create() {
    return false;
  },
  update: function update(value, tr) {
    var _iterator3 = _createForOfIteratorHelper(tr.effects),
        _step3;

    try {
      for (_iterator3.s(); !(_step3 = _iterator3.n()).done;) {
        var e = _step3.value;
        if (e.is(showVimPanel)) value = e.value;
      }
    } catch (err) {
      _iterator3.e(err);
    } finally {
      _iterator3.f();
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
  var cm = view.cm;

  if (cm.state.dialog) {
    dom.appendChild(cm.state.dialog);
  }

  return {
    top: false,
    dom: dom
  };
}

function statusPanel(view) {
  var dom = document.createElement("div");
  dom.className = "cm-vim-panel";
  var cm = view.cm;
  cm.state.statusbar = dom;
  cm.state.vimPlugin.updateStatus();
  return {
    dom: dom
  };
}

function vim() {
  var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var use_system_clipboard = options.use_system_clipboard || false;
  return [vimStyle, vimPlugin(use_system_clipboard), _blockCursor.hideNativeSelection, options.status ? _view.showPanel.of(statusPanel) : vimPanelState];
}

function getCM(view) {
  return view.cm || null;
}


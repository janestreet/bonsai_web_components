"use strict";

function _typeof(obj) { "@babel/helpers - typeof"; return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) { return typeof obj; } : function (obj) { return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }, _typeof(obj); }

globalThis.codemirror_emacs__block_cursor = {}
globalThis.codemirror_emacs__block_cursor.hideNativeSelection = void 0;

var _state = codemirror.State;

var View = _interopRequireWildcard(codemirror.View);

function _getRequireWildcardCache(nodeInterop) { if (typeof WeakMap !== "function") return null; var cacheBabelInterop = new WeakMap(); var cacheNodeInterop = new WeakMap(); return (_getRequireWildcardCache = function _getRequireWildcardCache(nodeInterop) { return nodeInterop ? cacheNodeInterop : cacheBabelInterop; })(nodeInterop); }

function _interopRequireWildcard(obj, nodeInterop) { if (!nodeInterop && obj && obj.__esModule) { return obj; } if (obj === null || _typeof(obj) !== "object" && typeof obj !== "function") { return { "default": obj }; } var cache = _getRequireWildcardCache(nodeInterop); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (key !== "default" && Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj["default"] = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

function _createForOfIteratorHelper(o, allowArrayLike) { var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"]; if (!it) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() { }; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = it.call(o); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it["return"] != null) it["return"](); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); Object.defineProperty(Constructor, "prototype", { writable: false }); return Constructor; }

// backwards compatibility for old versions not supporting getDrawSelectionConfig
var getDrawSelectionConfig = View.getDrawSelectionConfig || function () {
  var defaultConfig = {
    cursorBlinkRate: 1200
  };
  return function () {
    return defaultConfig;
  };
}();

var Piece = /*#__PURE__*/function () {
  function Piece(left, top, height, fontFamily, fontSize, fontWeight, color, className, letter, partial) {
    _classCallCheck(this, Piece);

    this.left = left;
    this.top = top;
    this.height = height;
    this.fontFamily = fontFamily;
    this.fontSize = fontSize;
    this.fontWeight = fontWeight;
    this.color = color;
    this.className = className;
    this.letter = letter;
    this.partial = partial;
  }

  _createClass(Piece, [{
    key: "draw",
    value: function draw() {
      var elt = document.createElement("div");
      elt.className = this.className;
      this.adjust(elt);
      return elt;
    }
  }, {
    key: "adjust",
    value: function adjust(elt) {
      elt.style.left = this.left + "px";
      elt.style.top = this.top + "px";
      elt.style.height = this.height + "px";
      elt.style.lineHeight = this.height + "px";
      elt.style.fontFamily = this.fontFamily;
      elt.style.fontSize = this.fontSize;
      elt.style.fontWeight = this.fontWeight;
      elt.style.color = this.partial ? "transparent" : this.color;
      elt.className = this.className;
      elt.textContent = this.letter;
    }
  }, {
    key: "eq",
    value: function eq(p) {
      return this.left == p.left && this.top == p.top && this.height == p.height && this.fontFamily == p.fontFamily && this.fontSize == p.fontSize && this.fontWeight == p.fontWeight && this.color == p.color && this.className == p.className && this.letter == p.letter;
    }
  }]);

  return Piece;
}();

var BlockCursorPlugin = /*#__PURE__*/function () {
  function BlockCursorPlugin(view, em) {
    _classCallCheck(this, BlockCursorPlugin);

    _defineProperty(this, "rangePieces", []);

    _defineProperty(this, "cursors", []);

    this.view = view;
    this.em = em;
    this.measureReq = {
      read: this.readPos.bind(this),
      write: this.drawSel.bind(this)
    };
    this.cursorLayer = view.scrollDOM.appendChild(document.createElement("div"));
    this.cursorLayer.className = "cm-cursorLayer cm-vimCursorLayer";
    this.cursorLayer.setAttribute("aria-hidden", "true");
    view.requestMeasure(this.measureReq);
    this.setBlinkRate();
  }

  _createClass(BlockCursorPlugin, [{
    key: "setBlinkRate",
    value: function setBlinkRate() {
      var config = getDrawSelectionConfig(this.view.state);
      var blinkRate = config.cursorBlinkRate;
      this.cursorLayer.style.animationDuration = blinkRate + "ms";
    }
  }, {
    key: "update",
    value: function update(_update) {
      if (_update.selectionSet || _update.geometryChanged || _update.viewportChanged) {
        this.view.requestMeasure(this.measureReq);
        this.cursorLayer.style.animationName = this.cursorLayer.style.animationName == "cm-blink" ? "cm-blink2" : "cm-blink";
      }

      if (configChanged(_update)) this.setBlinkRate();
    }
  }, {
    key: "scheduleRedraw",
    value: function scheduleRedraw() {
      this.view.requestMeasure(this.measureReq);
    }
  }, {
    key: "readPos",
    value: function readPos() {
      var state = this.view.state;
      var cursors = [];

      var _iterator = _createForOfIteratorHelper(state.selection.ranges),
        _step;

      try {
        for (_iterator.s(); !(_step = _iterator.n()).done;) {
          var r = _step.value;
          var prim = r == state.selection.main;
          var piece = measureCursor(this.em, this.view, r, prim);
          if (piece) cursors.push(piece);
        }
      } catch (err) {
        _iterator.e(err);
      } finally {
        _iterator.f();
      }

      return {
        cursors: cursors
      };
    }
  }, {
    key: "drawSel",
    value: function drawSel(_ref) {
      var _this = this;

      var cursors = _ref.cursors;

      if (cursors.length != this.cursors.length || cursors.some(function (c, i) {
        return !c.eq(_this.cursors[i]);
      })) {
        var oldCursors = this.cursorLayer.children;

        if (oldCursors.length !== cursors.length) {
          this.cursorLayer.textContent = "";

          var _iterator2 = _createForOfIteratorHelper(cursors),
            _step2;

          try {
            for (_iterator2.s(); !(_step2 = _iterator2.n()).done;) {
              var c = _step2.value;
              this.cursorLayer.appendChild(c.draw());
            }
          } catch (err) {
            _iterator2.e(err);
          } finally {
            _iterator2.f();
          }
        } else {
          cursors.forEach(function (c, idx) {
            return c.adjust(oldCursors[idx]);
          });
        }

        this.cursors = cursors;
      }
    }
  }, {
    key: "destroy",
    value: function destroy() {
      this.cursorLayer.remove();
    }
  }]);

  return BlockCursorPlugin;
}();

globalThis.codemirror_emacs__block_cursor.BlockCursorPlugin = BlockCursorPlugin;

function configChanged(update) {
  return getDrawSelectionConfig(update.startState) != getDrawSelectionConfig(update.state);
}

var themeSpec = {
  ".cm-line": {
    "& ::selection": {
      backgroundColor: "transparent !important"
    },
    "&::selection": {
      backgroundColor: "transparent !important"
    },
    caretColor: "transparent !important"
  },
  ".cm-fat-cursor": {
    position: "absolute",
    background: "#ff9696",
    border: "none",
    whiteSpace: "pre"
  },
  "&:not(.cm-focused) .cm-fat-cursor": {
    background: "none",
    outline: "solid 1px #ff9696",
    color: "transparent !important"
  }
};

var hideNativeSelection = _state.Prec.highest(View.EditorView.theme(themeSpec));

globalThis.codemirror_emacs__block_cursor.hideNativeSelection = hideNativeSelection;

function getBase(view) {
  var rect = view.scrollDOM.getBoundingClientRect();
  var left = view.textDirection == View.Direction.LTR ? rect.left : rect.right - view.scrollDOM.clientWidth;
  return {
    left: left - view.scrollDOM.scrollLeft,
    top: rect.top - view.scrollDOM.scrollTop
  };
}

function measureCursor(em, view, cursor, primary) {
  var head = cursor.head;
  var fatCursor = true;
  var hCoeff = 1;

  if (em.$data.count || em.$data.keyChain) {
    hCoeff = 0.5;
  }

  if (fatCursor) {
    var _coordsForChar, _ref2;

    var _letter = head < view.state.doc.length && view.state.sliceDoc(head, head + 1);

    if (_letter && /[\uDC00-\uDFFF]/.test(_letter) && head > 1) {
      // step back if cursor is on the second half of a surrogate pair
      head--;
      _letter = view.state.sliceDoc(head, head + 1);
    }

    var pos = view.coordsAtPos(head, 1);
    if (!pos) return null;
    var base = getBase(view);
    var domAtPos = view.domAtPos(head);
    var node = domAtPos ? domAtPos.node : view.contentDOM;

    while (domAtPos && domAtPos.node instanceof HTMLElement) {
      node = domAtPos.node;
      domAtPos = {
        node: domAtPos.node.childNodes[domAtPos.offset],
        offset: 0
      };
    }

    if (!(node instanceof HTMLElement)) {
      if (!node.parentNode) return null;
      node = node.parentNode;
    }

    var style = getComputedStyle(node);
    var _left = pos.left; // TODO remove coordsAtPos when all supported versions of codemirror have coordsForChar api

    var charCoords = (_coordsForChar = (_ref2 = view).coordsForChar) === null || _coordsForChar === void 0 ? void 0 : _coordsForChar.call(_ref2, head);

    if (charCoords) {
      _left = charCoords.left;
    }

    if (!_letter || _letter == "\n" || _letter == "\r") {
      _letter = "\xa0";
    } else if (_letter == "\t") {
      _letter = "\xa0";
      var nextPos = view.coordsAtPos(head + 1, -1);

      if (nextPos) {
        _left = nextPos.left - (nextPos.left - pos.left) / parseInt(style.tabSize);
      }
    } else if (/[\uD800-\uDBFF]/.test(_letter) && head < view.state.doc.length - 1) {
      // include the second half of a surrogate pair in cursor
      _letter += view.state.sliceDoc(head + 1, head + 2);
    }

    var h = pos.bottom - pos.top;
    return new Piece(_left - base.left, pos.top - base.top + h * (1 - hCoeff), h * hCoeff, style.fontFamily, style.fontSize, style.fontWeight, style.color, primary ? "cm-fat-cursor cm-cursor-primary" : "cm-fat-cursor cm-cursor-secondary", _letter, hCoeff != 1);
  } else {
    return null;
  }
}

Codemirror VIM Mode
-------------------

These are OCaml bindings for the [codemirror-vim plugin](https://github.com/replit/codemirror-vim).

You can use it in your project by including

```ocaml
Codemirror_vim.create ()
```

in your codemirror's extension list.

Depending on the hierarchy/priority you want the keybindings to have, you may want this to
be the first extension that you register so that all of its "VIM" keybindings "win" over
the keybindings defined in other places. For example [Ctrl-A] in VIM mode increments a
number, but if you register the "basic setup" first, then [Ctrl-A] will select the entire
contents of the editor.

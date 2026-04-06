Codemirror Emacs Keybindings
----------------------------

These are OCaml bindings for the [codemirror-emacs plugin](https://github.com/replit/codemirror-emacs).

This is very similar to the codemirror-vim bindings (the setup was copied). You can use it
in your project by including

```ocaml
Codemirror_emacs.create ()
```

in your codemirror's extension list.

Depending on the hierarchy/priority you want the keybindings to have, you may want this to
be the first extension that you register so that all of its "emacs" keybindings "win" over
the keybindings defined in other places. For example [Ctrl-A] in Emacs mode goes to the
beginning of the line, but if you register the "basic setup" first, then [Ctrl-A] will
select the entire contents of the editor.

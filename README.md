# Waffle: Haskell Gopher Protocol Client

Both _gopher_ and _waffle_ in French is "gaufre:"

> The origin of the word 'gopher' is uncertain; French gaufre, meaning
> 'waffle', has been suggested, on account of the gopher tunnels resembling the
> honeycomb-like pattern of holes in a waffle…

—"Gopher," Wikipedia.

Waffle is a
[Gopher protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29) client written
in Haskell, implemented according to
[RFC 1436](https://tools.ietf.org/html/rfc1436) and
[Gopher+](https://gopher.floodgap.com/gopher/gw?a=gopher%3A%2F%2Fgopher.floodgap.com%2F0%2Fgopher%2Ftech%2Fgopherplus.txt)
technical/protocoal specifications.

## Alpha

This is a project in alpha. It is not fully functional. It is currently a demo.
This is a project that is helping me learn a few things, namely Haskell, but I
intend to make this a really good Gopher client. I got the idea to make a
Gopher client because the default `gopher` client in Ubuntu was lacking and I
wanted to provide fixes and improvements.

Special thanks to @Garmelon for mentoring me through all of this.

## Try it out!

Compile it with `stack build` and then try it out with
`stack exec hopher -- sdf.org 70 phlogs`.

### Controls

**Move the cursor and scroll:** `j` (down), `k` (up), `h` (left), `l` (right).
`page down` and `page up` moves page up and page down. `g` and `home` goes to
beginning.  `G` and `end` goes to end. 

**Get info on the current line** in menu/map mode, with `i`.

**Jump between menu links:** `p` (previous link), `n` (next link).

**Quit:** `esc`.

**When saving a file:** use arrow keys and enter to choose a directory, then
`n` to start typing a file anme to save as--use the `enter` key to accept the
filename. This will likely change.

**During index search mode:** type your query then hit `enter`.

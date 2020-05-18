# 🧇 Waffle: Haskell Gopher Protocol Client

Both _gopher_ and _waffle_ in French is "gaufre:"

> The origin of the word 'gopher' is uncertain; French gaufre, meaning
> 'waffle', has been suggested, on account of the gopher tunnels resembling the
> honeycomb-like pattern of holes in a waffle…

—"Gopher," Wikipedia.

Waffle is a [Gopher
protocol](https://en.wikipedia.org/wiki/Gopher_%28protocol%29) client with a
text interface written in Haskell, implemented according to [RFC
1436](https://tools.ietf.org/html/rfc1436) technical specification.

## Alpha

This is a project in alpha. It is not fully functional. It is currently a demo.
This is a project that is helping me learn a few things, namely Haskell, but I
intend to make this a really good Gopher client. I got the idea to make a
Gopher client because the default `gopher` client in Ubuntu was lacking and I
wanted to provide fixes and improvements.

Special thanks to @Garmelon for mentoring me through all of this.

## Try it out!

Compile it with `cabal build` and then try it out with
`cabal run waffle sdf.org 70 phlogs`.

Press `?` while using the browser for full details on using it!

## Built with

  * Cabal 3
  * Brick

## Tests

Tests are currently just `doctest`, but you can run with `cabal test`.

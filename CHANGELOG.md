# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2020-05-17

This release is preparing for uploading to Hackage. On the non-code
side we switch from Stack to Cabal and start using GitHub actions.

### Add

  * Tests, namely doctests

### Fix

  * Fix going up a directory, which in some cases could result
    in new history with the same page.

  * Fix get parent directory which in some edge cases would
    return the same path as it was given. Also fix the leading/root
    slash being omitted in all cases. Simply start using System.FilePath
    to get the parent directory in a path.

## [0.2.0] - 2020-05-09

This release is dedicated to Adrian Cochrane's
[Rhapsode](https://rhapsode.adrian.geek.nz/), as these features were requested,
which also happened to benefit this project as well.

Interpret Gopher URI item types based on their selector/path.

Bonus: make everything align nicely by simply putting item type descriptors in
gophermaps/menus at the end of the line.

### Add

The high level:

  * Item type guessing based on selector, allowing you to goto and startup URIs that require
    any RenderMode (namely: text files, menus, downloads).

The nitty-gritty:

  * `GopherNet.hs`: modularize things further by starting a networking module,
    which `writeAllBytes` was moved to.
  * ItemType type, which unifies both canonical and noncanonical item types
  * `selectorExtToItemType` for determining an item type based off of a selector's
    file extension
  * `selectorPrefixItemType` for determining an item type based off a selector's prefix
    according to RFC 4266.
  * `selectorItemType` for determining a selector's item type by first preferring 
    to use RFC 4266, but if that fails determine based off of file extension (using the two
    functions named above).
  * `counterMutator` for the new higher order version of the `writeAllBytes` function.

### Fix

The high level:

  * Goto breaking if not going to a menu/gophermap as URI destination
  * Startup args breaking if not specifying a gophermap as URI destination

### Change

The nitty-gritty:

  * All `Either GopherCanonicalItemType GopherNonCanonicalItemType` instances to
    simply use the new `ItemType` type.
  * `writeAllBytes` to be agnostic of UI stuff; made into a higher order function.

## [0.1.0] - 2020-05-03

This is the first release. It is in alpha. All the basic features required to browse Gopherspace!

See [the GitHub milestone for this release](https://github.com/hyperreal-gopher/waffle/milestone/1?closed=1)! 

# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Bug fixes regarding re-entering modes, popup notification for saving config.

### Add

  * Popup notifying of saved configuration changes for open config mode

### Fix

  * Prevent open config mode being relaunched while already in open config
    mode
  * Prevent goto mode being relaunched while already in goto mode
  * Prevent help mode from being relaunched while already in help mode

### Change

  * A bit under-the-hood, but now `esc` is the universal popup
    closer key

## [0.6.0] - 2020-07-03

Changes for static builds.

### Change

  * Replace uses of `Paths_` library to use `file-embed`, so static builds can be
    packaged/archived with just the binary and no other files
  * Cabal project config for static building

## [0.5.0] - 2020-06-28

Introduce the ability to open Gopher menu items with an external application
by associating commands with specific item types.

### Add

  * General configuration infrastructure for managing Waffle's current
    `open.ini` configuration, as well as future configurations, all residing
    in `~/.config/waffle/`
  * Default `open.ini` in `data/`
  * UI for editing which command opens an item type (ctrl+c to bring up UI,
    ctrl+s to save changes), which manipulates the corresponding config stored
    at `~/.config/waffle/open.ini`
  * Instructions in the help screen for the new open feature
  * Introduce `AnyName` which is a Brick name that encompasses *all* names as
    a "sum type"
  * A progress handler (`initOpenMode`, `progressOpen`) for downloading and
    then opening the download with the associated command
  * Open certain menu items with a command using ctrl+o
  * Representations for open config stuff, including helper functions

### Change

  * Pretty much all instances of `MyName` to use the new `AnyName` (which `MyName`
    is now a constructor of)

## [0.4.0] - 2020-05-31

This release is just a little bit of cleanup and fixes.

### Add

  * `newMenuBuffer` and `getMenu` to assist in handling `Menu` directly
    instead of passing around `GopherBrowserState`.
  * Pedantic-style warnings (`-Wall` and `-Werror`) to ghc build options
    in cabal config
  * Make it so moving a line down/up will wrap to the be beggining or end
    of the list

### Change

  * Reduce redundancy by making `updateMenuList` a top level function
  * Loosely couple `jumpNextLink`, `jumpPrevLink`, `listDrawElement`,
    and `selectedMenuLine` so they both only need `Menu` instead of
    `GopherBrowserState`.

### Fix

  * Refactor `jumpNextLink` and `jumpPrevLink` to be safe. Otherwise, moving
    to next or previous link would crash Waffle on menus (waffle: Prelude.head: empty list)
    where there were no links, such as gopher://sdf.org:70/users/raoeupb/. This also covers a
    situation where a line, for whatever reason, might not be selected, and
    thus would cause an error.

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

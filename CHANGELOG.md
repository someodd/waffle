# Changelog
All notable changes to this project will be documented in this file. This is
mostly for the enduser.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.24.0] - 2020-12-09

### Fix

  * Prevent homepage from being launched during modes where it shouldn't, like
    GotoMode and SearchMode
  * Allow GotoMode to be launched while viewing bookmarks

## [0.23.0] - 2020-11-25

### Add

  * Homepage support
  * "Ok" and "cancel" buttons for popups
  * Bookmark title in bookmark mode
  * Support for "waffle addresses" like waffle://bookmarks, waffle://help, and waffle://assocs

### Change

  * Replace popup system with Brick's Dialog system

### Fix

  * OpenConfigMode (setting associations) menu now less buggy when scrolling
    around/tabbing through the fields
  * You can no longer keep opening bookmark mode, which might not be noticeable,
    except it would keep consuming memory

## [0.22.0] - 2020-09-12

### Add

  * Saving of menus and text files in gopherspace!

## [0.21.0] - 2020-09-12

### Add

  * More default bookmarks

### Change

  * Under-the-hood: further modularize the bookmark system just a little bit

## [0.20.0] - 2020-09-03

### Change

  * Rename theme attributes to give a much more readable and intuitive theme/INI
    interface

## [0.19.0] - 2020-08-24

### Add

  * Ability to theme/stylize Waffle through a `~/.config/waffle/theme.ini`
    file!

## [0.18.0] - 2020-08-14

### Fix

  * Issue with menu find and pressing enter to follow a link found
    when it's a link to a text file, the find search bar would stay
    open in the bottom/status bar

## [0.17.0] - 2020-08-14

### Add

  * Docs/help for using jump to menu item #
  * Titlebar uses display string from links which led the user to the resource
  * Refresh feature! Press F5 to reload a text file or a menu!

### Change

  * Make it so menu item find is case-insensitive

## [0.16.0] - 2020-08-09

### Fix

  * Any bookmark which would alphabetically come before `DEFAULT`
    would cause reading the bookmarks to crash Waffle. This has
    been fixed!

## [0.15.0] - 2020-08-05

### Add

  * Emojis for `GifFile` and `ImageFile` entries in menus

## [0.14.0] - 2020-08-05

### Fix

  * Made it so add bookmark could only be activated
    if current mode is MenuMode or TextFileMode,
    avoiding weird bugs like trying to bookmark
    save file screen

## [0.13.0] - 2020-08-05

Menu item finder!

### Add

  * Help documentation for menu find
  * Menu finder: find a menu item that contains input text
  * Utilities for the status bar editor

### Fix

  * Use a new `doEventIfModes` to replace `eventDependingMode`,
    resolving some issues relating to the fact that some modes
    should only be initiated if the current mode matches a
    whitelist of modes. Tangibly, this means you will no longer
    get stuck, for instance, if you enter goto mode while saving
    a file, or in progress mode, etc.

### Change

  * Refactor a little bit of `Handle.hs`

## [0.12.0] - 2020-08-03

Bookmarks!

### Add

  * Bookmarks are stored in `bookmarks.ini`, but
    can also be edited with the TUI using `ctrl` + `b`
  * A default set of bookmarks, which will be copied if
    ~/.config/waffle/bookmarks.ini doesn't exist
  * Bookmark current page with `ctrl` + `p`
  * Help instructions for the new bookmark feature

### Change

  * Refactor utility code to be more reusable, specifically
    making `Menu`s into `MenuBuffer`s

## [0.11.0] - 2020-07-22

Reorganize and clean up the structure of the UI package (the brick app portion)...
This is all under-the-hood stuff. Refactoring.

### Change

  * Rename the UI modules to BrickApp since that more accurately describes
    their role
  * Re-organize UI/BrickApp into these parts: handle (for handling events),
    mode action (for manipulating models/types), and types (for models).
    Basically the MVC structure. There's also a utils section.

### Change

## [0.10.1] - 2020-07-17

### Fix

  * Network errors resulting in Waffle crashing. Now, instead,
    there are error popups for network errors

## [0.10.0] - 2020-07-15

### Add

  * You can now jump to a link in a menu by typing out the link #!

## [0.9.0] - 2020-07-14

Fix gopher URIs (gophertype)

In RFC 4266 the path/resource in a Gopher URI has a
"gopheritem" prefix in the format /1/foo/bar (`1`
being the gopheritem). This gopheritem prefix allows
clients to decipher the content the URI points to,
a job usually fulfilled by a menu's item type
(RFC 1436).

### Fix

  * Waffle didn't go to the right resource if the resource
    in the URI supplied had a gopheritem prefix (RFC 4266).
    Now it will both use that gopheritem prefix to decipher
    the content and go to the correct resource which excludes
    said prefix

### Add

A bit under-the-hood:

  * Implement a function which gets the "gopheritem"
    (a single character) from a selector/resource

### Change

Again, a bit under-the-hood:

  * Use these new tools for `GotoMode` and for
    the starting location provided when executing
    Waffle
  * Remove `itemTypeToRenderMode` and `guessMode`
    in favor for the new tools used with the new
    `selectorToRenderMode` function which will
    give a `RenderMode` for the supplied URI.

## [0.8.0] - 2020-07-10

Fixes, error handling, code cleanup/refactoring, and a performance enhancement.
Some under-the-hood refactoring for cleanups.

### Add

  * Popups for errors related to `GotoMode` (bad URIs, cannot connect error)

### Fix

  * No longer doing static releases due to the standalone crashing a lot for some
    reason I'm too lazy ot figure out yet.
  * Errors related to `GotoMode` which would crash Waffle, like malformed URIs,
    nonresponsive servers, etc.
  * Inability to goto URIs that include a port specification due to `read` error
  * Extreme performance issues when viewing large text files; now viewing very large

## [0.7.0] - 2020-07-04

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

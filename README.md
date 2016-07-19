A library for handling RSS/Atom feeds

The library implements most of the elements in the RSS and Atom specs and handles feed discovery, feed type
detection and feed pulling as well as feed generation.  It is also fairly easy to extend by subclassing the
appropriate classes and specializing the generic functions `%to-feed`, `make-item`, `%get-items` and
`generate-xml`.

It is, however, noticeably incomplete: particularly, not all the useful functions have been exported from the
packages they reside in.  Also, it relies on code in my fork of chronicity
<http://github.com/fiddlerwoaroof/chronicity> to handle certain wordpress feeds.

There are a couple additional features thrown in:

- server-test.lisp runs a demo web server that pulls a bunch of feeds from reddit and serves them up as a
  single page website.  It's main reason for existence is to test this library and the library I've been
  working on for web stuff <http://github.com/fiddlerwoaroof/araneus>

- alimenta-clim.lisp runs a demo graphical application that uses McCLIM
  <https://github.com/robert-strandh/mcclim> to generate an X application that can be used to navigate an RSS
  feed.

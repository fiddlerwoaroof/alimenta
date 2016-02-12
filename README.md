A library for handling RSS/Atom feeds

The library implements most of the elements in the RSS and Atom specs and handles feed discovery, feed type
detection and feed pulling as well as feed generation.  It is also fairly easy to extend by subclassing the
appropriate classes and specializing the generic functions `%to-feed`, `make-item`, `%get-items` and
`%generate-xml`.

It is, however, noticeably incomplete: particularly, not all the useful functions have been exported from the
packages they reside in.  Also, it relies on code in my fork of chronicity <http://github.com/fiddlerwoaroof/chronicity>
to handle certain wordpress feeds.

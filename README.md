Common Lisp client for [MATCH](https://github.com/pavlovai/match) (reverse image search). Allows you to reverse image search your local files from anywhere!

Features
========

* Upload images from your computer with `(update)`. MATCH-CLIENT will search for images in the specified directories and add any new images to the MATCH server.
* Search for images using `(match url)`

Installation
============

1. Set up [MATCH](https://github.com/pavlovai/match) server.
2. Copy `settings.lisp.template` to `settings.lisp` and change the settings appropriately.
3. Run `(update)` to upload all the images in `*root-dirs*` to the server.
4. Next time you run `(update)` only files that changed will be updated. Deleted files will also be cleared from the server.

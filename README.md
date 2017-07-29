Common Lisp client for [MATCH](https://github.com/pavlovai/match) (reverse image search). Allows you to reverse image search your local files from anywhere!

[Read the technical blog post for more info about this thing](http://readevalprint.tumblr.com/post/163569150438/your-personal-diy-image-search).

Features
========

* Upload images from your computer with `(update)`. MATCH-CLIENT will search for images in the specified directories and add any new images to the MATCH server.
* Search for images using `(match url)`

Installation
============

1. match-client only works with [SBCL](http://www.sbcl.org).
1. All the requirements except [cl-graphicsmagick](https://github.com/muyinliu/cl-graphicsmagick) are installable through quicklisp. Install [graphicsmagick](http://www.graphicsmagick.org/) libraries and cl-graphicsmagick manually. Note that there are issues with using graphicsmagick on Linux, see below.
1. Set up [MATCH](https://github.com/pavlovai/match) server.
2. Copy `settings.lisp.template` to `settings.lisp` and change the settings appropriately.
3. Run `(update)` to upload all the images in `*root-dirs*` to the server.
4. Next time you run `(update)` only files that changed will be updated. Deleted files will also be cleared from the server.

Known issues
============

There might be a problem with calling graphicsmagick on Linux from SBCL with the latter crashing right into a debugger (ldb). I recommend installing graphicsmagick from source, and comment out the following line in `magick.c`.

```
InitializeMagickSignalHandlers(); /* Signal handlers */
```

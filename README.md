# raw-bindings-sdl2-image

## Overview

Low level (raw) Common Lisp bindings to the [SDL2_image](https://www.libsdl.org/projects/SDL_image/) library.

## Dependencies

Lisp:

* [cffi](https://common-lisp.net/project/cffi/)
* [defpackage-plus](https://github.com/rpav/defpackage-plus)
* [raw-bindings-sdl2](https://github.com/Zulu-Inuoe/raw-bindings-sdl2)

Runtime:

* [SDL2](https://www.libsdl.org/) version 2.0.6 runtime libraries
* [SDL2_image](https://www.libsdl.org/projects/SDL_image/) runtime library.

## Notes

This is a low level library meant to have minimal fluff.

In that vein, keep the following in mind:

* No returned pointers are set up for [finalization](https://common-lisp.net/project/trivial-garbage/).
    * If you decide to implement such a scheme keep in mind that SDL objects are thread sensitive, and must be cleaned up in the same thread they were created. Also that finalization occurs in an unknown thread.
* No error codes are checked for you and coerced to errors.
* No integer values are coerced to booleans, not even sdl-bool.

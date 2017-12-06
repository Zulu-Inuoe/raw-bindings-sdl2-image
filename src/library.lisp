;;;raw-bindings-sdl2-image - FFI bindings to SDL2_image
;;;Written in 2017 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:raw-bindings-sdl2-image)

(cffi:define-foreign-library libsdl2-image
  (:darwin (:or (:framework "SDL2_image") (:default "libSDL2_image")))
  (:unix "libSDL2_image.so")
  (:windows "SDL2_image.dll")
  (t (:default "libSDL2_image")))

(cffi:use-foreign-library libsdl2-image)

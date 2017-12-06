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

(defimageconstant +sdl-image-major-version+ 2)
(defimageconstant +sdl-image-minor-version+ 0)
(defimageconstant +sdl-image-patchlevel+ 2)

(defimage-lispfun sdl-image-version (x)
  (cffi:with-foreign-slots ((major minor patch) x sdl-version)
    (setf major +sdl-image-major-version+
          minor +sdl-image-minor-version+
          patch +sdl-image-patchlevel+))
  (values))

(defimageconstant +sdl-image-compiledversion+
    (sdl-versionnum +sdl-image-major-version+ +sdl-image-minor-version+ +sdl-image-patchlevel+))

(defimage-lispfun sdl-image-version-atleast (x y z)
  (if (>= +sdl-image-compiledversion+ (sdl-versionnum x y z))
      1
      0))

(defimagefun ("IMG_Linked_Version" img-linked-version) (:pointer sdl-version))

(defimageenum img-init-flags
  (+img-init-jpg+ #x00000001)
  (+img-init-png+ #x00000002)
  (+img-init-tif+ #x00000004)
  (+img-init-webp+ #x00000008))

(defimagefun ("IMG_Init" img-init) :int
  (flags :int))

(defimagefun ("IMG_Quit" img-quit) :void)

(defimagefun ("IMG_LoadTyped_RW" img-load-typed-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops))
  (freesrc :int)
  (type (:string :encoding :utf-8)))

(defimagefun ("IMG_Load" img-load) (:pointer sdl-surface)
  (file (:string :encoding :utf-8)))

(defimagefun ("IMG_Load_RW" img-load-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops))
  (freesrc :int))

(defimagefun ("IMG_LoadTexture" img-load-texture) (:pointer sdl-texture)
  (renderer (:pointer sdl-renderer))
  (file (:string :encoding :utf-8)))

(defimagefun ("IMG_LoadTexture_RW" img-load-texture-rw) (:pointer sdl-texture)
  (renderer (:pointer sdl-renderer))
  (src (:pointer sdl-rwops))
  (freesrc :int))

(defimagefun ("IMG_LoadTextureTyped_RW" img-load-texture-typed-rw) (:pointer sdl-texture)
  (renderer (:pointer sdl-renderer))
  (src (:pointer sdl-rwops))
  (freesrc :int)
  (type (:string :encoding :utf-8)))

(defimagefun ("IMG_isICO" img-is-ico) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isCUR" img-is-cur) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isBMP" img-is-bmp) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isGIF" img-is-gif) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isJPG" img-is-jpg) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isLBM" img-is-lbm) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isPCX" img-is-pcx) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isPNG" img-is-png) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isPNM" img-is-pnm) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isSVG" img-is-svg) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isTIF" img-is-tif) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isXCF" img-is-xcf) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isXPM" img-is-xpm) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isXV" img-is-xv) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_isWEBP" img-is-webp) :int
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadICO_RW" img-load-ico-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadCUR_RW" img-load-cur-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadBMP_RW" img-load-bmp-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadGIF_RW" img-load-gif-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadJPG_RW" img-load-jpg-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadLBM_RW" img-load-lbm-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadPCX_RW" img-load-pcx-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadPNG_RW" img-load-png-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadPNM_RW" img-load-pnm-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadSVG_RW" img-load-svg-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadTGA_RW" img-load-tga-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadTIF_RW" img-load-tif-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadXCF_RW" img-load-xcf-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadXPM_RW" img-load-xpm-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadXV_RW" img-load-xv-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_LoadWEBP_RW" img-load-webp-rw) (:pointer sdl-surface)
  (src (:pointer sdl-rwops)))

(defimagefun ("IMG_ReadXPMFromArray" img-read-xpmfrom-array) (:pointer sdl-surface)
  (xpm  (:pointer (:pointer :char))))

(defimagefun ("IMG_SavePNG" img-save-png) :int
  (surface (:pointer sdl-surface))
  (file (:string :encoding :utf-8)))

(defimagefun ("IMG_SavePNG_RW" img-save-png-rw) :int
  (surface (:pointer sdl-surface))
  (dst (:pointer sdl-rwops))
  (freedst :int))

(defimagefun ("IMG_SaveJPG" img-save-jpg) :int
  (surface (:pointer sdl-surface))
  (file (:string :encoding :utf-8))
  (quality :int))

(defimagefun ("IMG_SaveJPG_RW" img-save-jpg-rw) :int
  (surface (:pointer sdl-surface))
  (dst (:pointer sdl-rwops))
  (freedst :int)
  (quality :int))

(defmacro img-set-error (fmt &rest args)
  `(sdl-set-error ,fmt ,@args))

(export 'img-set-error)

(defimage-lispfun img-get-error ()
  (sdl-get-error))
;; tile-viewer-gl.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tile-viewer-gl)

(defparameter *tile-servers*
  '((:osm-mapnick "http://tile.openstreetmap.org/~d/~d/~d.png" :zxy)
    (:osm-cycle "http://tile.thunderforest.com/cycle/~d/~d/~d.png" :zxy)
    (:osm-bw "http://tiles.wmflabs.org/bw-mapnik/~d/~d/~d.png" :zxy)
    (:stamen-toner "http://tiles.wmflabs.org/bw-mapnik/~d/~d/~d.png" :zxy)
    (:esri-sat "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/~d/~d/~d" :zxy)
    (:esri-street "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/~d/~d/~d" :zxy)
    (:esri-top "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/~d/~d/~d" :zxy)
    (:goog-sat "https://mt1.google.com/vt/lyrs=s&x=~d&y=~d&z=~d" :xyz)
    (:goog-street "https://mt1.google.com/vt/lyrs=m&x=~d&y=~d&z=~d" :xyz)
    (:carto-posi "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/~d/~d/~d.png" :zxy)
    (:terrain "http://a.tile.stamen.com/terrain/~d/~d/~d.png" :zxy)))

(defun xyz-tile-url (type x y z)
  (destructuring-bind (type url layout) (assoc type *tile-servers*)
    (declare (ignorable type))
    (if (eq layout :xyz)
        (format nil url x y z)
        (format nil url z x y))))

(defparameter *cache-directory* "/home/jeremiah/tile-cache")

(defun fetch-tile (tile-server x y z)
  (let ((cache-file (format nil
                            "~a/~a/~a/~a/~a.png"
                            *cache-directory*
                            tile-server
                            z x y))
        (tile-url (xyz-tile-url tile-server x y z)))
    (ensure-directories-exist cache-file)
    (when (not (probe-file cache-file))
      (let ((stream (dex:get tile-url :want-stream t)))
        (format t "Downloading ~a to ~a~%" tile-url cache-file)
        (with-output-to-file (outf cache-file
                                   :if-exists :supersede
                                   :element-type (stream-element-type stream))
          (copy-stream stream outf))))
    cache-file))

(defun render-tiles-to-image (img ctx tile-server tile-x tile-y tile-z width height)
  (declare (ignorable img ctx))

  (bl:with-objects ((texture  bl:image-core)
                    (pattern  bl:pattern-core)
                    (matrix  bl:matrix2d)
                    (rect  bl:rect-i))
    (bl:lookup-error (bl:matrix2d-set-identity matrix))

    (let ((tile-size 256))
      (loop
        for x-pos below width by tile-size
        for cur-tile-x from tile-x do
          (loop
            for y-pos below height by tile-size
            for cur-tile-y from tile-y do
              (let ((texture-file-name (fetch-tile tile-server
                                                   cur-tile-x
                                                   cur-tile-y
                                                   tile-z)))
                (format t "Texture: ~a~%" texture-file-name)
                (bl:lookup-error (bl:image-init texture))
                (bl:lookup-error (bl:image-read-from-file texture
                                                          texture-file-name
                                                          (cffi:null-pointer)))

                (bl:lookup-error (bl:pattern-init-as pattern
                                                     texture
                                                     (cffi:null-pointer)
                                                     bl:+extend-mode-repeat+
                                                     matrix))
                (bl:lookup-error (bl:context-set-fill-style-object ctx
                                                                   pattern))

                (bl:lookup-error (bl:context-set-comp-op ctx
                                                         bl:+comp-op-src-over+))

                (setf (bl:rect-i.x rect) x-pos)
                (setf (bl:rect-i.y rect) y-pos)
                (setf (bl:rect-i.w rect) tile-size)
                (setf (bl:rect-i.h rect) tile-size)
                (bl:lookup-error (bl:context-fill-geometry ctx
                                                           bl:+geometry-type-recti+
                                                           rect))))))))

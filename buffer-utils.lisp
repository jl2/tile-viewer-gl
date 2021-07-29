;; buffer-utils.lisp
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

(declaim (inline allocate-gl-array free-gl-array gl-fset gl-iset gl-get to-gl-float-array to-gl-array fill-buffer))
(defun allocate-gl-array (type count)
  (declare (optimize (speed 3))
           (type fixnum count))
  (gl:alloc-gl-array type count))

(defun free-gl-array (array)
  (declare (optimize (speed 3))
           (type gl:gl-array array))
  (gl:free-gl-array array))

(defun gl-iset (array idx value)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type fixnum value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-fset (array idx value)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type single-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-dset (array idx value)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type double-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-get (array idx)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type gl:gl-array array))
  (gl:glaref array idx))

(defun to-gl-array (gl-type size arr)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3)))
  (let* ((gl-array (allocate-gl-array gl-type size)))
    (fill-buffer arr gl-array 0)
    gl-array))

(defgeneric fill-buffer (data ptr offset))
(defmethod fill-buffer ((data vec2) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (+ 2 offset))

(defmethod fill-buffer ((data vector) ptr offset)
  (loop for obj across data
        for off = offset then next-off
        for next-off = (fill-buffer obj ptr off)
        finally (return next-off)))

(defmethod fill-buffer ((data vec3) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (+ 3 offset))

(defmethod fill-buffer ((data vec4) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (gl-fset ptr (+ 3 offset) (vw data))
  (+ 4 offset))

(defmethod fill-buffer ((data mat3) ptr offset)
  (loop for off from 0
        for d across (marr (mtranspose data))
        do
           (gl-fset ptr (+ off offset) d)
        finally (return (+ off offset))))

(defmethod fill-buffer ((data mat4) ptr offset)
  (loop for off from 0
        for d across (marr (mtranspose data))
        do
           (gl-fset ptr (+ off offset) d)
        finally (return (+ off offset))))

(defmethod fill-buffer ((data list) ptr offset)
  (loop for obj in data
        for off = offset then next-off
        for next-off = (fill-buffer obj ptr off)
        finally (return next-off)))

(defmethod fill-buffer ((data integer) ptr offset)
  (gl-iset ptr offset data)
  (1+ offset))

(defmethod fill-buffer ((data real) ptr offset)
  (gl-fset ptr offset data)
  (1+ offset))

(defparameter *glsl-type-db*
  ;; gl-type, component type,  component count, byte size
  `(
    (:float   :float   1  ,(cffi:foreign-type-size :float) 1)
    (:int     :int     1  ,(cffi:foreign-type-size :int) 1)
    (:double  :double  1  ,(cffi:foreign-type-size :double) 1)

    (:vec3    :float   3  ,(* 3 (cffi:foreign-type-size :float)) 1)
    (:vec4    :float   4  ,(* 4 (cffi:foreign-type-size :float)) 1)
    (:vec2    :float   2  ,(* 2 (cffi:foreign-type-size :float)) 1)

    (:mat4    :float   16  ,(* 4 4 (cffi:foreign-type-size :float)) 4)
    (:mat3    :float   9  ,(* 3 3 (cffi:foreign-type-size :float)) 4)

    (:dvec3   :double  3  ,(* 3 (cffi:foreign-type-size :double)) 2)
    (:dvec4   :double  4  ,(* 4 (cffi:foreign-type-size :double)) 2)
    (:dvec2   :double  2  ,(* 2 (cffi:foreign-type-size :double)) 2)

    (:dmat4   :double 4  ,(* 4 4 (cffi:foreign-type-size :double)) 8)
    (:dmat3   :double  9  ,(* 3 3 (cffi:foreign-type-size :double)) 8)

    ))

(defun glsl-byte-size (tname)
  (when-let ((val (assoc tname
                         *glsl-type-db*)))
    (cadddr val)))

(defun glsl-type-info (tname)
  (cdr (assoc tname *glsl-type-db*)))

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

(defparameter *want-forward-context*
  #+(or windows linux freebsd) nil
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

(defparameter *shader-dir* (asdf:system-relative-pathname :tile-viewer-gl "shaders/")
  "Directory containing tile-viewer-gl shaders.")

(defun read-shader (fname)
  (ju:read-file (merge-pathnames fname *shader-dir*)))

(define-condition shader-error (error)
  ((status :initarg :status :reader shader-compile-status)
   (info-log :initarg :info-log :reader shader-compile-info-log)))
(define-condition shader-link-error (shader-error) ())
(define-condition shader-validate-error (shader-error) ())

(defun lookup-shader-type (file-name)
  (let ((pn (pathname-name file-name)))
    (cond ((ends-with-subseq "-vertex" pn)
           :vertex-shader)
          ((ends-with-subseq "-fragment" pn)
           :fragment-shader)
          ((ends-with-subseq "-compute" pn)
           :compute-shader)
          ((ends-with-subseq "-geometry" pn)
           :geometry-shader)
          ((ends-with-subseq "-tess-eval" pn)
           :tess-evaluation-shader)
          ((ends-with-subseq "-tess-control" pn)
           :tess-control-shader))))

(defun compile-shader (file-name &optional (shader-type (lookup-shader-type file-name)))
  (let ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader (read-shader file-name))
    (gl:compile-shader shader)
    (let ((compile-result (gl:get-shader shader :compile-status)))
      (when (not (eq t compile-result))
        (error 'shader-compile-error
               :status compile-result
               :info-log (gl:get-shader-info-log shader))))
    shader))

(defun build-program (tv)

  (with-slots (vert-shader frag-shader program) tv
    (setf vert-shader (compile-shader "position-uv-vertex.glsl")
          frag-shader (compile-shader "textured-fragment.glsl")
          program (gl:create-program))

    (gl:attach-shader program vert-shader)
    (gl:attach-shader program frag-shader)

    ;; Link
    (gl:link-program program)

    ;; Check for errors and validate program
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq t status))
        (error 'shader-link-error
               :status status
               :info-log (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq t status))
        (restart-case
            (error 'shader-link-error
                   :status status
                   :info-log (gl:get-program-info-log program))
          (ignore-validation-error () t))))))


(defclass tile-viewer ()
  ((vao :initform 0 :type fixnum)
   (vbos :initform nil :type (or null list))
   (program :initform 0 :type fixnum)
   (vert-shader :initform 0 :type fixnum)
   (frag-shader :initform 0 :type fixnum)
   (textures :initform nil :type (or null list))
   (tex-type :initform :texture-2d)
   (parameters :initform '((:texture-wrap-s . :repeat)
                           (:texture-wrap-t . :repeat)
                           (:texture-base-level . 0)
                           (:texture-max-level . 8)
                           (:texture-min-filter . :linear-mipmap-linear)
                           (:texture-mag-filter . :linear)))
   (attributes :initform '(("in_position" . :vec3)
                           ("in_uv" . :vec2)))
   (stride :initform nil)
   (s-min :initarg :s-min :initform -1.0f0 :type single-float)
   (t-min :initarg :t-min :initform -1.0f0 :type single-float)
   (s-max :initarg :s-max :initform 1.0f0 :type single-float)
   (t-max :initarg :t-max :initform 1.0f0 :type single-float)
   (previous-seconds :initform 0 :type number)
   (show-fps :initform nil :type (or null t))
   (desired-fps :initform 60 :type fixnum)
   (cull-face  :initform nil :type (or null t))
   (front-face :initform :ccw)
   (wire-frame :initform nil :type (or null t))
   (background-color :initform (vec4 0.4 0.4 0.4 0.4) :type vec4)
   ))


(defun set-uniform (program name type value)
  "Bind the uniform's value in the program."

  (let ((location (gl:get-uniform-location program name)))

    ;; Only assign values to uniforms that are used by the program
    (when (>= location 0)
      (cond (
             (eq :mat4 type)
             (gl:program-uniform-matrix program location
                                        4
                                        (vector (marr4 value))
                                        t))

            ((eq :mat3 type)
             (gl:program-uniform-matrix program location
                                        3
                                        (vector (marr3 value))
                                        t))
            ((eq :dmat4 type)
             (gl:program-uniform-matrix program location
                                        4
                                        (map 'vector (lambda (v) (coerce v 'double-float)) (vector (marr4 value)))
                                        t))

            ((eq :dmat3 type)
             (gl:program-uniform-matrix program location
                                        3
                                        (map 'vector (lambda (v) (coerce v 'double-float)) (vector (marr3 value)))
                                        t))

            ((eq :int type)
             (gl:program-uniformi program location
                                  value))

            ((eq :float type)
             (gl:program-uniformf program location
                                  value))

            ((eq :vec2 type)
             (gl:program-uniformf program location
                                  (vx value)
                                  (vy value)))

            ((eq :vec3 type)
             (gl:program-uniformf program location
                                  (vx value)
                                  (vy value)
                                  (vz value)))

            ((eq :vec4 type)
             (gl:program-uniformf program location
                                  (vx value)
                                  (vy value)
                                  (vz value)
                                  (vw value)))
            (t
             (error "Don't know how to set type ~a" type))))))

(defun compute-stride (attributes)
  (loop for (nil . type) in attributes
        summing (glsl-byte-size type)))

(defun associate-attributes (tv)
  (with-slots (attributes program stride) tv
    (when (null stride)
      (setf stride (compute-stride attributes)))
    (loop
          for offset = 0 then (+ offset byte-size)
          for (name . type) in attributes
          for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
          do
             (let ((entry-attrib (gl:get-attrib-location program name)))
               (when (>= entry-attrib 0)
                 (loop for i below vec4-size
                       for attrib-idx = (+ entry-attrib i)
                       do
                          (gl:vertex-attrib-pointer attrib-idx
                                                    comp-count
                                                    comp-type
                                                    :false
                                                    stride
                                                    (+ offset (* comp-count 4 i)))
                          (gl:enable-vertex-attrib-array attrib-idx)
                       )))))
  t)




(defun gl-init (tv)
  (with-slots (vao vbos textures parameters tex-type
               attributes program
               s-min t-min s-max t-max) tv
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (setf vbos (gl:gen-buffers 2))
    (let ((vert-data (to-gl-array :float
                                  20
                                  `#(-1.0f0  1.0f0 0.0f0
                                     ,s-min ,t-min
                                     -1.0f0 -1.0f0 0.0f0
                                     ,s-min ,t-max
                                     1.0f0  1.0f0 0.0f0
                                     ,s-max ,t-min
                                     1.0f0 -1.0f0 0.0f0
                                     ,s-max ,t-max)))
          (idx-data (to-gl-array :unsigned-int
                                 6
                                 #(0 1 2 1 3 2))))
      (gl:bind-buffer :array-buffer (car vbos))
      (gl:buffer-data :array-buffer :static-draw vert-data)
      (gl:bind-buffer :element-array-buffer (cadr vbos))
      (gl:buffer-data :element-array-buffer :static-draw idx-data)
      (free-gl-array vert-data)
      (free-gl-array idx-data))

    (build-program tv)
    (associate-attributes tv)
    (set-uniform program "view_transform" :mat4 (meye 4))

    (setf textures (gl:gen-textures 1))
    (gl:bind-texture tex-type (car textures))
    (dolist (param parameters)
      (gl:tex-parameter tex-type (car param) (cdr param)))
    (gl:tex-image-2d tex-type 0 :rgba 1 1 0 :rgba :unsigned-byte #(255 120 120 255))
    (gl:generate-mipmap tex-type)))

(defun gl-cleanup (tv)
  (with-slots (vao vbos textures tex-type program vert-shader frag-shader) tv
    (when (/= 0 vao)
      (gl:bind-vertex-array vao)

      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:delete-buffers vbos)
      (setf vbos nil)

      (gl:bind-texture tex-type 0)
      (gl:delete-textures textures)
      (setf textures nil)

      (gl:detach-shader program vert-shader)
      (gl:detach-shader program frag-shader)
      (gl:delete-shader vert-shader)
      (gl:delete-shader frag-shader)
      (gl:delete-program program)
      (setf vert-shader 0)
      (setf frag-shader 0)
      (setf program 0)

      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))


(defparameter *tile-viewer* nil)

;; Keyboard callback.
(glfw:def-key-callback keyboard-handler (window key scancode action mod-keys)
  (format t "window ~a key ~a scancode ~a action ~a mod-keys ~a~%" window key scancode action mod-keys)
  (cond ((and (eq key :escape) (eq action :press))
         (glfw:set-window-should-close window)
         t)
        ((and (eq key :f) (eq action :press))
         (with-slots (show-fps) *tile-viewer*
           (setf show-fps (null show-fps)))
         t)
        (t nil)))

;; Mouse handler callback
(glfw:def-mouse-button-callback mouse-handler (window button action mod-keys)
  (let* ((cpos (glfw:get-cursor-position window)))
    (format t "cpos ~a window ~a button ~a action ~a mod-keys ~a~%" cpos window button action mod-keys)))


;; GLFW scroll handler
(glfw:def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (format t "window ~a x-scroll ~a y-scroll ~a~%" window x-scroll y-scroll)
  )

;; GLFW error callback
(glfw:def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

;; Resize event handler
(glfw:def-framebuffer-size-callback resize-handler (window width height)
  (format t "window ~a width ~a height ~a~%" window width height)
  (gl:viewport 0 0 width height)
  )

(defun handle-3d-mouse-event (window event)
  (format t "window ~a event ~a~%" window event))

(defun library-init ()
  (glfw:initialize))

(defun library-terminate ()
  (glfw:terminate))


(defun view-map (tv &key (center (3d-vectors:vec2 0.0 0.0)) )
  (library-init)
  (with-slots (background-color previous-seconds show-fps cull-face front-face wire-frame desired-fps) tv
    (let* (
           (window (glfw:create-window :title "OpenGL Viewer"
                                       :width 1000
                                       :height 1000
                                       :decorated nil
                                       :opengl-profile :opengl-core-profile
                                       :context-version-major 4
                                       :context-version-minor 0
                                       :opengl-forward-compat *want-forward-context*
                                       :samples 0
                                       :resizable t)))
      (when (null window)
        (format t "Could not create-window!")
        (error "Could not create-window!"))
      (unwind-protect
           (progn
             (gl-init tv)
             (setf *tile-viewer* tv)

             #+spacenav(sn:sn-open)
             ;; GLFW Initialization
             (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

             (glfw:set-key-callback 'keyboard-handler window)
             (glfw:set-mouse-button-callback 'mouse-handler window)
             (glfw:set-scroll-callback 'scroll-handler window)
             (glfw:set-framebuffer-size-callback 'resize-handler window)

             ;; Initialize OpenGL state
             (gl:enable :line-smooth
                        :polygon-smooth
                        :depth-test
                        )
             (gl:depth-func :lequal
                            )

             ;; The event loop
             (gl:clear-color (vx background-color)
                             (vy background-color)
                             (vz background-color)
                             (vw background-color))
             #+spacenav(sn:sensitivity 0.25d0)
             (loop
               with start-time = (glfw:get-time)
               for frame-count from 0
               until (glfw:window-should-close-p window)

               for current-seconds = (glfw:get-time)
               for elapsed-seconds = (- current-seconds previous-seconds)
               for elapsed-time = (- current-seconds start-time)

               when (and show-fps (> elapsed-seconds 0.25))
                 do
                    (format t "~,3f fps~%" (/ frame-count elapsed-seconds))
                    (setf previous-seconds current-seconds)
                    (setf frame-count 0)

                    ;; This do is important...
               do (progn
                    (glfw:swap-buffers window)
                    #+spacenav(when-let (ev (sn:poll-event))
                                (sn:remove-events :motion)
                                (handle-3d-mouse-event window ev)))
               do
                  ;; TODO: Update for next frame
                  ;; ...

                  ;; Apply viewer-wide drawing settings
                  (gl:clear :color-buffer :depth-buffer)

                  (if cull-face
                      (gl:enable :cull-face)
                      (gl:disable :cull-face))

                  (gl:front-face front-face)

                  (gl:polygon-mode :front-and-back
                                   (if wire-frame
                                       :line
                                       :fill))

                  ;; TODO: Render
                  ;; ...
                  (with-slots (vao program tex-type textures) tv
                    (gl:bind-vertex-array vao)
                    (gl:use-program program)

                    (gl:bind-texture tex-type (car textures))
                    (gl:draw-elements :triangles
                                      (gl:make-null-gl-array :unsigned-int)
                                      :count 6))
               do (glfw:poll-events)
               do (let* ((now (glfw:get-time))
                         (rem-time (- (+ current-seconds (/ 1.0 desired-fps))
                                      now)))
                    ;; (format t "Start: ~a now ~a sleep ~a~%" current-seconds Now rem-time)
                    (when (> rem-time 0)
                      (sleep rem-time))))))

      (progn
        ;; Cleanup before exit
        (format t "Cleaning up!~%")
        (setf *tile-viewer* nil)
        (gl-cleanup tv))
      #+spacenav(sn:sn-close)
      (glfw:destroy-window window)))
  (library-terminate))



(defun render-tiles-to-file (&key
                       (output-file-name "~/images/maps/tiles.bmp")
                       (tile-server :osm-bw)
                       (tile-x 27208)
                       (tile-y 49628)
                       (tile-z 17)
                       (width 1024) (height 1024))
  (bl:with-image-context (img ctx output-file-name :width width :height height :codec-name "BMP")
    (render-tiles-to-image img ctx tile-server tile-x tile-y tile-z width height)))

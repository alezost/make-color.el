;;; make-color.el --- 

;; Copyright (C) 2014 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Jan 2014
;; Version: 
;; URL: http://github.com/alezost/make-color.el
;; Keywords: color

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'color)
(require 'cl-macs)

(defgroup make-color nil
  "Options for \"make-color\" package.")

(defcustom macol-shift-step 0.02
  "Step of shifting a component of the current color.
Should be a floating number from 0.0 to 1.0."
  :type 'float
  :group 'make-color)

(defcustom macol-sample
  "Neque porro quisquam est qui dolorem ipsum quia dolor sit amet
consectetur adipisci velit.
                                         Marcus Tullius Cicero"
  "Default text sample used for probing a color.
See also `macol-sample-beg' and `macol-sample-end'."
  :type 'string
  :group 'make-color)

(defcustom macol-sample-beg 133
  "Start position of text in `macol-sample' for probing a color.
If nil, start probing text from the beginning of the sample."
  :type '(choice integer (const nil))
  :group 'make-color)

(defcustom macol-sample-end 154
  "End position of text in `macol-sample' for probing a color.
If nil, end probing text in the end of the sample."
  :type '(choice integer (const nil))
  :group 'make-color)

(defcustom macol-buffer-name "*macol*"
  "Default name of the make-color buffer."
  :type 'string
  :group 'make-color)

(defcustom macol-use-single-buffer t
  "If non-nil, use only one make-color buffer."
  :type 'boolean
  :group 'make-color)

(defcustom macol-show-color t
  "If non-nil, show message in minibuffer after changing current color."
  :type 'boolean
  :group 'make-color)

(defcustom macol-face-keyword :foreground
  "Default face parameter for colorizing."
  :type '(choice
          (const :tag "Foreground" :foreground)
          (const :tag "Background" :background))
  :group 'make-color)

(defvar macol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'macol-change-step)
    (define-key map "f" 'macol-use-foreground)
    (define-key map "d" 'macol-use-background)
    (define-key map "n" 'macol-set-probing-region)
    (define-key map "q" 'bury-buffer)
    map)
  "Keymap containing make-color commands.")


;;; Changing colors

(defun macol-+ (nums &optional overlap)
  "Return sum of float numbers from 0.0 to 1.0 from NUMS list.
Returning value is always between 0.0 and 1.0 inclusive.
If OVERLAP is non-nil and the sum exceeds the limits, oh god i
can't formulate it, look at the code."
  (let ((res (apply #'+ nums)))
    (if overlap
        (let ((frac (- res (truncate res))))
          (if (> frac 0)
              frac
            (+ 1 frac)))
      (color-clamp res))))

(cl-defun macol-shift-color-by-rgb (color &key (red 0) (green 0) (blue 0))
  "Return RGB color by modifying COLOR with RED/GREEN/BLUE values.
COLOR and returning value are lists in a form (R G B).
RED/GREEN/BLUE are numbers from 0.0 to 1.0."
  (list (macol-+ (list (car   color) red))
        (macol-+ (list (cadr  color) green))
        (macol-+ (list (caddr color) blue))))

(cl-defun macol-shift-color-by-hsl (color &key (hue 0) (saturation 0) (luminance 0))
  "Return RGB color by modifying COLOR with HUE/SATURATION/LUMINANCE values.
COLOR and returning value are lists in a form (R G B).
HUE/SATURATION/LUMINANCE are numbers from 0.0 to 1.0."
  (let ((hsl (apply 'color-rgb-to-hsl color)))
    (color-hsl-to-rgb
     (macol-+ (list (car   hsl) hue) 'overlap)
     (macol-+ (list (cadr  hsl) saturation))
     (macol-+ (list (caddr hsl) luminance)))))


;;; Shifting current color

(defvar macol-current-color nil
  "Current color - a list in a form (R G B).")

(defmacro macol-define-shift-function (name direction sign-fun color-fun key &rest components)
  "Define function `macol-DIRECTION-NAME' for shifting current color.

NAME is a unique part of function name like \"red\" or \"hue\".
Can be a string or a symbol.

DIRECTION is a string used in the name and
description (\"increase\" or \"decrease\").

SIGN-FUN is a function used for direction of shifting (`+' or `-').

COLOR-FUN is a function used for modifying current color
\(`macol-shift-color-by-rgb' or `macol-shift-color-by-hsl').

If KEY is non-nil, a binding with KEY will be defined in `macol-mode-map'.

Each component from COMPONENTS list is one of the keywords
accepted by COLOR-FUN.  Specified COMPONENTS of the current color
will be shifted by the defined function."
  (let* ((fun-name (intern (format "macol-%s-%s" direction name)))
         (fun-doc (format "%s %s component of the current color by VAL.\nIf VAL is nil, use `macol-shift-step'."
                          (capitalize direction) name))
         (fun-def
          `(defun ,fun-name (&optional val)
             ,fun-doc
             (interactive)
             (let ((color
                    (apply ,color-fun
                           (or macol-current-color '(0 0 0))
                           (cl-mapcan
                            (lambda (elt)
                              (list elt
                                    (if val
                                        (funcall ,sign-fun val)
                                      (funcall ,sign-fun macol-shift-step))))
                            ',components))))
               (macol-update-sample color)))))
    (if key
        `(progn ,fun-def (define-key macol-mode-map ,key ',fun-name))
      fun-def)))

(defmacro macol-define-shift-functions (model name key &rest components)
  "Define functions for increasing/decreasing current color.

Define 2 functions: `macol-increase-NAME' and
`macol-decrease-NAME' with optional argument VAL.

MODEL is a string \"rgb\" or \"hsl\" for choosing
`macol-shift-color-by-rgb' or `macol-shift-color-by-hsl'.

KEY should be nil or a string of one letter.  If KEY is non-nil,
bindings for defined functions will be defined in
`macol-mode-map'.  Up-case letter will be used for increasing
function, down-case letter - for decreasing function.

For other args, see `macol-define-shift-function'."
  (let ((shift-fun (intern (concat "macol-shift-color-by-" model))))
    `(progn
       (macol-define-shift-function
        ,name "increase" #'+ #',shift-fun ,(upcase key) ,@components)
       (macol-define-shift-function
        ,name "decrease" #'- #',shift-fun ,(downcase key) ,@components))))

(macol-define-shift-functions "rgb" "red"        "r" :red)
(macol-define-shift-functions "rgb" "green"      "g" :green)
(macol-define-shift-functions "rgb" "blue"       "b" :blue)
(macol-define-shift-functions "rgb" "cyan"       "c" :green :blue)
(macol-define-shift-functions "rgb" "magenta"    "m" :blue  :red)
(macol-define-shift-functions "rgb" "yellow"     "y" :red   :green)

(macol-define-shift-functions "hsl" "hue"        "h" :hue)
(macol-define-shift-functions "hsl" "saturation" "s" :saturation)
(macol-define-shift-functions "hsl" "luminance"  "l" :luminance)


;;; UI

(defvar macol-probing-region-bounds nil
  "Cons cell of start and end positions of a probing text.")

(define-derived-mode macol-mode nil "MaCol"
  "Major mode for making color.

\\{macol-mode-map}"
  (make-local-variable 'macol-current-color)
  (make-local-variable 'macol-probing-region-bounds)
  (make-local-variable 'macol-face-keyword))

(defun macol-check-mode (&optional buffer)
  "Raise error if BUFFER is not in `macol-mode'.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (eq major-mode 'macol-mode)
        (error "Current buffer should be in macol-mode"))))

(defun macol-get-buffer (&optional clear)
  "Return macol buffer.
If CLEAR is non-nil, delete its contents."
  (let ((buf (get-buffer-create
              (if macol-use-single-buffer
                  macol-buffer-name
                (generate-new-buffer-name macol-buffer-name)))))
    (when clear
      (with-current-buffer buf (erase-buffer)))
    buf))

(defun macol-update-sample (color &optional buffer)
  "Update current color and text sample in the macol BUFFER with COLOR.
COLOR should be a list in a form (R G B).
If BUFFER is nil, use current buffer."
  (macol-check-mode buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if macol-probing-region-bounds
        ;; if bounds are nils, use the whole sample
        (let ((beg (or (car macol-probing-region-bounds)
                       (point-min)))
              (end (or (cdr macol-probing-region-bounds)
                       (point-max))))
          (setq macol-current-color color
                color (apply 'color-rgb-to-hex color))
          (facemenu-add-face (list (list macol-face-keyword color))
                             beg end)
          (and macol-show-color
               (message "Current color: %s" color)))        
      (macol-set-probing-region))))

;;;###autoload
(defun macol-make-color ()
  "Begin to make color by modifying a text sample.
If region is active, use it as the sample."
  (interactive)
  ;; `sample' is the whole text yanking in macol buffer;
  ;; `region' is a part of this text used for colorizing
  (cl-multiple-value-bind (sample region)
      (if (region-active-p)
          (list (buffer-substring (region-beginning) (region-end))
                nil)
        (list macol-sample
              (cons macol-sample-beg macol-sample-end)))
    (pop-to-buffer-same-window (macol-get-buffer 'clear))
    (macol-mode)
    (insert sample)
    (goto-char (point-min))
    (setq-local macol-probing-region-bounds region)))

(defun macol-set-probing-region ()
  "Use current region for colorizing."
  (interactive)
  (macol-check-mode)
  (if (region-active-p)
      (progn (setq macol-probing-region-bounds
                   (cons (region-beginning) (region-end)))
             (deactivate-mark)
             (message "The region was set for color probing."))
    (if (y-or-n-p (concat "No active region. Use the whole sample for colorizing?"))
        (setq macol-probing-region-bounds (cons nil nil))
      ;; TODO do not hard-code "n"
      (message "Select a region for colorizing and press \"n\"."))))

(defun macol-use-foreground ()
  "Set foreground as the parameter for further changing."
  (interactive)
  (setq-local macol-face-keyword :foreground)
  (message "Foreground has been set for colorizing."))

(defun macol-use-background ()
  "Set background as the parameter for further changing."
  (interactive)
  (setq-local macol-face-keyword :background)
  (message "Background has been set for colorizing."))

(provide 'make-color)

;;; make-color.el ends here


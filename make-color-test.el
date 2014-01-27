(require 'ert)

;; We need additional functions to compare floats

(cl-defun make-color-floats-equal-p (f1 f2 &optional (precision .001))
  "Return t if floating numbers F1 and F2 are equal.
PRECISION is a float telling how many decimal places are taken.
F1 and F2 are rounded to that PRECISION."
  (= (round f1 precision)
     (round f2 precision)))

(cl-defun make-color-float-lists-equal-p (l1 l2 &optional (precision .001))
  "Return t if lists L1 and L2 contain equal floating numbers.
See `make-color-floats-equal-p' for PRECISION."
  (cl-loop for elt1 in l1
           for elt2 in l2
           unless (make-color-floats-equal-p elt1 elt2 precision) return nil
           finally return t))

(ert-deftest make-color-test-+ ()
  "Test `make-color-+'."
  (should (make-color-floats-equal-p (make-color-+ '(0.23 0.401)) .631))
  (should (make-color-floats-equal-p (make-color-+ '(0.23 0.93)) 1))
  (should (make-color-floats-equal-p (make-color-+ '(0.1 -0.8 -0.7)) 0))
  (should (make-color-floats-equal-p (make-color-+ '(0.1 -0.8 -0.7) t) .6)))

(ert-deftest make-color-test-shift-color ()
  "Test `make-color-shift-color-by-rgb' and `make-color-shift-color-by-hsl'."
  (should (make-color-float-lists-equal-p
           (make-color-shift-color-by-rgb '(0.1 0 0.71) :red -0.2 :blue 0.01)
           '(0.0 0.0 0.72)))
  (should (make-color-float-lists-equal-p
           (make-color-shift-color-by-hsl '(0 0 0)
            :hue -0.3 :luminance 0.4 :saturation 0.1)
           '(0.376 0.36 0.44))))

(ert-deftest make-color-test-get-color-from-face-spec ()
  "Test `make-color-get-color-from-face-spec'."
  (should (equal (make-color-get-color-from-face-spec
                  :foreground '(:background "blue"))
                 nil))
  (should (equal (make-color-get-color-from-face-spec
                  :background '((font-lock-doc-face (:foreground "blue"))
                                some-unknown-face (((:background "#123456")))))
                 "#123456")))


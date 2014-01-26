(require 'ert)

;; We need additional functions to compare floats

(cl-defun macol-floats-equal-p (f1 f2 &optional (precision .001))
  "Return t if floating numbers F1 and F2 are equal.
PRECISION is a float telling how many decimal places are taken.
F1 and F2 are rounded to that PRECISION."
  (= (round f1 precision)
     (round f2 precision)))

(cl-defun macol-float-lists-equal-p (l1 l2 &optional (precision .001))
  "Return t if lists L1 and L2 contain equal floating numbers.
See `macol-floats-equal-p' for PRECISION."
  (cl-loop for elt1 in l1
           for elt2 in l2
           unless (macol-floats-equal-p elt1 elt2 precision) return nil
           finally return t))

(ert-deftest macol-test-+ ()
  "Test `macol-+'."
  (should (macol-floats-equal-p (macol-+ '(0.23 0.401)) .631))
  (should (macol-floats-equal-p (macol-+ '(0.23 0.93)) 1))
  (should (macol-floats-equal-p (macol-+ '(0.1 -0.8 -0.7)) 0))
  (should (macol-floats-equal-p (macol-+ '(0.1 -0.8 -0.7) t) .6)))

(ert-deftest macol-test-get-color ()
  "Test `macol-get-color-by-rgb' and `macol-get-color-by-hsl'."
  (should (macol-float-lists-equal-p
           (macol-get-color-by-rgb :color '(0.1 0 0.71) :red -0.2 :blue 0.01)
           '(0.0 0.0 0.72)))
  (should (macol-float-lists-equal-p
           (macol-get-color-by-hsl :hue -0.3 :luminance 0.4 :saturation 0.1)
           '(0.376 0.36 0.44))))

(ert-deftest macol-test-get-color-from-face-spec ()
  "Test `macol-get-color-from-face-spec'."
  (should (equal (macol-get-color-from-face-spec
                  :foreground '(:background "blue"))
                 nil))
  (should (equal (macol-get-color-from-face-spec
                  :background '((font-lock-doc-face (:foreground "blue"))
                                some-unknown-face (((:background "#123456")))))
                 "#123456")))


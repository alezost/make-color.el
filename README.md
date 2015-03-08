## About

`Make-color` provides a convenient (IMHO) way for finding a suitable
color by updating a color of some text with keyboard.

Basically, you press <kbd>r</kbd>/<kbd>g</kbd>/<kbd>b</kbd>/... keys and
see how a probing region of text changes its foreground/background color
(red/green/blue/... components).

## Installation

### Manual

- Put `make-color.el` into any directory from `load-path` or if you
  prefer to keep it in a separate directory:

  ```lisp
  (add-to-list 'load-path "/path/to/make-color-dir")
  ```

- Add autoloads for the main functions:

  ```lisp
  (autoload 'make-color "make-color" nil t)
  (autoload 'make-color-switch-to-buffer "make-color" nil t)
  ```

### MELPA

The package can be installed from [MELPA](http://melpa.org) (with `M-x
package-install` or `M-x list-packages`).

## Usage

Shortly: `M-x make-color` and press
<kbd>r</kbd>/<kbd>R</kbd>/<kbd>g</kbd>/<kbd>G</kbd>/... keys (see
[below](#key-bindings)).

More detailed description:

1. (Optional) Select any region (it will be used as a sample, so you may
   want to select a colorful text, for example a piece of elisp code).
2. `M-x make-color` or `M-x make-color-switch-to-buffer` to make a
   buffer in `make-color-mode`.
3. (Optional if *step 1* was omitted) Select a text a color of which you
   want to change and press <kbd>n</kbd> (stands for "**n**ew probing region").
4. Press keys to change a color of the probing region (see
   [below](#key-bindings)).

MakeColor buffer is not read-only, so you can "cut and paste" (I mean
"kill and yank") text there as usual, as well as undo the changes.

### Key bindings

Key bindings in `make-color-mode` (can be explored with <kbd>C-h m</kbd>):

- <kbd>r</kbd>/<kbd>R</kbd>, <kbd>g</kbd>/<kbd>G</kbd>,
  <kbd>b</kbd>/<kbd>B</kbd> – decrease/increase red, green, blue
  components (RGB model)
- <kbd>c</kbd>/<kbd>C</kbd>, <kbd>m</kbd>/<kbd>M</kbd>,
  <kbd>y</kbd>/<kbd>Y</kbd> – decrease/increase cyan, magenta, yellow
  components (RGB model)
- <kbd>h</kbd>/<kbd>H</kbd>, <kbd>s</kbd>/<kbd>S</kbd>,
  <kbd>l</kbd>/<kbd>L</kbd> – decrease/increase hue, saturation, luminance
  (HSL model)
- <kbd>RET</kbd> – change current color (prompt for a value)
- <kbd>n</kbd> – set new probing region
- <kbd>SPC</kbd> – highlight current probing region; with arg switch to the specified region
- <kbd>N</kbd> – switch to the next probing region
- <kbd>P</kbd> – switch to the previous probing region
- <kbd>p</kbd> – change a step of components shifting
- <kbd>f</kbd>/<kbd>d</kbd>, <kbd>t</kbd> – switch between changing
  foreground/background color
- <kbd>k</kbd> – put current color into kill-ring
- <kbd>F</kbd>/<kbd>D</kbd> – put foreground/background color at point
  into kill-ring
- <kbd>u</kbd> – undo


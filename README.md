## About

`Make-color` (`macol`) provides a convenient (IMHO) way for finding a
suitable color by updating a color of some text with keyboard.

Basically, you press <kbd>r</kbd>/<kbd>g</kbd>/<kbd>b</kbd>/... keys and
see how a probing region of text changes its foreground/background color
(red/green/blue/... components).

*Note:* The package and a custom group are called `make-color`;
all functions and variables have a prefix `macol-`.

## Installation

### Manual

- Put `make-color.el` into any directory from `load-path` or if you
  prefer to keep it in a separate directory:

  ```lisp
  (add-to-list 'load-path "/path/to/make-color-dir")
  ```

- Add autoloads for the main functions:

  ```lisp
  (autoload 'macol-make-color "make-color" nil t)
  (autoload 'macol-switch-to-buffer "make-color" nil t)
  ```

### MELPA

not yet

## Usage

Shortly: `M-x macol-make-color` and press
<kbd>r</kbd>/<kbd>R</kbd>/<kbd>g</kbd>/<kbd>G</kbd>/... keys (see
[below](#key-bindings)).

More detailed description:

1. (Optional) Select any region (it will be used as a sample, so you may
   want to select a colorful text, for example a piece of elisp code).
2. `M-x macol-make-color` or `M-x macol-switch-to-buffer` to make a
   buffer in `macol-mode`.
3. (Optional if *step 1* was omitted) Select a text a color of which you
   want to change and press <kbd>n</kbd> (stands for "**n**ew probing region").
4. Press keys to change a color of the probing region (see
   [below](#key-bindings)).

Macol buffer is not read-only, so you can "cut and paste" (I mean "kill
and yank") text there as usual, as well as undo the changes.

### Key bindings

Key bindings in `macol-mode` (can be explored with <kbd>C-h m</kbd>):

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
- <kbd>p</kbd> – change a step of components shifting
- <kbd>f</kbd>/<kbd>d</kbd>, <kbd>t</kbd> – switch between changing
  foreground/background color
- <kbd>k</kbd> – put current color into kill-ring
- <kbd>u</kbd> – undo


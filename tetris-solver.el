;;; tetris-solver.el --- implementation of Tetris-Solver for Emacs

;; Copyright (C) 1997, 2001-2017 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 2.01
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tetris-solver nil
  "Play a game of Tetris-Solver."
  :prefix "tetris-solver-"
  :group 'games)

(defcustom tetris-solver-use-glyphs t
  "Non-nil means use glyphs when available."
  :group 'tetris-solver
  :type 'boolean)

(defcustom tetris-solver-use-color t
  "Non-nil means use color when available."
  :group 'tetris-solver
  :type 'boolean)

(defcustom tetris-solver-draw-border-with-glyphs t
  "Non-nil means draw a border even when using glyphs."
  :group 'tetris-solver
  :type 'boolean)

(defcustom tetris-solver-default-tick-period 0.3
  "The default time taken for a shape to drop one row."
  :group 'tetris-solver
  :type 'number)

(defcustom tetris-solver-update-speed-function
  'tetris-solver-default-update-speed-function
  "Function run whenever the Tetris-Solver score changes.
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped.
ROWS is the number of rows which have been completed.

If the return value is a number, it is used as the timer period."
  :group 'tetris-solver
  :type 'function)

(defcustom tetris-solver-mode-hook nil
  "Hook run upon starting Tetris-Solver."
  :group 'tetris-solver
  :type 'hook)

(defcustom tetris-solver-tty-colors
  ["blue" "white" "yellow" "magenta" "cyan" "green" "red"]
  "Vector of colors of the various shapes in text mode."
  :group 'tetris-solver
  :type '(vector (color :tag "Shape 1")
		 (color :tag "Shape 2")
		 (color :tag "Shape 3")
		 (color :tag "Shape 4")
		 (color :tag "Shape 5")
		 (color :tag "Shape 6")
		 (color :tag "Shape 7")))

(defcustom tetris-solver-x-colors
  [[0 0 1] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]
  "Vector of RGB colors of the various shapes."
  :group 'tetris-solver
  :type '(vector (vector :tag "Shape 1" number number number)
                 (vector :tag "Shape 2" number number number)
                 (vector :tag "Shape 3" number number number)
                 (vector :tag "Shape 4" number number number)
                 (vector :tag "Shape 5" number number number)
                 (vector :tag "Shape 6" number number number)
                 (vector :tag "Shape 7" number number number)))

(defcustom tetris-solver-buffer-name "*Tetris-Solver*"
  "Name used for Tetris-Solver buffer."
  :group 'tetris-solver
  :type 'string)

(defcustom tetris-solver-buffer-width 30
  "Width of used portion of buffer."
  :group 'tetris-solver
  :type 'number)

(defcustom tetris-solver-buffer-height 22
  "Height of used portion of buffer."
  :group 'tetris-solver
  :type 'number)

(defcustom tetris-solver-width 10
  "Width of playing area."
  :group 'tetris-solver
  :type 'number)

(defcustom tetris-solver-height 20
  "Height of playing area."
  :group 'tetris-solver
  :type 'number)

(defcustom tetris-solver-top-left-x 3
  "X position of top left of playing area."
  :group 'tetris-solver
  :type 'number)

(defcustom tetris-solver-top-left-y 1
  "Y position of top left of playing area."
  :group 'tetris-solver
  :type 'number)

(defvar tetris-solver-next-x (+ (* 2 tetris-solver-top-left-x) tetris-solver-width)
  "X position of next shape.")

(defvar tetris-solver-next-y tetris-solver-top-left-y
  "Y position of next shape.")

(defvar tetris-solver-score-x tetris-solver-next-x
  "X position of score.")

(defvar tetris-solver-score-y (+ tetris-solver-next-y 6)
  "Y position of score.")

;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp
;; pointing to a file you don't want to clobber.
(defvar tetris-solver-score-file "tetris-solver-scores"
;; anybody with a well-connected server want to host this?
;(defvar tetris-solver-score-file "/anonymous@ftp.pgt.com:/pub/cgw/tetris-solver-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-solver-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar tetris-solver-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    ;; color information is taken from tetris-solver-x-colors and tetris-solver-tty-colors
    ))

(defvar tetris-solver-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar tetris-solver-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tetris-solver-shapes
  [[[[0  0] [1  0] [0  1] [1  1]]]

   [[[0  0] [1  0] [2  0] [2  1]]
    [[1 -1] [1  0] [1  1] [0  1]]
    [[0 -1] [0  0] [1  0] [2  0]]
    [[1 -1] [2 -1] [1  0] [1  1]]]

   [[[0  0] [1  0] [2  0] [0  1]]
    [[0 -1] [1 -1] [1  0] [1  1]]
    [[2 -1] [0  0] [1  0] [2  0]]
    [[1 -1] [1  0] [1  1] [2  1]]]

   [[[0  0] [1  0] [1  1] [2  1]]
    [[1  0] [0  1] [1  1] [0  2]]]

   [[[1  0] [2  0] [0  1] [1  1]]
    [[0  0] [0  1] [1  1] [1  2]]]

   [[[1  0] [0  1] [1  1] [2  1]]
    [[1  0] [1  1] [2  1] [1  2]]
    [[0  1] [1  1] [2  1] [1  2]]
    [[1  0] [0  1] [1  1] [1  2]]]

   [[[0  0] [1  0] [2  0] [3  0]]
    [[1 -1] [1  0] [1  1] [1  2]]]]
  "Each shape is described by a vector that contains the coordinates of
each one of its four blocks.")

;;the scoring rules were taken from "xtetris-solver".  Blocks score differently
;;depending on their rotation

(defconst tetris-solver-shape-scores
  [[6] [6 7 6 7] [6 7 6 7] [6 7] [6 7] [5 5 6 5] [5 8]] )

(defconst tetris-solver-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst tetris-solver-blank 7)

(defconst tetris-solver-border 8)

(defconst tetris-solver-space 9)

(defun tetris-solver-default-update-speed-function (_shapes rows)
  (/ 20.0 (+ 50.0 rows)))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-solver-shape 0)
(defvar tetris-solver-rot 0)
(defvar tetris-solver-next-shape 0)
(defvar tetris-solver-n-shapes 0)
(defvar tetris-solver-n-rows 0)
(defvar tetris-solver-score 0)
(defvar tetris-solver-pos-x 0)
(defvar tetris-solver-pos-y 0)
(defvar tetris-solver-paused nil)

(make-variable-buffer-local 'tetris-solver-shape)
(make-variable-buffer-local 'tetris-solver-rot)
(make-variable-buffer-local 'tetris-solver-next-shape)
(make-variable-buffer-local 'tetris-solver-n-shapes)
(make-variable-buffer-local 'tetris-solver-n-rows)
(make-variable-buffer-local 'tetris-solver-score)
(make-variable-buffer-local 'tetris-solver-pos-x)
(make-variable-buffer-local 'tetris-solver-pos-y)
(make-variable-buffer-local 'tetris-solver-paused)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-solver-mode-map
  (let ((map (make-sparse-keymap 'tetris-solver-mode-map)))
    (define-key map "n"		'tetris-solver-start-game)
    (define-key map "q"		'tetris-solver-end-game)
    (define-key map "p"		'tetris-solver-pause-game)

    (define-key map " "		'tetris-solver-move-bottom)
    (define-key map [left]	'tetris-solver-move-left)
    (define-key map [right]	'tetris-solver-move-right)
    (define-key map [up]	'tetris-solver-rotate-prev)
    (define-key map [down]	'tetris-solver-move-down)
    map))

(defvar tetris-solver-null-map
  (let ((map (make-sparse-keymap 'tetris-solver-null-map)))
    (define-key map "n"		'tetris-solver-start-game)
    map))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-solver-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c tetris-solver-blank)
                   tetris-solver-blank-options)
                  ((and (>= c 0) (<= c 6))
		   (append
		    tetris-solver-cell-options
		    `((((glyph color-x) ,(aref tetris-solver-x-colors c))
		       (color-tty ,(aref tetris-solver-tty-colors c))
		       (t nil)))))
                  ((= c tetris-solver-border)
                   tetris-solver-border-options)
                  ((= c tetris-solver-space)
                   tetris-solver-space-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun tetris-solver-get-tick-period ()
  (if (boundp 'tetris-solver-update-speed-function)
      (let ((period (apply tetris-solver-update-speed-function
			   tetris-solver-n-shapes
			   tetris-solver-n-rows nil)))
	(and (numberp period) period))))

(defun tetris-solver-get-shape-cell (block)
  (aref (aref  (aref tetris-solver-shapes
                     tetris-solver-shape) tetris-solver-rot)
        block))

(defun tetris-solver-shape-width ()
  (aref (aref tetris-solver-shape-dimensions tetris-solver-shape) 0))

(defun tetris-solver-shape-rotations ()
  (length (aref tetris-solver-shapes tetris-solver-shape)))

(defun tetris-solver-draw-score ()
  (let ((strings (vector (format "Shapes: %05d" tetris-solver-n-shapes)
			 (format "Rows:   %05d" tetris-solver-n-rows)
			 (format "Score:  %05d" tetris-solver-score))))
    (dotimes (y 3)
      (let* ((string (aref strings y))
             (len (length string)))
        (dotimes (x len)
          (gamegrid-set-cell (+ tetris-solver-score-x x)
                             (+ tetris-solver-score-y y)
                             (aref string x)))))))

(defun tetris-solver-update-score ()
  (tetris-solver-draw-score)
  (let ((period (tetris-solver-get-tick-period)))
    (if period (gamegrid-set-timer period))))

(defun tetris-solver-new-shape ()
  (setq tetris-solver-shape tetris-solver-next-shape)
  (setq tetris-solver-rot 0)
  (setq tetris-solver-next-shape (random 7))
  (setq tetris-solver-pos-x (/ (- tetris-solver-width (tetris-solver-shape-width)) 2))
  (setq tetris-solver-pos-y 0)
  (if (tetris-solver-test-shape)
      (tetris-solver-end-game)
    (tetris-solver-draw-shape)
    (tetris-solver-draw-next-shape)
    (tetris-solver-update-score)))

(defun tetris-solver-draw-next-shape ()
  (dotimes (x 4)
    (dotimes (y 4)
      (gamegrid-set-cell (+ tetris-solver-next-x x)
                         (+ tetris-solver-next-y y)
                         tetris-solver-blank)))
  (dotimes (i 4)
    (let ((tetris-solver-shape tetris-solver-next-shape)
          (tetris-solver-rot 0))
      (gamegrid-set-cell (+ tetris-solver-next-x
                            (aref (tetris-solver-get-shape-cell i) 0))
                         (+ tetris-solver-next-y
                            (aref (tetris-solver-get-shape-cell i) 1))
                         tetris-solver-shape))))

(defun tetris-solver-draw-shape ()
  (dotimes (i 4)
    (let ((c (tetris-solver-get-shape-cell i)))
      (gamegrid-set-cell (+ tetris-solver-top-left-x
                            tetris-solver-pos-x
                            (aref c 0))
                         (+ tetris-solver-top-left-y
                            tetris-solver-pos-y
                            (aref c 1))
                         tetris-solver-shape))))

(defun tetris-solver-erase-shape ()
  (dotimes (i 4)
    (let ((c (tetris-solver-get-shape-cell i)))
      (gamegrid-set-cell (+ tetris-solver-top-left-x
                            tetris-solver-pos-x
                            (aref c 0))
                         (+ tetris-solver-top-left-y
                            tetris-solver-pos-y
                            (aref c 1))
                         tetris-solver-blank))))

(defun tetris-solver-test-shape ()
  (let ((hit nil))
    (dotimes (i 4)
      (unless hit
        (setq hit
              (let* ((c (tetris-solver-get-shape-cell i))
                     (xx (+ tetris-solver-pos-x
                            (aref c 0)))
                     (yy (+ tetris-solver-pos-y
                            (aref c 1))))
                (or (>= xx tetris-solver-width)
                    (>= yy tetris-solver-height)
                    (/= (gamegrid-get-cell
                         (+ xx tetris-solver-top-left-x)
                         (+ yy tetris-solver-top-left-y))
                        tetris-solver-blank))))))
    hit))

(defun tetris-solver-full-row (y)
  (let ((full t))
    (dotimes (x tetris-solver-width)
      (if (= (gamegrid-get-cell (+ tetris-solver-top-left-x x)
                                (+ tetris-solver-top-left-y y))
             tetris-solver-blank)
          (setq full nil)))
    full))

(defun tetris-solver-shift-row (y)
  (if (= y 0)
      (dotimes (x tetris-solver-width)
	(gamegrid-set-cell (+ tetris-solver-top-left-x x)
			   (+ tetris-solver-top-left-y y)
			   tetris-solver-blank))
    (dotimes (x tetris-solver-width)
      (let ((c (gamegrid-get-cell (+ tetris-solver-top-left-x x)
                                  (+ tetris-solver-top-left-y y -1))))
        (gamegrid-set-cell (+ tetris-solver-top-left-x x)
                           (+ tetris-solver-top-left-y y)
			   c)))))

(defun tetris-solver-shift-down ()
  (dotimes (y0 tetris-solver-height)
    (when (tetris-solver-full-row y0)
      (setq tetris-solver-n-rows (1+ tetris-solver-n-rows))
      (cl-loop for y from y0 downto 0 do
               (tetris-solver-shift-row y)))))

(defun tetris-solver-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      tetris-solver-draw-border-with-glyphs))

(defun tetris-solver-init-buffer ()
  (gamegrid-init-buffer tetris-solver-buffer-width
			tetris-solver-buffer-height
			tetris-solver-space)
  (let ((buffer-read-only nil))
    (if (tetris-solver-draw-border-p)
	(cl-loop for y from -1 to tetris-solver-height do
                 (cl-loop for x from -1 to tetris-solver-width do
                          (gamegrid-set-cell (+ tetris-solver-top-left-x x)
                                             (+ tetris-solver-top-left-y y)
                                             tetris-solver-border))))
    (dotimes (y tetris-solver-height)
      (dotimes (x tetris-solver-width)
        (gamegrid-set-cell (+ tetris-solver-top-left-x x)
                           (+ tetris-solver-top-left-y y)
                           tetris-solver-blank)))
    (if (tetris-solver-draw-border-p)
	(cl-loop for y from -1 to 4 do
                 (cl-loop for x from -1 to 4 do
                          (gamegrid-set-cell (+ tetris-solver-next-x x)
                                             (+ tetris-solver-next-y y)
                                             tetris-solver-border))))))

(defun tetris-solver-reset-game ()
  (gamegrid-kill-timer)
  (tetris-solver-init-buffer)
  (setq tetris-solver-next-shape (random 7))
  (setq tetris-solver-shape	0
	tetris-solver-rot	0
	tetris-solver-pos-x	0
	tetris-solver-pos-y	0
	tetris-solver-n-shapes	0
	tetris-solver-n-rows	0
	tetris-solver-score	0
	tetris-solver-paused	nil)
  (tetris-solver-new-shape))

(defun tetris-solver-shape-done ()
  (tetris-solver-shift-down)
  (setq tetris-solver-n-shapes (1+ tetris-solver-n-shapes))
  (setq tetris-solver-score
	(+ tetris-solver-score
	   (aref (aref tetris-solver-shape-scores tetris-solver-shape) tetris-solver-rot)))
  (tetris-solver-update-score)
  (tetris-solver-new-shape))

(defun tetris-solver-update-game (tetris-solver-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not tetris-solver-paused)
	   (eq (current-buffer) tetris-solver-buffer))
      (let (hit)
	(tetris-solver-erase-shape)
	(setq tetris-solver-pos-y (1+ tetris-solver-pos-y))
	(setq hit (tetris-solver-test-shape))
	(if hit
	    (setq tetris-solver-pos-y (1- tetris-solver-pos-y)))
	(tetris-solver-draw-shape)
	(if hit
	    (tetris-solver-shape-done)))))

(defun tetris-solver-move-bottom ()
  "Drop the shape to the bottom of the playing area."
  (interactive)
  (unless tetris-solver-paused
    (let ((hit nil))
      (tetris-solver-erase-shape)
      (while (not hit)
        (setq tetris-solver-pos-y (1+ tetris-solver-pos-y))
        (setq hit (tetris-solver-test-shape)))
      (setq tetris-solver-pos-y (1- tetris-solver-pos-y))
      (tetris-solver-draw-shape)
      (tetris-solver-shape-done))))

(defun tetris-solver-move-left ()
  "Move the shape one square to the left."
  (interactive)
  (unless tetris-solver-paused
    (tetris-solver-erase-shape)
    (setq tetris-solver-pos-x (1- tetris-solver-pos-x))
    (if (tetris-solver-test-shape)
        (setq tetris-solver-pos-x (1+ tetris-solver-pos-x)))
    (tetris-solver-draw-shape)))

(defun tetris-solver-move-right ()
  "Move the shape one square to the right."
  (interactive)
  (unless tetris-solver-paused
    (tetris-solver-erase-shape)
    (setq tetris-solver-pos-x (1+ tetris-solver-pos-x))
    (if (tetris-solver-test-shape)
	(setq tetris-solver-pos-x (1- tetris-solver-pos-x)))
    (tetris-solver-draw-shape)))

(defun tetris-solver-move-down ()
  "Move the shape one square to the bottom."
  (interactive)
  (unless tetris-solver-paused
    (tetris-solver-erase-shape)
    (setq tetris-solver-pos-y (1+ tetris-solver-pos-y))
    (if (tetris-solver-test-shape)
	(setq tetris-solver-pos-y (1- tetris-solver-pos-y)))
    (tetris-solver-draw-shape)))

(defun tetris-solver-rotate-prev ()
  "Rotate the shape clockwise."
  (interactive)
  (unless tetris-solver-paused
      (tetris-solver-erase-shape)
      (setq tetris-solver-rot (% (+ 1 tetris-solver-rot)
                          (tetris-solver-shape-rotations)))
      (if (tetris-solver-test-shape)
          (setq tetris-solver-rot (% (+ 3 tetris-solver-rot)
                              (tetris-solver-shape-rotations))))
      (tetris-solver-draw-shape)))

(defun tetris-solver-rotate-next ()
  "Rotate the shape anticlockwise."
  (interactive)
  (unless tetris-solver-paused
        (tetris-solver-erase-shape)
        (setq tetris-solver-rot (% (+ 3 tetris-solver-rot)
                            (tetris-solver-shape-rotations)))
        (if (tetris-solver-test-shape)
            (setq tetris-solver-rot (% (+ 1 tetris-solver-rot)
                                (tetris-solver-shape-rotations))))
        (tetris-solver-draw-shape)))

(defun tetris-solver-end-game ()
  "Terminate the current game."
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map tetris-solver-null-map)
  (gamegrid-add-score tetris-solver-score-file tetris-solver-score))

(setq i 0)
(defun update-move (tetrissolver-buffer)
  (if (= i 0)
      (tetrissolver-move-left))
  (setq i (+ 1 i)))


(defun tetris-solver-start-game ()
  "Start a new game of Tetris-Solver."
  (interactive)
  (tetris-solver-reset-game)
  (use-local-map tetris-solver-mode-map)
  (let ((period (or (tetris-solver-get-tick-period)
		    tetris-solver-default-tick-period)))
    (gamegrid-start-timer period 'tetris-solver-update-game))

  (let ((period 0.5))
    (gamegrid-start-timer period 'move)))


(defun tetris-solver-pause-game ()
  "Pause (or resume) the current game."
  (interactive)
  (setq tetris-solver-paused (not tetris-solver-paused))
  (message (and tetris-solver-paused "Game paused (press p to resume)")))

(defun tetris-solver-active-p ()
  (eq (current-local-map) tetris-solver-mode-map))

(put 'tetris-solver-mode 'mode-class 'special)

(define-derived-mode tetris-solver-mode nil "Tetris-Solver"
  "A mode for playing Tetris-Solver."

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map tetris-solver-null-map)

  (unless (featurep 'emacs)
    (setq mode-popup-menu
	  '("Tetris-Solver Commands"
	    ["Start new game"	tetris-solver-start-game]
	    ["End game"		tetris-solver-end-game
	     (tetris-solver-active-p)]
	    ["Pause"		tetris-solver-pause-game
	     (and (tetris-solver-active-p) (not tetris-solver-paused))]
	    ["Resume"		tetris-solver-pause-game
	     (and (tetris-solver-active-p) tetris-solver-paused)])))

  (setq show-trailing-whitespace nil)

  (setq gamegrid-use-glyphs tetris-solver-use-glyphs)
  (setq gamegrid-use-color tetris-solver-use-color)

  (gamegrid-init (tetris-solver-display-options)))

;;;###autoload
(defun tetris-solver ()
  "Play the Tetris-Solver game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-solver-mode keybindings:
   \\<tetris-solver-mode-map>
\\[tetris-solver-start-game]	Starts a new game of Tetris-Solver
\\[tetris-solver-end-game]	Terminates the current game
\\[tetris-solver-pause-game]	Pauses (or resumes) the current game
\\[tetris-solver-move-left]	Moves the shape one square to the left
\\[tetris-solver-move-right]	Moves the shape one square to the right
\\[tetris-solver-rotate-prev]	Rotates the shape clockwise
\\[tetris-solver-rotate-next]	Rotates the shape anticlockwise
\\[tetris-solver-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)

  (select-window (or (get-buffer-window tetris-solver-buffer-name)
		     (selected-window)))
  (switch-to-buffer tetris-solver-buffer-name)
  (gamegrid-kill-timer)
  (tetris-solver-mode)
  (tetris-solver-start-game))

(provide 'tetris-solver)
(tetris-solver)
;;; tetris-solver.el ends here

;;; tetris.el --- implementation of Tetris for Emacs
;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Substantial changes made by Henry Baker <hbaker1@pipeline.com> 6/2003
;; http://home.pipeline.com/~hbaker1/emacs/tetris.el
;; To adjust the probabilites of the 7 pieces, put new non-negative
;; integers into the vector "my-weights".
;; Version: 2.01
;; Created: 1997-08-13
;; Keywords: games
;; This file is part of GNU Emacs.
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;

(defvar tetris-use-glyphs nil
  "Non-nil means use glyphs when available.")

(defvar tetris-use-color t
  "Non-nil means use color when available.")

(defvar tetris-draw-border-with-glyphs nil
  "Non-nil means draw a border even when using glyphs.")

(defvar tetris-default-tick-period 0.3
  "The default time taken for a shape to drop one row.")

(defvar tetris-update-speed-function
  '(lambda (s r) 1.0)
  "Function run whenever the Tetris score changes
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped
ROWS is the number of rows which have been completed

If the return value is a number, it is used as the timer period.")

(defvar tetris-mode-hook nil
  "Hook run upon starting Tetris.")

(defvar tetris-tty-colors
  [nil "blue" "white" "yellow" "magenta" "cyan" "green" "red"]
  "Vector of colors of the various shapes in text mode
Element 0 is ignored.")

(defvar tetris-x-colors
  [nil [0 0 1] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]
  "Vector of colors of the various shapes
Element 0 is ignored.")

(defvar tetris-buffer-name "*Tetris*"
  "Name used for Tetris buffer.")

(defvar tetris-buffer-width 30
  "Width of used portion of buffer.")

(defvar tetris-buffer-height 22
  "Height of used portion of buffer.")

(defvar tetris-width 10
  "Width of playing area.")

(defvar tetris-height 20
  "Height of playing area.")

(defvar tetris-next-x 0
  "X position of next shape.")

(defvar tetris-next-y 0
  "Y position of next shape.")

(defvar tetris-score-x 0
  "X position of score.")

(defvar tetris-score-y 0
  "Y position of score.")

(defvar tetris-score-file (concat temporary-file-directory "tetris-scores")
;; anybody with a well-connected server want to host this?
;(defvar tetris-score-file "/anonymous@ftp.pgt.com:/pub/cgw/tetris-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (t nil))
    (((glyph color-x) [0.5 0.5 0.5])
     (t nil))))

(defvar tetris-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty)
     (t nil))
    (((glyph color-x) [0 0 0])
     (color-tty "black")
     (t nil))))

(defvar tetris-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty)
     (t nil))
    ;; color information is taken from tetris-x-colors and tetris-tty-colors
    ))

(defvar tetris-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;

(defconst tetris-shapes
  [[[[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[1 1 0 0] [1 1 0 0] [1 1 0 0] [1 1 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[2 2 2 0] [0 2 0 0] [2 0 0 0] [2 2 0 0]]
    [[0 0 2 0] [0 2 0 0] [2 2 2 0] [2 0 0 0]]
    [[0 0 0 0] [2 2 0 0] [0 0 0 0] [2 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[3 3 3 0] [3 3 0 0] [0 0 3 0] [3 0 0 0]]
    [[3 0 0 0] [0 3 0 0] [3 3 3 0] [3 0 0 0]]
    [[0 0 0 0] [0 3 0 0] [0 0 0 0] [3 3 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[4 4 0 0] [0 4 0 0] [4 4 0 0] [0 4 0 0]]
    [[0 4 4 0] [4 4 0 0] [0 4 4 0] [4 4 0 0]]
    [[0 0 0 0] [4 0 0 0] [0 0 0 0] [4 0 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 5 5 0] [5 0 0 0] [0 5 5 0] [5 0 0 0]]
    [[5 5 0 0] [5 5 0 0] [5 5 0 0] [5 5 0 0]]
    [[0 0 0 0] [0 5 0 0] [0 0 0 0] [0 5 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[0 6 0 0] [6 0 0 0] [6 6 6 0] [0 6 0 0]]
    [[6 6 6 0] [6 6 0 0] [0 6 0 0] [6 6 0 0]]
    [[0 0 0 0] [6 0 0 0] [0 0 0 0] [0 6 0 0]]
    [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]]

   [[[7 7 7 7] [7 0 0 0] [7 7 7 7] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]
    [[0 0 0 0] [7 0 0 0] [0 0 0 0] [7 0 0 0]]]])

;;the scoring rules were taken from "xtetris".  Blocks score differently 
;;depending on their rotation

(defconst tetris-shape-scores 
  [ [6 6 6 6] [6 7 6 7] [6 7 6 7] [6 7 6 7] [6 7 6 7] [5 5 6 5] [5 8 5 8]] )

(defconst tetris-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst tetris-blank 0)

(defconst tetris-border 8)

(defconst tetris-space 9)

(defconst tetris-top-left-x 3
  "X position of top left of playing area.")

(defconst tetris-top-left-y 1
  "Y position of top left of playing area.")

(defun tetris-default-update-speed-function (shapes rows)
  (/ 20.0 (+ 50.0 rows)))

(defvar my-weights [1 1 1 1 1 1 1]
  ;;; length of vector must be 7.
  ;;; Entries are unscaled probabilities of each shape.
  "Unscaled probabilities of the shapes.")

(defun index-pairs (v i)
  ;; Compute list of pairs (index, value)
  ;; Seems to work.
  (if (= i (length v)) nil
    (cons (cons i (aref v i)) (index-pairs v (1+ i)))))

(defun walker-split (pairs awn awd less greater)
  ;; pairs is result of index-pairs
  ;; aw is avg of weights -- awn is numerator, awd is denominator.
  ;; less, greater start out as nil.
  ;; return pair of lists; those <aw, those >=aw
  (if (null pairs) (cons less greater)
    (let* ((pair (car pairs)))
      (if (< (* (cdr pair) awd) awn)
	  (walker-split (cdr pairs) awn awd (cons pair less) greater)
	(walker-split (cdr pairs) awn awd less (cons pair greater))))))

(defconst walker-pn [0 0 0 0 0 0 0])

(defconst walker-pd [0 0 0 0 0 0 0])

(defconst walker-y [0 0 0 0 0 0 0])

(defun test-walker (weights)
  ;; test do-walker
  (setq walker-pn (make-vector 7 0) walker-pd (make-vector 7 0) walker-y (make-vector 7 0))
  (let* ((n (length weights))
	 (nweights (map 'vector #'(lambda (w) (* w n)) weights))
	 (sum (reduce #'+ nweights))
	 (pair (walker-split (index-pairs nweights 0) sum n nil nil)))
    (do-walker (car pair) (cdr pair) sum (* n sum))))

(defun do-walker (l g sumw nsumw)
  ;; Implement A. J. Walker's discrete probability method from Knuth, Vol. II, 3.4.1.
  ;; Use integer weights instead of probabilities.
  ;; e = sumw/nsumw = avg probability.
  ;; l is list of those <=e.
  ;; g is list of those >e.
  (if l (if g (let* ((n (/ nsumw sumw))
		     (lpair (car l))
		     (lx (car lpair))	; first l index
		     (lv (cdr lpair))	; first l value
		     (nl (cdr l))
		     (gpair (car g))	; there _must_ be one >=!
		     (gx (car gpair))	; first g index
		     (gv (cdr gpair))	; first g value
		     (r (- (+ lv gv) (/ sumw n))) ; should be >0.
		     (gcd (gcd (* lv n) sumw))
		     (npair (cons gx r))
		     (ng (cdr g)))
		;; We're getting rid of index lx.
		(setf (aref walker-pn lx) (/ (* lv n) gcd) (aref walker-pd lx) (/ sumw gcd) (aref walker-y lx) gx)
		(if (< (* r n) sumw) (do-walker (cons npair nl) ng sumw nsumw)
		  (do-walker nl (cons npair ng) sumw nsumw)))
	  (car 'error))
    (if g (let* ((n (/ nsumw sumw))
		 (gpair (car g))
		 (gx (car gpair))
		 (gv (cdr gpair)))
	    ;; None less, rest must be all =e.
	    (setf (aref walker-pn gx) 1 (aref walker-pd gx) 1 (aref walker-y gx) gx)
	    (do-walker l (cdr g) sumw nsumw))
      nil)))

(defun my-random-shape ()
  ;; 0 2x2 blue block	; packs perfectly
  ;; 1 purple		; packs perfectly pointing up&down (2 wide); 3 wide has trouble.
  ;; 2 yellow		; packs the same as purple.
  ;; 3 pink		; packs perfectly, but only when standing on end.
  ;; 4 turquoise	; packs perfectly, but only when standing on end.
  ;; 5 green		; packs perfectly, but only when standing on end.
  ;; 6 red 4x1		; packs perfectly, but only when standing on end.
  (let* ((n (length tetris-shapes))
	 (bucket (random n))
	 (pn (aref walker-pn bucket))
	 (pd (aref walker-pd bucket))
	 (ran (random pd)))
    (if (< ran pn) bucket
      (aref walker-y bucket))))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;

(defvar tetris-shape 0)		; index of the current shape
(defvar tetris-rot 0)		; rotation of current shape
(defvar tetris-next-shape 0)	; index of next shape
(defvar tetris-n-shapes 0)
(defvar tetris-n-rows 0)
(defvar tetris-score 0)
(defvar tetris-pos-x 0)		; x position of current shape
(defvar tetris-pos-y 0)		; y position of current shape
(defvar tetris-paused nil)

(make-variable-buffer-local 'tetris-shape)
(make-variable-buffer-local 'tetris-rot)
(make-variable-buffer-local 'tetris-next-shape)
(make-variable-buffer-local 'tetris-n-shapes)
(make-variable-buffer-local 'tetris-n-rows)
(make-variable-buffer-local 'tetris-score)
(make-variable-buffer-local 'tetris-pos-x)
(make-variable-buffer-local 'tetris-pos-y)
(make-variable-buffer-local 'tetris-paused)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))

(define-key tetris-mode-map "n"		'tetris-start-game)
(define-key tetris-mode-map "q"		'tetris-end-game)
(define-key tetris-mode-map "p"		'tetris-pause-game)
(define-key tetris-mode-map " "		'tetris-move-bottom)
(define-key tetris-mode-map [left]	'tetris-move-left)
(define-key tetris-mode-map [right]	'tetris-move-right)
(define-key tetris-mode-map [up]	'tetris-rotate-prev)
(define-key tetris-mode-map [down]	'tetris-rotate-next)

(defvar tetris-null-map
  (make-sparse-keymap 'tetris-null-map))

(define-key tetris-null-map "n"		'tetris-start-game)

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-display-options ()
  (let ((options (make-vector 256 nil)))
    (do ((c 0 (1+ c))) ((= c 256))
      (aset options c
	    (cond ((= c tetris-blank) tetris-blank-options)
		  ((and (>= c 1) (<= c 7))
		   (append
		    tetris-cell-options
		    `((((glyph color-x) ,(aref tetris-x-colors c))
		       (color-tty ,(aref tetris-tty-colors c))
		       (t nil)))))
		  ((= c tetris-border) tetris-border-options)
		  ((= c tetris-space) tetris-space-options)
		  (t '(nil nil nil)))))
    options))

(defun tetris-get-tick-period ()
  (if (boundp 'tetris-update-speed-function)
      (let ((period (apply tetris-update-speed-function
			   tetris-n-shapes
			   tetris-n-rows
			   nil)))
	(and (numberp period) period))))

(defun tetris-get-shape-cell (shape rot x y)
  (aref (aref (aref (aref tetris-shapes shape)
		    y)
	      rot)
	x))

(defun tetris-shape-width (shape rot)
  (aref (aref tetris-shape-dimensions shape)
	(% rot 2)))

(defun tetris-shape-height (shape rot)
  (aref (aref tetris-shape-dimensions shape)
	(- 1 (% rot 2))))

(defun tetris-draw-score (n-shapes n-rows score)
  (let ((strings (vector (format "Shapes: %05d" n-shapes)
			 (format "Rows:   %05d" n-rows)
			 (format "Score:  %05d" score))))
    (do ((y 0 (1+ y))) ((= y 3))
      (let* ((string (aref strings y)) (len (length string)))
	(do ((x 0 (1+ x))) ((= x len))
	  (gamegrid-set-cell (+ tetris-score-x x) (+ tetris-score-y y) (aref string x)))))))

(defun tetris-update-score (n-shapes n-rows score)
  (tetris-draw-score n-shapes n-rows score)
  (let ((period (tetris-get-tick-period)))
    (if period (gamegrid-set-timer period))))

(defun tetris-new-shape ()
  (setq tetris-shape tetris-next-shape tetris-next-shape (my-random-shape))
  (setq tetris-rot 0)
  (setq tetris-pos-x (/ (- tetris-width (tetris-shape-width tetris-shape tetris-rot)) 2))
  (setq tetris-pos-y 0)
  (if (tetris-test-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
      (tetris-end-game)
    (tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y))
  (tetris-draw-next-shape tetris-next-shape)
  (tetris-update-score tetris-n-shapes tetris-n-rows tetris-score))

(defun tetris-draw-next-shape (tetris-next-shape)
  (do ((y 0 (1+ y))) ((= y 4))
    (do ((x 0 (1+ x))) ((= x 4))
      (gamegrid-set-cell (+ tetris-next-x x)
			 (+ tetris-next-y y)
			 (tetris-get-shape-cell tetris-next-shape 0 x y)))))

(defun tetris-draw-shape (shape rot pos-x pos-y)
  (do ((y 0 (1+ y))) ((= y (tetris-shape-height shape rot)))
    (do ((x 0 (1+ x))) ((= x (tetris-shape-width shape rot)))
      (let ((c (tetris-get-shape-cell shape rot x y)))
	(when (/= c tetris-blank)
	  (gamegrid-set-cell (+ tetris-top-left-x pos-x x)
			     (+ tetris-top-left-y pos-y y)
			     c))))))

(defun tetris-erase-shape (shape rot pos-x pos-y)
  (do ((y 0 (1+ y))) ((= y (tetris-shape-height shape rot)))
    (do ((x 0 (1+ x))) ((= x (tetris-shape-width shape rot)))
      (let ((c (tetris-get-shape-cell shape rot x y)))
	(when (/= c tetris-blank)
	  (gamegrid-set-cell (+ tetris-top-left-x pos-x x)
			     (+ tetris-top-left-y pos-y y)
			     tetris-blank))))))

(defun tetris-test-shape (shape rot pos-x pos-y)
  (let ((hit nil))
    (do ((y 0 (1+ y))) ((= y (tetris-shape-height shape rot)))
      (do ((x 0 (1+ x))) ((= x (tetris-shape-width shape rot)))
	(unless hit
	  (setq hit
		(let* ((c (tetris-get-shape-cell shape rot x y))
		       (xx (+ pos-x x))
		       (yy (+ pos-y y))
		       (px (+ tetris-top-left-x xx))
		       (py (+ tetris-top-left-y yy)))
		  (and (/= c tetris-blank)
		       (or (>= xx tetris-width)
			   (>= yy tetris-height)
			   (/= (gamegrid-get-cell px py)
			       tetris-blank))))))))
    hit))

(defun tetris-full-row (y)
  (let ((full t))
    (do ((x 0 (1+ x))) ((= x tetris-width))
      (if (= (gamegrid-get-cell (+ tetris-top-left-x x)
				(+ tetris-top-left-y y))
	     tetris-blank)
	  (setq full nil)))
    full))

(defun tetris-shift-row (y)
  (if (= y 0)
      (do ((x 0 (1+ x))) ((= x tetris-width))
	(gamegrid-set-cell (+ tetris-top-left-x x)
			   (+ tetris-top-left-y y)
			   tetris-blank))
    (do ((x 0 (1+ x))) ((= x tetris-width))
      (let ((c (gamegrid-get-cell (+ tetris-top-left-x x)
				  (+ tetris-top-left-y y -1))))
	(gamegrid-set-cell (+ tetris-top-left-x x)
			   (+ tetris-top-left-y y)
			   c)))))

(defun tetris-shift-down ()
  (do ((y0 0 (1+ y0))) ((= y0 tetris-height))
    (if (tetris-full-row y0)
	(progn (setq tetris-n-rows (1+ tetris-n-rows))
	       (do ((y y0 (1- y))) ((< y 0))
		 (tetris-shift-row y))))))

(defun tetris-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      tetris-draw-border-with-glyphs))

(defun tetris-init-buffer (
			   width height
			   top-left-x
			   top-left-y
			   next-x
			   next-y)
  (let ((buffer-read-only nil))
    (if (tetris-draw-border-p)
	(do ((y (- top-left-y 1) (1+ y))) ((> y (+ top-left-y height)))
	  (do ((x (- tetris-top-left-x 1) (1+ x))) ((> x (+ tetris-top-left-x tetris-width)))
	    (gamegrid-set-cell x y tetris-border))))
    (do ((y top-left-y (1+ y))) ((= y (+ top-left-y height)))
      (do ((x top-left-x (1+ x))) ((= x (+ top-left-x width)))
	(gamegrid-set-cell x y tetris-blank)))
    (if (tetris-draw-border-p)
	(do ((y (- next-y 1) (1+ y))) ((> y (+ next-y 4)))
	  (do ((x (- next-x 1) (1+ x))) ((> x (+ next-x 4)))
	    (gamegrid-set-cell x y tetris-border))))))

(defun tetris-reset-game (buffer-width buffer-height)
  (gamegrid-kill-timer)
  (gamegrid-init-buffer buffer-width buffer-height tetris-space)
  (setq tetris-next-x		(+ (* 2 tetris-top-left-x) tetris-width)
	tetris-next-y		tetris-top-left-y
	tetris-score-x		tetris-next-x
	tetris-score-y		(+ tetris-next-y 6))
  (tetris-init-buffer
   tetris-width tetris-height
   tetris-top-left-x tetris-top-left-y
   tetris-next-x tetris-next-y)
  (test-walker my-weights)
  (setq tetris-shape	0
	tetris-rot	0
	tetris-next-shape (my-random-shape)
	tetris-pos-x	0
	tetris-pos-y	0
	tetris-n-shapes	0
	tetris-n-rows	0
	tetris-score	0
	tetris-paused	nil)
  (tetris-new-shape))

(defun tetris-shape-done ()
  (tetris-shift-down)
  (setq tetris-n-shapes (1+ tetris-n-shapes))
  (setq tetris-score
	(+ tetris-score 
	   (aref (aref tetris-shape-scores tetris-shape) tetris-rot)))
  (tetris-update-score tetris-n-shapes tetris-n-rows tetris-score)
  (tetris-new-shape))

(defun tetris-update-game (tetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not tetris-paused)
	   (eq (current-buffer) tetris-buffer))
      (let (hit)
	(tetris-erase-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
	(setq tetris-pos-y (1+ tetris-pos-y))
	(setq hit (tetris-test-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y))
	(if hit (setq tetris-pos-y (1- tetris-pos-y)))
	(tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
	(if hit (tetris-shape-done)))))

(defun tetris-move-bottom ()
  ;;; Called by key map
  "Drops the shape to the bottom of the playing area"
  (interactive)
  (let ((hit nil))
    (tetris-erase-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
    (while (not hit)
      (setq tetris-pos-y (1+ tetris-pos-y))
      (setq hit (tetris-test-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)))
    (setq tetris-pos-y (1- tetris-pos-y))
    (tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
    (tetris-shape-done)))

(defun tetris-move-left ()
  ;;; Called by key map
  "Moves the shape one square to the left"
  (interactive)
  (unless (= tetris-pos-x 0)
    (tetris-erase-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
    (let ((new-pos-x (1- tetris-pos-x)))
      (unless (tetris-test-shape tetris-shape tetris-rot new-pos-x tetris-pos-y)
	(setq tetris-pos-x new-pos-x)))
    (tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)))

(defun tetris-move-right ()
  ;;; Called by key map
  "Moves the shape one square to the right"
  (interactive)
  (unless (= (+ tetris-pos-x (tetris-shape-width tetris-shape tetris-rot))
	     tetris-width)
    (tetris-erase-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
    (let ((new-pos-x (1+ tetris-pos-x)))
      (unless (tetris-test-shape tetris-shape tetris-rot new-pos-x tetris-pos-y)
	(setq tetris-pos-x new-pos-x)))
    (tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)))

(defun tetris-rotate-prev ()
  ;;; Called by key map
  "Rotates the shape clockwise"
  (interactive)
  (tetris-erase-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
  (let ((new-rot (% (+ 1 tetris-rot) 4)))
    (unless (tetris-test-shape tetris-shape new-rot tetris-pos-x tetris-pos-y)
      (setq tetris-rot new-rot)))
  (tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y))

(defun tetris-rotate-next ()
  ;;; Called by key map
  "Rotates the shape anticlockwise"
  (interactive)
  (tetris-erase-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
  (let ((new-rot (% (+ 3 tetris-rot) 4)))
    (unless (tetris-test-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y)
      (setq tetris-rot new-rot)))
  (tetris-draw-shape tetris-shape tetris-rot tetris-pos-x tetris-pos-y))

(defun tetris-end-game ()
  ;;; Called by key map
  "Terminates the current game"
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map tetris-null-map)
  (gamegrid-add-score tetris-score-file tetris-score))

(defun tetris-start-game ()
  ;;; Called by key map
  "Starts a new game of Tetris"
  (interactive)
  (tetris-reset-game tetris-buffer-width tetris-buffer-height)
  (use-local-map tetris-mode-map)
  (let ((period (or (tetris-get-tick-period)
		    tetris-default-tick-period)))
    (gamegrid-start-timer period 'tetris-update-game)))

(defun tetris-pause-game ()
  ;;; Called by key map
  "Pauses (or resumes) the current game"
  (interactive)
  (setq tetris-paused (not tetris-paused))
  (message (and tetris-paused "Game paused (press p to resume)")))

(defun tetris-active-p ()
  (eq (current-local-map) tetris-mode-map))

(put 'tetris-mode 'mode-class 'special)

(defun tetris-mode ()
  "A mode for playing Tetris.

tetris-mode keybindings:
   \\{tetris-mode-map}
"
  (kill-all-local-variables)

  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map tetris-null-map)

  (setq major-mode 'tetris-mode)
  (setq mode-name "Tetris")

  (setq mode-popup-menu
	'("Tetris Commands"
	  ["Start new game"	tetris-start-game]
	  ["End game"		tetris-end-game
	   (tetris-active-p)]
	  ["Pause"		tetris-pause-game
	   (and (tetris-active-p) (not tetris-paused))]
	  ["Resume"		tetris-pause-game
	   (and (tetris-active-p) tetris-paused)]))

  (setq gamegrid-use-glyphs tetris-use-glyphs)
  (setq gamegrid-use-color tetris-use-color)

  (gamegrid-init (tetris-display-options))

  (run-hooks 'tetris-mode-hook))

;;;###autoload
(defun tetris ()
  "Play the Tetris game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-pause-game]	Pauses (or resumes) the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)
  (switch-to-buffer tetris-buffer-name)
  (gamegrid-kill-timer)
  (tetris-mode)
  (tetris-start-game))

(provide 'tetris)

;;; tetris.el ends here

;;;
;;; tictactoe.el -- play tic tac toe in Emacs
;;;


;; Variables

(defconst *tictactoe-size* 3
  "The size of the board -- width and height")

(defconst *tictactoe-empty-char* ?\.
  "Char representing an empty space")

(defconst *tictactoe-empty-cell* (vector "   " "   " "   ")
  "Vector defining tic-tac-toe empty cell filling")

(defconst *tictactoe-players-chars* (cons ?\X ?\O)
  "Characters representing tic-tac-toe players")

(defconst *tictactoe-players-cells* (cons
				     (vector "X X" " X " "X X")
				     (vector " O " "O O" " O "))
  "Vector defining tic-tac-toe players cells filling")

(defvar *tictactoe-board* nil
  "The board itself")

(defvar *tictactoe-current-turn* nil
  "Number of the current turn")

(defvar *tictactoe-current-player* nil
  "The character representing current player")


;; tic tac toe mode

(defun tictactoe ()
  "Start playing tic-tac-toe"
  (interactive)
  (switch-to-buffer "tictactoe")
  (tictactoe-mode)
  (tictactoe-init))

(define-derived-mode tictactoe-mode special-mode
  "tic-tac-toe"
  "Major mode for playing tic-tac-toe"
  (define-key tictactoe-mode-map "m" 'tictactoe-mark-at-postion))


;; Draw functions

(defun tictactoe-draw-hor-line ()
  "Print tic-tac-toe board horizontal separating line"
  (insert "+---+---+---+"))

(defun tictactoe-draw-frame ()
  "Print tic-tac-toe board frame"
  (tictactoe-draw-hor-line)
  (dotimes (row *tictactoe-size*)
    (insert "\n")
    (dotimes (col *tictactoe-size*)
      ;;      (insert (tictactoe-get-char row col)))
      (insert "||||\n"))
    (tictactoe-draw-hor-line))
  (insert "\n"))

(defun tictactoe-draw-cell (line_n char_n lines)
  "Draw a tic-tac-toe cell"
  (let ((line_stop (+ line_n *tictactoe-size*))
	(lines_i 0))
    (while (< line_n line_stop)
      (goto-char (point-min))
      (forward-line line_n)
      (forward-char char_n)
      (insert (aref lines lines_i))
      (setq line_n (1+ line_n))
      (setq lines_i (1+ lines_i))
      )))

(defun tictactoe-draw-cells ()
  "Draw cells on tic-tac-toe board"
  (dotimes (row *tictactoe-size*)
    (dotimes (col *tictactoe-size*)
      (let ((char (tictactoe-get-char row col))
	    (line_n (1+ (* row (1+ *tictactoe-size*))))
	    (char_n (1+ (* col (1+ *tictactoe-size*)))))
	(cond
	 ((char-equal char *tictactoe-empty-char*)
	  (tictactoe-draw-cell line_n char_n *tictactoe-empty-cell*))
	 ((char-equal char (car *tictactoe-players-chars*))
	  (tictactoe-draw-cell line_n char_n (car *tictactoe-players-cells*)))
	 ((char-equal char (cdr *tictactoe-players-chars*))
	  (tictactoe-draw-cell line_n char_n (cdr *tictactoe-players-cells*))))))))

(defun tictactoe-draw-board ()
  "Print tic-tac-toe board into the buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (tictactoe-draw-frame)
    (tictactoe-draw-cells)
    (forward-line 2)
    (insert (format "\nPlayer: %c\n" *tictactoe-current-player*))
    (insert (format "\nTurn: %d\n" *tictactoe-current-turn*))
    (insert "\nPlayer vs Player\n\n")
    (when (tictactoe-game-draw)
      (insert "A DRAW !"))
    (when (tictactoe-game-won)
      (insert (format "PLAYER - %c WON !" *tictactoe-current-player*)))
    (goto-char 6)))


;; Game functions

(defun tictactoe-init ()
  "Start a new game of tic-tac-toe"
  (setq *tictactoe-board* (make-vector (* *tictactoe-size* *tictactoe-size*) *tictactoe-empty-char*))
  (let ((coin (random 2)))
    (if (equal coin 0)
	(setq *tictactoe-current-player* (cdr *tictactoe-players-chars*))
      (setq *tictactoe-current-player* (car *tictactoe-players-chars*))))
  (setq *tictactoe-current-turn* 0)
  (tictactoe-draw-board))

(defun tictactoe-get-char (row col)
  "Get the character in the (row, col) position"
  (aref *tictactoe-board*
	(+ col (* row *tictactoe-size*))))

(defun tictactoe-set-char (row col char)
  "Set the character in the (row, col) position to the given value"
  (aset *tictactoe-board*
	(+ col (* row *tictactoe-size*))
	char))

(defun tictactoe-ask-new-game ()
  "Ask if the player wants to begin a new game"
  (if (y-or-n-p "Start new game?")
      (tictactoe-init)))

(defun tictactoe-get-row ()
  "Return board row's index corresponding to the current cursor position"
  (let ((row (1- (line-number-at-pos))))
    (cond
     ((and (> row 0) (< row 4)) 0)
     ((and (> row 4) (< row 8)) 1)
     ((and (> row 8) (< row 12)) 2))))

(defun tictactoe-get-col ()
  "Return board column's index corresponding to the current cursor position"
  (let ((col (current-column)))
    (cond
     ((and (> col 0) (< col 4)) 0)
     ((and (> col 4) (< col 8)) 1)
     ((and (> col 8) (< col 12)) 2))))

(defun tictactoe-mark-at-postion ()
  "Set a mark at a current cursor position"
  (interactive)
  (if (tictactoe-game-finished)
      (tictactoe-ask-new-game)
    (let ((row (tictactoe-get-row))
	  (col (tictactoe-get-col)))
      (if (or (equal row nil) (equal col nil))
	  ()
	(if (char-equal (tictactoe-get-char row col) *tictactoe-empty-char*)
	    (progn
	      (tictactoe-set-char row col *tictactoe-current-player*)
	      (setq *tictactoe-current-turn* (1+ *tictactoe-current-turn*))
	      (if (tictactoe-game-finished)
		  (progn
		    (tictactoe-draw-board)
		    (tictactoe-ask-new-game))
		(tictactoe-swap-player))
	      (tictactoe-draw-board))
	  (message "This postion is already filled"))))))


(defun tictactoe-swap-player ()
  "Make turn to other player"
  (setq *tictactoe-current-player*
	(if (char-equal *tictactoe-current-player* (car *tictactoe-players-chars*))
	    (cdr *tictactoe-players-chars*)
	  (car *tictactoe-players-chars*))))


;; Functions for checking winning condition

(defun tictactoe-game-finished ()
  "Rturns t if the game have been finished, nil otherwise"
  (or (tictactoe-game-draw)
      (tictactoe-game-won)))

(defun tictactoe-game-draw ()
  "Returns t if the game ended in a draw, nil otherwise"
  (and (not (< *tictactoe-current-turn* (* *tictactoe-size* *tictactoe-size*)))
       (not (tictactoe-game-won))))

(defun tictactoe-game-won ()
  "Returns t if the game has ben won, nil otherwise"
  (or (tictactoe-diag-win)
      (tictactoe-row-win)
      (tictactoe-col-win)))

(defun tictactoe-diag-win()
  "Check if the player have filled any diagonal"
  (or (tictactoe-all-same-player (vector (cons 0 0) (cons 1 1) (cons 2 2)))
      (tictactoe-all-same-player (vector (cons 0 2) (cons 1 1) (cons 2 0)))))

(defun tictactoe-row-win()
  "Check if the player have filled any row"
  (or (tictactoe-all-same-player (vector (cons 0 0) (cons 0 1) (cons 0 2)))
      (tictactoe-all-same-player (vector (cons 1 0) (cons 1 1) (cons 1 2)))
      (tictactoe-all-same-player (vector (cons 2 0) (cons 2 1) (cons 2 2)))))

(defun tictactoe-col-win()
  "Check if the player have filled any column"
  (or (tictactoe-all-same-player (vector (cons 0 0) (cons 1 0) (cons 2 0)))
      (tictactoe-all-same-player (vector (cons 0 1) (cons 1 1) (cons 2 1)))
      (tictactoe-all-same-player (vector (cons 0 2) (cons 1 2) (cons 2 2)))))

(defun tictactoe-all-same-player (pos-vec)
  (let ((is-win t))
    (dotimes (i (length pos-vec))
      (when (not (char-equal
		  (tictactoe-get-char (car (aref pos-vec i)) (cdr (aref pos-vec i)))
		  *tictactoe-current-player*))
	(setq is-win nil)))
    is-win))

(require 'tetris)

(defun tetris-solver ()
  (interactive)

  (select-window (or (get-buffer-window tetris-buffer-name)
		     (selected-window)))
  (switch-to-buffer tetris-buffer-name)
  (gamegrid-kill-timer)
  (tetris-mode)
  (tetris-start-game))

(tetris-solver)


;;; itimer.el -- list and operate on emacs timers

;; Copyright (C) 2009, 2010, 2011 Philip Weaver

;; Author: Philip Weaver <philip.weaver@gmail.com>
;; Maintainer: Philip Weaver <philip.weaver@gmail.com>
;; Created: December 2009
;; Keywords:
;; URL: http://github.com/pheaver/itimer-el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; TODO
;; visualization:
;;   - colorized output
;;   - support sorting
;;   - custom formats, like ibuffer
;;   - customize window behavior
;;   - customizable exit action such as restore window configuration
;; timer modification:
;;   - create timer
;;   - delete timer
;;   - change timer--time
;;   - change timer--function

;; FIXME
;; if the list of timers changes while we are in the buffer,
;; itimer-current-timer could return the wrong timer.

;; note to self, here are some important functions:
;; timer--activate      timer--args
;; timer--function      timer--high-seconds
;; timer--idle-delay    timer--low-seconds
;; timer--repeat-delay  timer--time
;; timer--time-less-p   timer--triggered
;; timer--usecs         timer-activate
;; timer-activate-when-idle     timer-create
;; timer-duration       timer-event-handler
;; timer-inc-time       timer-next-integral-multiple-of-time
;; timer-relative-time  timer-set-function
;; timer-set-idle-time  timer-set-time
;; timer-set-time-with-usecs    timer-until

;;(define-derived-mode itimer-mode fundamental-mode "Timer")

(defconst itimer-buffer-name " *timer-list*"
  "Name of buffer to display the timer list")

(defcustom itimer-use-other-window nil
  "If non-nil, display Itimer in another window by default."
  :type 'boolean
  :group 'itimer)

(defvar itimer-parent-buffer nil
  "Parent buffer of itimer list buffer.")
(make-variable-buffer-local 'itimer-parent-buffer)

(defvar itimer-mode-map nil)

(defun itimer-setup-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'itimer-quit)
    (define-key map (kbd "p") #'itimer-previous-timer)
    (define-key map (kbd "n") #'itimer-next-timer)
    (define-key map (kbd "g") 'itimer-update)
    (define-key map (kbd "c") #'itimer-create-timer)
    (define-key map (kbd "a") #'itimer-activate-timer)
    (define-key map (kbd "k") #'itimer-cancel-timer)
    (setq itimer-mode-map map)))

(defun itimer-next-timer ()
  (interactive)
  (let ((line (line-number-at-pos (point))))
    (when (< line (length timer-list))
      (forward-line 1))))

(defun itimer-previous-timer () (interactive) (forward-line -1))

;;(add-hook 'itimer-mode-hook 'timer-key-setup)

(defun itimer-mode ()
  (kill-all-local-variables)
  (itimer-setup-map)
  (use-local-map itimer-mode-map)
  (setq major-mode 'itimer-mode
        mode-name "Timer")
  (set (make-local-variable 'revert-buffer-function) #'itimer-update)
  (run-mode-hooks 'itimer-mode-hook)
  )

(defun itimer-activate-timer ()
  (interactive)
  "Activate timer at point"
  (when (y-or-n-p "Activate timer? ")
    (timer-activate (itimer-current-timer))))

(defun itimer-cancel-timer ()
  (interactive)
  "Kill timer at point"
  (when (y-or-n-p "Kill timer? ")
    (cancel-timer (itimer-current-timer))
    (itimer-update t)))

(defun itimer-update (&optional silent)
  (interactive "P")
  (itimer-assert-itimer-mode)
  (let ((line (line-number-at-pos (point))))
    (unless silent
      (message "Updating timer list..."))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq ts timer-list)
    (while (not (null ts))
      (insert (itimer-format-timer (car ts)))
      (newline)
      (pop ts))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line (1- line))
    (unless silent
      (message "Updating timer list...done"))
    ))

(defun itimer-create-timer ()
  "Create a new timer.  Runs `run-at-time' and then updates the timer list."
  (interactive)
  (call-interactively 'run-at-time)
  (when (derived-mode-p 'itimer-mode)
    (itimer-update t)))

(defun itimer-current-timer ()
  "Get the timer at cursor."
  (itimer-assert-itimer-mode)
  (let ((line (line-number-at-pos)))
    (if (> line (length timer-list))
        (error (format "itimer invalid timer number: %d" line))
      (nth (1- line) timer-list))))

(defun itimer-format-timer (timer)
  (concat
   (itimer-format-column (format-time-string "%c" (timer--time timer))
                         30 :left)
   (itimer-format-column (format-seconds "%Y, %D, %z%.2h:%.2m:%.2s"
                          (float-time
                           (time-subtract (timer--time timer) (current-time))))
                         12 :left)
   (itimer-format-column (format "%S %S"
                                 (timer--function timer)
                                 " "
                                 (timer--args timer))
                         35 :left)
   ))

(defun itimer-list-timers (&optional other-window-p noselect)
  "Run Itimer, switching to a buffer and populating it with a list of timers.

All arguments are optional.
OTHER-WINDOW-P says to use other window.  Note that when itimer-use-other-window
is non-nil, then the meaning of OTHER-WINDOW-P is actually inverted.
If NOSELECT is non-nil, then do not select the newly created buffer if it was
created in other window."

  (interactive "P")
  (when itimer-use-other-window (setq other-window-p (not other-window-p)))
  (let ((parent-buff (current-buffer))
        (buf (get-buffer-create itimer-buffer-name)))
    (if other-window-p
        (if noselect (display-buffer buf t) (pop-to-buffer buf))
      (if noselect (display-buffer buf) (switch-to-buffer buf)))

    (with-current-buffer buf
      (when (not (eq major-mode 'itimer-mode))
        (itimer-mode))
      (itimer-update nil)
      (setq itimer-parent-buffer parent-buff))))

;; most of this was taken from `undo-tree-visualizer-quit' in undo-tree.el
(defun itimer-quit ()
  (interactive)
  (itimer-assert-itimer-mode)
  (let ((parent itimer-parent-buffer)
        window)
    (kill-buffer nil)
    (if (setq window (get-buffer-window parent))
        (select-window window)
      (switch-to-buffer parent))))

;; --------------------
;; the code in this block was copied from ibuffer

(defsubst itimer-assert-itimer-mode ()
  (assert (derived-mode-p 'itimer-mode)))

(defun itimer-format-column (str width alignment)
  (let ((pad (- width (length str))))
    (if (> pad 0)
        (let ((left (make-string (/ pad 2) ?\s))
              (right (make-string (- pad (/ pad 2)) ?\s)))
          (case alignment
            (:right (concat left right str))
            (:center (concat left str right))
            (t (concat str left right))))
      (concat (substring str 0 (- width 3)) "..."))))

;; --------------------

(provide 'itimer)

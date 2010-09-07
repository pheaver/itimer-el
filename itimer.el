;;; itimer.el -- list and operate on emacs timers

;; Copyright (C) 2009, 2010 Philip Weaver

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
;;     e.g. an option like ibuffer-use-other-window,
;;     or customizable exit action such as restore window configuration
;; timer modification:
;;   - create timer
;;   - delete timer
;;   - change timer--time
;;   - change timer--function

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

(defconst timer-list-buffer-name " * timer-list*"
  "Name of buffer to display the timer list")

(defvar itimer-mode-map nil)

(defun timer-setup-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "p") #'previous-timer)
    (define-key map (kbd "n") #'next-timer)
    (define-key map (kbd "g") 'itimer-update)
    (setq itimer-mode-map map)))

(defun next-timer () (interactive) (forward-line 3))
(defun previous-timer () (interactive) (forward-line -3))

;;(add-hook 'itimer-mode-hook 'timer-key-setup)

(defun itimer-mode ()
  (kill-all-local-variables)
  (timer-setup-map)
  (use-local-map itimer-mode-map)
  (setq major-mode 'itimer-mode
        mode-name "Timer")
  (set (make-local-variable 'revert-buffer-function) #'itimer-update)
  (run-mode-hooks 'itimer-mode-hook)
  )

(defun itimer-update (arg &optional silent)
  (interactive "P")
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

;; TODO
;;   prettier output, use columns, fixed width
;;   customizable: column width
;;   customizable: display stop time or remaining time
(defun itimer-format-timer (timer)
  (concat
   (format-time-string "%c" (timer--time timer))
   "\n\t"
   (format-seconds "%Y, %D, %H, %M, %z%S"
                   (float-time
                    (time-subtract (timer--time timer) (current-time))))
   "\n\t"
   (symbol-name (timer--function timer))
   " "
   (format "%S" (timer--args timer))
   ))

(defun itimer-list-timers (&optional no-select)
  (interactive "P")
  (let ((buf (get-buffer-create timer-list-buffer-name)))
    (if no-select
        (display-buffer buf)
      (pop-to-buffer buf))

    (with-current-buffer buf
      (when (not (eq major-mode 'itimer-mode))
        (itimer-mode))
      (itimer-update nil))))

(provide 'itimer)

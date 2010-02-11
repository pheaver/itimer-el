;;; itimer.el -- list and operate on emacs timers

;; Copyright (C) ??

;; Author: Philip Weaver <philip.weaver@gmail.com>
;; Maintainer: Philip Weaver <philip.weaver@gmail.com>
;; Created: December 2009
;; Keywords:

;;; Commentary:

;; TODO
;; menu items
;; sort by time
;; rename to something spiffy, like "iTimer"
;; customizable exit action, such as restore window configuration
;; support canceling timers
;; support editing timer function
;; support editing timer time, including adding/substracting relative time
;; prettier output, modeled after list-processes and ibuffer
;; colorize
;; activate timer
;; display idle timer list, also

;; note to self, here are some important functions:
;; timer--activate 	timer--args
;; timer--function 	timer--high-seconds
;; timer--idle-delay 	timer--low-seconds
;; timer--repeat-delay 	timer--time
;; timer--time-less-p 	timer--triggered
;; timer--usecs 	timer-activate
;; timer-activate-when-idle 	timer-create
;; timer-duration 	timer-event-handler
;; timer-inc-time 	timer-next-integral-multiple-of-time
;; timer-relative-time 	timer-set-function
;; timer-set-idle-time 	timer-set-time
;; timer-set-time-with-usecs 	timer-until

;; (timer-duration string)
;; return number of seconds specified by string

;; current-time
;; (sechigh seclow usec)

;; format-time-string

;; timer-until  
;; negative if timer will run in future?

;;(define-derived-mode itimer-mode fundamental-mode "Timer")

(defvar timer-list-buffer-name " *Timer List*"
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

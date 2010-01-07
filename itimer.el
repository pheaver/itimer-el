;;; itimer.el -- list and operate on emacs timers

;; Copyright (C) ??

;; Author: Philip Weaver <philip.weaver@gmail.com>
;; Maintainer: Philip Weaver <philip.weaver@gmail.com>
;; Created: December 2009
;; Keywords:

;;; Commentary:

;; TODO
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

(define-derived-mode itimer-mode fundamental-mode "Timer")

(defvar timer-list-buffer-name " *Timer List*"
  "Name of buffer to display the timer list")

(defvar itimer-mode-map nil)

(defun timer-setup-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "p") #'previous-timer)
    (define-key map (kbd "n") #'next-timer)
    (define-key map (kbd "g") 'revert-buffer)
    (setq itimer-mode-map map)))

(defun next-timer () (interactive) (next-line 3))
(defun previous-timer () (interactive) (previous-line 3))

;;(add-hook 'itimer-mode-hook 'timer-key-setup)

(defun itimer-mode ()
  (kill-all-local-variables)
  (timer-setup-map)
  (use-local-map itimer-mode-map)
  (setq major-mode 'itimer-mode
        mode-name "Timer")
  (set (make-local-variable 'revert-buffer-function) #'timer-update)
  (run-mode-hooks 'itimer-mode-hook)
  )

(defun timer-update (arg &optional silent)
  (interactive "P")
  (let ((line (line-number-at-pos (point))))
    (unless silent
      (message "Updating timer list..."))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq ts timer-list)
    (while (not (null ts))
      (insert (format-time-string "%c" (timer--time (car ts))))
      (insert "\n\t")
      (insert (format-seconds "%Y, %D, %H, %M, %z%S"
               (time-to-seconds
                (time-subtract (timer--time (car ts)) (current-time)))))
      (insert "\n\t")
      (insert (symbol-name (timer--function (car ts))))
      (insert " ")
      (insert (format "%S" (timer--args (car ts))))
      (newline)
      (pop ts))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line (1- line))))
 
(defun list-timers (&optional no-select)
  (interactive "P")
  (let ((buf (get-buffer-create timer-list-buffer-name)))
    (if no-select
        (display-buffer buf)
      (pop-to-buffer buf))

    (with-current-buffer buf
      (when (not (eq major-mode 'itimer-mode))
        (itimer-mode))
      (timer-update nil))))

(provide 'itimer)
;; Terror awaits...

(require 'cl-lib)

(message "Enabling Emacs hard-mode...")
(mapatoms (lambda (a)
            (when (and (fboundp a)
                       (= (random (+ 100 (random 6000))) 90))
	      (message "I hope you liked %s!" a)
              (defalias a 'ignore))))

(set-face-attribute 'default nil :font "Liberation Serif" :weight 'light :height 300 :foreground "white" :background "red3")
(set-face-attribute 'mode-line nil :weight 'light :height 500 :foreground "purple" :background "yellow")
(set-face-attribute 'region nil :weight 'light :height 300 :foreground nil :background "red3")

(defun random-color ()
  (let ((red (random 256))
        (blue (random 256))
        (green (random 256)))
    (format "#%02x%02x%02x" red blue green)))

(defun random-color-face (&optional face frame)
  (set-face-attribute 'mode-line frame :foreground (random-color) :background (random-color))
  (set-face-attribute 'font-lock-builtin-face frame :foreground (random-color))
  (set-face-attribute 'font-lock-function-name-face frame :foreground (random-color))
  (set-face-attribute 'font-lock-keyword-face frame :foreground (random-color)))

(add-hook 'post-command-hook 'random-color-face)

(defun morse-my-buffer ()
  (interactive)
  (morse-region
   (point-min)
   (point-max)))

(defun rot13-my-buffer ()
  (interactive)
  (rot13-region
   (point-min)
   (point-max)))

(setq functions-of-typing-hell
      '[morse-my-buffer
	rot13-my-buffer])

(defun resize-mode-line ()
  (set-face-attribute 'mode-line nil :height (+ 100 (random 600))))

(defun ransom-text ()
  (cl-loop for x from 1 to (buffer-size) do
           (put-text-property x (+ x 1) 'font-lock-face `(:height ,(+ 200 (random 200))))))

(setq functions-of-idle-hell
      '[resize-mode-line
	zone
	ransom-text])

(defun engine-of-hell (frequency funcs)
    (let* ((idx (random frequency))
  	   (func-to-call (when (< idx (length funcs)) (aref funcs idx))))
    (when func-to-call  (funcall func-to-call))))

(defun haunt-me-plenty ()
  (interactive)
  (engine-of-hell 50 functions-of-typing-hell))

(defun haunt-me-randomly ()
  (interactive)
  (engine-of-hell 10 functions-of-idle-hell))

(defun haunt-every-command ())
  
;; Things to run on idle
(setq random-hell-timer (run-at-time 5 t #'haunt-me-randomly))
(setq random-idle-timer (run-with-idle-timer 1 t #'haunt-me-randomly))

;; Things to run every time a key is pressed
(add-hook 'post-self-insert-hook #'haunt-me-plenty)


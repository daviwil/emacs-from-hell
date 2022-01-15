;;; hell.el --- Emacs from Hell -*- lexical-binding: t; -*-

;;; Commentary:

;; Inspired by the System Crafter live stream 2022-01-14, titled "Creating the
;; World's WORST Emacs Configuration:" https://youtu.be/L4NaIUqx8fw
;; https://youtu.be/IDFm4y6KLks

;; This package implements `hell-mode', which will turn your (or a victim's)
;; Emacs into the Emacs from Hell.  There are a variety of changes, from the
;; purely aesthetic to the downright destructive.  Read the source code for
;; more.

;;; Code:

;;; Customization options

(defgroup hell nil
  "Tweaks to making Emacs Hell."
  :group 'emacs
  :prefix "hell-")

(defcustom hell-text-height-minimum 100
  "The minimum height for text."
  :type 'integer)

(defcustom hell-text-height-maximum 600
  "The maximum height for text."
  :type 'integer)

(defcustom hell-ignore-safe-commands '(kill-emacs
                                       hell-mode
                                       hell-exit-hell)
  "Commands that are safe from `hell-ignore-commands'."
  :type '(repeat function))

(defcustom hell-ignore-command-probability 6000
  "Inverse of the probability to ignore a command.
Every `hell-ignore-command-probability' commands will, on
average, be ignored.")

(defcustom hell-ignore-function-probability 8000
  "Inverse of the probability to ignore a function.
Every `hell-ignore-functionf-probability' functions will, on
average, be ignored.")

(defcustom hell-ignore-safe-functions '(hell-message
                                        hell-random-color
                                        hell-random-text-height
                                        hell-random-buffer
                                        hell--filter-atoms
                                        hell--random-hit-p
                                        hell-ignore-commands
                                        hell-ignore-functions
                                        hell-no-or-yes-p
                                        hell-exit-hell
                                        hell-run-timer
                                        hell-ignore
                                        random
                                        hell-random-number)
  "Functions that are safe from `hell-ignore-functions'."
  :type '(repeat function))

(defcustom hell-idle-functions '(zone)
  "Functions to randomly call idly."
  :type '(repeat function))

(defcustom hell-typing-functions '(hell-morse-buffer
                                   hell-rot13-buffer)
  "Functions to randomly fire when typing."
  :type '(repeat function))

;;; Internal variables

(defvar hell--saved nil
  "Alist of things to return to normal when exiting hell.")

(defvar hell--timers nil
  "Alist of timers set by hell.")

(defvar hell--hooks nil
  "Alist of hooks set by hell.")

;;; Helper functions

(defun hell-random-number (&optional max min)
  "Return a random number between MIN and MAX.
The order of MIN and MAX doesn't matter."
  (cond ((and (null min) (null max)) (random t))
        ((null min) (random max))
        (t (+ (min max min) (random (- (max min max) (min max min)))))))

(defun hell-message (format &rest args)
  "Display a message, Hell-style.
FORMAT and ARGS are passed to `message'."
  (message "😈 %s" (apply #'format format args)))

(defun hell-random-color ()
  "Return a random color in HTML format."
  (let ((red (random 256))
        (blue (random 256))
        (green (random 256)))
    (format "#%02x%02x%02x" red blue green)))

(defun hell-random-text-height ()
  "Return a random height acceptable for text."
  (+ hell-text-height-minimum (random (- hell-text-height-maximum
                                         hell-text-height-minimum))))

(defun hell-theme ()
  "Theme Emacs randomlly."
  (set-face-attribute 'mode-line nil
                      :foreground (hell-random-color)
                      :background (hell-random-color)
                      :height (hell-random-text-height))
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground (random-color))
  (set-face-attribute 'font-lock-function-name-face nil
                      :background (random-color))
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground (random-color)))

(defun hell-morse-buffer ()
  "Morse-code the buffer."
  (interactive)
  (morse-region (point-min) (point-max)))

(defun hell-rot13-buffer ()
  "Rot-13 the buffer."
  (interactive)
  (rot13-region (point-min) (point-max)))

(defun hell-random-buffer (&optional frame)
  "Return a random buffer from FRAME."
  (let ((bl (seq-remove #'minibufferp (buffer-list frame))))
    (nth (random (length bl)) bl)))

(defun hell--filter-atoms (pred &optional obarr)
  "Return all atoms in obarray OBARR that match PRED.
PRED should be a function that takes one atom and returns nil or
non-nil."
  (let (ret)
    (mapatoms (lambda (a) (when (funcall pred a) (push a ret))) obarr)
    ret))

(defun hell--random-hit-p (probability)
  "Return t 1/PROBABILITY of the time, else nil."
  (zerop (mod (random t) probability)))

(defun hell-ignore-commands ()
  "Alias random commands to `ignore'.
Don't ignore those commands in `hell-ignore-safe-commands'."
  (dolist (a (hell--filter-atoms #'commandp))
    (when (hell--random-hit-p hell-ignore-command-probability)
      (hell-message "Say goodbye to %s!" a)
      )))

(defun hell-ignore-functions ()
  "Alias random functions to `ignore'.
Don't ignore those functions in `hell-ignore-safe-functions'."
  (dolist (a (hell--filter-atoms #'functionp))
    (when (and (not (commandp a))
               (hell--random-hit-p hell-ignore-function-probability))
      (hell-message "Say goodbye to %s!" a)
      (defalias a 'ignore))))

(defun hell-no-or-yes-p (prompt &optional prob)
  "PROMPT the user for a yes-or-no answer, but flip the answer.
If optional PROB is passed, only flip the answer 1/PROB times."
  (funcall (if (and prob (hell--random-hit-p prob)) #'not #'identity)
           (yes-or-no-p prompt)))

(defun hell-exit-hell ()
  "Exit `hell-mode'."
  (interactive)
  (when (and (hell-no-or-yes-p "Are you sure you want to exit Hell? " 4)
             ;; other tests here ...
             )
    (hell-mode -1)))

(defun hell-run-timer (idle time repeat function &rest args)
  "Perform an action at time TIME.
TIME, REPEAT, FUNCTION, and ARGS are all passed to `run-at-time',
or `run-with-idle-timer' if IDLE is non-nil.  The timer object is
saved in `hell--timers' for easy cancellation."
  (push (apply (if idle #'run-with-idle-timer #'run-at-time)
               time repeat function args)
        hell--timers))

(defun hell-ignore (fn &optional actually)
  "Alias FN to ignore.
This actually advises FN with :override ignore, for easier
undoing, unless ACTUALLY is non-nil.  Then it's really aliased to
ignore."
  (if actually
      (defalias fn #'ignore)
    (push fn hell--saved)
    (advice-add fn :override #'ignore)))

(defun hell-add-hook (fn &rest hooks)
  "Add FN to HOOKS."
  (dolist (hook hooks)
    (push (list hook fn) hell--hooks)
    (add-hook hook fn -90)))

;;; Minor mode

(define-minor-mode hell-mode
  "A mode of Hell."
  :lighter " 😈"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "heaven" #'hell-exit-hell)
            map)
  (if hell-mode
      (progn
        (add-hook 'post-self-insert-hook 'hell-theme)
        (dolist (fn hell-idle-functions)
          (hell-run-timer t (hell-random-number 10 60) t fn)))
    (progn
      (dolist (hookfn hell--hooks)
        (apply #'remove-hook hookfn))
      (dolist (timer hell--timers)
        (cancel-timer timer))
      (dolist (fn hell--saved)
        (advice-remove fn #'ignore)))))

(provide 'hell)
;;; hell.el ends here

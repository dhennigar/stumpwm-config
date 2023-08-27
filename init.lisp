;;; init.lisp --- stumpwm init file

;;; Commentary:

;; My personal configuration for the StumpWM X window manager.

;;; Code:

(in-package :stumpwm)

;; -----------------------------------------------------------------------------
;; general settings

(setf
 *mouse-focus-policy* :click
 *run-or-raise-all-groups* t
 *run-or-raise-all-screens* t)

;; -----------------------------------------------------------------------------
;; key bindings

(set-prefix-key (kbd "C-z"))

;; -----------------------------------------------------------------------------
;; applications

(defcommand chrome ()()
  (run-or-raise "flatpak run --file-forwarding com.google.Chrome" '(:class "Google-chrome")))
(define-key *root-map* (kbd "b") "chrome")

(define-key *top-map* (kbd "M-Tab") "next")
(define-key *top-map* (kbd "M-S-Tab") "prev")

;; -----------------------------------------------------------------------------
;; mode line

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))

(setf *screen-mode-line-format*
      (list "%g | %v ^>" '(:eval (run-shell-command "date '+%k:%M'" t))))

;; -----------------------------------------------------------------------------
;; window rules

(clear-window-placement-rules) ; clear all window placement rules

;;; init.lisp ends here

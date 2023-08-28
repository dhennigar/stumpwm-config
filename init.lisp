;;; init.lisp --- stumpwm init file

;;; Commentary:

;; My personal configuration for the StumpWM X window manager.

;;; Code:

(in-package :stumpwm)

;; -----------------------------------------------------------------------------
;; general settings

(setf
 *mouse-focus-policy* :click
 *run-or-raise-all-groups* t   ; search across groups for run-or-raise
 *run-or-raise-all-screens* t) ; search across screens for run-or-raise

(set-prefix-key (kbd "C-z"))


;; -----------------------------------------------------------------------------
;; Launch applications

;; Jump to Chrome if it exists, else launch it.
(defcommand chrome ()()
  (run-or-raise "flatpak run --file-forwarding com.google.Chrome" '(:class "Google-chrome")))
(define-key *root-map* (kbd "b") "chrome")

(define-key *top-map* (kbd "M-Tab") "next")
(define-key *top-map* (kbd "M-S-Tab") "prev")


;; -----------------------------------------------------------------------------
;; Fonts
;; Note: TrueType fonts are basically not supported at all.

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "Noto Mono"
			 :subfamily "Regular" :size 10))


;; -----------------------------------------------------------------------------
;; Mode-line

(setf *group-format* "%t")
(setf *window-format "%n: %20t")
(setf *time-modeline-string* "%k:%M")

;(setf *screen-mode-line-format*
;    (list "%g | %v ^>" '(:eval (run-shell-command "date '+%k:%M'" t))))

(setf *screen-mode-line-format*
      (list "%n: %v ^> %d"))

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))


;; -----------------------------------------------------------------------------
;; Groups

(when *initializing*
  (grename "Workspace 1")
  (gnew "Workspace 2")
  (gnew "Workspace 3")
  (gnew "Emacs")
  (gselect "Workspace 1"))


;; -----------------------------------------------------------------------------
;; Window placement rules

(clear-window-placement-rules) ; Clear all window placement rules

;; Place emacs in its own group and jump to it
(define-frame-preference "Emacs" (1 t t :class "Emacs"))

;; I will probably want to configure R's graphics windows here.

;;; init.lisp ends here

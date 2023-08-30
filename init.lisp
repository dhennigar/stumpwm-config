;;; init.lisp --- stumpwm init file

;;; Commentary:

;; My personal configuration for the StumpWM X window manager.

;;; Code:

(in-package :stumpwm)

;; -----------------------------------------------------------------------------
;; General

(setf
 *mouse-focus-policy* :click
 *run-or-raise-all-groups* t   ; search across groups for run-or-raise
 *run-or-raise-all-screens* t) ; search across screens for run-or-raise


;; -----------------------------------------------------------------------------
;; Application shortcuts

;; Jump to Chrome if it exists, else launch it.
(defcommand chrome
    ()()
    (run-or-raise "flatpak run --file-forwarding com.google.Chrome"
		  '(:class "Google-chrome")))
(define-key *root-map* (kbd "b") "chrome")


;; -----------------------------------------------------------------------------
;; Fonts

(ql:quickload :clx-truetype) ; note that clx-truetype must be obtained from a repo
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "Noto Mono"
			 :subfamily "Regular" :size 10))


;; -----------------------------------------------------------------------------
;; Mode-line

(load-module "battery-portable") ; provides the %B formatter

(setf *window-format* "%n:%s%c")
(setf *time-modeline-string* "%k:%M")
(setf *screen-mode-line-format*
      (list "%n: " "%v" "^> %B %d"))

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))


;; -----------------------------------------------------------------------------
;; Keybindings

(set-prefix-key (kbd "C-z"))

;; Top-level switching
(define-key *top-map* (kbd "M-Tab") "pull-hidden-next")
(define-key *top-map* (kbd "M-S-Tab") "pull-hidden-prev")
(define-key *top-map* (kbd "C-M-Tab") "gnext")
(define-key *top-map* (kbd "C-M-S-Tab") "gprev")

;; Windows Compat
(define-key *top-map* (kbd "C-s-d") "gnew")
(define-key *top-map* (kbd "C-s-Right") "gnext")
(define-key *top-map* (kbd "C-s-Left") "gprev")

;; Media
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pamixer -i 2")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pamixer -d 2")
(define-key *top-map* (kbd "XF86AudioMute")        "exec pamixer -t")

;; -----------------------------------------------------------------------------
;; Groups

;; Default groups
(when *initializing*
  (grename "Group 1")
  (gnew "Group 2")
  (gnew "Group 3")
  (gselect "Group 1"))


;; -----------------------------------------------------------------------------
;; Swank

(require 'swank)

(defcommand swank ()()
	"Start a swank server."
	(message "Swank server started on port 4004")
	(swank:create-server :port 4004))

(define-key *root-map* (kbd "C-s") "swank")


;;; init.lisp ends here

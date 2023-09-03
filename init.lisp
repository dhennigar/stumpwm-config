;;; init.lisp --- stumpwm init file

;;; Commentary:

;; My personal configuration for the StumpWM X window manager.

;;; Code:

(in-package :stumpwm)

;; -----------------------------------------------------------------------------
;; X initialization

(run-shell-command "autorandr --force")
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "xrdb -merge .Xresources")
(run-shell-command "xset s 600")
(run-shell-command "xss-lock slock &")


;; -----------------------------------------------------------------------------
;; General

(setf
 *message-window-gravity* :center
 *input-window-gravity* :center
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
(define-key *root-map* (kbd "w") "chrome")


;; -----------------------------------------------------------------------------
;; Fonts

(ql:quickload :clx-truetype) ; note that clx-truetype must be obtained from a repo
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "Noto Mono"
			 :subfamily "Regular" :size 10))


;; -----------------------------------------------------------------------------
;; Mode-line

(load-module "battery-portable") ; provides the %B formatter


(setf *window-format* "%n:%s%c"
      *time-modeline-string* "%k:%M"
      *mode-line-position* :bottom
      *mode-line-border-width* 0
      *mode-line-pad-x* 5
      *mode-line-pad-y* 2
      *mode-line-background-color* 'black)
(setf *screen-mode-line-format*
      (list "%n: " "%v" "^> %B %d"))

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))


;; -----------------------------------------------------------------------------
;; Keybindings

(set-prefix-key (kbd "C-z"))

;; remove redundant bindings
(undefine-key *root-map* (kbd "s"))
(undefine-key *root-map* (kbd "S"))

;; emacs-like navigation
(define-key *root-map* (kbd "b") "select-window")
(define-key *root-map* (kbd "0") "remove-split")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

;; no need for an exchange map
(undefine-key *root-map* (kbd "x"))
(setf *exchange-window-map* nil)
(define-key *root-map* (kbd "S-Left") "exchange-direction left")
(define-key *root-map* (kbd "S-Right") "exchange-direction right")
(define-key *root-map* (kbd "S-Up") "exchange-direction up")
(define-key *root-map* (kbd "S-Down") "exchange-direction down")

;; Media
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pamixer -i 2")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pamixer -d 2")
(define-key *top-map* (kbd "XF86AudioMute")        "exec pamixer -t")


;; -----------------------------------------------------------------------------
;; Groups

;; Default groups
(when *initializing*
	(setf (group-name (car (screen-groups (current-screen))))
      		"Group1")
	(gnewbg "Group2")
	(gnewbg "Group3"))


;; -----------------------------------------------------------------------------
;; Swank

(require 'swank)

(swank-loader:init)

(defcommand swank ()()
	"Start a swank server."
	(message "Swank server started on port 4004")
  (swank:create-server :port 4004
		       :style swank:*communication-style*
		       :dont-close t))

;;; init.lisp ends here

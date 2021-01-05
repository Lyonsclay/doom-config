;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Clay Morton"
      user-mail-address "lyonsclay@yahoo.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; (setq doom-font (font-spec :family "Source Code Pro" :size 14 :weight 'normal)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 14))
(setq doom-font (font-spec :family "Fira Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      doom-big-font (font-spec :family "Fira Mono" :size 19))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))


(require 'key-chord)
(key-chord-mode t)
(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-define-global "JK" 'evil-normal-state)

;; in elisp file like `this'
;; in other files like `this` 😜
(after! smartparens
  (sp-pair "`" "`" :wrap "M-`")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "<" ">" :wrap "M-<")
  (sp-pair "'" "'" :wrap "M-'")
  (sp-pair "\"" "\"" :wrap "M-\""))

(map! "M-)" #'sp-unwrap-sexp)

;; multiple cursor quick edit commands
(map!
 "C-?" #'mc/mark-all-like-this
 "C->" #'mc/mark-next-like-this
 "C-<" #'mc/mark-previous-like-this)

(map! :leader "SPC" #'counsel-M-x)
(map! :leader "t t" #'treemacs)

(map! :localleader
      :map go-mode-map
      "x x" #'go-run)

(map! :localleader
      :map python-mode-map
      "p" #'pipenv-activate
      "r" #'+python/open-ipython-repl)

(map! :localleader
      :map lisp-mode-map
  "f" #'indent-pp-sexp)

(map! :leader
      "0" #'treemacs-window
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9
      "w a" #'ace-swap-window)

(map! :leader
      "v" #'toggle-presentation
      "l" #'toggle-log-keys
      "y" #'copy-path)

(defun presentation ()
  "Global command-log mode and buffer setup."
  (interactive)
  (toggle-frame-fullscreen)
  (set-frame-size (selected-frame) 210 60)
  (doom-big-font-mode)
  ;; (setq command-log-mode-window-size 185)
  (toggle-log-keys)
  (treemacs-window)
)

(defun working ()
  "This will hopefully reset the choices from presentation mode."
  (interactive)
  (message "get working 🏃🏻 💻 💰")
  (toggle-frame-fullscreen)
  (toggle-log-keys)
  (doom-big-font-mode 0))


(defun toggle-presentation ()
  "Switch between presentation and working views. This will set the variable presentation-view and avert any error when undefined."
  (interactive)
  (condition-case nil
  (if presentation-view
      (progn (working) (setq presentation-view nil))
    (progn (presentation) (setq presentation-view t))
    )
  (error (progn (presentation) (setq presentation-view t))))
)

;; Got this from the internet -
;; - an issue in keycast repo -
;; someone pasted this code specific to working in doom.
(after! keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast-mode-line-update t)
      (remove-hook 'pre-command-hook 'keycast-mode-line-update))))
(add-to-list 'global-mode-string '("" mode-line-keycast))

(defun toggle-log-keys ()
  "Send all keystrokes to dedicated buffer."
  (interactive)
  (command-log-mode)
  (global-command-log-mode)
  (clm/toggle-command-log-buffer)
  )

;; Is this the best place for this declaration? 🌛
;; It would be better if this was dynamic based on longest project name.
;; (setq treemacs-width 35)
;; 🌶🌶🌶
(defun treemacs-window ()
  "Switch to treemacs; start if not already running."
  (interactive)

  ;; If treemacs has not been run in this session we will
  ;; fire it up. See below...
  (unless (fboundp 'treemacs-current-visibility)
    (progn (message "treemacs is 🔥 ready to go 🦎")
           (treemacs)))
  ;; Need to guard against 'treemacs-current-visibility not being defined,
  ;; which is only exposed after the first time treemacs is called.
  ;;
  ;; 'condition-case is equivalent to try/catch in other languages.
  (condition-case nil
      (if (string= (treemacs-current-visibility) "visible")
        (evil-window-top-left)
        (treemacs))
    (error nil)))

;; ﷽
(defun go-run ()
  "Compile and run buffer."
  (interactive)

  ;; (let (filename-callee (buffer-file-name)))
  ;; (message filename-callee)
  ;; (shell-command "touch '#go#run#session'")

  ;; (evil-delete-buffer "#go#run#session")
  (if (get-buffer "#go#run#session")(evil-delete-buffer "#go#run#session"))
  (generate-new-buffer "#go#run#session")

  ;; (buffer-disable-undo "#go#run#session")
  ;; (get-buffer "#go#run#session")
  ;; (unless nil (error "Whoops!"))
  (shell-command (format "go run %s > '#go#run#session'" (buffer-file-name)))
  (defvar go-run-path (concat (file-name-directory buffer-file-name) "#go#run#session"))
  (display-buffer
   (find-file-noselect go-run-path)
   ;; '((display-buffer-below-selected display-buffer-at-bottom)
   '(
     (display-buffer-reuse-window)
     ;; (display-buffer-below-selected)
     (display-buffer-in-direction "right")
     ;; alist - '(right)
     ;; (window-height . fit-window-to-buffer)
     ))
  )

;; List of all key bindings for current buffer/mode.
;; (counsel--descbinds-cands)
;;

(defun copy-path ()
  "Copy current file path to kill ring and os/gui clipboard."
  (interactive)
  (kill-new buffer-file-name)
  (insert buffer-file-name))

;; Start in fullscreen mode.
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;; NOTE with the brew railwaycat/emacs-app install
;; the ns-use-native-fullscreen flag has no effect
;; ;; (setq ns-use-native-fullscreen nil)

;; This package works nicely! I am currently trying to forgo this particular tool and rely on
;; built in auto indenting. But still there is a use case and I might go back. 🏃
;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; Using after! instead of mode-hook avoids calling the window size function more than once 🚅
;; (add-hook 'command-log-mode-hook #'(lambda () (setq command-log-mode-window-size 60)))
(after! command-log-mode
  (setq command-log-mode-window-size 50))

;; .venv directory contains all imported packages so it should not be watched by lsp.
(add-hook 'lsp-mode-hook #'(lambda () (push "/\\.venv$" lsp-file-watch-ignored)))
;; (redisplay)
;; (setq mytimer (run-with-timer 10 20 'bleep))

;; (defun bleep ()
;;   "A simple message notification to test mytimer :)"
;;   (message "haiku maiku when I went to raiku -- in the rain."))
;
;;;

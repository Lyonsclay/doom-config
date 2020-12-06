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

;; (defun save-all ()
;; (interactive)
;; (save-some-buffers t))

;; (add-hook 'focus-out-hook 'save-all)
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

(require 'key-chord)
(key-chord-mode t)
(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-define-global "JK" 'evil-normal-state)

;; (windmove-default-keybindings 'super)
;; (progn
;; ;; automatically save buffers associated with files on buffer switch
;; ;; and on windows switch
;;         (defadvice switch-to-buffer (before save-buffer-now activate)
;;         (when buffer-file-name (save-buffer)))
;;         (defadvice other-window (before other-window-now activate)
;;         (when buffer-file-name (save-buffer)))
;;         (defadvice windmove-up (before other-window-now activate)
;;         (when buffer-file-name (save-buffer)))
;;         (defadvice windmove-down (before other-window-now activate)
;;         (when buffer-file-name (save-buffer)))
;;         (defadvice windmove-left (before other-window-now activate)
;;         (when buffer-file-name (save-buffer)))
;;         (defadvice windmove-right (before other-window-now activate)
;;         (when buffer-file-name (save-buffer)))
;; )

;; in elisp file like `this'
;; in other files like `this` :smile:
(after! smartparens
  (sp-pair "`" "`" :wrap "M-`"))

;; (global-set-key (kbd "M-{") 'insert-pair)
;; (global-set-key (kbd "M-[") 'insert-pair)
;; (global-set-key (kbd "M-\"") 'insert-pair)
;; (global-set-key (kbd "M-'") 'insert-pair)
;; (global-set-key (kbd "M-(") 'insert-pair)
;; TODO doesn't work result has single quote at end like `word'
;; (global-set-key (kbd "M-`") 'insert-pair)
;; (global-set-key (kbd "M-<") 'insert-pair)
;; (global-set-key (kbd "M-)") 'delete-pair)

;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;; (setq ns-use-native-fullscreen t)
;; make electric-pair-mode work on more brackets
;; (setq electric-pair-pairs '(
;;                              (?\" . ?\")
;;                             (?\{ . ?\})
;;                             ) )
(map! :leader "SPC" #'counsel-M-x)
(map! :leader "t t" #'treemacs)
;;
;;
;; (add-hook! 'go-mode-hook
;;            (map! :localleader "x x" #'go-run))
(map! :localleader
      :map go-mode-map
      "x x" #'go-run)

;; (add-hook! 'python-mode-hook
;;            (map! :localleader "p" #'pipenv-activate))

;; (add-hook! 'python-mode-hook
;;   (map!
;;    :localleader "p" #'pipenv-activate
;;  :localleader "r" #'+python/open-ipython-repl))
;;

(map! :localleader
      :map python-mode-map
      "p" #'pipenv-activate
      "r" #'+python/open-ipython-repl)

;; TODO add mode specific commands for python
;; (map! :leader "m s" python-)
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

(defun treemacs-window ()
      "switch to treemacs; start if not already"
      (interactive)
      (if  (string= (treemacs-current-visibility) "visible")
      (evil-window-move-far-left)
      (treemacs)))

(defun go-run ()
  "go run"
  (interactive)
  (shell-command (format "go run %s" (buffer-file-name))))

(defun mountain ()
  "Shifting"
  (message "me is good"))

;;
;;;

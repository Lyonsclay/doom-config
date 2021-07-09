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
;; - `add-load-path!' for adding directories to the `loadEWath', relative to
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


;; ----------------------------------------------------------------------------
;; CUSTOM CODE BELOW THIS POINT.
;; MIT @COPYRIGHT
;; USE AT YOUR LEISURE!
;; ----------------------------------------------------------------------------


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
      "w a" #'ace-swap-window
      "w w" #'ace-delete-window)


;; multiple cursor quick edit commands
(map!
 "C-/" #'mc/mark-all-like-this
 "C->" #'mc/mark-next-like-this
 "C-<" #'mc/mark-previous-like-this
 "M-)" #'sp-unwrap-sexp)


(map! :leader
      "SPC" #'counsel-M-x
      "t t" #'treemacs
      "t s" #'toggle-center-scroll
      "y" #'copy-path)

(key-seq-define-global "qd" 'dired)

(key-seq-define text-mode-map "qf" 'flyspell-buffer)

;; key-chord responsiveness can vary; this can make the upcase and downcase
;; functionality particularly problematic. It might be worth investigating
;; alternative key mappings as this functionality is used less than jkpop(it' hard to type that lol)
(require 'key-chord)
(key-chord-mode t)
(key-seq-define-global "jk" 'jkpop)
(key-seq-define-global "JK" 'jkpop)
(key-seq-define-global "kj" 'jkpop)
(key-seq-define-global "JK" 'jkpop)
(key-chord-define-global "ii" 'caps-lock-mode)
(key-seq-define-global "iu" 'upcase-word)
(key-seq-define-global "oi" 'downcase-word)


;; RESTORE ctrl-D
(define-key evil-insert-state-map   (kbd "C-d") #'evil-delete-char)
(define-key evil-normal-state-map   (kbd "C-d") #'evil-delete-char)


;; in elisp file like `this'
;; in other files like `this` 😜
(after! smartparens
  (sp-pair "`" "`" :wrap "M-`")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "{" "}" :wrap "M-{")
  ;; (sp-pair "<" ">" :wrap "M-<")
  (sp-pair "'" "'" :wrap "M-'")
  (sp-pair "\"" "\"" :wrap "M-\""))

;; The unmapping of "spc m e" needs to happen after a go buffer is being visited, apparently.
(add-hook 'go-mode-hook (lambda ()
                          ;; I want to disable the go playground which is mapped to "spc m e".
                          ;; The go playground will send your code to the web -- bad!
                          (map! :localleader :map go-mode-map :nv "e" nil)
                          (map! :localleader :map go-mode-map "e e" #'go-run)
                          (map! :localleader :map go-mode-map "h d" #'go-guru-definition-other-window)
                          ))
(map! :localleader
      :map go-mode-map
      "f" #'gofmt
      "e e" #'go-run)

(map! :localleader
      :map python-mode-map
      "p" #'pipenv-env
      "r" #'ipython-startup-norm
      "f" #'pipenv-blackify)

(map! :localleader
      :map sql-mode-map
      "c" #'sql-connect
      "r" #'sql-set-sqli-buffer
      "e e" #'sql-send-paragraph
      "s" #'tws-region-to-process
      "v" #'vterm-sql-startup)

(map! :localleader
      :map emacs-lisp-mode-map
      "f" #'indent-pp-sexp)

;; --------------------------------------------------------------------------
;; --- global state management -------
;;
;;

;; **TODO we aren't using eglot currently but always worth checking out again.
;;
;; (after! eglot
;;   (setq eglot-connect-timeout 30)
;;   (setq jsonrpc-default-request-timeout 30))


;; **TODO Verify this is still an issue with the current nix compiled build.
;; flycheck is unusable otherwise -- process intensive -- slows typing
;;
(after! flycheck
(setq flycheck-check-syntax-automatically '(save)))

;; Emoji: 😄, 🤦, 🏴
;; How do we get kick ass emojis? It's an eternal quest.
;;
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)
;; sometimes any character change deletes the whole line
;; https://stackoverflow.com/a/3024055/2252501
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; ace-window is required by treemacs and certain variables are not defined if not loaded here(this is probably a temp work around)
(require 'ace-window)

;; startup is much quicker without loading saved history
(savehist-mode -1)

;; These two hooks should catch any shift of focus event and save the current buffer
;; or any unsaved buffer for that matter.
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))
(add-hook 'doom-switch-window-hook (lambda () (save-some-buffers t)))

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
  ;; 'condition-case' is being used like try/catch.
  (condition-case nil
      (if (string= (treemacs-current-visibility) "visible")
        (evil-window-top-left)
        (treemacs))
    (error nil)))

(defun jkpop ()
  "Prevent caps-lock-mode from being selected."
  (interactive)
  (if (fboundp 'treemacs-current-visibility)

      (condition-case nil
          (unless (eq treemacs--in-this-buffer t)
            (evil-normal-state)
            (if caps-lock-mode
                (progn (command-execute 'caps-lock-mode) (message "👼 CAPS-LOCK 👼")))
            )
        (error nil))
    (progn (evil-normal-state)
           (if caps-lock-mode
               (progn (command-execute 'caps-lock-mode)
                      (message "👼 CAPS-LOCK 👼"))))
    ))

;; (setq scroll-preserve-screen-position t
(defun toggle-center-scroll ()
  (interactive)
  (if (> scroll-margin 0) (center-scroll-off)(center-scroll-on)))

(defun center-scroll-on ()
  (interactive)
  (setq scroll-conservatively 0
        maximum-scroll-margin 0.5
        scroll-margin 99999)
  )

(defun center-scroll-off ()
  ;; Set the default values
  (interactive)
  (setq
      scroll-conservatively 101
      maximum-scroll-margin 0.25
      scroll-margin 0)
  )

;; List of all key bindings for current buffer/mode.
;; (counsel--descbinds-cands)
;;

(defun copy-path ()
  "Copy current file path to kill ring and os/gui clipboard."
  (interactive)
  (kill-new buffer-file-name)
  (insert buffer-file-name))

;; This is the best way to get full screen without menu bar and without osx transit animation.
(setq ns-auto-hide-menu-bar t)
(toggle-frame-maximized)
(toggle-frame-fullscreen)
(toggle-frame-fullscreen)

;; ------------------------------------------------------------------------
;; ---- mode specific functions ------------
;; (require 'web-mode)

;; https://www.reddit.com/r/emacs/comments/cztjdl/tsx_setup/ez8m3mc?utm_source=share&utm_medium=web2x&context=3
;; (use-package web-mode
;;   :ensure t
;;   :mode
;;   ;; ("\\.ejs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")
;;   ("\\.tsx?\\'")
;;   :config
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-script-padding 2)
;;   (setq web-mode-block-padding 2)
;;   (setq web-mode-style-padding 2)
;;   (setq web-mode-enable-auto-pairing t)
;;   (setq web-mode-enable-auto-closing t)
;;   (setq web-mode-enable-current-element-highlight t))

;; **TODO Does this happen by default now?
;;
;; .venv directory contains all imported packages so it should not be watched by lsp.
;; (add-hook 'lsp-mode-hook #'(lambda () (push "/\\.venv$" lsp-file-watch-ignored)))

;; Leaving this here for reference to some potentially useful variables.
;;
;; '(python-check-command "./__pypackages__/*/bin/pyflake")
;; '(python-pytest-executable "pipenv run pytest -vv")
;; '(python-shell-extra-pythonpaths '("./__pypackages__/3.8/lib/"))
;; '(python-shell-interpreter-args "-i")
;; '(pyvenv-virtualenvwrapper-python "/Users/clay/.pyenv/shims/python")
;; '(treemacs-python-executable "python")


;; (setq python-shell-interpreter "~/.pyenv/shims/python" flycheck-python-pycompile-executable "~/.pyenv/shims/python")
(setq python-pytest-executable "pytest -vv")

(defun pipenv-blackify ()
  "Format python code"
  (interactive)
  (save-buffer (current-buffer))
  (message (concat "   🥁 pipenv run black 🥝    " (buffer-file-name)))
  (shell-command (concat "PIPENV_VERBOSITY=-1 pipenv run black " (buffer-file-name)))
  ;; (save-buffer (current-buffer))
  (revert-buffer (current-buffer))
  )

;; Ahh pdm; everything worked so well!
;;
;; (after! python
;;   (setq python-shell-interpreter "pdm"
;;         python-shell-interpreter-args "run ipython --simple-prompt"
;;       +python-ipython-command '("pdm" "run" "ipython" "-i" "--simple-prompt" "--no-color-info")
;;       python-shell-prompt-detect-failure-warning nil)
;;   ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
;;   )


;; **TODO review is this still necessary.
(after! python
  (setq python-ipython-command "$(which python) -m ipython -i --simple-prompt --no-color-info"
        ))

(defun ipython-startup-norm ()
  "Set up ipython with hot reloading of code."
  (interactive)

  (+python/open-ipython-repl)
  (process-send-string (get-buffer-process (current-buffer)) "%load_ext autoreload \n %autoreload 2 \n"))

(defun ipython-startup ()
  "Initiate ipython from current buffer with shell command."
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1)
  (+vterm/here default-directory)
  (process-send-string (get-buffer-process (current-buffer)) "pdm run ipython -i --simple-prompt \n")
  (process-send-string (get-buffer-process (current-buffer)) "%load_ext autoreload \n %autoreload 2 \n"))

;; The main reason to use this is to access numpy and pytorch.
;;
(defun conda-repl ()
  "Open an IPython REPL."
  (interactive)
  (require 'python)
  (shell-command "conda init zsh && conda activate")
  (let ((python-shell-interpreter "/Users/clay/miniconda3/bin/ipython")
        (python-shell-interpreter-args
         (string-join (cdr +python-ipython-command) " ")))
    (+python/open-repl)))

;; Use this if gofmt is screwing things up.
;;
;; (after! go-mode
;;               (remove-hook 'go-mode-hook 'gofmt))
;; ﷽
(defun go-run ()
  "Compile and run buffer."
  (interactive)

  (if (get-buffer "#go#run#session")(evil-delete-buffer "#go#run#session"))
  (generate-new-buffer "#go#run#session")

  (shell-command (format "go run %s 2> '#go#run#session' > '#go#run#session'" (buffer-file-name)))
  (shell-command "go tool pprof --text cpu.pprof >> '#go#run#session'")

  (defvar go-run-path (concat (file-name-directory buffer-file-name) "#go#run#session"))
  (display-buffer
   (find-file-noselect go-run-path)
   '(
     (display-buffer-reuse-window)
     (display-buffer-in-direction "right")
     ))
  )

'(sql-connection-alist
   '((districts
      (sql-product 'postgres)
      (sql-server "localhost")
      (sql-user "clay")
      (sql-database "districts"))
     (yf-bi
      (sql-product 'postgres)
      (sql-server "10.0.1.102")
      (sql-user "postgres")
      (sql-database "yf-bi"))
     ("gop_voters"
      (sql-product 'postgres)
      (sql-user "postgres")
      (sql-database "query_gop_prod")
      (sql-server "citus-v2.middleearth.eltoro.com"))) t)


(defun choose-db ()
  "Prompt user to pick a database to connect to."
  (interactive)
  (let ((choices '("districts" "yf-bi")))
    (set-db "%s" (ido-completing-read "Choose database: " choices ))))

;; clay add on to sql mode -- postgres specific
;;
(defun set-db (_ ans)
  "Set the db conn string specified by ANS."
  (interactive)

  (defvar clay-db)
  ;; (defvar clay-districts)
  (defvar  yf-bi)
  (let ((clay-districts "psql -d districts --echo-queries --echo-hidden -P pager=off \n")
        (yf-bi "psql -U postgres -h 10.0.1.102 -d yf-bi --echo-queries --echo-hidden -P pager=off \n"))

    (if (string-equal ans "districts")(setq clay-db clay-districts ))
    (if (string-equal ans "yf-bi")(setq clay-db yf-bi))
    )
    clay-db)

(set-db "one" "districts")
;; A startup function to pair with tws-region-to-process.
;; **TODO need to review startup functions.
(defun vterm-sql-startup ()
  "Call up vterm then send a command to start psql with the given config."
  (interactive)
  (let ((start-db (choose-db)))
    (evil-window-vsplit)
    (evil-window-right 1)
    (+vterm/here nil)
    (start-psql-remote start-db)
    (evil-window-left 1)
    ))

(defun start-psql-remote (db)
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) db)
  (process-send-string (get-buffer-process (current-buffer)) "\\timing \n")
  (message db)
  )

;; https://emacs.stackexchange.com/a/37889
(defun tws-region-to-process (arg beg end)
  "Send the current region to a process buffer.
The first time it's called, will prompt for the buffer to
send to. Subsequent calls send to the same buffer, unless a
prefix argument is used (C-u), or the buffer no longer has an
active process."
  (interactive "P\nr")
  ;; (message "the args of tws -- ")
  ;; (message  arg beg end)
  ;; (print arg)
  (if (or arg                                ;; user asks for selection
          (not (boundp 'tws-process-target)) ;; target not set
          ;; or target is not set to an active process:
          (not (process-live-p (get-buffer-process
                                tws-process-target))))
      (setq tws-process-target
            (completing-read
             "Process: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list)))))
  (process-send-string tws-process-target "\\echo `date` \n")
  (process-send-region tws-process-target beg end)
  (process-send-string tws-process-target "\n ")
  )

  ;; This value may persist without being set here. We will see.
  (setq source-directory "/usr/local/Cellar/emacs-plus@28/28.0.50/share/emacs/28.0.50/lisp")

;; This is not firing when expected as in starting a new sql session.
;; **TODO need to review sql client methods.
 ;; (add-hook 'sql-login-hook 'clay-sql-login-hook)
 (defun clay-sql-login-hook ()
   "Custom SQL log-in behaviours. See `sql-login-hook'."
   ;; n.b. If you are looking for a response and need to parse the
   ;; response, use `sql-redirect-value' instead of `comint-send-string'.
   (interactive)
   (message "Postgres gonna light up -->>   🚀🚀🚀🚀 ")
   ;; Output each query before executing it. (n.b. this also avoids
   ;; the psql prompt breaking the alignment of query results.)
   ;; remote server needs this
   (comint-send-string sql-buffer "\\set ECHO queries \n")
   (comint-send-string sql-buffer "\\set VERBOSITY verbose \n")
   (comint-send-string sql-buffer "\\timing \n"))

(add-hook 'sql-mode-hook #'sqlup-mode)
;; (add-hook 'sql-mode-hook #'(lambda () (message "🖐🏽 configging sql for action 🐲")))

;; https://emacs.stackexchange.com/a/16692
;;
;; (defun sql-add-newline-first (output)
;;    "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
;;   (concat "\n" output))

;; (defun sqli-add-hooks ()
;;   "Add hooks to `sql-interactive-mode-hook'."
;;   (add-hook 'comint-preoutput-filter-functions
;;             'sql-add-newline-first))

;; (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)


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
  (clm/toggle-command-log-buffer))

;; Using after! instead of mode-hook avoids calling the window size function more than once 🚅
(after! command-log-mod
  (setq command-log-mode-window-size 50))

;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;;
;;
;; rescript-mode
;; ;; Tell `rescript-mode` how to run your copy of `server.js` from rescript-vscode
;; ;; (you'll have to adjust the path here to match your local system):
;; (custom-set-variables
;;  '(lsp-rescript-server-command
;;    '("node" "/path/to/rescript-vscode/server/out/server.js" "--stdio")))
;;
;;
;; (after! rescript-mode
;;  (after! lsp-rescript (add-hook 'rescript-mode-hook 'lsp-deferred))
;;  (after! lsp-ui (add-hook 'rescript-mode-hook 'lsp-ui-doc-mode)))
;;
;;
;; (map! :localleader
;;       :map rescript-mode
;;       "q" #'indent-pp-sexp)
;; (add-to-list 'auto-mode-alist '("\\.resi*\\'" . rescript-mode))
;;
;; /Users/clay/developer/rescript/rescript-vscode/node_modules/.bin
;;
;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;                `(rescript-mode . ("tsc" "-b" "-w")))
;;   )


;; I got this from som redit feed.
;; (define-derived-mode rescript-mode prog-mode "Rescript"

;;   (setq-local tab-width 2)
;;   (setq-local indent-tabs-mode nil)

;;   (setq-local comment-start "// ")
;;   (setq-local comment-start-skip "//+\\s-*")

;;   (setq-local parse-sexp-lookup-properties t)
;;   (setq-local parse-sexp-ignore-comments t)

;;   ;; (setq-local forward-sexp-function #'python-nav-forward-sexp)
;;   (setq-local forward-sexp-function #'js2-mode-forward-sexp)

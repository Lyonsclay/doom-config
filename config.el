;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-python-lsp-server

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

;; (setq
;;  doom-font (font-spec :family "Fira Mono" :size 14)
;;  doom-variable-pitch-font (font-spec :family "Fira Sans")
;;  )

 (setq doom-font (font-spec :family "Source Code Pro" :style "Regular" :size 14 :height 1.0)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :style "Regular" :size 13)
      doom-big-font (font-spec :family "Source Code Pro" :style "Regular" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-oceanic-next)

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
;
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
      "w w" #'ace-delete-window
      "t t" #'treemacs
      "t s" #'toggle-center-scroll
      "y" #'copy-path
      "k" #'comint-kill-region)


;; multiple cursor quick edit commands
(map!
 "C-/" #'mc/mark-all-like-this
 "C->" #'mc/mark-next-like-this
 "C-<" #'mc/mark-previous-like-this
 "M-)" #'sp-unwrap-sexp)


(map! :leader
      "SPC" nil
      "SPC" #'execute-extended-command)
;; (key-seq-define-global "qd" 'dired)

;; (key-seq-define text-mode-map "qf" 'flyspell-buffer)

(require 'key-chord)
(key-chord-mode t)
(key-seq-define-global "jk" 'jkpop)
(key-seq-define-global "JK" 'jkpop)
(key-seq-define-global "kj" 'jkpop)
(key-seq-define-global "JK" 'jkpop)
(key-seq-define-global "ii" 'caps-lock-mode)
(key-seq-define-global "=-" 'upcase-word)
(key-seq-define-global "-=" 'downcase-word)


;; RESTORE ctrl-D
(define-key evil-insert-state-map   (kbd "C-d") #'evil-delete-char)
(define-key evil-normal-state-map   (kbd "C-d") #'evil-delete-char)


;; in elisp file like `this'
;; in other files like `this` 😜
(after! smartparens
  (sp-pair "`" "`" :wrap "M-`")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "<" ">" :wrap "M-<")
  (sp-pair "'" "'" :wrap "M-'")
  (sp-pair "\"" "\"" :wrap "M-\""))

;; The unmapping of "spc m e" needs to happen after a go buffer is being visited, apparently.
;; (add-hook 'go-mode-hook (lambda ()
;;                           ;; I want to disable the go playground which is mapped to "spc m e".
;;                           ;; The go playground will send your code to the web -- bad!
;;                           (map! :localleader :map go-mode-map :nv "e" nil)
;;                           (map! :localleader :map go-mode-map "e e" #'go-run)
;;                           (map! :localleader :map go-mode-map "e p" #'go-prof-test)
;;                           (map! :localleader :map go-mode-map "t v" #'go-test-verbose)
;;                           (map! :localleader :map go-mode-map "h d" #'go-guru-definition-other-window)
;;                           ))
;; (require 'project)

;; (defun project-find-go-module (dir)
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;     (cons 'go-module root)))

;; (cl-defmethod project-root ((project (head go-module)))
;;   (cdr project))

;; (add-hook 'project-find-functions #'project-find-go-module)

(map! :localleader
      :map rustic-mode-map
      "t r" #'rustic-cargo-test-rerun)

(map! :localleader
      :map python-mode-map
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


(setq ns-auto-hide-menu-bar t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (toggle-frame-fullscreen)
(setq ns-use-native-fullscreen t)

;; (after! flycheck
;; (setq flycheck-check-syntax-automatically '(save))
;; (set-popup-rule! "^\\*Flycheck errors\\*$" :side 'bottom :size 0.1)
;; )
;; (setq company-idle-delay 3)

;; never lose your cursor again
(beacon-mode 1)

;; Emoji: 😄, 🤦, 🏴
;; How do we get kick ass emojis? It's an eternal quest.
;;
; (setq use-default-font-for-symbols nil)
;; (set-fontset-font t 'symbol "Apple Color Emoji")
;; (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;; (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;; (set-fontset-font t 'symbol "Symbola" nil 'append)
;; (setq doom-unicode-font (font-spec :family "Noto Color Emoji"))

;; sometimes any character change deletes the whole line
;; https://stackoverflow.com/a/3024055/2252501
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; startup is much quicker without loading saved history
(savehist-mode -1)

;; These two hooks should catch any shift of focus event and save the current buffer
;; or any unsaved buffer for that matter.
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))
(add-hook 'doom-switch-window-hook (lambda () (save-some-buffers t)))

;; Treemacs --
;;
;; (require 'treemacs)

;; (setq treemacs-expand-after-init nil)
;; (setq doom-themes-treemacs-enable-variable-pitch nil)
;; ace-window is required by treemacs and certain variables are not defined if not loaded here(this is probably a temp work around)
(require 'ace-window)
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

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

(after! treemacs
  ;; (irrelevant preferences elided)
  (setq treemacs-position 'left)
  (setq treemacs-width 35)
  (setq treemacs-show-cursor t)
  (setq treemacs-expand-after-init nil)
  ;; workaround here:
  (set-popup-rule! "^ \\*Treemacs"
    :side treemacs-position
    :window-width treemacs-width)
  )

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

(remove-hook! 'before-save-hook #'+format/buffer)
;; List of all key bindings for current buffer/mode.
;; (counsel--descbinds-cands)
;;


;; This is the best way to get full screen without menu bar and without osx transit animation.
;; (setq ns-auto-hide-menu-bar t)
;; (toggle-frame-maximized)
;; (toggle-frame-fullscreen)
;; (toggle-frame-fullscreen)
;;
;; ;; ** PYTHON CONFIG ** ----------->
;;
;;

(defun pipenv-blackify ()
  "Format python code"
  (interactive)
  (save-buffer (current-buffer))
  (message (concat "run black in " (buffer-file-name)))
  (shell-command (concat "black --line-length 80 " (buffer-file-name)))
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


;; (after! python
;;   (require 'lsp-pyright)
;;   (setq python-pytest-executable "pytest -vv --disable-warnings")
;;   )

(defun ipython-startup-norm ()
  "Set up ipython with hot reloading of code."
  (interactive)

  (+python/open-ipython-repl)
  (process-send-string (get-buffer-process (current-buffer)) "%load_ext autoreload\n %autoreload 2\n")
  (process-send-string (get-buffer-process (current-buffer)) "from IPython.core import ultratb;ultratb.VerboseTB._tb_highlight = 'bg:ansired';"))

(defun ipython-startup ()
  "Initiate ipython from current buffer with shell command."
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1)
  ;; (+vterm/here default-directory)
  (process-send-string (get-buffer-process (current-buffer)) "ipython -i --simple-prompt -c \"from IPython.core import ultratb;ultratb.VerboseTB._tb_highlight = 'bg:ansired'\n\" \n")
  (process-send-string (get-buffer-process (current-buffer)) "%load_ext autoreload \n %autoreload 2 \n"))

;; (defun conda-init ()
;;   "Create a conda virtual environemnt."
;;   (interactive)
;;   (shell-command "conda init zsh")
;;   (shell-command "conda activate")
;;   )

;; ;; The main reason to use this is to access numpy and pytorch.
;; ;;
;; (defun conda-repl ()
;;   "Open an IPython REPL."
;;   (interactive)
;;   (require 'python)
;;   (shell-command "conda init zsh && conda activate")
;;   (let ((python-shell-interpreter "/Users/clay/miniconda3/bin/ipython")
;;         (python-shell-interpreter-args
;;          (string-join (cdr +python-ipython-command) " ")))
;;     (+python/open-repl)))

;;
;;

;; ;; ** RUST CONFIG ** ----------->
;; https://github.com/emacs-lsp/lsp-mode/issues/3490
;; (require 'tramp)


(after! tramp
  (setq tramp-inline-compress-start-size 1000)
  (setq tramp-copy-size-limit 10000)
  (setq vc-handled-backends '(Git))
  (setq tramp-verbose 1)
  (setq tramp-default-method "scp")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq projectile--mode-line "Projectile")
  (setq tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )


;; (require 'eglot)
;; (add-hook 'rust-mode-hook (setq eglot-events-buffer-config 0
;;                                 ;; eglot-ignored-server-capabilities '(:inlayHintProvider)
;;                                 eglot-confirm-server-edits 'diff))

;; (require 'rustic)
;; ;;   ; Tell rustic where to find the cargo binary
;; ;;   ;; (setq rustic-cargo-bin-remote "~/.cargo/bin/cargo")
(setq rustic-cargo-bin-remote "/usr/local/cargo/bin/cargo")
;;   (setq rustic-lsp-client 'eglot)

;; (add-hook 'rust-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs
;;              '((rust-ts-mode rust-mode) .
;;                ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

;; (require 'evil-surround)
;; (add-hook 'rustic-mode-local-vars-hook #'lsp!)
;; (add-hook 'rust-mode-hook (lambda ()
;;                            (push '(< . ("< " . " >")) evil-surround-pairs-alist)))


;; (setq lsp-rust-analyzer-server '(:cmd "~/.cargo/bin/rust-analyzer" :initOptions (:linkedProjects ["~/developer/eltoro/spark-livy-starter/Cargo.toml"])))
;; ;; (require 'toml-mode)
;; ;;(require 'flycheck-inline)

;; (use-package! rustic
;;               :defer t
;;               :config
;;               (my-message "rustic configuration")
;;               (setq! rustic-format-trigger 'on-save
;;                      rustic-format-on-save t
;;                      rustic-format-on-save-method 'rustic-format-buffer
;;                      rustic-compile-backtrace "1"
;;                      rustic-compile-directory-method 'rustic-buffer-crate ;;'rustic-project-root
;;                      ;;rustic-format-display-method #'switch-to-buffer
;;                      ;;rustic-kill-buffer-and-window nil
;;                      ;;rustic-compile-display-method #'ace-display-buffer
;;                      rustic-lsp-server 'rust-analyzer)
;;               (custom-set-faces!
;;                '(rustic-message :inherit default)
;;                '(rustic-compilation-error :inherit compilation-error )
;;                '(rustic-compilation-warning :inherit compilation-warning)
;;                '(rustic-compilation-info :inherit compilation-info)
;;                '(rustic-compilation-line :inherit compilation-line-number)
;;                '(rustic-compilation-column :inherit compilation-column-number))
;;               (setq! rustic-ansi-faces ansi-color-names-vector))

;; (use-package! flycheck
;;               :defer t
;;               :config
;;               (require 'flycheck-rust)
;;               (push 'rustic-clippy flycheck-checkers))

;; (add-hook 'rust-mode-hook 'flycheck-mode)

;; ;; https://github.com/flycheck/flycheck-rust
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; ﷽
;;
;; ;; ** SQL CONFIG ** ----------->
;;
;;
;;
;; (add-hook 'sql-mode-hook
;;           (lambda ()
;;             (progn
;;               (setq-default tab-width 4)
;;               (setq-default sqlup-mode nil)
;;               )))

;; (add-hook 'sql-mode-hook #'sqlup-mode)
;; (add-hook 'sql-mode-hook #'(lambda () (message "🖐🏽 configging sql for action 🐲")))


(defun sql-mode-hook-function ()
  "All your add-ons to `sql-mode'."
  (setq indent-tabs-mode nil)
  ;; (setq align-mode-rules-list align-sql-indent-rules-list)
  (message "SQL - moduls  🦉 ")
  (setq tab-width 4))

(add-hook 'sql-mode-hook 'sql-mode-hook-function)
(defun choose-db ()
  "Prompt user to pick a database to connect to."
  (interactive)
  (let ((choices '("districts" "animal")))
    (set-db "%s" (ido-completing-read "Choose database: " choices ))))

;; clay add on to sql mode -- postgres specific
;;
(defun set-db (_ ans)
  "Set the db conn string specified by ANS."
  (interactive)

  (defvar clay-db)
  (defvar gorge)
  (defvar  animal)
  (let ((districts "psql -d districts --echo-queries --echo-hidden -P pager=off \n")
        (gorge "psql -U postgres -h 127.0.0.1 -d gorge --echo-queries --echo-hidden -P pager=off \n")
        (animal "psql -U postgres -h 127.0.0.1 -d animal --echo-queries --echo-hidden -P pager=off \n"))

    (if (string-equal ans "districts")(setq clay-db districts))
    (if (string-equal ans "gorge")(setq clay-db gorge))
    (if (string-equal ans "animal")(setq clay-db animal))
    )
    clay-db)

;; (set-db "one" "districts")
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

  ;; (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  ;; (sql-set-product-feature 'postgres :prompt-cont-regexp
                           ;; "^[-[:alnum:]_]*[-(][#>] ")
;; This is not firing when expected as in starting a new sql session.
;; **TODO need to review sql client methods.
 (add-hook 'sql-login-hook 'clay-sql-login-hook)
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



;; javascript rjsx-mode
;;
;; (setq-hook! 'rjsx-mode-hook +format-with :none)

(use-package web-mode
  :hook ((web-mode . lsp)
         (typescript-tsx-mode . lsp))
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript-tsx")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package prettier
  :hook ((typescript-tsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         ;; (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

;; (require 'prettier-js)

;; (add-hook 'json-mode-hook
;;   (lambda ()
;;     (setq prettier-js-args '("--trailing-comma" "all" "--bracket-spacing" "false"))
;;     (setq prettier-js-executable-path "~/.yarn/bin/prettier")))

(define-derived-mode tsx-mode typescript-mode
  "typescript but better because such X")

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; ein mode <Juptyer Notebooks>
(setq ein:output-area-inlined-images t)
;; enable undo in ein buffer
(add-hook 'fundamental-mode 'turn-on-undo-tree-mode)


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


(yas-global-mode 1)


;; ;; ** c++ CONFIG ** ----------->
(set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}"))



;; GPTEL ---->
(setq
 gptel-model "llama3"
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '("llama3")))


;; LaTeX ----->
(setq +latex-viewers '(pdf-tools))


;; Org Mode ------))
(setq
    org-superstar-headline-bullets-list '("✿" "◉" "○" "✸" "⁖")
)

;; https://github.com/daviwil/emacs-from-scratch/blob/1a13fcf0dd6afb41fce71bf93c5571931999fed8/init.el#L206C1-L257C47
(require 'org)
;; (require 'org-bullets)
(require 'visual-fill-column)

(require 'org-tempo)

(after! org-mode
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun clm/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.45)
                  (org-level-3 . 1.20)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(setq org-ellipsis " ▾")
(clm/org-font-setup)

;; (set-popup-behavior t)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

(defun clm-org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook 'clm/org-font-setup)
(add-hook 'org-mode-hook 'clm-org-mode-visual-fill)

(defun nolinum ()
  (global-display-line-numbers-mode 0)
)
(add-hook 'org-mode-hook 'nolinum)

;; Org Roam
;;
 ;; ("^\\*Capture\\*$\\|CAPTURE-.*$" (+popup-buffer) (actions) (side . right)
 ;;  (size . 0.42) (window-width . 40) (window-height . 0.42) (slot) (vslot)
 ;;  (window-parameters (ttl . 5) (quit) (select . t) (modeline)
 ;;                     (autosave . ignore) (transient . t) (no-other-window . t)))


;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; Vterm mode
;;
(dolist (mode '(org-mode-hook
                    term-mode-hook
                    vterm-mode-hook
                    shell-mode-hook
                   treemacs-mode-hook
                    eshell-mode-hook))
      (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; Github Copilot ----->
;;

;; accept completion from copilot and fallback to company
(after! copilot
  ;; Enable copilot-mode in prog-mode
  (add-hook 'prog-mode-hook #'copilot-mode)

  ;; Key bindings for copilot-completion-map
  (map! :map copilot-completion-map
        "<tab>" #'copilot-accept-completion
        "TAB" #'copilot-accept-completion
        "C-TAB" #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word))


(after! copilot
  ;; Enable copilot-mode in prog-mode
  (add-hook 'prog-mode-hook #'copilot-mode)

  ;; Key bindings for copilot-completion-map
  (map! :map copilot-completion-map
        "<tab>" #'copilot-accept-completion
        "TAB" #'copilot-accept-completion
        "C-TAB" #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word
        "C-n" #'copilot-next-completion
        "C-p" #'copilot-previous-completion)

  ;; Configure copilot-indentation-alist
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


(after! request
  (setq copilot-chat-frontend 'org))

(require 'copilot-chat-shell-maker)
(after! copilot-chat
  (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
  (setq copilot-chat-frontend 'shell-maker)
  (setq copilot-chat-model "claude-3.5-sonnet")
  (copilot-chat-shell-maker-init))

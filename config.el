;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;


(require 'acp)
(require 'agent-shell)
(require 'transient)
(setq! transient-show-common-commands t)

(map! :leader
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)


(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq mouse-wheel-tilt-scroll t)

;; Force most buffers to open in the current window by default
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-same-window)
        (reusable-frames . t)))

;; Ensure switch-to-buffer and friends respect display-buffer-alist
(setq switch-to-buffer-obey-display-actions t)
(set-popup-rule! ".*" :ignore t)

(setq! evil-escape-excluded-major-modes '())

(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))
(add-hook 'doom-switch-window-hook (lambda () (save-some-buffers t)))

(defvar-local my/buffer-theme-cookies
    nil
  "Stores the face remapping cookies for the current buffer so they can be removed later.")

(defvar-local my/buffer-theme-cookies nil
  "Stores the face remapping cookies for the current buffer.")

(load "~/.config/doom/buffer-theme.el")

(map! :leader
      "0" #'treemacs-select-window
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

;; ;; multiple cursor quick edit commands
;; (map!
;;  "C-(" #'mc/mark-all-like-this
;;  "C->" #'mc/mark-next-like-this
;;  "C-<" #'mc/mark-previous-like-this
;;  "M-)" #'sp-unwrap-sexp)

(use-package evil-multiedit
  :after evil
  :init
  (map!
   "M-d"   #'evil-multiedit-match-and-next
   "M-D"   #'evil-multiedit-match-and-prev
   "M-a"   #'evil-multiedit--paste
   "C-M-d" #'evil-multiedit-restore))

;; in elisp files like `this'
;; in other files like `this` 😜

(with-eval-after-load 'smartparens
  (sp-pair "`" "`" :wrap "M-`")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "<" ">" :wrap "M-<")
  (sp-pair "'" "'" :wrap "M-'")
  (sp-pair "\"" "\"" :wrap "M-\"")
  (sp-pair "=" "=" :wrap "M-="))

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

;; (require 'key-chord)
;; (key-chord-mode t)
;; (key-seq-define-global "jk" 'jkpop)
;; (key-seq-define-global "JK" 'jkpop)
;; (key-seq-define-global "kj" 'jkpop)
;; (key-seq-define-global "KJ" 'jkpop)
;; (key-seq-define-global "ii" 'caps-lock-mode)
;; (key-seq-define-global "=-" 'upcase-word)
;; (key-seq-define-global "-=" 'downcase-word)

(setq evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))


;; Eglot      ------>
;; Prevent change in line height when there are linting hints.
(setq! eglot-code-action-indications '(eldoc-hint))

;; Treemacs   ------>
;; Force Treemacs to stay on the far left and full height
(setq treemacs-is-never-other-window t)

;; Force Treemacs into a dedicated window that won't be resized
(add-hook 'treemacs-mode-hook
          (lambda ()
            (let ((win (get-buffer-window "*Treemacs*")))
              (when win
                (set-window-dedicated-p win t)))))

;; Set Flycheck to open below the current window (splits current window)
(add-to-list 'display-buffer-alist
             '("^\\*Flycheck errors\\*$"
               (display-buffer-reuse-window display-buffer-below-selected)
               (window-height . 0.3)))


;; Projectile ------>
(after! projectile (add-to-list 'projectile-project-root-files "__pycache__"))

;; LSP mode ------->
(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories
               "\\`/opt/.*\\'"))

;; (lsp-inlay-hints-mode)


(setq lsp-inlay-hint-enable t)


;; PYTHON ----->
(defun pipenv-blackify ()
  "Format python code"
  (interactive)
  (save-buffer (current-buffer))
  (message (concat "run black in " (buffer-file-name)))
  (shell-command (concat "black --line-length 80 " (buffer-file-name)))
  ;; (save-buffer (current-buffer))
  (revert-buffer (current-buffer))
  )

(defun ipython ()
  (interactive)
  (term "ipython"))

;; (add-hook 'vterm-mode-hook (lambda () (vterm-copy-mode -1)))


;; (add-hook 'inferior-python-mode (setq python-shell-completion-native-enable nil))


;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt -i")

(setq! +python-ipython-command '("ipython" "-i"))

(map! :localleader
      :map python-mode-map
      "r" #'+python/open-ipython-repl
      "f" #'pipenv-blackify)


;; RUSTIC ----->
(map! :localleader
      :map rustic-mode-map
      "t r" #'rustic-cargo-test-rerun)


(add-hook 'rust-mode-hook (setq rustic-cargo-test-disable-warnings t))

;; TYPESCRIPT  ------->
;;
;; 1) When you open a .ts file, disable electric-indent
(add-hook! 'typescript-mode-hook
  (electric-indent-local-mode -1))

;; 2) When you open a .tsx file (in web-mode), disable electric-indent there too
(add-hook! 'web-mode-hook
  (when buffer-file-name
    (electric-indent-local-mode -1)))


;; CLOJURE ----->

(map! :after clojure-mode
      :map clojure-mode-map
      :n "<leader> c d" #'evil-goto-definition)

(use-package! jarchive
  :config
  (jarchive-mode 1))

;; TREEMACS ---->
(after! treemacs
  (setq treemacs-show-cursor t)
  (setq treemacs-expand-after-init nil)
  )


;; GPTEL ---->
(with-eval-after-load 'gptel
     (add-hook 'gptel-mode-hook
               (lambda ()
                 (setq-local gptel-context nil))))

;; Default gptel scope to "buffer" in Org files
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local gptel--set-buffer-locally t)))


;; 1. Load the tool definitions BEFORE creating presets that use them
(load "~/.config/doom/gptel-agents.el")
(load "~/.config/doom/gptel-patch-diff.el")


(with-eval-after-load 'gptel
  (define-key gptel-mode-map (kbd "<normal-state> RET") nil)
  (define-key gptel-mode-map (kbd "<visual-state> RET") nil)
  (message "Disabled gptel-send on RET in gptel-mode-map for Normal and Visual states."))

;; Set Gemini as default
(after! gptel
  (setq
   gptel-model 'gemini-flash-latest
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (getenv "GEMINI_API_KEY")
                   :stream t))
  )

(gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key (getenv "CLAUDE_API_KEY"))

(gptel-make-openai "ChatGPT"
  :stream t
  :key (getenv "OPENAI_API_KEY"))


;; (gptel-make-ollama "Ollama"
;;   :host "localhost:11434"
;;   :stream t
;;   :models '(llama3.2))

;; (gptel-make-openai "Github Models llama3.3"
;;   :host "models.github.ai"
;;   :endpoint "/inference/chat/completions"
;;   :stream t
;;   :key (getenv "GITHUB_TOKEN")
;;   :models '(Llama-3.3-70B-Instruct))

(after! gptel
  (setq gptel-use-tools t)
  (setq gptel-include-reasoning nil)
  (setq gptel-context-restrict-to-project-files nil)

  (defun my/gptel-escape-source-blocks (beg end)
    "Prepend a space to lines starting with an asterisk in new gptel responses."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (let ((case-fold-search nil)
              (end-marker (copy-marker end)))
          (goto-char beg)
          (while (re-search-forward "^[ \t]*#\\+begin_src" end-marker t)
            (let ((cbeg (line-beginning-position 2)))
              (when (re-search-forward "^[ \t]*#\\+end_src" end-marker t)
                (let ((cend (copy-marker (line-beginning-position))))
                  (when (< cbeg cend)
                    (save-excursion
                      (goto-char cbeg)
                      (while (re-search-forward "^\\*" cend t)
                        (replace-match " *" nil nil))))
                  (set-marker cend nil)))))))))
  (remove-hook 'gptel-post-response-functions #'my/gptel-indent-source-blocks)
  (add-hook 'gptel-post-response-functions #'my/gptel-escape-source-blocks))

;; Agent Shell ------>
(setopt agent-shell-show-context-usage-indicator t)
(setopt agent-shell-show-usage-at-turn-end t)
(setopt agent-shell-session-strategy 'prompt)
(setq agent-shell-google-authentication
      (agent-shell-google-make-authentication
       :api-key (lambda () (auth-source-pass-get "secret" "GEMINI_API_KEY"))))
;; ORG ROAM  ------>

(after! org-roam
  ;; 1. Tell Doom's popup system to ignore Org Roam and Capture buffers
  ;; This allows them to obey the 'display-buffer-same-window' rule.
  (set-popup-rule! "^\\*org-roam" :ignore t)
  (set-popup-rule! "^CAPTURE-" :ignore t)

  ;; 2. Force these buffers to open in the current window
  (add-to-list 'display-buffer-alist
               '("\\.org$" . (display-buffer-same-window)))
  
  ;; 3. Ensure org-roam-node-find specifically uses the current window
  ;; by ensuring the capture process doesn't jump to a new split.
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))

  ;; Force org-roam-node-find to use the current window even if buffer is open elsewhere
  (setq org-roam-display-functions '(display-buffer-same-window)))

;; `org-roam-find` open window in current buffer.
(setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file) ;; This line changes the default behavior for file links
                             (wl . wl-other-frame)))
(with-eval-after-load 'org
  ;; 1. Ensure links open in the same window
  (setf (alist-get 'file org-link-frame-setup) 'find-file)

  ;; 2. Force all org files to stay in the current window
  (add-to-list 'display-buffer-alist
               '("\\.org$"
                 (display-buffer-same-window))))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((dot . t))) ; this line activates dot

(after! org
  (setq org-src-preserve-indentation t
        org-src-content-indentation 0)
  (require 'org-modern)
  (add-hook 'org-mode-hook #'org-modern-mode)
  ;; Optional: Customize headline bullets
  (setq org-modern-star 'replace
        org-modern-replace-stars '("◉" "○" "✸" "✿" "➤" "◆" "▲" "▼"))
  )

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
     (call-interactively 'org-babel-tangle)
))

(defun my/org-escape-source-block-at-point ()
  "Prepend a space to lines starting with an asterisk in the current block."
  (interactive)
  (save-excursion
    (let ((case-fold-search nil)
          (start-pos (point)))
      (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
        (let ((block-start (point))
              (cbeg (line-beginning-position 2)))
          (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
            (let ((cend (copy-marker (line-beginning-position)))
                  (block-end (point)))
              (when (and (>= start-pos block-start) (<= start-pos block-end))
                (save-excursion
                  (goto-char cbeg)
                  (while (re-search-forward "^\\*" cend t)
                    (replace-match " *" nil nil)))
                (message "Escaped asterisks in source block.")
                (set-marker cend nil)))))))))

(map! :localleader
      :map org-mode-map
      "D" #'gptel/org-git-apply-diff-at-point
      "B" #'org-babel-tangle-block)

;; Insure tangle works in org roam file.
(setq org-id-link-to-org-use-id 'create-if-interactive)

;; Allow org-babel to provide syntax highlighting.
(add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
(add-to-list 'org-src-lang-modes '("ts" . typescript-ts))
(add-to-list 'org-src-lang-modes '("tsx" . typescript-ts))
(add-to-list 'org-src-lang-modes '("json" . js-json))


;; https://github.com/daviwil/emacs-from-scratch/blob/1a13fcf0dd6afb41fce71bf93c5571931999fed8/init.el#L206C1-L257C47
;; (require 'visual-fill-column)

(setq org-hide-emphasis-markers t)
(let* ((variable-tuple
        (cond
         ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
         ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
         ((x-list-fonts "Verdana")         '(:font "Verdana"))
         ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
         ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
         (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       ;; (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold
                             ;; :foreground ,base-font-color
                             )))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(defun clm-org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook #'clm-org-mode-visual-fill)


(defun nolinum ()
  (global-display-line-numbers-mode 0)
)
(add-hook 'org-mode-hook 'nolinum)

;; Org Latex integeration ----->

;; (add-to-list 'org-preview-latex-process-alist
;;              '(tectonic :programs ("tectonic" "convert")
;;                         :description "pdf > png"
;;                         :message "you need install the programs: tectonic and imagemagick."
;;                         :image-input-type "pdf"
;;                         :image-output-type "png"
;;                         :image-size-adjust (1.0 . 1.0)
;;                         :latex-compiler
;;                         ("tectonic -Z shell-escape-cwd=%o --outfmt pdf --outdir %o %f")
;;                         :image-converter
;;                         ("convert -density %D -trim -antialias %f -quality 300 %O")))
;; (setq org-preview-latex-default-process 'tectonic)
(use-package org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; ;; Use dvisvgm to generate previews
  ;; ;; You don't need this, it's the default:
  ;; (setq org-latex-preview-process-default 'dvisvgm)
  
  ;; Turn on `org-latex-preview-mode', it's built into Org and much faster/more
  ;; featured than org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-mode)

  ;; ;; Block C-n, C-p etc from opening up previews when using `org-latex-preview-mode'
  ;; (setq org-latex-preview-mode-ignored-commands
  ;;       '(next-line previous-line mwheel-scroll
  ;;         scroll-up-command scroll-down-command))

  ;; ;; Enable consistent equation numbering
  ;; (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-mode-display-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-mode-update-delay 0.25))

;; (setq! org-latex-preview-appearance-options  '(:foreground auto :background "Transparent" :scale 1.0 :zoom 1.5 :page-width 0.6 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; New Org Roam windows ------>
;; Reuse same window for Org files
(add-to-list 'display-buffer-alist
             '("\\.org\\'" (display-buffer-same-window)))

;; Keep the capture buffer in the same window too
(dolist (re '("\\`\\*Capture\\*\\'" "\\`\\*Org Capture\\*\\'"))
  (add-to-list 'display-buffer-alist
               `(,re (display-buffer-same-window))))




;; Terraform/OpenTofu ------>>>>

(setq! terraform-command "tofu")


;; Markdown-mode   --------->>>>

(after! grip-mode
  (setq grip-command 'go-grip
        grip-preview-in-webkit t
        grip-sleep-time 3))

(load! "org-gfm-preview")
(map! :localleader
      :map org-mode-map
      "m p" #'org-gfm-preview-toggle
      "m e" #'org-gfm-preview-execute)


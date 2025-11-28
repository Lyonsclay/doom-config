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
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



(map! :leader
      :desc "Open like spacemacs" "SPC" #'execute-extended-command)


(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq! evil-escape-excluded-major-modes '())

(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))
(add-hook 'doom-switch-window-hook (lambda () (save-some-buffers t)))

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

(use-package! evil-multiedit
  :after evil
  :init
  (map!
   "M-d"   #'evil-multiedit-match-and-next
   "M-D"   #'evil-multiedit-match-and-prev
   "M-a"   #'evil-multiedit--paste
   "C-M-d" #'evil-multiedit-restore))

;; in elisp files like `this'
;; in other files like `this` 😜
(after! smartparens
  (sp-pair "`" "`" :wrap "M-`")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "<" ">" :wrap "M-<")
  (sp-pair "'" "'" :wrap "M-'")
  (sp-pair "\"" "\"" :wrap "M-\""))

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

;; Set Flycheck to open at the bottom without affecting other windows
(add-to-list 'display-buffer-alist
             '("^\\*Flycheck errors\\*$"
               (display-buffer-reuse-window display-buffer-at-bottom)
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
(setq tide-format-options
      '(:indentSize 2
        :tabSize    2))

;; 1) When you open a .ts file, disable electric-indent
(add-hook! 'typescript-mode-hook
  (electric-indent-local-mode -1))

;; 2) When you open a .tsx file (in web-mode), disable electric-indent there too
(add-hook! 'web-mode-hook
  (when buffer-file-name
    (electric-indent-local-mode -1)))


;; TREEMACS ---->
(after! treemacs
  (setq treemacs-show-cursor t)
  (setq treemacs-expand-after-init nil)
  )


;; GPTEL ---->

(use-package! gptel
 :config
 (setq! gptel-api-key (getenv "OPENAI_API_KEY")))
;; Set Gemini as default
(after! gptel
  (setq
   gptel-model 'gemini-2.5-flash
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (getenv "GEMINI_API_KEY")
                   :request-params '(:tools [(:google_search ())])
                   :stream t))
  )
(gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key (getenv "CLAUDE_API_KEY"))

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
  (setq gptel-include-reasoning nil))

(load "~/.config/doom/gptel-patch-diff.el")

;; ORG ---->
;; 
(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
     (call-interactively 'org-babel-tangle)
))

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
(require 'org-bullets)
(require 'visual-fill-column)


;; (setq org-src-fontify-natively t)

;; (setq org-ellipsis " ▾")

(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))


(require 'org-modern)
(after! org
  ;; Load org-modern and enable it in org-mode and agenda

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  ;; Appearance customization
  (setq
   ;; Headline bullets
   org-modern-star '("◉" "○" "✿" "✸" "⁖")
   org-modern-hide-stars nil

   ;; Folding symbol
   org-ellipsis "…"

   ;; Checklist symbols
   org-modern-checkbox '((?X . "✔") (?- . "✘") (?\s . "☐"))

   ;; Block and quote appearance
   org-modern-block-name '("⟦" . "⟧")
   org-modern-block-fringe nil

   ;; Horizontal rules
   org-modern-horizontal-rule '(-0.5)

   ;; Hide tag brackets
   org-modern-tag nil))

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

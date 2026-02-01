;;; gptel-patch-diff.el --- gptel prompt + apply diffs  -*- lexical-binding: t; -*-

;;; gptel/org-git-apply-diff-at-point — minimal, block-local :dir only  -*- lexical-binding: t; -*-

(require 'org)
(require 'ob-core)
(require 'cl-lib)
(require 'subr-x) ;; string-suffix-p
(require 'vc)     ;; For vc-root-dir

(defun gptel/org-git-get-dir-from-src-line ()
  "Extract directory from a path on the #+begin_src line.
This is non-standard for Org Babel but provides a useful shorthand."
  (save-excursion
    (when (org-babel-goto-src-block-head)
      (when (looking-at org-babel-src-block-regexp)
        (let ((switches (match-string-no-properties 3)))
          (when (and switches (string-match "\\S-" switches))
            (let ((path (expand-file-name (thread-first switches string-trim))))
              (cond ((file-directory-p path) path)
                    ((file-exists-p path) (file-name-directory path))
                    ;; Assume it's a dir path even if it doesn't exist yet
                    (t (file-name-as-directory path))))))))))

(defun gptel/org-git-apply-diff-at-point (threeway)
  "Apply the unified diff in the Org src block at point using =git apply=.

The function determines the target repository directory in this order:
1. The value of a =:dir= header in the block.
2. A path provided on the #+begin_src line itself.
3. The root of the git repo containing the current buffer's file.
4. The directory of the current buffer.

With prefix arg THREEWAY (C-u), try =git apply --3way= in the final step."
  (interactive "P")
  (let* ((info (or (ignore-errors (org-babel-get-src-block-info 'light))
                   (user-error "Place point inside the diff src block to apply")))
         (lang   (nth 0 info))
         (patch  (nth 1 info))
         (params (nth 2 info)))
    (cl-block gptel/org-git-apply-diff-at-point
      (unless (and (stringp lang) (string-equal (downcase lang) "diff"))
        (user-error "This src block is %S; expected language: diff" lang))
      (let* ((repo-dir (or (cdr (assq :dir params))
                           (gptel/org-git-get-dir-from-src-line)
                           (and (buffer-file-name) (vc-root-dir))
                           default-directory))
             (default-directory (file-name-as-directory (expand-file-name repo-dir)))
             (tmp (make-temp-file "org-diff-" nil ".patch"))
             (buf (get-buffer-create "*gptel git apply*"))
             (exit 0))
        ;; Write patch to a temporary file
        (with-temp-file tmp
          (set-buffer-file-coding-system 'unix)
          (insert patch)
          (unless (string-suffix-p "\n" patch) (insert "\n")))

        ;; 1. Attempt to apply the patch directly and safely.
        (with-current-buffer buf (erase-buffer))
        (let ((apply-args (append '("--whitespace=fix" "--recount" "--ignore-whitespace")
                                  (when threeway '("--3way"))
                                  (list tmp))))
          (setq exit (apply #'call-process "git" nil buf t "apply" apply-args)))

        ;; 2. If the direct apply failed, offer the --reject fallback.
        (unless (zerop exit)
          (pop-to-buffer buf)
          (if (yes-or-no-p "git apply failed. Try again with --reject to create .rej files?")
              (progn
                (with-current-buffer buf (erase-buffer))
                (setq exit (call-process "git" nil buf t "apply"
                                         "--reject" "--recount" "--ignore-whitespace" tmp))
                (if (zerop exit)
                    (message "Applied with --reject. Inspect any *.rej files and finish manually.")
                  (pop-to-buffer buf)
                  (error "git apply --reject also failed. See *gptel git apply*")))
            ;; User chose not to try --reject
            (cl-return-from gptel/org-git-apply-diff-at-point)))

        ;; 3. Report final success if exit code was zero at any point.
        (when (zerop exit)
          (message "git apply: patch applied successfully in %s" repo-dir))))))


;;;; --- 2) System prompt for consistent, git-apply-ready diffs ---------------

(with-eval-after-load 'gptel
  (defconst gptel-patch-diff--system
    "You are a coding assistant that proposes changes as separate diffs in Emacs org mode source blocks that can be applied with =git apply=. Follow these rules exactly:

*1) Context:*
-   If the user has not provided the necessary files to provide an accurate answer or solution *STOP* and ask the user to provide more context.
-   Ask for specific files if they are not included, but are referenced.
-   *Important*: Don't guess or estimate a files content.

*1) Output Format:*
-   Produce one Org source block per patch.
-   Put all commentary outside the block.
-   *Important*: Always specify the project's root directory using the =:dir= header argument (e.g., =~/src/my-project= or =/path/to/project=).

    #+begin_src diff :dir /path/to/project
    ... unified diff content ...
    #+end_src

*2) Unified Diff Structure:*
-   Use standard Git headers. Paths must be relative to the repo root specified in =:dir= and must always include =a/= and =b/= prefixes.
    #+begin_src :dir ~/src/my-project
    diff --git a/REL/PATH b/REL/PATH
    --- a/REL/PATH
    +++ b/REL/PATH
    @@ -<old-start>,<old-len> +<new-start>,<new-len> @@
    #+end_src
-   *Never* use placeholders, ellipses (`...`), or any commentary inside the diff content. Provide complete, valid hunks.
-   Ensure a trailing newline if the change adds one.

*3) Handling New, Deleted, or Complex Changes:*
-   *For new files*, provide the entire file content in a =:tangle= block. Do not create a diff from =/dev/null=.

    #+begin_src python :tangle ~/src/my-project/new/file.py :mkdirp yes
    # New file content here
    #+end_src

-   *For file deletions*, use the standard Git diff format:

    #+begin_src diff :dir ~/src/my-project
    diff --git a/path/to/file.py b/path/to/file.py
    deleted file mode 100644
    index <hash>..0000000
    --- a/path/to/file.py
    +++ /dev/null
    @@ -1,10 +0,0 @@
    - ... all lines from the deleted file ...
    #+end_src

-   *For changes you cannot model with high confidence* (e.g., complex, multi-hunk changes where line counts are uncertain), provide the entire modified file in a =:tangle= block instead of a diff to ensure correctness.

*4) Make sure not to use emacs or org mode specific syntax in source code blocks.
-  Surrounding strings or template literals with equal signs i.e. =var_name= is only an org mode standard.

-  Most programming languages implement string interpolation with other characters like backticks i.e in typescript `Hello ${var_name}`.
"
    "System prompt used by the gptel “patch-diff” directive/preset.")

  ;; Make it selectable from gptel’s “System directives” menu.
  ;; (This is the built-in alist gptel uses for system prompts.)
  ;; https://elpa.nongnu.org/nongnu/gptel.html#usage (system directives are selectable from the menu) :contentReference[oaicite:0]{index=0}
  (setq gptel-directives
        (cons (cons 'patch-diff gptel-patch-diff--system)
              (assq-delete-all 'patch-diff gptel-directives)))

  ;; Also provide a preset that sets just the system message.
  ;; You can activate it from gptel’s menu or via @patch-diff in a prompt.
  ;; Presets are defined with gptel-make-preset. :contentReference[oaicite:1]{index=1}
  (when (fboundp 'gptel-make-preset)
    (gptel-make-preset 'patch-diff
      :description "Org =diff= blocks with a/b paths; apply via git"
      :system gptel-patch-diff--system
      :temperature 0)))

(provide 'gptel-patch-diff)
;;; gptel-patch-diff.el ends here

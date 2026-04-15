;;; gptel-agents.el --- Agent presets and tools for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Clay Morton
;; Keywords: convenience, tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides agent presets and tools for gptel, including
;; a context-manager tool that allows LLM agents to manage their own context.
;;
;; The context-manager tool supports three operations:
;; - add: Add files to the current context
;; - remove: Remove files from the current context
;; - replace: Replace the entire context with a new set of files

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'project)
(require 'cl-lib)
(require 'subr-x)

(defgroup gptel-agents nil
  "Agent presets and tools for gptel."
  :group 'gptel)

;;; Context Manager Tool

(defcustom gptel-agents-project-root-fallback nil
  "Fallback project root for gptel agent tools.

If non-nil, should be a directory path.  This is used when a project
cannot be detected via =project-current'."
  :group 'gptel-agents
  :type '(choice (const :tag "None" nil)
                 (directory :tag "Directory")))

(defun gptel-agents--project-root (&optional buffer)
  "Return the project root for BUFFER, or nil if none can be detected."
  (with-current-buffer (or buffer (current-buffer))
    (let ((featured (and (boundp 'gptel-context-manager-roots)
                         (car-safe gptel-context-manager-roots)))
          (proj (project-current nil)))
      (cond
       ((and (stringp featured) (not (string-empty-p featured)))
        (expand-file-name featured))
       (proj (expand-file-name (project-root proj)))
       ((and gptel-agents-project-root-fallback
             (file-directory-p gptel-agents-project-root-fallback))
        (expand-file-name gptel-agents-project-root-fallback))
       (t nil)))))

(defun gptel-agents--normalize-context-path (path &optional buffer)
  "Normalize PATH for adding/removing context in BUFFER.

If PATH is absolute, return it expanded.
If PATH is relative and a project root can be determined, expand it
relative to that root.  Otherwise expand relative to BUFFER's
=default-directory'."
  (let* ((buf (or buffer (current-buffer)))
         (root (gptel-agents--project-root buf)))
    (cond
     ((not (stringp path)) path)
     ((file-name-absolute-p path) (expand-file-name path))
     (root (expand-file-name path root))
     (t (with-current-buffer buf (expand-file-name path))))))

(defun gptel-agents--context-add-files (files &optional buffer)
  "Add FILES to the gptel context for BUFFER.
FILES is a list of file paths. BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer)))
        (added-count 0))
    (with-current-buffer buf
      (dolist (file files)
        (let* ((expanded (gptel-agents--normalize-context-path file buf))
               (root (gptel-agents--project-root buf)))
          (if (file-exists-p expanded)
              (progn
                (gptel-context-add-file expanded)
                (cl-incf added-count))
            (message "gptel-agents: File not found: %s (expanded to %s%s)"
                     file
                     expanded
                     (if root (format ", root %s" root) ""))))))
    (format "Added %d file(s) to context." added-count)))

(defun gptel-agents--context-remove-files (files &optional buffer)
  "Remove FILES from the gptel context for BUFFER.
FILES is a list of file paths. BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer)))
        (removed-count 0))
    (with-current-buffer buf
      (dolist (file files)
        (let ((expanded (gptel-agents--normalize-context-path file buf)))
          ;; Find and remove matching context entries
          (setq gptel-context
                (cl-remove-if
                 (lambda (item)
                   (let ((source (if (consp item) (car item) item)))
                     (when (and (stringp source)
                                (string= (expand-file-name source) expanded))
                       (cl-incf removed-count)
                       t)))
                 gptel-context)))))
    (format "Removed %d file(s) from context." removed-count)))

(defun gptel-agents--context-replace-files (files &optional buffer)
  "Replace the gptel context for BUFFER with FILES.
FILES is a list of file paths. BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      ;; Clear existing context (clean up overlays)
      (dolist (item gptel-context)
        (let ((props (cdr-safe item)))
          (when props
            (dolist (ov (plist-get props :overlays))
              (when (overlayp ov) (delete-overlay ov))))))
      (setq gptel-context nil))
    ;; Add new files
    (gptel-agents--context-add-files files buf)
    (format "Replaced context with %d file(s)." (length files))))

(defun gptel-agents--search-project (query &optional mode)
  "Search the current project for QUERY.
MODE can be \"content\" (default) or \"filename\"."
  (let* ((root (or (gptel-agents--project-root (current-buffer))
                   (let ((proj (project-current))) (and proj (project-root proj)))
                   default-directory))
         (default-directory root)
         (search-mode (or mode "content")))
    ;; Message the user so they know search is happening
    (message "gptel-agents: Searching %s for '%s'..." search-mode query)
    (pcase search-mode
      ("filename"
       (cond
        ((executable-find "fd")
         ;; fd is fast and respects gitignore
         (shell-command-to-string (format "fd -p %s" (shell-quote-argument query))))
        (t
         ;; Fallback to find
         (shell-command-to-string (format "find . -path *%s*" (shell-quote-argument query))))))
      ("content"
       (cond
        ((executable-find "rg")
         ;; rg: line number, smart case, no heading, limit to 50 results to save tokens
         (shell-command-to-string
          (format "rg -nS --no-heading %s | head -n 50" (shell-quote-argument query))))
        (t
         ;; Fallback to grep
         (shell-command-to-string
          (format "grep -rnI %s . | head -n 50" (shell-quote-argument query))))))
      (_ (format "Unknown mode: %s" mode)))))

;; Define the search tool
(gptel-make-tool
 :name "search_project"
 :function #'gptel-agents--search-project
 :description "Search the project codebase for text patterns or file names. Useful for finding definitions or locating files when you don't know the exact path.

Modes:
- 'content': (Default) Search file contents for the query string (like grep). Use this to find function definitions, class names, or usage.
- 'filename': Search for file paths matching the query.

Returns a list of matching lines or file paths."
 :args '((:name "query" :type string :description "The string to search for")
         (:name "mode" :type string :description "Search mode: 'content' or 'filename'" :enum ["content" "filename"]))
 :category "context"
 :confirm nil)

(defun gptel-agents--context-manager (operation files)
  "Manage gptel context based on OPERATION and FILES.
OPERATION is one of: \"add\", \"remove\", or \"replace\".
FILES is a list of file paths."
  ;; FIX: Convert vector to list if necessary (JSON parsing returns vectors)
  (let ((files (if (vectorp files) (append files nil) files)))
    (pcase operation
      ("add" (gptel-agents--context-add-files files))
      ("remove" (gptel-agents--context-remove-files files))
      ("replace" (gptel-agents--context-replace-files files))
      (_ (format "Unknown operation: %s. Use 'add', 'remove', or 'replace'." operation)))))

;; Define the tool
(gptel-make-tool
 :name "context_manager"
 :function #'gptel-agents--context-manager
 :description "Manage the files included in the conversation context. Use this tool to add, remove, or replace files that will be sent with subsequent prompts to the LLM.

Operations:
- 'add': Add the specified files to the existing context
- 'remove': Remove the specified files from the context
- 'replace': Clear all existing context and set it to only the specified files

File paths may be absolute or relative. Relative paths are resolved against the current project root if available. The tool will report how many files were successfully processed."
 :args '((:name "operation"
          :type string
          :description "The operation to perform: 'add', 'remove', or 'replace'"
          :enum ["add" "remove" "replace"])
         (:name "files"
          :type array
          :description "List of absolute file paths to add, remove, or use as replacement"
          :items (:type string)))
 :category "context"
 :confirm nil)

;;; System Prompts

(defvar gptel-agents--context-builder-system
  "You are a context-building assistant. Your GOAL is to prepare the conversation context by adding relevant files.

INSTRUCTIONS:
1. Analyze the user's request to identify which files are necessary.
2. Use =search_project= to find files if needed.
3. Use =context_manager= to add or replace files.
4. CRITICAL: Once you have added the necessary files, STOP calling tools.
5. When finished, respond with a summary and the text \"Context ready.\"
6. Do NOT attempt to answer the user's question.
7. Do NOT loop. If you have the core files, you are done.

Use project documentation/file trees to identify relevant files. Be selective."
  "System prompt for context-building agents.
These agents only add files to context and do not answer questions.")

(defvar gptel-agents--context-management-instructions
  "Context Management:
You have a =context_manager= tool to manage file context.

- Use 'add' ONLY if missing critical files.
- Use 'remove' to clean up.
- Use 'replace' to reset.

IMPORTANT: If you use =context_manager=, STOP immediately after the tool call.
 Do not continue generating text or calling more tools. Wait for the next user turn."
  "Instructions for agents on how to use the context_manager tool.
This is appended to system prompts that should have context management capability.")

;;; Presets

(with-eval-after-load 'gptel
  ;; Context builder presets - these only build context, they don't answer questions
  (gptel-make-preset 'clarify-api-build-context
    :description "Clarify API context builder"
    :model 'gemini-flash-lite-latest
    :tools (list (gptel-get-tool "context_manager")
                 (gptel-get-tool "search_project"))
    :context '("/Users/claymorton/developer/clarify-api/AGENTS.md")
    :system gptel-agents--context-builder-system)

  (gptel-make-preset 'clarify-ui-build-context
    :description "Clarify UI context builder"
    :model 'gemini-flash-lite-latest
    :tools (list (gptel-get-tool "context_manager")
                 (gptel-get-tool "search_project"))
    :context '("/Users/claymorton/developer/clarify-ui/AGENTS.md")
    :system gptel-agents--context-builder-system)

  (gptel-make-preset 'clarify-fullstack-build-context
    :description "Clarify fullstack context builder"
    :model 'gemini-flash-lite-latest
    :tools (list (gptel-get-tool "context_manager")
                 (gptel-get-tool "search_project"))
    :context '("/Users/claymorton/developer/clarify/AGENTS.md")
    :system gptel-agents--context-builder-system))

(provide 'gptel-agents)
;;; gptel-agents.el ends here

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

(defgroup gptel-agents nil
  "Agent presets and tools for gptel."
  :group 'gptel)

;;; Context Manager Tool

(defun gptel-agents--context-add-files (files &optional buffer)
  "Add FILES to the gptel context for BUFFER.
FILES is a list of file paths. BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer)))
        (added-count 0))
    (dolist (file files)
      (let ((expanded (expand-file-name file)))
        (if (file-exists-p expanded)
            (progn
              (gptel-context-add-file expanded buf)
              (cl-incf added-count))
          (message "gptel-agents: File not found: %s" file))))
    (format "Added %d file(s) to context." added-count)))

(defun gptel-agents--context-remove-files (files &optional buffer)
  "Remove FILES from the gptel context for BUFFER.
FILES is a list of file paths. BUFFER defaults to current buffer."
  (let ((buf (or buffer (current-buffer)))
        (removed-count 0))
    (with-current-buffer buf
      (dolist (file files)
        (let ((expanded (expand-file-name file)))
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
  (let* ((proj (project-current))
         (root (if proj (project-root proj) default-directory))
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

File paths should be absolute paths. The tool will report how many files were successfully processed."
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
  "You are a context-building assistant. Your ONLY task is to analyze the user's request and use the =context_manager= tool to add relevant files to the conversation context.

IMPORTANT RULES:
1. Do NOT attempt to answer the user's question or provide any code solutions.
2. Do NOT engage in conversation beyond acknowledging what files you are adding.
3. Your ONLY action should be calling the =context_manager= tool with operation 'add' or 'replace'.
4. Analyze the user's request to determine which files from the project would be necessary to answer their question.
5. After making the tool call, provide a brief summary of what files were added and why.
6. If you do not know the location of a file or definition, use the =search_project= tool to find it.

You have access to project documentation and file trees. Use these to identify:
- Source files directly related to the user's question
- Configuration files if relevant
- Type definitions or interfaces if working with typed code
- Test files if the question involves testing
- Related utility or helper files

Be thorough but selective - include files that are necessary, not every tangentially related file."
  "System prompt for context-building agents.
These agents only add files to context and do not answer questions.")

(defvar gptel-agents--context-management-instructions
  "
Context Management:*
You have access to a =context_manager= tool that allows you to manage the files included in this conversation's context.

-   Use =context_manager= with operation 'add' when you need additional files to understand or complete a task.
-   Use =context_manager= with operation 'remove' to remove files that are no longer relevant.
-   Use =context_manager= with operation 'replace' to completely reset the context to a specific set of files.

When to manage context:
-   If you need to see a file that was referenced but not provided, use 'add' to request it.
-   If the user asks you to work on a different part of the codebase, consider using 'replace' to focus on relevant files.
-   If context becomes cluttered with irrelevant files, use 'remove' to clean it up.

Always explain to the user when you are modifying the context and why."
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
    :context '("/Users/claymorton/developer/clarify-api/llm-docs/CLARIFY_IQ_BACKEND_CONTEXT.md"
               "/Users/claymorton/developer/clarify-api/llm-docs/file-tree.md")
    :system gptel-agents--context-builder-system)

  (gptel-make-preset 'clarify-ui-build-context
    :description "Clarify UI context builder"
    :model 'gemini-flash-lite-latest
    :tools (list (gptel-get-tool "context_manager")
                 (gptel-get-tool "search_project"))
    :context '("/Users/claymorton/developer/clarify-ui/llm-docs/CLARIFY_IQ_LLM_CONTEXT.md"
               "/Users/claymorton/developer/clarify-ui/llm-docs/file-tree.md")
    :system gptel-agents--context-builder-system)

  (gptel-make-preset 'clarify-fullstack-build-context
    :description "Clarify fullstack context builder"
    :model 'gemini-flash-lite-latest
    :tools (list (gptel-get-tool "context_manager")
                 (gptel-get-tool "search_project"))
    :context '("/Users/claymorton/developer/clarify-api/llm-docs/CLARIFY_IQ_BACKEND_CONTEXT.md"
               "/Users/claymorton/developer/clarify-api/llm-docs/file-tree.md"
               "/Users/claymorton/developer/clarify-ui/llm-docs/CLARIFY_IQ_LLM_CONTEXT.md"
               "/Users/claymorton/developer/clarify-ui/llm-docs/file-tree.md")
    :system gptel-agents--context-builder-system))

(provide 'gptel-agents)
;;; gptel-agents.el ends here

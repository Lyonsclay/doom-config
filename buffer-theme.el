;;; buffer-theme.el --- Buffer local theme management -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides functions to apply standard Emacs themes to individual
;; buffers using face-remapping.

;;; Code:

(require 'face-remap)

(defvar-local my/buffer-theme-cookies nil
  "Stores the face remapping cookies for the current buffer.")

(defun my/set-buffer-local-theme (theme)
  "Apply THEME to the current buffer only via face-remapping."
  (interactive
   (list
    (intern (completing-read "Load buffer-local theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))

  ;; 1. Load the theme definition (no-confirm=t, no-enable=t)
  ;; We load it so Emacs knows the settings, but we don't enable it globally.
  (load-theme theme t t)

  ;; 2. Clear existing remappings if any
  (when my/buffer-theme-cookies
    (mapc #'face-remap-remove-relative my/buffer-theme-cookies)
    (setq my/buffer-theme-cookies nil))

  ;; 3. Iterate over the theme's settings
  ;; Structure of entry: (type symbol theme-name spec)
  (let ((settings (get theme 'theme-settings)))
    (if (not settings)
        (message "No settings found for theme: %s" theme)
      (dolist (entry settings)
        (let ((type (nth 0 entry))
              (face (nth 1 entry))
              (spec (nth 3 entry)))

          ;; We only care about face definitions, not variables
          (when (eq type 'theme-face)
            ;; face-spec-choose calculates the attributes for the current display
            (let ((attrs (face-spec-choose spec)))
              (when attrs
                (push (face-remap-add-relative face attrs)
                      my/buffer-theme-cookies))))))
      (message "Buffer-local theme set to: %s" theme))))

(defun my/clear-buffer-local-theme ()
  "Remove the buffer-local theme."
  (interactive)
  (when my/buffer-theme-cookies
    (mapc #'face-remap-remove-relative my/buffer-theme-cookies)
    (setq my/buffer-theme-cookies nil)
    (message "Cleared buffer local theme")))

(provide 'buffer-theme)
;;; buffer-theme.el ends here


  ;; (display-buffer
  ;;  go-run-session-buffer
  ;;  '(
  ;;    ;; (display-buffer-reuse-window display-buffer-pop-up-window)
  ;;    (display-buffer-below-selected display-buffer-at-bottom)
  ;;    ;; (inhibit-same-window . t)
  ;;    (window-height . fit-window-to-buffer)
  ;;    ;; (display-buffer-reuse-window display-buffer-pop-up-frame)
  ;;    ;; (reusable-frames . visible)
  ;;    ))
  ;; (shell-command (format "go run %s > \"#go#run#session\"" (buffer-file-name)))
  ;; (with-current-buffer go-run-session-buffer
  ;;       ;; (find-file-noselect "#go#run#session")
  ;;   ;; (find-file "#go#run#session")
  ;;   (revert-buffer t t t )
  ;;   )
  ;;
  ;;
  ;; (find-file "~/developer/vblog/spinout/postgres/#go#run#session")
  ;; (if (find-buffer-visiting go-run-path)
  ;;     (find-file-noselect go-run-path)
  ;;   (find-file-other-window go-run-path)
  ;;   )
    ;; (find-file-other-window go-run-path)
    ;; (evil-window-prev 1)
    ;; (find-buffer-visiting go-run-path)
  ;; (evil-window-split)
  ;; (evil-window-down 1)
  ;; (evil-window-up 1)

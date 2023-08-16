(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]venv\\'" "[/\\\\]\\.venv\\'" "[/\\\\]\\.mypy_cache\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "bazel-[^/\\\\]+\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]\\checkouts\\'" "[/\\\\]\\.m2\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'" "[/\\\\]\\.elixir_ls\\'" "[/\\\\]\\.direnv\\'"))
 '(nodejs-repl-command "node --experimental-fetch")
 '(py-yapf-options '("column_limit=80"))
 '(safe-local-variable-values
   '((eglot-workspace-configuration
      (:pyls :plugins
       (:jedi_completion
        (:include_params t)))
      (:gopls :usePlaceholders t))
     (eglot-workspace-configuration
      (gopls
       (staticcheck . t)
       (matcher . "CaseSensitive")))
     (lsp-python-ms-extra-paths .
      ["/Users/claymorton/developer/eltoro/venv/lib/python3.10/site-packages/"])
     (lsp-python-ms-extra-paths .
      ["~/developer/eltoro/venv/lib/python3.10/site-packages/"])))
 '(sql-connection-alist
   '(("gorge"
      (sql-product 'postgres)
      (sql-user "admin")
      (sql-database "gorge")
      (sql-password "*****")
      (sql-server "postgresql://postgres@127.0.01:5432"))))
 '(warning-suppress-types '((initialization))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)

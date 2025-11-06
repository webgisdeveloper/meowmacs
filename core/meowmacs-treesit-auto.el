;;; meowmacs-treesit-auto.el -*- lexical-binding: t; -*-

;;; Commentary:
;; treesit-auto configuration for automatic tree-sitter mode selection

;;; Code:

(use-package treesit-auto
  :ensure t
  :demand t  ;; Load immediately, don't defer
  :config
  (setq treesit-auto-install 'prompt)  ;; Prompt before installing grammars
  (global-treesit-auto-mode))

(provide 'meowmacs-treesit-auto)

;;; meowmacs-treesit-auto.el ends here

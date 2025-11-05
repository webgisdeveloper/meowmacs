;;; meowmacs-treesit-auto.el -*- lexical-binding: t; -*-

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'meowmacs-treesit-auto)

;;; meowmacs-treesit-auto.el-treesitter.el ends here

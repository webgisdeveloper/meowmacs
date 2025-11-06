;;; funmacs-yaml.el --- YAML configuration -*- lexical-binding: t; -*-

;; Associate .yml and .yaml files with yaml-ts-mode
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(provide 'funmacs-yaml)

;;; funmacs-yaml.el ends here

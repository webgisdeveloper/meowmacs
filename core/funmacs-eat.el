;;; funmacs-eat.el --- Terminal configuration using EAT -*- lexical-binding: t; -*-

(use-package eat
  :defer t
  :commands (eat eat-other-window eat-project)
  :init
  ;; Use default login shell from environment
  (setq eat-shell (getenv "SHELL")
        eat-shell-options '("--login"))  ;; --login is equivalent to -l

  ;; Better scrolling
  (setq eat-term-maximum-scrollback 10000)

  ;; Keybindings - use C-c e prefix instead of conflicting C-c p
  (global-set-key (kbd "C-c e t") #'eat)
  (global-set-key (kbd "C-c e T") #'eat-other-window)

  :config
  ;; Enable 256 color support in eat
  (setq eat-term-name "xterm-256color")
  
  ;; Enable color output
  (setq eat-enable-auto-line-mode t
        eat-enable-blinking-text t
        eat-enable-alternative-display t)
  
  ;; Keybindings
  (global-set-key (kbd "C-c e t") #'eat)
  (global-set-key (kbd "C-c e T") #'eat-other-window)
  ;; Integrate with project.el
  (defun funmacs-eat-project ()
    "Open an EAT terminal in the project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (eat-other-window)))
  (global-set-key (kbd "C-c e p") #'funmacs-eat-project))


(provide 'funmacs-eat)

;;; funmacs-eat.el ends here

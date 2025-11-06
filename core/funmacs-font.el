;;; funmacs-font.el --- fonts -*- lexical-binding: t; -*-
;; Use Nerd font if available, fallback silently otherwise

;;; Commentary:
;; font settings

;;; code

(when (member "0xProto Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :family "0xProto Nerd Font Mono" :height 160)
  (set-face-attribute 'fixed-pitch nil :family "0xProto Nerd Font Mono" :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "Inter" :height 1.0))

(setq-default line-spacing 0.2)

(provide 'funmacs-font)

;;; funmacs-font.el ends here

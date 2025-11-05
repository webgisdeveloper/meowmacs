;;; early-init.el --- Early Initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Funmacs Maximum performance early-init Configuration
;; Loaded before GUI initialization and package system

;;; Code:

;; =============================================================================
;; GARBAGE COLLECTION OPTIMIZATION
;; =============================================================================

;; Maximize GC threshold during startup to prevent collections
;; This can reduce startup time by 50% or more
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Store default file-name-handler-alist and restore after startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; =============================================================================
;; PACKAGE SYSTEM OPTIMIZATION
;; =============================================================================

;; Disable package.el at startup (we'll handle it in init.el)
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Prevent premature loading of packages
(setq load-prefer-newer t)

;; =============================================================================
;; UI ELEMENT REMOVAL
;; =============================================================================

;; Disable UI elements before they're rendered (faster startup)
;; Set frame parameters before frame creation
(setq default-frame-alist
      '((min-height . 1)
        (height . 45)
        (min-width . 1)
        (width . 101)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 0)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

;; Set the initial frame size and appearance *before* the frame is created
(setq initial-frame-alist
      '((height . 45)
        (width . 101)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)))

;; macOS-specific frame size fix
;; On macOS, frame dimensions sometimes need to be set explicitly after frame creation
(when (eq system-type 'darwin)
  ;; Ensure width and height are at the front of the alist (higher priority)
  (setq default-frame-alist
        (cons '(width . 101) 
              (cons '(height . 45)
                    (assq-delete-all 'height
                                     (assq-delete-all 'width default-frame-alist)))))
  (setq initial-frame-alist
        (cons '(width . 101)
              (cons '(height . 45)
                    (assq-delete-all 'height
                                     (assq-delete-all 'width initial-frame-alist)))))
  
  ;; Force frame size after GUI initialization
  (add-hook 'window-setup-hook
            (lambda ()
              (when (display-graphic-p)
                (set-frame-width (selected-frame) 101)
                (set-frame-height (selected-frame) 45)))))


;; Early GUI toggles must be guarded for batch/tty
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))   ;; safe in tty too
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))   ;; safe in GUI only
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; GUI-only

;; Disable startup screens
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; =============================================================================
;; FRAME AND WINDOW OPTIMIZATION
;; =============================================================================

;; Prevent frame resizing when adjusting fonts
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

;; Window divider settings
(setq window-divider-default-right-width 2
      window-divider-default-bottom-width 2
      window-divider-default-places t)
(window-divider-mode 1)

;; No frame padding for maximum content area
(modify-all-frames-parameters '((internal-border-width . 0)))

;; Slim, symmetric fringes for minimal gutters
(when (fboundp 'fringe-mode) (fringe-mode '(4 . 4))) ;; GUI-only

;; =============================================================================
;; NATIVE COMPILATION
;; =============================================================================

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors 'silent  ;; Suppress native-comp warnings
        native-comp-deferred-compilation t
        native-comp-async-jobs-number 4))

;; Suppress compiler and bytecomp warnings from displaying
(setq warning-suppress-types '((comp) (bytecomp)))

;; =============================================================================
;; PROCESS OPTIMIZATION
;; =============================================================================

;; Increase the amount of data read from processes
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; =============================================================================
;; PGTK OPTIMIZATION (Wayland/Pure GTK)
;; =============================================================================

;; Reduce pgtk timeout for better performance on Wayland
(when (featurep 'pgtk)
  (setq pgtk-wait-for-event-timeout 0.001))

;; =============================================================================
;; VERSION CONTROL OPTIMIZATION
;; =============================================================================

;; Disable version control during startup
(defvar default-vc-handled-backends vc-handled-backends)
(setq vc-handled-backends nil)

;; =============================================================================
;; MISC STARTUP OPTIMIZATIONS
;; =============================================================================

;; Disable site-run-file
(setq site-run-file nil)

;; Disable bidirectional text rendering for slight performance boost
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disable warnings and reduce noise
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;; Faster font rendering
(setq inhibit-compacting-font-caches t)

;; Disable automatic file handler during startup
(setq auto-mode-case-fold nil)

;; =============================================================================
;; RESTORE SETTINGS AFTER STARTUP
;; =============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore garbage collection settings (16MB threshold)
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            
            ;; Restore file name handler
            (setq file-name-handler-alist default-file-name-handler-alist)
            
            ;; Restore version control backends
            (setq vc-handled-backends default-vc-handled-backends)
            
            ;; Garbage collect when losing focus (optional but useful)
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))
            
            ;; ;; Display startup time
            ;; (message "Emacs loaded in %s with %d garbage collections."
            ;;          (emacs-init-time "%.2f seconds")
            ;;          gcs-done)
	    ))

(provide 'early-init)

;;; early-init.el ends here

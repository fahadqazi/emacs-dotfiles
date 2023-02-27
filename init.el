(setq user-full-name "Fahad Qazi"
      user-mail-address "fahadqazi@gmail.com")

(setq inhibit-startup-message t) ;Don't show Emacs start up message
(setq make-backup-files nil)  ;Disable backup file creation
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(global-hl-line-mode t)
;;(global-visual-line-mode t)
(setq visible-bell nil)
(setq org-agenda-include-diary t)
(setq org-adapt-indentation t
      org-hide-leading-stars t)


;; Experimental time tracking stuff
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Adding denote
(setq denote-directory (expand-file-name "~/time-tracking/"))

;; Move some of the messy stuff from the bottom to separate file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Font Configuration ----------------------------------------------------------
;;(set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 170)
(set-face-attribute 'default nil :font "Iosevka Comfy" :height 170)

;; Load theme -----------------------------------------------------------------
(load-theme 'ef-light t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Not needed - use M-x package-refresh-contents
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
;;(unless (package-installed-p 'use-package)
;;(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil)

(column-number-mode)

(global-display-line-numbers-mode t)
;;(setq linum-format "%d ")

(defun fahad/toggle-line-numbers ()
  "Toggles display of line numbers"
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode)))

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package corfu
  :ensure t
  :config
  (corfu-mode 1))

(use-package org)
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

;; `consult', `orderless', `embark' (the latter for the future) 

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org-bullets
  :ensure t
  :config
  (org-bullets-mode 1))

(setq lsp-keymap-prefix "s-l")

;; Should I get rid of this "M-3" - I added this because I coudn't type #
(global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#"))):
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c d") #'fahad/duplicate-line)
(global-set-key (kbd "C-v") #'fahad/scroll-down-half-page)
(global-set-key (kbd "M-v") #'fahad/scroll-up-half-page)

(setq org-directory "~/org")

(setq org-agenda-files (list "gtd.org"
                             "my-life.org"))

(setq org-capture-templates
      '(
	("t" "Task" entry
	 (file+headline "gtd.org" "Tasks")
         "* TODO %?")))

;; Todo keywords. Change these to your liking
(setq org-todo-keywords
    '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")))

(defun fahad/multi-line-next()
  (interactive)
  (next-line 10))

(defun fahad/multi-line-previous()
  (interactive)
  (next-line -10))

(defun fahad/toggle-whitespace ()
  "Toggles whitespace"
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(defun fahad/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; (defun fahad/scroll-half-page-forwards-and-center ()
;;   "Scroll half the page forward and center point"
;;   (interactive)
;;   (View-scroll-half-page-forward)
;;   (recenter-top-bottom))

;; (defun fahad/scroll-half-page-backwards-and-center ()
;;   "Scroll half the page and center point"
;;   (interactive)
;;   (View-scroll-half-page-backward)
;;   (recenter-top-bottom))
(defun fahad/scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
      ((= ln lmax) (recenter (window-end)))
      (t (progn
           (move-to-window-line -1)
           (recenter))))))

(defun fahad/scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
      ((= ln lmax) (move-to-window-line nil))
      (t (progn
           (move-to-window-line 0)
           (recenter))))))

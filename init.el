(setq user-full-name "Fahad Qazi"
      user-mail-address "fahadqazi@gmail.com")

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(global-hl-line-mode t)
;;(global-visual-line-mode t)
(setq visible-bell nil)
(setq org-agenda-include-diary t)
(setq org-adapt-indentation t
      org-hide-leading-stars t)
(setq org-use-speed-commands t)

;; Experimental time tracking stuff
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-use-speed-commands 1)
;; Adding denote
(setq denote-directory (expand-file-name "~/org/"))

;; Move some of the messy stuff from the bottom to separate file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Font Configuration ----------------------------------------------------------
;;(set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 170)
;;(set-face-attribute 'default nil :font "Iosevka Comfy" :height 170)
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 150)

;; Load theme -----------------------------------------------------------------
(load-theme 'ef-light t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Not needed - use M-x package-refresh-contents
;;(unless package-archive-contents
;; (package-refresh-contents))

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
(use-package avy)
(use-package org)
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package denote)

(use-package which-key
    :config
    (which-key-mode))

;; `consult', `orderless', `embark' (the latter for the future) 

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org-bullets
  :hook (( org-mode ) . org-bullets-mode))



(global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#"))):
;;(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c d") #'fahad/duplicate-line)
(global-set-key (kbd "C-v") #'fahad/scroll-down-half-page)
(global-set-key (kbd "M-v") #'fahad/scroll-up-half-page)
(global-set-key (kbd "C-c ;") #'avy-goto-char-timer)
(global-set-key [(shift return)] #'fahad/new-line-below)

(setq org-directory "~/org")

(setq org-agenda-files (list "tasks.org" "gtd.org" "studying.org" "work.org"
                             "my-life.org"))

(setq org-todo-keywords
    '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")))

(defun fahad/jump-to-heading()
  (interactive)
  (org-back-to-heading)
  (beginning-of-line))

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

(defun fahad/new-line-below()
  "Create a new line bellow point."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(setq org-capture-templates
      `(("b" "Basic task for future review" entry
         (file+headline "tasks.org" "Tasks to be reviewed")
         ,(concat "* %^{Title}\n"
                  ":PROPERTIES:\n"
                  ":CAPTURED: %U\n"
                  ":END:\n\n"
                  "%i%l")
         :empty-lines-after 1)
        ("s" "Clock in to a study task" entry
         (file+olp+datetree "studying.org" "Clocked tasks")
         ,(concat "* TODO %^{Title}\n"
                  "SCHEDULED: %T\n"
                  ":PROPERTIES:\n"
                  ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                  ":END:\n\n")
         :prepend t
         :clock-in t
         :clock-keep t
         :immediate-finish t
         :empty-lines-after 1)
	("w" "Clock in to a work task" entry
         (file+olp+datetree "work.org" "Clocked tasks")
         ,(concat "* TODO %^{Title}\n"
                  "SCHEDULED: %T\n"
                  ":PROPERTIES:\n"
                  ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                  ":END:\n\n")
         :prepend t
         :clock-in t
         :clock-keep t
         :immediate-finish t
         :empty-lines-after 1)
        ("m" "Memorandum of conversation" entry
         (file+headline "tasks.org" "Tasks to be reviewed")
         ,(concat "* Memorandum of conversation with %^{Person}\n"
                  ":PROPERTIES:\n"
                  ":CAPTURED: %U\n"
                  ":END:\n\n"
                  "%i%?")
         :empty-lines-after 1)
        ("t" "Task with a due date" entry
         (file+headline "tasks.org" "Tasks with a date")
         ,(concat "* TODO %^{Title} %^g\n"
                  "SCHEDULED: %^t\n"
                  ":PROPERTIES:\n"
                  ":CAPTURED: %U\n"
                  ":END:\n\n"
                  "%a\n%i%?")
         :empty-lines-after 1)
        ("e" "Email note" entry
         (file+headline "tasks.org" "Tasks to be reviewed")
         ,(concat "* MAYBE %:subject :mail:\n"
                  ":PROPERTIES:\n"
                  ":CAPTURED: %U\n"
                  ":END:\n\n"
                  "%a\n%i%?")
         :empty-lines-after 1)
        ;; ;; NOTE 2023-01-29: See improved version in the
        ;; ;; prot-org.el further below.  It runs a custom function
        ;; ;; of mine that produces a more personalised template
        ;; ;; than what the built-in options support.
        ;;
        ;; ("p" "Private lesson or service" entry
        ;;  (file "coach.org")
        ;;  ,(concat "* COACH %^{Title} %^g\n"
        ;;           "%(prot/org-date-prompt-range-increment)"
        ;;           ":PROPERTIES:\n"
        ;;           ":CAPTURED: %U\n"
        ;;           ":APPT_WARNTIME: 20\n"
        ;;           ":END:\n\n"
        ;;           "%a\n%i%?")
        ;;  :prepend t
        ;;  :empty-lines 1)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package prettier-js							     ;;
;;   :ensure t)									     ;;
;; 										     ;;
;; (use-package add-node-modules-path						     ;;
;;   :ensure t									     ;;
;;   :hook (typescript-mode . add-node-modules-path))				     ;;
;; 										     ;;
;; (add-hook 'web-mode-hook #'(lambda ()					     ;;
;;                              (enable-minor-mode				     ;;
;;                               '("\\.jsx?\\'" . prettier-js-mode))		     ;;
;; 			     (enable-minor-mode					     ;;
;;                               '("\\.tsx?\\'" . prettier-js-mode))))		     ;;
;; 										     ;;
;; (eval-after-load 'web-mode							     ;;
;;   '(progn									     ;;
;;      (add-hook 'web-mode-hook #'add-node-modules-path)			     ;;
;;      (add-hook 'web-mode-hook #'prettier-js-mode)))				     ;;
;; 										     ;;
;; (use-package flycheck							     ;;
;;   :ensure t									     ;;
;;   :hook ((after-init . global-flycheck-mode)))				     ;;
;; 										     ;;
;; (use-package company								     ;;
;;   :ensure t									     ;;
;;   :hook ((after-init . global-company-mode)))				     ;;
;; 										     ;;
;; (use-package typescript-mode							     ;;
;;   :ensure t									     ;;
;;   :after (flycheck)								     ;;
;;   :config									     ;;
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))		     ;;
;; 										     ;;
;; (use-package tide								     ;;
;;   :ensure t									     ;;
;;   :after (typescript-mode flycheck)						     ;;
;;   :hook ((typescript-mode . tide-setup)					     ;;
;; 	 (typescript-mode . tide-hl-identifier-mode)				     ;;
;; 	 )									     ;;
;;   :config (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))	     ;;
;; 										     ;;
;; (use-package lsp-mode							     ;;
;;   :init									     ;;
;;   (setq lsp-keymap-prefix "C-c l")						     ;;
;;   :bind (:map lsp-mode-map							     ;;
;; 	      ("C-c l" . lsp-command-map)					     ;;
;; 	      ("C-c d" . lsp-describe-thing-at-point)				     ;;
;; 	      ("C-c a" . lsp-execute-code-action))				     ;;
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)	     ;;
;;          (typescript-mode . lsp)						     ;;
;;          (lsp-mode . lsp-enable-which-key-integration))			     ;;
;;   :commands lsp)								     ;;
;; 										     ;;
;; (use-package lsp-ui :commands lsp-ui-mode)					     ;;
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)			     ;;
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)			     ;;
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)		     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package evil									        ;;
;;   :ensure t										        ;;
;;   :init										        ;;
;;   (setq evil-want-integration t) ;; This is optional since it's already set to t by default. ;;
;;   (setq evil-want-keybinding nil)							        ;;
;;   :config										        ;;
;;   (evil-mode 1))									        ;;
;; 											        ;;
;; (use-package evil-collection								        ;;
;;   :after evil									        ;;
;;   :ensure t										        ;;
;;   :config										        ;;
;;   (evil-collection-init))								        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package prettier-js
  :ensure t)

(add-hook 'lsp-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

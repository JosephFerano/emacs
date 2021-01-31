;; TODO: Look into this, maybe it's useful
;; (require 'server)
;; (if (not (server-running-p)) (server-start))

;; General Settings
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks t)
(setq backup-directory-alist `((".*" . "~/.emacs.d/saves"))
      delete-old-versions t)
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq delete-by-moving-to-trash t)
(setq user-full-name "Joseph Ferano"
      user-mail-address "joseph@ferano.io")
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
(global-auto-revert-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Visuals
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(setq visible-bell nil ring-bell-function 'flash-mode-line)
(setq-default display-line-numbers 'relative)
(dolist (mode '(org-mode-hook term-mode-hook eshell-mode-hook dired-mode-hook shell-mode-hook magit-mode-hook))
              (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(defun joe/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(global-hl-line-mode +1)
(column-number-mode +1)

;; Text Settings
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(defun joe/edit-init()
  "Edit 'init.el' quickly"
  (interactive)
  (find-file user-init-file))

(require 'package)
(setq package-archives
    '(("org"       . "http://orgmode.org/elpa/")
      ("gnu"       . "http://elpa.gnu.org/packages/")
      ("melpa"     . "https://melpa.org/packages/")))
      ;; ("marmalade" . "http://marmalade-repo.org/packages/")))

(add-hook 'before-save-hook 'whitespace-cleanup)

;; TODO: We need 3 bindings; 1.) close buffer 2.) close window 3.) close buffer and window
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Packages

(package-initialize)

;; Why we use this line
;; https://www.reddit.com/r/emacs/comments/1rdstn/set_packageenableatstartup_to_nil_for_slightly/
(setq package-enable-at-startup nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(beacon-color "#f2777a")
 '(custom-safe-themes
   '("aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" default))
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(ahk-mode magit rainbow-delimiters csharp-mode doom-themes marginalia eglot fsharp-mode selectrum-prescient prescient selectrum avy evil-commentary evil-embrace evil-snipe evil-collection evil-surround undo-tree which-key dashboard))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-hunk-heading-highlight ((t (:extend t :background "cornflower blue" :foreground "#212337" :weight bold)))))

(dolist (p package-selected-packages)
    (when (not (package-installed-p p))
	(package-install p)))

(load-theme 'doom-moonlight +1)

(setq evil-want-keybinding nil)
(setq evil-undo-system 'undo-tree)
(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)
(global-undo-tree-mode)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode)

(require 'dired)
(setq ls-lisp-dirs-first t)
(put 'dired-find-alternate-file 'disabled nil)
(defun joe/dired-find-file-other-window ()
    "In Dired, visit this file or directory in another window."
    (interactive)
    (find-file-other-window (dired-get-file-for-visit)))
(add-hook 'dired-mode-hook
          (lambda ()
            (evil-define-key 'normal dired-mode-map (kbd "-")
              (lambda () (interactive) (find-alternate-file "..")))
            (evil-define-key 'normal dired-mode-map (kbd "SPC")
              (lambda () (interactive) (evil-send-leader)))
            (evil-define-key 'normal dired-mode-map (kbd "<return>")
              (lambda () (interactive) (dired-find-alternate-file)))))

(evil-define-key 'normal 'global (kbd "-") 'dired-jump)

(require 'dashboard)
(dashboard-setup-startup-hook)

(require 'which-key)
(setq which-key-idle-delay 0.3)
(which-key-mode)

(which-key-add-keymap-based-replacements evil-normal-state-map
  "<leader>f" '("Files")
  "<leader>b" '("Buffers")
  "<leader>h" '("Help"))

(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fi") 'edit-init)
(evil-define-key 'normal 'global (kbd "<leader>bl") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>h") 'help-command)

(defun joe/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(evil-define-key 'normal 'global (kbd "<leader>bb") 'joe/switch-to-previous-buffer)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-snipe)
(evil-snipe-override-mode 1)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'avy)
(setq avy-keys '(?a ?s ?d ?f ?w ?e ?r ?u ?i ?o ?h ?j ?k ?l ?x ?c ?m))
(setq avy-all-windows nil)
(setq avy-background t)
(defvar avy-map (make-sparse-keymap))

(define-key evil-normal-state-map (kbd ",") avy-map)
(define-key avy-map "b" #'avy-goto-word-0-above)
(define-key avy-map "w" #'avy-goto-word-0-below)
(define-key avy-map "c" #'avy-goto-char-timer)

(require 'selectrum)
(require 'prescient)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
(marginalia-mode +1)
(advice-add #'marginalia-cycle :after
  (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

(setq recentf-max-menu-items 50)
(defun joe/recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(evil-define-key 'normal 'global (kbd "<leader>fr") 'recentf-open-files+)

(require 'smartparens)
(smartparens-global-mode +1)
(show-paren-mode +1)


;; TODO: Packages to check out
;; expand-region
;; company
;; flycheck
;; magit
;; projectile
;; Hydra (we can use it for some of the ideas I've had about repeating and arranging stuff)
;; CTRLF (figure out if it does anything interesting)

;;      helpful
;;    exec-path-from-shell
;;    company-quickhelp
;;    color-theme-sanityinc-tomorrow
;;    flycheck
;;    general
;;    evil-mc
;;    markdown-preview-mode
;;    markdown-mode
;;    company-mode
;;    company
;;    omnisharp
;;    sly
;;      )))
;;  '(send-mail-function (quote smtpmail-send-it)))

;;(use-package general
;;  :init
;;  (general-evil-setup)
;;  (setq leader "SPC")
;;  (general-nmap :prefix leader "fi" 'edit-init)
;;  (general-nmap :prefix leader "h" 'help-command)
;;  (general-nmap :prefix leader "s" 'isearch-forward)
;;  (general-nmap "Y" 'evil-yank)
;;  (general-nmap "C-h" 'evil-window-left)
;;  (general-nmap "C-j" 'evil-window-down)
;;  (general-nmap "C-k" 'evil-window-up)
;;  (general-nmap "C-l" 'evil-window-right)
;;  (general-nmap "C-s" 'save-buffer))

;; (electric-pair-mode 1)

;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq slime-contribs '(slime-fancy slime-company))

;; (use-package recentf-mode
;;   :init
;;   (setq recentf-max-menu-items 50)
;;   (recentf-mode 1)
;;   :config
;;   (add-hook 'find-file-hook 'recentf-save-list)
;;   :general
;;   (general-nmap :prefix leader "fr" 'recentf-open-files))

; (use-package company
;   :init
;   (setq company-idle-delay 0.05)
;   (setq company-minimum-prefix-length 2)
;   (add-hook 'after-init-hook 'global-company-mode))

; (use-package flycheck
;   :init
;   (global-flycheck-mode))

;;; init.el ends here

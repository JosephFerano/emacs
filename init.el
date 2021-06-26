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

(defun joe/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil ring-bell-function 'joe/flash-mode-line)
(setq-default display-line-numbers 'relative)
(dolist (mode '(org-mode-hook term-mode-hook vterm-mode-hook eshell-mode-hook dired-mode-hook shell-mode-hook magit-mode-hook))
              (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(global-hl-line-mode +1)
(column-number-mode +1)

(set-face-attribute 'default nil :height 90)

;; Text Settings
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows nil)

(defun joe/edit-init()
  "Edit 'init.el' quickly"
  (interactive)
  (find-file user-init-file))

(defun joe/show-full-path ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun joe/toggle-buffer-mode ()
  "Toggles the current major mode between actual and fundamental mode. This will act as a way to easily get
all of the evil keybindings in buffers like magit, without compromises."
  (interactive)
  (let ((previous-mode major-mode))
    (unless (boundp 'joe/buffer-previous-mode)
      (setq-local joe/buffer-previous-mode major-mode))
    (if (equal major-mode 'fundamental-mode)
        (funcall joe/buffer-previous-mode)
        (progn
          (fundamental-mode)
          (setq-local joe/buffer-previous-mode previous-mode)))))

(defun joe/revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

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
 '(mini-frame-show-parameters '((top . 0.3) (width . 0.8) (left . 0.5)))
 '(package-selected-packages
   '(all-the-icons-dired all-the-icons ranger org-bullets sudoku vterm select-themes fsharp-mode eglot-fsharp fish-mode find-file-in-project helpful ahk-mode magit rainbow-delimiters csharp-mode doom-themes marginalia eglot selectrum-prescient prescient selectrum avy evil-commentary evil-embrace evil-snipe evil-collection evil-surround undo-tree which-key dashboard))
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

(setq custom-safe-themes t)
(load-theme 'doom-one t)

(setq evil-want-keybinding nil)
(setq evil-undo-system 'undo-tree)
(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)
(global-undo-tree-mode)
(require 'evil)
(evil-mode)
(require 'evil-collection)
(evil-collection-init)

(require 'dired)
(if (eq system-type 'windows-nt)
    (setq ls-lisp-dirs-first t)
    (setq dired-listing-switches "-al --group-directories-first"))

(put 'dired-find-alternate-file 'disabled nil)

(defun joe/dired-find-file-other-window ()
    "In Dired, visit this file or directory in another window."
    (interactive)
    (find-file-other-window (dired-get-file-for-visit)))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; (evil-define-key 'normal dired-mode-map (kbd "j") 'peep-dired-next-file)
            ;; (evil-define-key 'normal dired-mode-map (kbd "k") 'peep-dired-prev-file)
            ;; (evil-define-key 'normal dired-mode-map (kbd "-")
              ;; (lambda () (interactive) (find-alternate-file "..")))
              ;; (lambda () (interactive) (find-alternate-file "..")))
            (evil-define-key 'normal dired-mode-map (kbd "<return>")
              (lambda () (interactive) (dired-find-alternate-file)))))

;; (evil-define-key 'normal 'global (kbd "-") 'dired-jump)
(evil-define-key 'normal 'global (kbd "-") 'ranger)

(require 'dashboard)
(dashboard-setup-startup-hook)

(require 'which-key)
(setq which-key-idle-delay 0.3)
(which-key-mode)

(which-key-add-keymap-based-replacements evil-normal-state-map
  "<leader>f" '("Files")
  "<leader>b" '("Buffers")
  "<leader>d" '("Dired")
  "<leader>g" '("Git")
  "<leader>t" '("Tabs")
  "<leader>p" '("Packages")
  "<leader>h" '("Help"))

(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>w")  'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>q")  'kill-buffer-and-window)
(evil-define-key 'normal 'global (kbd "<leader>h")  'help-command)
(evil-define-key 'normal 'global (kbd "<leader>hf") 'helpful-callable)
(evil-define-key 'normal 'global (kbd "<leader>hv") 'helpful-variable)
(evil-define-key 'normal 'global (kbd "<leader>hk") 'helpful-key)
(evil-define-key 'normal 'global (kbd "<leader>ho") 'helpful-symbol)
(evil-define-key 'normal 'global (kbd "<leader>hg") 'helpful-at-point)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'bookmark-jump)
(evil-define-key 'normal 'global (kbd "<leader>fi") 'joe/edit-init)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'mode-line-other-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bl") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bi") 'ibuffer)
(evil-define-key 'normal 'global (kbd "<leader>bm") 'joe/toggle-buffer-mode)
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
(evil-define-key 'normal 'global (kbd "<leader>gr") 'joe/revert-buffer-no-confirm )
(evil-define-key 'normal 'global (kbd "<leader>tn") 'tab-new)
(evil-define-key 'normal 'global (kbd "<leader>pi") 'package-install)
(evil-define-key 'normal 'global (kbd "<leader>pd") 'package-delete)
(evil-define-key 'normal 'global (kbd "<leader>pr") 'package-refresh-contents)
(evil-define-key 'normal 'global (kbd "<leader>pl") 'package-list-packages)

(evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)
(evil-define-key 'normal 'global (kbd "M-l") 'tab-next)
(evil-define-key 'normal 'global (kbd "M-h") 'tab-previous)

;; TODO figure out how to create these kinds of aliases
;; (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)

;; abbr -a -g ga     "git add"
;; abbr -a -g gch    "git checkout"
;; abbr -a -g gchm   "git checkout master"
;; abbr -a -g gchb   "git checkout -b"
;; abbr -a -g gd     "git diff"
;; abbr -a -g gdh    "git diff HEAD"
;; abbr -a -g gm     "git merge"
;; abbr -a -g gms    "git merge --squash"
;; abbr -a -g gb     "git branch"
;; abbr -a -g gba    "git branch -a"
;; abbr -a -g gf     "git fetch"
;; abbr -a -g gr     "git rebase"
;; abbr -a -g gc     "git commit"
;; abbr -a -g gcm    "git commit -m"
;; abbr -a -g gcau   "git commit --author"
;; abbr -a -g gcam   "git commit -am"
;; abbr -a -g ga     "git add"
;; abbr -a -g gaa    "git add -A"
;; abbr -a -g gpl    "git pull"
;; abbr -a -g gp     "git push"
;; abbr -a -g gpd    "git push -d origin"
;; abbr -a -g gpu    "git push -u origin"
;; abbr -a -g gpr    "git remote prune origin"
;; abbr -a -g grh    "git reset --hard"
;; abbr -a -g gcl    "git clean -fd"
;; abbr -a -g gst    "git stash"
;; abbr -a -g gsl    "git stash list"
;; abbr -a -g gsp    "git stash pop"
;; abbr -a -g gsu    "git submodule update"
;; abbr -a -g glom   "git lop -10 origin/master"
;; abbr -a -g gmom   "git merge origin/master"
;; abbr -a -g gmm    "git merge master"

(defvar global-evil-leader-map (make-sparse-keymap))
(evil-define-key 'normal 'global-evil-leader-map (kbd "SPC") 'evil-send-leader)

(define-minor-mode global-evil-leader-mode
  "Minor mode to make evil leader blobal"
  :global t
  :keymap global-evil-leader-map)
(global-evil-leader-mode)

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

(require 'org-bullets)
(defun joe/org-mode-setup ()
    (org-bullets-mode)
    (org-indent-mode))
(add-hook 'org-mode-hook 'joe/org-mode-setup)

(require 'selectrum)
(require 'prescient)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
(marginalia-mode +1)
(advice-add #'marginalia-cycle :after
  (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
(setq marginalia-annotators
      '(marginalia-annotators-heavy marginalia-annotators-light nil))

(setq recentf-max-menu-items 1000)
(defun joe/recentf-open-files ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(evil-define-key 'normal 'global (kbd "<leader>fr") 'joe/recentf-open-files)

(require 'smartparens)
(smartparens-global-mode +1)
(show-paren-mode +1)

;; (require 'mini-frame)
;; (setq mini-frame-show-parameters
;;       '((top . 0.3) (width . 0.7)
;;         (left . 0.5) (left-fringe . 10) (right-fringe . 10) ))
;; (setq mini-frame-color-shift-step 25)

;; (setq mini-frame-internal-border-color "dark")
;; (mini-frame-mode)

(require 'vterm)
(setq vterm-shell "/bin/fish")

(require 'eglot)
(require 'eglot-fsharp)

(require 'ranger)
(setq ranger-show-literal nil)

;; Workaround for the initial candidates of mini frame not being shown
;; https://github.com/raxod502/selectrum/issues/169
(define-advice fit-frame-to-buffer (:around (f &rest args) dont-skip-ws-for-mini-frame)
  (cl-letf* ((orig (symbol-function #'window-text-pixel-size))
            ((symbol-function #'window-text-pixel-size)
             (lambda (win from to &rest args)
               (apply orig
                      (append (list win from
                                    (if (and (window-minibuffer-p win)
                                             (frame-root-window-p win)
                                             (eq t to))
                                        nil
                                      to))
                              args)))))
    (apply f args)))

(require 'find-file-in-project)
(setq ffip-use-rust-fd 't)

;; TODO: Look into this, maybe it's useful
;; (require 'server)
;; (if (not (server-running-p)) (server-start))

;; TODO: Packages to check out
;; expand-region
;; company
;; flycheck
;; projectile
;; Hydra (we can use it for some of the ideas I've had about repeating and arranging stuff)
;; CTRLF (figure out if it does anything interesting)
;; Embark (provides ivy style "actions" on fuzzy searches for selectrum)

;;    exec-path-from-shell
;;    company-quickhelp
;;    flycheck
;;    evil-mc
;;    markdown-preview-mode
;;    markdown-mode
;;    company-mode
;;    company
;;    omnisharp
;;    sly
;;      )))
;;  '(send-mail-function (quote smtpmail-send-it)))

;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq slime-contribs '(slime-fancy slime-company))

; (use-package company
;   :init
;   (setq company-idle-delay 0.05)
;   (setq company-minimum-prefix-length 2)
;   (add-hook 'after-init-hook 'global-company-mode))

; (use-package flycheck
;   :init
;   (global-flycheck-mode))

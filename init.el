;; ;; CUSTOMIZE (in extra file)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; ;; GENERAL
(setq-default inhibit-startup-screen t)
(global-linum-mode t)
(setq-default column-number-mode t)
(global-hl-line-mode t) ; turn on highlighting current line
;; (global-visual-line-mode t) ; break lines at word boundaries
(delete-selection-mode t) ; delete selected text when typing
(setq-default indent-tabs-mode nil
              tab-width 2)

;; autofill mode
(setq fill-column 100)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tex-mode-hook 'turn-on-auto-fill)


;; reactivate downcasing
(put 'downcase-region 'disabled nil)

;; various key bindings
(global-set-key (kbd "C-x a r") 'align-regexp)

;; always ask the same way
(fset 'yes-or-no-p 'y-or-n-p)

;; define location of backups and save-places
(setq 
    backup-directory-alist `(("." . "~/.emacs.cache/emacssaves"))
    kept-new-versions 1
    kept-old-versions 1)


;; ;; BUILTIN MODES
;; dired
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

;; lisp mode
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent))) 


;; ;; FUNCTIONS
;; function to mark a whole line
(defun select-current-line ()
   "Mark the current line"
   (interactive "p")
   (end-of-line)
   (set-mark (line-beginning-position))
   (message "Selected line!"))

;; function to mark a whole line
(defun copy-line (&optional arg)
   "Copy lines (as many as prefix argument) in the kill ring"
   (interactive "p")
   (kill-ring-save (line-beginning-position)
                   (line-beginning-position (+ 1 arg)))
   (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key (kbd "C-M-w") 'copy-line)

;; shortcuts to customize this file
(defun reload-init-file ()
   "Reloads the emacs config file"
   (interactive)
   (load-file "~/.emacs.d/init.el")
   (message "New init file loaded!"))

(defun edit-init-file ()
  "Opens .init file in a new buffer"
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; ;; MELPA and packages
(require 'package)
;(add-to-list 'package-archives
;             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize) ;; You might already have this line

;; automatically load use-package to subsequently do loading automatically
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(setq-default use-package-always-ensure t)


;; ;; OTHER CUSTOMIZATION MODES

(use-package saveplace
  :demand t
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.cache/saved-places"))

;; add rainbow delimiters to all programming modes
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package paren
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))
(show-paren-mode t)

;; automatically set color theme depending on display/console
(use-package color-theme-modern)
(if (display-graphic-p)
      (progn (load-theme 'bharadwaj t t)
             (enable-theme 'bharadwaj))
    (progn (load-theme 'tty-dark t t)
           (enable-theme 'tty-dark)))

;; tabbar: show tabs at the top, automatically grouped
(defun tabbar-buffer-groups ()
  ;; http://stackoverflow.com/a/3814313/1346276
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's
tabbar-buffer-groups.  This function group all buffers into 3
groups: Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer" )
    ((eq major-mode 'dired-mode)
     "Dired")
    (t
     "User Buffer"))))

(use-package tabbar
  :bind (([M-left] . tabbar-backward-tab)
         ([M-right] . tabbar-forward-tab))
  :config
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))
(tabbar-mode t)

;; markdown/pandoc
(use-package markdown-mode
  :mode "\\.text\\'"
  :mode "\\.md\\'"
  :config
  (require 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook '(lambda ()
                                   (turn-off-auto-fill)))
  (remove-hook 'markdown-mode-hook 'turn-on-auto-fill))

;; haskell mode
(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (defun my-haskell-mode-save-buffer ()
    (interactive)
    (save-buffer))
  :bind (("M-q" . align)
         :map haskell-mode-map
         ("C-c <right>" . comment-region)
         ("C-c <left>" . uncomment-region)
         ("C-x C-s" . my-haskell-mode-save-buffer))
  :config
  ; alignment rules (https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code)
  (add-hook 'align-load-hook (lambda ()
                               (progn
                                 (add-to-list 'align-rules-list
                                              '(haskell-types
                                                (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                                                (modes quote (haskell-mode literate-haskell-mode))))
                                 (add-to-list 'align-rules-list
                                              '(haskell-assignment
                                                (regexp . "\\(\\s-+\\)=\\s-+")
                                                (modes quote (haskell-mode literate-haskell-mode))))
                                 (add-to-list 'align-rules-list
                                              '(haskell-arrows
                                                (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                                                (modes quote (haskell-mode literate-haskell-mode))))
                                 (add-to-list 'align-rules-list
                                              '(haskell-left-arrows
                                                (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                                                (modes quote (haskell-mode literate-haskell-mode))))))))
  ;;(autoload 'ghc-init "ghc" nil t)
  ;;(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode))

;; cc mode
(use-package cc-mode
  :bind (:map c-mode-base-map
              ("RET" . newline-and-indent)
              ("C-c M-c" . uncomment-region))
  :mode ("\\.cpp\\'" . c++-mode)
  :mode ("\\.h\\'" . c++-mode)
  :config
  (setq c-default-style "ellemtel"
        c-basic-offset 2)
  ;; indentation for c++-code
  (setq c-echo-syntactic-information-p t) 
  (add-to-list 'c-offsets-alist
               '(access-label . +)
               '(inclass . +)
               ;;'(case-label . +)
               )
  (c-set-offset 'access-label -2)
  (c-set-offset 'inclass 4)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label 2)
  (c-set-offset 'cpp-macro 0)
  (c-set-offset 'friend -2)
  (c-set-offset 'innamespace 2)
  (c-set-offset 'comment-intro 0))

;; octave mode
(use-package octave
  :mode "\\.m$")

;; ess-mode
(use-package ess
  :mode ("\\.r\\'" . R-mode))

;; scala-mode
(use-package scala-mode
  :mode ("\\.scala$" . scala-mode))

;; web-mode
(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :mode ("\\.xhtml\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :mode ("\\.php$" . web-mode))

;; darkroom-mode
(use-package darkroom
  :disabled t)

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c a" . mc/edit-beginnings-of-lines)
         ("C-S-c e" . mc/edit-ends-of-lines)
         ("C-S-c %" . mc/mark-all-in-region)))

;; auctex
(use-package tex
  :ensure auctex
  :config
  (add-to-list 'auto-mode-alist 
               '("\\.sg$" . (lambda ()
                              (latex-mode)
                              (electric-indent-mode 0)))))

;; JavaScript
(use-package js-mode
  :ensure js2-mode
  :config
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2))

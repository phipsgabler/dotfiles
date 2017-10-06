(defun in-emacs-d (filename)
  (expand-file-name filename user-emacs-directory))

;; ;; CUSTOMIZE (in extra file)
(setq custom-file (in-emacs-d "custom.el"))
(load custom-file)


;; ;; GENERAL
;; TODO:
;; - look at the following:
;;   - https://github.com/stevenbagley/emacs-init/blob/master/emacs-init.el
;;   - https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;   - https://github.com/tpapp/unicode-math-input, or like this, for julia
;;   - https://github.com/bbatsov/prelude
;;   - https://dotfiles.github.io/
;;   - https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el
;; - https://github.com/alexeyr/company-auctex/
;; - emacs as daemon: https://www.emacswiki.org/emacs/EmacsAsDaemon
;; - alternative fonts: source code pro, inconsolata, dejavu sans mono, droid sans mono, hack


(setq inhibit-startup-screen t
      column-number-mode t
      indent-tabs-mode nil
      tab-width 2)
(global-linum-mode t)
(global-hl-line-mode t) ; turn on highlighting current line
(delete-selection-mode t) ; delete selected text when typing

;; font stuff
(when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11")))

;; autofill mode
(setq-default fill-column 100)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; reactivate downcasing
(put 'downcase-region 'disabled nil)

;; always ask the same way
(fset 'yes-or-no-p 'y-or-n-p)

;; define location of backups and auto-saves
(setq backup-directory-alist '(("." . (in-emacs-d "cache/backups")))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; ;; FUNCTIONS
(defun select-current-line ()
   "Mark the current line"
   (interactive)
   (end-of-line)
   (set-mark (line-beginning-position))
   (message "Selected line!"))

(defun copy-current-line (&optional arg)
   "Copy lines (as many as prefix argument) in the kill ring"
   (interactive "p")
   (kill-ring-save (line-beginning-position)
                   (line-beginning-position (+ 1 arg)))
   (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun kill-current-line ()
  "Kills entire current line."
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line t))
    (kill-line))
  (message "Line killed!"))

(defun copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end))
  (message "Copied rectangle from %d to %d" start end))

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

(defun kill-to-bol ()
  "Kill from point to beginning of line."
  (interactive)
  (kill-line 0))


(defun smarter-move-beginning-of-line (arg)
  ;; from: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))



;; ;; MELPA and packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; automatically load use-package to subsequently do loading automatically
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(setq-default use-package-always-ensure t)
(require 'bind-key)

;; automatic updating every 7 days
(use-package auto-package-update
  :init (progn
          (setq auto-package-update-interval 7)
	  (setq auto-package-update-prompt-before-update t)
	  (setq auto-package-update-delete-old-versions t)
          (auto-package-update-maybe)))


;; ;; KEY BINDINGS

;; custom stuff defined above
(bind-key "C-@" 'select-current-line)
(bind-key "C-M-w" 'copy-current-line)
(bind-key "C-S-k" 'kill-current-line)
(bind-key "C-x r w" 'copy-rectangle)
(bind-key "C-<backspace>" 'kill-to-bol)
(bind-key "C-a" 'smarter-move-beginning-of-line)

;; various missing stuff
(bind-key "C-x a r" 'align-regexp)
(bind-key "C-x C-r" 'revert-buffer)
(bind-key "M-SPC" 'cycle-spacing)

;; redefinitions
(bind-key "M-;" 'comment-line)
(bind-key "C-x M-;" 'comment-dwim)


;; ;; BUILTIN MODES
(use-package dired
  :ensure f
  :init (progn
          (put 'dired-find-alternate-file 'disabled nil))
  :config (progn
            (bind-key "^" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)))


;; ;; GLOBALLY USED MINOR MODES

(use-package saveplace
  :init (progn
          (setq-default save-place t)
          (setq save-place-file (in-emacs-d "cache/saved-places"))))

;; useful visualization stuff
(use-package rainbow-delimiters
  :init (progn
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
          (add-hook 'TeX-mode-hook 'rainbow-delimiters-mode)))

(use-package paren
  :init (show-paren-mode t)
  :config (progn
            (set-face-background 'show-paren-match (face-background 'default))
            (set-face-foreground 'show-paren-match "#def")
            (set-face-attribute 'show-paren-match nil :weight 'extra-bold)))

(use-package fill-column-indicator
  :init (progn
          (add-hook 'prog-mode-hook 'fci-mode)
          (add-hook 'TeX-mode-hook 'fci-mode)))

;; automatically set color theme depending on display/console
(use-package solarized-theme
  :init (if (display-graphic-p)
            (load-theme 'solarized-light)
	  (load-theme 'solarized-light)))

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
     "Emacs Buffer")
    ((eq major-mode 'dired-mode)
     "Dired")
    (t
     "User Buffer"))))

(use-package tabbar
  :bind (([M-left] . tabbar-backward-tab)
         ([M-right] . tabbar-forward-tab))
  :init (tabbar-mode t)
  :config (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

;; multiple-cursors
(use-package multiple-cursors
  :init (multiple-cursors-mode t)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c a" . mc/edit-beginnings-of-lines)
         ("C-S-c e" . mc/edit-ends-of-lines)
         ("C-S-c %" . mc/mark-all-in-region)
         ("C-S-c h" . mc/mark-all-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; ido and stuff
(use-package ido
  :init (progn
          (setq ido-save-directory-list-file (in-emacs-d "cache/ido.last"))
          (ido-mode t)
          (setq ido-enable-flex-matching t
	        ido-everywhere t))
  :bind ("C-x C-b" . ibuffer))

(use-package recentf
  :init (progn
          (recentf-mode 1)
          (customize-set-variable recentf-save-file (in-emacs-d "cache/recentf"))
          (setq recentf-max-menu-items 25)))

(defun recentf-ido-find-file ()
  ;; http://www.xsteve.at/prg/emacs/power-user-tips.html
  "Find a recent file using Ido."
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Open recent file: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))
(bind-key "C-x C-S-f" 'recentf-ido-find-file)

(use-package smex
  :init (progn
          (setq smex-save-file (in-emacs-d "cache/smex-items"))
          (smex-initialize))
  :bind ("M-x" . smex))

;; visual completion
(use-package company
  :commands company-mode use
  :init (progn
          (global-company-mode t)
          (setq company-idle-delay 0)
          (if (display-graphic-p)
              (progn use
                (set-face-attribute 'company-tooltip nil :background "#eee8d5" :foreground "#586e75")
                (set-face-attribute 'company-scrollbar-bg nil :background "#93a1a1")
                (set-face-attribute 'company-scrollbar-fg nil :background "#073642")))))


;; MAJOR MODES

;; markdown/pandoc
(use-package markdown-mode
  :mode "\\.text\\'"
  :mode "\\.md\\'"
  :mode "\\.Rmd\\'"
  :init (progn
          (use-package pandoc-mode)
          (add-hook 'markdown-mode-hook 'pandoc-mode)
          (remove-hook 'markdown-mode-hook 'turn-on-auto-fill)
          (add-hook 'markdown-mode-hook 'turn-off-auto-fill)))


;; haskell mode
(defun haskell-mode-save-buffer ()
  (interactive)
  (save-buffer))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("M-q" . align)
              ("C-c <right>" . comment-region)
              ("C-c <left>" . uncomment-region)
              ("C-x C-s" . haskell-mode-save-buffer))
  :config
  (progn
    ;; (haskell-indent-offset 2)
    ;; (haskell-program-name "ghci")
    ;; alignment rules after: https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code
    (add-hook 'align-load-hook
              (lambda ()
                (progn
                  (add-to-list 'align-rules-list
                               '(haskell-types
                                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                                 (modes '(haskell-mode literate-haskell-mode))))
                  (add-to-list 'align-rules-list
                               '(haskell-assignment
                                 (regexp . "\\(\\s-+\\)=\\s-+")
                                 (modes '(haskell-mode literate-haskell-mode))))
                  (add-to-list 'align-rules-list
                               '(haskell-arrows
                                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                                 (modes '(haskell-mode literate-haskell-mode))))
                  (add-to-list 'align-rules-list
                               '(haskell-left-arrows
                                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                                 (modes '(haskell-mode literate-haskell-mode)))))))))
  ;;(autoload 'ghc-init "ghc" nil t)
  ;;(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode))

;; cc mode
(use-package cc-mode
  :bind (:map c-mode-base-map
              ("RET" . newline-and-indent)
              ("C-c M-c" . uncomment-region))
  :mode ("\\.cpp\\'" . c++-mode)
  :mode ("\\.h\\'" . c++-mode)
  :config (progn
            (setq c-default-style "ellemtel"
                  c-basic-offset 2)
            (setq c-echo-syntactic-information-p t) ; print type at every indent
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
            (c-set-offset 'comment-intro 0)))

;; octave mode
(use-package octave
  :mode ("\\.m\\'" . octave-mode))

;; ESS-mode for R and julia
(use-package ess
  :init (progn
          (require 'ess-site)
          (add-hook 'julia-mode-hook
                    (lambda () (set-input-method "TeX"))))
  :config (progn
            (setq ess-swv-processor 'knitr)))

;; scala-mode
;; (use-package ensime
;;   :config (progn
;; 	    (setq ensime-startup-notification nil
;; 		  ensime-startup-snapshot-notification nil)))
;; (add-hook 'ensime-mode-hook
;;           (lambda ()
;;             (let ((backends (company-backends-for-buffer)))
;; 	      (setq company-backends (push '(ensime-company company-yasnippet) backends)))))

;; (use-package scala-mode
;;   :interpreter ("scala" . scala-mode)
;;   :mode ("\\.scala\\'" . scala-mode))

;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map))


;; web-mode
(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.xhtml\\'"
  :mode "\\.css\\'"
  :mode "\\.php\\'")

;; distraction free writing
(use-package writeroom-mode
  ;;:init (add-hook 'writeroom-mode-hook (lambda () (linum-mode -1)))
  )

;; auctex
(use-package tex
  :ensure auctex
  :init (progn
          ;; this is just for editing song files
          (add-to-list 'auto-mode-alist 
                       '("\\.sg\\'" . (lambda ()
                                        (LaTeX-mode)
                                        (electric-indent-mode f)
                                        (turn-off-auto-fill))))
          (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill))
  :config (progn
            (setq TeX-PDF-mode t)
            (setq TeX-auto-save t)
            (setq TeX-command-default "LaTeX")
            (setq TeX-view-program-list '(("TeXworks" "texworks %o")
                                          ("Evince" "evince %o")))
            (setq TeX-view-program-selection '((output-pdf "TeXworks")
                                               (output-dvi "Evince")))
            (setq TeX-command-list
                  '(("TeX" "%(PDF)%(tex) -shell-escape %`%S%(PDFout)%(mode)%' %t"
                     TeX-run-TeX nil (plain-tex-mode)
                     :help "Run plain TeX")
                    ("LaTeX" "%(PDF)%(latex) -shell-escape %t"
                     TeX-run-TeX nil (latex-mode)
                     :help "Run LaTeX")
                    ("XeLaTeX" "xelatex -shell-escape %t"
                     TeX-run-TeX nil (latex-mode)
                     :help "Run XeLaTeX")
                    ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
                    ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
                    ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
                    ("Dvips" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
                    ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
                    ("latexmk" "latexmk -pdf %s"
                     TeX-run-TeX nil t
                     :help "Run latexmk on file")
                    ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
                     (context-mode)
                     :help "Run ConTeXt once")
                    ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
                     (context-mode)
                     :help "Run ConTeXt until completion")
                    ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
                    ("Check" "lacheck %s" TeX-run-compile nil
                     (latex-mode)
                     :help "Check LaTeX file for correctness")
                    ("Spell" "(TeX-ispell-document \"\")"
                     TeX-run-function nil t
                     :help "Spell-check the document")
                    ("Clean" "TeX-clean"
                     TeX-run-function nil t
                     :help "Delete generated intermediate files")
                    ("Clean All" "(TeX-clean t)"
                     TeX-run-function nil t
                     :help "Delete generated intermediate and output files")
                    ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))))

;; JavaScript
(use-package js-mode
  :ensure js2-mode
  :config (progn
            (setq js-indent-level 2)
            (setq js-switch-indent-offset 2)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-c l" . python-indent-shift-left)
              ("C-c r" . python-indent-shift-right))
  :config (progn
            (setq python-python-command "python")
            (setq python-shell-interpreter "ipython")))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package cperl-mode
  :init (defalias 'perl-mode 'cperl-mode)
  :mode "\\.pl\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

;; ;; CUSTOMIZE (in extra file)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; ;; GENERAL
;; TODO: look at this: https://github.com/stevenbagley/emacs-init/blob/master/emacs-init.el

(setq-default inhibit-startup-screen t)
(global-linum-mode t)
(setq-default column-number-mode t)
(global-hl-line-mode t) ; turn on highlighting current line
;; (global-visual-line-mode t) ; break lines at word boundaries
(delete-selection-mode t) ; delete selected text when typing
(setq-default indent-tabs-mode nil
              tab-width 2)

;; autofill mode
(setq-default fill-column 100)
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; reactivate downcasing
(put 'downcase-region 'disabled nil)


;; always ask the same way
(fset 'yes-or-no-p 'y-or-n-p)

;; define location of backups and save-places
(setq 
    backup-directory-alist `(("." . "~/.emacs.cache/emacssaves"))
    kept-new-versions 1
    kept-old-versions 1)



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
          (auto-package-update-maybe)))


;; ;; KEY BINDINGS

;; costom stuff defined above
(bind-key "C-@" 'select-current-line)
(bind-key "C-M-w" 'copy-current-line)
(bind-key "C-S-k" 'kill-current-line)
(bind-key "C-x r w" 'copy-rectangle)
(bind-key "C-<backspace>" 'kill-to-bol)

;; various missing stuff
(bind-key "C-x a r" 'align-regexp)
(bind-key "C-x C-r" 'revert-buffer)
(bind-key "M-SPC" 'cycle-spacing)


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
          (setq save-place-file "~/.emacs.cache/saved-places")))

;; add rainbow delimiters to all programming modes
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

;; automatically set color theme depending on display/console
(use-package color-theme-modern
  :init (if (display-graphic-p)
            (progn (load-theme 'bharadwaj t t)
                   (enable-theme 'bharadwaj))
          (progn (load-theme 'tty-dark t t)
                 (enable-theme 'tty-dark))))


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
          (ido-mode t)
          (setq ido-enable-flex-matching t)
          (setq ido-everywhere t)))

(use-package smex
  :init (progn
          (setq smex-save-file "~/.emacs.cache/.smex-items")
          (smex-initialize))
  :bind ("M-x" . smex))


;; MAJOR MODES

;; markdown/pandoc
(use-package markdown-mode
  :mode "\\.text\\'"
  :mode "\\.md\\'"
  :mode "\\.Rmd\\'"
  :init (progn
          (use-package pandoc-mode)
          (add-hook 'markdown-mode-hook 'pandoc-mode)
          (remove-hook 'markdown-mode-hook 'turn-on-auto-fill)))


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
    (haskell-program-name "ghci")
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
  :init (require 'ess-site)
  :config (progn
            (setq ess-swv-processor 'knitr)))

;; scala-mode
(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode))

;; web-mode
(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.xhtml\\'"
  :mode "\\.css\\'"
  :mode "\\.php\\'")

;; darkroom-mode
(use-package darkroom
  :disabled t)

;; auctex
(use-package tex
  :ensure auctex
  :init (progn
          ;; this is just for editing song files
          (add-to-list 'auto-mode-alist 
                       '("\\.sg\\'" . (lambda ()
                                        (LaTe-Xmode)
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
                    ("Biber" "biber %s.bib" TeX-run-Biber nil t :help "Run Biber")
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

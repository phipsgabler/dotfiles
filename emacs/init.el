(defun in-emacs-d (filename)
  (expand-file-name filename user-emacs-directory))

;; ;; CUSTOMIZE AND OTHER EXTRA FILES
(setq custom-file (in-emacs-d "custom.el"))
(load custom-file)

(load (in-emacs-d "solarized-colors.el"))

;; ;; GENERAL
;; TODO:
;; - look at the following:
;;   - https://github.com/stevenbagley/emacs-init/blob/master/emacs-init.el
;;   - https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;   - https://github.com/tpapp/unicode-math-input, or like this, for julia
;;   - https://github.com/bbatsov/prelude
;;   - https://dotfiles.github.io/
;;   - https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el
;;   - https://github.com/mattfidler/emacs.d
;;   - https://github.com/gicmo/dot-emacs/blob/master/init.el
;; - emacs as daemon: https://www.emacswiki.org/emacs/EmacsAsDaemon,
;;   https://askubuntu.com/questions/682898/how-to-open-files-with-emacs-in-new-tabs
;; - htmlize: https://tpapp.github.io/post/htmlize-screenshot/
;; - Weave for Julia: https://github.com/mpastell/Weave.jl
;; - Add phi-search: https://github.com/zk-phi/phi-search
;; - color customizations: constants -- should be independent of display-graphics-p!


(setq inhibit-startup-screen t
      column-number-mode t)
(setq-default indent-tabs-mode nil
              tab-width 2)
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
(setq backup-directory-alist `(("." . ,(in-emacs-d "cache/backups")))
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

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

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
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; automatically load use-package to subsequently do loading automatically
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
(setq-default use-package-always-ensure t)

;; automatic updating every 7 days
(use-package auto-package-update
  :init (auto-package-update-maybe)
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t))


;; ;; KEY BINDINGS

;; custom stuff defined above
(bind-key "C-@" 'select-current-line)
(bind-key "C-M-w" 'copy-current-line)
(bind-key "C-S-k" 'kill-current-line)
(bind-key "C-x r w" 'copy-rectangle)
(bind-key "C-<backspace>" 'kill-to-bol)
(bind-key "C-a" 'smarter-move-beginning-of-line)
(bind-key "C-c r" 'rename-current-buffer-file)

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
  :init (put 'dired-find-alternate-file 'disabled nil)
  :config (progn
            (bind-key "^" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)))


;; ;; ;; GLOBALLY USED MINOR MODES

(use-package saveplace
  :init (save-place-mode t)
  :custom save-place-file (in-emacs-d "cache/saved-places"))

;; multiple-cursors
(use-package multiple-cursors
  :init (multiple-cursors-mode t)
  :config (mc/always-run-for-all t)
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c a" . mc/edit-beginnings-of-lines)
         ("C-S-c e" . mc/edit-ends-of-lines)
         ("C-S-c %" . mc/mark-all-in-region)
         ("C-S-c h" . mc/mark-all-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; ido and stuff
(use-package ido
  :init (ido-mode t)
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ido-save-directory-list-file (in-emacs-d "cache/ido.last"))
  (ido-enable-flex-matching t)
  (ido-everywhere t))

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

(use-package recentf
  :init (recentf-mode t)
  :bind ("C-x C-S-f" . recentf-ido-find-file)
  :custom (recentf-max-menu-items 25))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex)
  :custom (smex-save-file (in-emacs-d "cache/smex-items")))

;; visual completion
(use-package company
  :init (global-company-mode t)
  :commands company-mode
  :config (if (display-graphic-p)
            (progn
              (set-face-attribute 'company-tooltip nil
                                  :background solarized-base2 :foreground solarized-base01)
              (set-face-attribute 'company-scrollbar-bg nil :background solarized-base1)
              (set-face-attribute 'company-scrollbar-fg nil :background solarized-base02)))
  :custom (company-idle-delay 0))

(use-package company-auctex
  :init (company-auctex-init))

(use-package company-math
  :config (add-to-list 'company-backends 'company-math-symbols-unicode))

;; 'describe-unbound-keys' lets fone find unused key combos
;; (use-package unbound)

;; deleting a whitespace character will delete all whitespace until the next non-whitespace
;; character
(use-package hungry-delete
  :init (global-hungry-delete-mode))


;; ;; ;; VISUAL CUSTOMIZATIONS

;; ;; useful visualization stuff
(use-package rainbow-delimiters
  :hook ((prog-mode TeX-mode) . rainbow-delimiters-mode))

(use-package yalinum
  :init (global-yalinum-mode t)
  :config (progn
            (set-face-attribute 'yalinum-face nil
                                :background solarized-base2 :foreground solarized-base01)
            (set-face-attribute 'yalinum-bar-face nil
                                :background solarized-base1 :foreground solarized-base02)))

(use-package fill-column-indicator
  :hook ((prog-mode TeX-mode) . fci-mode))

;; automatically set color theme depending on display/console
;; alternative: color-theme-solarized; but this looks better.
(use-package solarized-theme
  :when (display-graphic-p)
  :config (load-theme 'solarized-light)
  :custom
  (solarized-distinct-fringe-background t)
  (x-underline-at-descent-line t))

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
  :init (tabbar-mode t))
;; :config (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; better looking tabs for tabbar, and automatic ruler
(use-package tabbar-ruler
  :custom (tabbar-ruler-global-tabbar t))

(use-package powerline
  :config (powerline-default-theme))

;; (use-package whitespace
;;   :init (global-whitespace-mode t)
;;   ;; TODO: adapt for terminal use
;;   :config (mapc (lambda (face)
;;                   (set-face-attribute face nil :background nil :foreground solarized-orange))
;;                 (list 'whitespace-trailing 'whitespace-line 'whitespace-tab 'whitespace-empty))
;;   :custom
;;   (whitespace-line-column fill-column)
;;   (whitespace-style '(face empty tabs lines-tail trailing tab-mark newline-mark))
;;   ;; mappings: <mark> <character to be replaced (html code)> <replacements (html code)>
;;   (whitespace-display-mappings '((newline-mark 10 [172 10]) ;; not sign: "¬"
;;                                  (space-mark 32 [183]) ;; middle dot: "⋅"
;;                                  (tab-mark 9 [8677 9])))) ;; rightwards arrow to bar: "⇥"

;; (use-package leerzeichen
;;   :commands (leerzeichen-enable)
;;   :config (progn
;;             (set-face-attribute 'leerzeichen nil :foreground solarized-base1)
;;             (setq
;;              leerzeichen-line-feed-glyph (make-glyph-code ?¬ 'leerzeichen)
;;              leerzeichen-tab-glyph (make-glyph-code ?⇥ 'leerzeichen)
;;              leerzeichen-space-glyph (make-glyph-code nil 'leerzeichen))))


;; MAJOR MODES

;; markdown/pandoc
(use-package markdown-mode
  :mode "\\.text\\'"
  :mode "\\.md\\'"
  :mode "\\.Rmd\\'")
;; :config (progn
;;           (remove-hook 'markdown-mode-hook 'turn-on-auto-fill)
;;           (add-hook 'markdown-mode-hook 'turn-off-auto-fill))

(use-package pandoc-mode
  :hook markdown-mode)


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
  :config (progn
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

;; ;; cc mode
;; (use-package cc-mode
;;   :bind (:map c-mode-base-map
;;               ("RET" . newline-and-indent)
;;               ("C-c M-c" . uncomment-region))
;;   :mode ("\\.cpp\\'" . c++-mode)
;;   :mode ("\\.h\\'" . c++-mode)
;;   :config (progn
;;             (add-to-list 'c-offsets-alist
;;                          '(access-label . +)
;;                          '(inclass . +)
;;                          ;;'(case-label . +)
;;                          )
;;             (c-set-offset 'access-label -2)
;;             (c-set-offset 'inclass 4)
;;             (c-set-offset 'topmost-intro 0)
;;             (c-set-offset 'topmost-intro-cont 0)
;;             (c-set-offset 'inline-open 0)
;;             (c-set-offset 'case-label 2)
;;             (c-set-offset 'cpp-macro 0)
;;             (c-set-offset 'friend -2)
;;             (c-set-offset 'innamespace 2)
;;             (c-set-offset 'comment-intro 0))
;;   :custom
;;   (c-default-style "ellemtel")
;;   (c-basic-offset 2)
;;   (c-echo-syntactic-information-p t)) ; print type at every indent

;; ;; octave mode
;; (use-package octave
;;   :mode ("\\.m\\'" . octave-mode))

;; ESS-mode for R and julia
(use-package ess
  :init (require 'ess-site)
  :config (progn
            (setq ess-swv-processor 'knitr)))
;; (setq auto-mode-alist (assq-delete-all "\\.jl\\'"))

(use-package julia-mode
  :mode "\\.jl\\'")

(use-package julia-repl
  :bind (:map julia-mode-map
              ;; ("C-c C-c" . julia-repl-send-region-or-line)
              ("C-c C-b" . julia-repl-send-buffer)
              ("C-c C-z" . julia-repl)
              ("<C-return>" . julia-repl-send-region-or-line)
              ("C-c C-e" . julia-repl-edit)
              ("C-c C-d" . julia-repl-doc)
              ("C-c C-w" . julia-repl-workspace)
              ("C-c C-m" . julia-repl-macroexpand)))

;; scala-mode
;; (use-package ensime
;;   :config (progn
;;    (setq ensime-startup-notification nil
;;  ensime-startup-snapshot-notification nil)))
;; (add-hook 'ensime-mode-hook
;;           (lambda ()
;;             (let ((backends (company-backends-for-buffer)))
;;      (setq company-backends (push '(ensime-company company-yasnippet) backends)))))

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
;; replace by olivetti:     https://login.yoursecurecloud.de/d/346a8921204a4ec5acc1/
(use-package writeroom-mode
  :init (setq writeroom-width fill-column)
  :bind (("C-M-<" . writeroom-decrease-width)
         ("C-M->" . writeroom-increase-width)
         ("C-M-=" . writeroom-adjust-width)))
  :init (add-hook 'writeroom-mode-hook (lambda () (yalinum-mode nil)))

;; auctex/reftex
(use-package reftex
  :init (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config (progn
            (setq reftex-plug-into-AUCTeX t)
            (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
            (setq reftex-default-bibliography '("ref.bib"))))

(use-package tex
  :ensure auctex
  :init (progn
          ;; this is just for editing song files
          (add-to-list 'auto-mode-alist
                       '("\\.sg\\'" . (lambda ()
                                        (LaTeX-mode)
                                        (electric-indent-mode nil)
                                        (turn-off-auto-fill))))
          (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill))
  ;; '(font-latex-fontify-sectioning (quote color))
  ;; '(font-latex-quotes nil)
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-command-default "LaTeX")
  (TeX-view-program-list '(("TeXworks" "texworks %o")
                           ("Evince" "evince %o")))
  (TeX-view-program-selection '((output-pdf "TeXworks")
                                (output-dvi "Evince")))
  (TeX-command-list
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
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))

;; ;; JavaScript
;; ;; (use-package js-mode
;; ;;   :ensure js2-mode
;; ;;   :custom
;; ;;   (js-indent-level 2)
;; ;;   (js-switch-indent-offset 2)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-c l" . python-indent-shift-left)
              ("C-c r" . python-indent-shift-right))
  :custom
  ;; (python-shell-interpreter "ipython")
  (python-python-command "python3"))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :mode "\\.yml\\'")

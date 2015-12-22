;; ;; GENERAL
(global-linum-mode 1)
(global-hl-line-mode 1) ; turn on highlighting current line
(global-visual-line-mode 1) ; break lines at word boundaries
(delete-selection-mode 1) ; delete selected text when typing
(load-library "paren")



;; ;; CUSTOMIZE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-command-BibTeX "Biber")
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command") ("Biber" "biber %s.bib" TeX-run-BibTeX nil t))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode nil)
 '(TeX-source-correlate-start-server nil)
 '(TeX-view-program-list (quote (("texworks" "texworks %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "texworks") (output-html "xdg-open"))))
 '(column-number-mode t)
 '(custom-safe-themes (quote ("6c57adb4d3da69cfb559e103e555905c9eec48616104e217502d0a372e63dcea" default)))
 '(ess-swv-processor (quote knitr))
 '(fill-column 100)
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-quotes nil)
 '(haskell-indent-offset 2)
 '(haskell-program-name "ghci")
 '(inhibit-startup-screen t)
 '(python-python-command "python3")
 '(python-shell-interpreter "ipython3")
 '(safe-local-variable-values (quote ((TeX-command-extra-options . "-shell-escape") (TeX-engine . xelatex))))
 '(tex-bibtex-command "Biber"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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



;; ;; MELPA
(require 'package)
;(add-to-list 'package-archives
;             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize) ;; You might already have this line



;; ;; OTHER CUSTOMIZATIONS
;; general stuff
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 100)
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

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.cache/saved-places")



;; ;; MODE OPTIONS

;; auto fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'tex-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)


;; color theme
;(require 'color-theme)
(if (display-graphic-p)
    (progn (load-theme 'bharadwaj t t)
           (enable-theme 'bharadwaj))           
  (progn (load-theme 'tty-dark t t)
         (enable-theme 'tty-dark)))



;; tabbar-mode
(require 'tabbar)
(tabbar-mode t)
(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)

; change tab grouping: http://stackoverflow.com/a/3814313/1346276
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer" )
    ((eq major-mode 'dired-mode)
     "Dired")
    (t
     "User Buffer")))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)


;; markdown/pandoc
;(autoload 'pandoc-mode "pandoc-mode")
;(add-hook 'markdown-mode-hook 'pandoc-mode)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; lisp mode
(add-hook 'lisp-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent))) 


;; haskell mode
(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(defun my-haskell-mode-save-buffer ()
  (interactive)
  (save-buffer))
(add-hook 'haskell-mode-hook 
          (lambda () (progn 
                       (define-key haskell-mode-map (kbd "C-c <right>") 'comment-region)
                       (define-key haskell-mode-map (kbd "C-c <left>") 'uncomment-region)
                       (define-key haskell-mode-map (kbd "C-x C-s") 'my-haskell-mode-save-buffer)
                       (local-set-key (kbd "M-q") 'align))))

; alignment rules (https://github.com/haskell/haskell-mode/wiki/Indentation#aligning-code)
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
               (modes quote (haskell-mode literate-haskell-mode))))
;;(autoload 'ghc-init "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))




;; cc mode
(require 'cc-mode)
(setq c-default-style "ellemtel"
      c-basic-offset 2)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-base-map (kbd "C-c M-c") 'uncomment-region)
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; indentation for c++-code
(setq c-echo-syntactic-information-p t)

(add-to-list 'c-offsets-alist
  '(access-label . +)
  '(inclass . +)
  ;'(case-label . +)
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
(c-set-offset 'comment-intro 0)


;; dired
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 )
)


;; octave mode
(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;; ess-mode
(require 'ess-site)


;; scala-mode2
(require 'scala-mode2)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))


;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))


;; darkroom-mode
(require 'darkroom)


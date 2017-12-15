(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-command-default "LaTeX" t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) -shell-escape %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%(PDF)%(latex) -shell-escape %t" TeX-run-TeX nil
      (latex-mode)
      :help "Run LaTeX")
     ("XeLaTeX" "xelatex -shell-escape %t" TeX-run-TeX nil
      (latex-mode)
      :help "Run XeLaTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file")
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
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-view-program-list (quote (("TeXworks" "texworks %o") ("Evince" "evince %o"))))
 '(TeX-view-program-selection (quote ((output-pdf "TeXworks") (output-dvi "Evince"))))
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-interval 7)
 '(auto-package-update-prompt-before-update t)
 '(c-basic-offset 2 t)
 '(c-default-style "ellemtel" t)
 '(c-echo-syntactic-information-p t t)
 '(company-idle-delay 0)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "6c57adb4d3da69cfb559e103e555905c9eec48616104e217502d0a372e63dcea" default)))
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-quotes nil)
 '(frame-background-mode (quote light))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-save-directory-list-file "/home/philipp/.emacs.d/cache/ido.last")
 '(nlinum-format "%4d ")
 '(nlinum-highlight-current-line t)
 '(package-selected-packages
   (quote
    (unicode-whitespace unicode-whitespaces hungry-delete julia-mode company-math company-auctex tabbar-ruler powerline unbound solarized-theme yaml-mode dockerfile-mode company company-mode writeroom-mode fill-column-indicator smex lisp-mode scala-mode js2-mode rainbow-delimiters pandoc-mode web-mode tabbar multiple-cursors markdown-mode idris-mode haskell-mode darkroom auctex)))
 '(python-python-command "python" t)
 '(python-shell-interpreter "ipython" t)
 '(recentf-max-menu-items 25)
 '(safe-local-variable-values
   (quote
    ((TeX-master . "../document")
     (TeX-master . "document")
     (TeX-command-extra-options . "-shell-escape")
     (TeX-engine . xelatex))))
 '(save-place-file "/home/philipp/.emacs.d/cache/saved-places")
 '(smex-save-file "/home/philipp/.emacs.d/cache/smex-items")
 '(solarized-distinct-fringe-background t)
 '(tabbar-ruler-global-tabbar t)
 '(tramp-syntax (quote default))
 '(whitespace-display-mappings
   (quote
    ((newline-mark 10
                   [172 10])
     (space-mark 32
                 [183])
     (tab-mark 9
               [8677 9]))))
 '(whitespace-line-column 100)
 '(whitespace-style
   (quote
    (face empty tabs lines-tail trailing tab-mark newline-mark)))
 '(x-underline-at-descent-line t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:background "#cb4b16" :foreground nil))))
 '(whitespace-trailing ((t (:background "#cb4b16" :foreground nil)))))

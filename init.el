;; Okno zmaksymalizowane na starcie
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Wyłączenie ekranu powitalnego
(setq inhibit-startup-screen t)
;; Płynne przewijanie
(setq scroll-conservatively 10000)
;; Niestandardowe motywy
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; Nie zapisywać plików zapasowych
(setq make-backup-files nil)
;; Wyłączenie dźwięku
(setq ring-bell-function 'ignore)
;; Nie pokazuj wiadomości w buforze *scratch*
(setq initial-scratch-message "")
;; Dziel okno poziomo, jeżeli szerokość >=140
(setq split-width-threshold 140)
;; Nie pokazuj paska ikonek
(custom-set-variables '(tool-bar-mode nil))

;; Zegarek w pasku stanu
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)
;; Stan baterii w pasku stanu
(display-battery-mode 1)
;; Numery kolumn
(setq column-number-mode t)

;; Wyłączony znacznik zakładek na lewym pasku
(setq bookmark-set-fringe-mark nil)

;; Traktowanie `_` jako części słowa
(modify-syntax-entry ?_ "w")

;; Straight
(setq straight-process-buffer " *straight-process*")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Magit
(straight-use-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; magit-todos
(straight-use-package 'magit-todos)
(magit-todos-mode)
(setq magit-todos-keyword-suffix "")

;; vterm
(straight-use-package 'vterm)
(setq vterm-kill-buffer-on-exit t)

;; <F5> - shell
(global-set-key (kbd "<f5>") 'vterm)

;; Tramp - obsługa zdalnych plików
(require 'tramp)
(setq tramp-default-method "ssh")

;; Ido - nawigacja między buforami
(require 'ido)
(ido-mode 'buffers)
(setq ido-ignore-buffers
      '("^ " "*Completions*" "*Messages*" "^magit" "*Flymake log*" "LaTeX Output"
	"*Calendar*" "*Straight" "*Backtrace*" "*tramp"))

;; Przełączanie między buforami poprzez Ctrl-(shift)-tab
(global-set-key (kbd "C-<tab>") 'previous-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'next-buffer)

;; Ustawienia ESS
(straight-use-package 'ess)
(with-eval-after-load 'ess-site
  (ess-toggle-underscore nil)
  (ess-set-style 'RStudio))

;; Org-mode
(add-to-list 'auto-mode-alist '("\\.md$" . org-mode))
(add-hook 'org-mode-hook #'visual-line-mode)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; Ładne symbole w Org mode
(defun prettify-org ()
  (interactive)
  (setq prettify-symbols-alist
	'(("#+BEGIN_SRC" . "=")
	  ("#+begin_src" . "=")
	  ("#+END_SRC"   . "=")
	  ("#+end_src"   . "=")))
  (prettify-symbols-mode 1)
  (set-face-attribute 'org-meta-line nil :foreground "#444455" :inherit 'default))
(add-hook 'org-mode-hook 'prettify-org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(setq org-todo-keywords '((sequence "TODO" "!" "*" "DONE")))
(setq org-todo-keyword-faces
      '(("!" . (:foreground "yellow" :weight bold))
	("*" . (:foreground "green" :weight bold))))
(setq org-directory "~/Org")

(setq org-hide-emphasis-markers t)

;; Org Mode 9 domyślnie włącza wcięcia pod nagłówkami
;; Ta linia to wyłącza:
(setq org-adapt-indentation nil)
;; Łatwe wstawianie bloków w org-mode za pomocą `<s`
;; W org-mode 9.2 ta opcja zniknęła
(require 'org-tempo)
;; Zwijanie nagłówków w Org Mode
(setq org-startup-folded t)
;; Łatka, żeby TAB w trybie komend <N>
;; rozwijał nagłówek Org-mode
(with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil))

;; Org-Agenda
(setq org-directory-as-dir (file-name-as-directory org-directory))
(setq org-default-notes-file (concat org-directory-as-dir "Notatki.org"))
(setq org-howto-file (concat org-directory-as-dir "HOWTO.org"))
(setq org-labbook-file (concat org-directory-as-dir "Labbook.org"))
(setq org-agenda-files (list org-default-notes-file))
(setq org-capture-templates
      '(("c" "Zadanie" entry (file org-default-notes-file) "* TODO %?" :prepend t)
	("n" "Notatka" entry (file org-default-notes-file) "* <%<%Y-%m-%d>> %?")
	("h" "HOWTO" entry (file org-howto-file) "* %?")
	("l" "Labbook" entry (file org-labbook-file) "* <%<%Y-%m-%d>>\n%?\n")))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-x C-x")
		(lambda () (interactive) (find-file org-default-notes-file)))

;; Org-Babel
(org-babel-do-load-languages 'org-babel-load-languages
       '((R . t) (plantuml . t) (shell . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)

;; Tryb skupienia się na tekście
(straight-use-package 'olivetti)
(setq olivetti-style t)
(setq olivetti-body-width 80)
(straight-use-package 'focus)
(global-set-key (kbd "<f9>")
		(lambda () (interactive) (olivetti-mode) (focus-mode)))

;; Deft
(straight-use-package 'deft)
(setq deft-directory org-directory)
(setq deft-extensions '("org" "txt" "md" "Rmd"))
(setq deft-use-filter-string-for-filename t)
(setq deft-use-filename-as-title t)
(setq deft-file-naming-rules
      '((noslash . "_")
	(nospace . "_")))
(global-set-key (kbd "<f8>") 'deft)
(global-set-key (kbd "C-c C-n") 'deft)

;; Beamer
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("beamer"
		 "\\documentclass\[presentation\]\{beamer\}"
		 ("\\section\{%s\}" . "\\section*\{%s\}")
		 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
		 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))

;; Obsługa markdowna
(straight-use-package 'markdown-mode)
(custom-set-faces
 '(markdown-code-face ((t (:inherit nil)))))

;; Quarto
(straight-use-package 'quarto-mode)
(require 'quarto-mode)

;; Obsługa YAML-a
(straight-use-package 'yaml-mode)

;; Evil mode
(straight-use-package 'evil)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-delete t)
(evil-mode 1)
(evil-set-initial-state 'deft-mode 'insert)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Undo/redo w evil-mode
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
;; Undo-tree w evil-mode
(evil-set-undo-system 'undo-tree)
;; Żeby undo-tree nie śmieciło swoimi plikami
(setq undo-tree-auto-save-history nil)

;; Ranger
(straight-use-package 'ranger)
(global-set-key (kbd "<f6>") 'ranger)
(ranger-override-dired-mode t)

;; Deadgrep (ripgrep)
(straight-use-package 'deadgrep)
(global-set-key (kbd "C-c C-g") #'deadgrep)
(evil-set-initial-state 'deadgrep-mode 'emacs)

;; Autouzupełnianie
(straight-use-package 'company)
(add-hook 'company-mode-hook
	  (lambda () (setq company-dabbrev-downcase nil)))
(global-company-mode)

;; Golang
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

;; Groovy
(straight-use-package 'groovy-mode)
(add-hook 'groovy-mode-hook
	  (lambda () (setq indent-tabs-mode nil)))

;; Nextflow
(straight-use-package
 '(nextflow-mode :type git :host github :repo "Emiller88/nextflow-mode"))

;; Dockerfile
(straight-use-package 'dockerfile-mode)
(setq compilation-scroll-output t)

;; Terraform
(straight-use-package 'terraform-mode)

;; Afternoon Theme
(straight-use-package 'afternoon-theme)
(load-theme 'afternoon t)

;; Zmiana kolorów ramki z podpowiedziami
;; Za: https://www.emacswiki.org/emacs/CompanyMode
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Elpher
(straight-use-package 'elpher)

;; Lokalna konfiguracja
(load "~/.emacs.d/init-local.el" t)

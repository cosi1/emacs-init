;; Okno zmaksymalizowane na starcie
(add-to-list 'default-frame-alist '(fullscreen . maximized))
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

;; Zegarek w pasku stanu
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)

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

;; <F5> - shell
(global-set-key (kbd "<f5>") 'eshell)

;; Ustawienia ESS
(straight-use-package 'ess)
(with-eval-after-load 'ess-site
  (ess-toggle-underscore nil)
  (setq ess-default-style 'RStudio))

;; Org-mode
(add-to-list 'auto-mode-alist '("\\.md$" . org-mode))
(add-hook 'org-mode-hook #'visual-line-mode)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(setq org-todo-keywords '((sequence "TODO" "!" "*" "DONE")))
(setq org-todo-keyword-faces
      '(("!" . (:foreground "yellow" :weight bold))
	("*" . (:foreground "green" :weight bold))))
(setq org-directory "~/Org")

;; Org-Agenda
(setq org-default-todo-file (concat (file-name-as-directory org-directory) "Zadania.org"))
(setq org-default-notes-file (concat (file-name-as-directory org-directory) "Notatki.org"))
(setq org-agenda-files (list org-default-notes-file org-default-todo-file))
(setq org-capture-templates '(("c" "Zadanie" entry (file org-default-todo-file) "* TODO %?" :prepend t)
			      ("n" "Notatka" entry (file org-default-notes-file) "" :prepend t)))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-x C-x") 'org-todo-list)

;; Org-Babel
(org-babel-do-load-languages 'org-babel-load-languages '((R . t)))
(setq org-confirm-babel-evaluate nil)

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

;; Obsługa YAML-a
(straight-use-package 'yaml-mode)

;; Evil mode
(straight-use-package 'evil)
(evil-mode 1)
(evil-set-initial-state 'deft-mode 'insert)
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Autouzupełnianie
(straight-use-package 'company)
(global-company-mode)

;; Golang
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

;; Gotham Theme
(straight-use-package 'gotham-theme)

;; Lokalna konfiguracja
(load "~/.emacs.d/init-local.el" t)

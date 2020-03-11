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

;; Straight
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
(with-eval-after-load 'ess-site
  (ess-toggle-underscore nil)
  (setq ess-default-style 'RStudio))

;; Org-mode - szybkie notatki
(setq org-directory "~/Org/")
(setq plik-notatnika (concat org-directory "Notatki.org"))
(global-set-key (kbd "C-c C-n") (lambda () (interactive)
				  (find-file plik-notatnika)))
(setq org-default-notes-file plik-notatnika)
(global-set-key (kbd "C-c C-c") 'org-capture)
(add-to-list 'auto-mode-alist '("\\.md$" . org-mode))
(add-hook 'org-mode-hook #'visual-line-mode)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;; <F4> - lista notatek
(global-set-key (kbd "<f4>") (lambda () (interactive) (find-file org-directory)))

;; Org-Babel
(org-babel-do-load-languages 'org-babel-load-languages '((R . t)))
(setq org-confirm-babel-evaluate nil)

;; Deft
(straight-use-package 'deft)
(setq deft-directory "~/Org")
(setq deft-extensions '("org" "txt" "md" "Rmd"))
(setq deft-use-filter-string-for-filename t)
(setq deft-use-filename-as-title t)
(setq deft-file-naming-rules
      '((noslash . "_")
	(nospace . "_")))
(global-set-key (kbd "<f8>") 'deft)

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

;; Autouzupełnianie
(straight-use-package 'company)
(global-company-mode)

;; Gotham Theme
(straight-use-package 'gotham-theme)

;; Lokalna konfiguracja
(load "~/.emacs.d/init-local.el" t)

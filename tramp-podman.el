(setq docker-command
      (string-trim
       (shell-command-to-string "command -v podman > /dev/null && echo podman || echo docker")))

;; Source: https://www.emacswiki.org/emacs/TrampAndDocker
(push
 (cons
  "docker"
  `((tramp-login-program ,docker-command)
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/sh")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string
			       (concat docker-command " ps | awk '$NF != \"NAMES\" { print $NF \":\" }'")))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

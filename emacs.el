(setq custom-file "~/.custom.el")

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	default-directory "/Users/daniel"))

(load-theme 'wombat t)

;; Navigation
;; Search
;; Knowldge organization
;; Writing

(setq org-special-ctrl-a/e t
      org-special-ctrl-o t
      org-use-speed-commands t
      org-odd-levels-only t
      org-insert-heading-respect-content t
      org-log-into-drawer t
      org-todo-keywords
      '((sequence
	 "TODO(t)"
	 "NEXT(n!)"
	 "WAITING(w@)"
	 "FUTURE(f)"
	 "|"
	 "DONE(d!)")
	(sequence "|" "CANCELED(c@)"))
      org-tag-alist '((:startgroup . nil)
		      ("@admin" . ?a)
		      ("@call" . ?c)
		      ("@decision" . ?d)
		      ("@errand" . ?e)
		      ("@message" . ?m)
		      (:endgroup . nil))
      org-agenda-custom-commands
      '(("n" "Next tasks" todo "NEXT")))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/todo.org" "Tasks")
	 "* TODO %?\n" :prepend t)
	("n" "Note" entry (file "~/org/notes.org")
	 "* %? %^g\n%U\n" :prepend t)))

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

;; Don't want thise in org mode though!
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

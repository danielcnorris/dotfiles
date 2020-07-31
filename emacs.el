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

(setq org-special-ctrl-a/e t
      org-special-ctrl-o t
      org-use-speed-commands t
      org-reverse-note-order t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-insert-heading-respect-content t
      org-log-into-drawer t
      org-todo-keywords
      '((sequence
	 "NEXT(n!)"
	 "TODO(t)"
	 "WAITING(w@)"
	 "|"
	 "DONE(d!)" "CANCELED(c@)"))
      org-tag-alist '((:startgroup . nil)
		      ("@admin" . ?a)
		      ("@call" . ?c)
		      ("@decision" . ?d)
		      ("@errand" . ?e)
		      ("@message" . ?m)
		      (:endgroup . nil))
      org-agenda-files '("~/org/todo.org")
      org-agenda-custom-commands
      '(("n" "Next tasks" todo "NEXT"))
      org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/todo.org" "Tasks")
	 "* TODO %? %^g\n" :prepend t)
	("n" "Note" entry (file "~/org/notes.org")
	 "* %? %^g\n%U\n" :prepend t)))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

;; Don't want thise in org mode though!
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

;;; init-org.el --- set up org mode
;;; Commentary:
;; Borrowed heavily from http://doc.norang.ca/org-mode.html
;;; Code:

;; Basic keymaps
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; Keymaps to use on tty and disable linum mode
(add-hook 'org-mode-hook
          (lambda ()
              (linum-mode -1)
              (local-set-key "\C-cj" 'org-insert-heading-respect-content)
              (local-set-key "\C-cm" 'org-insert-todo-heading-respect-content)
              (local-set-key "\C-cp" 'org-promote-subtree)
              (local-set-key "\C-cn" 'org-demote-subtree)))

;; Disable linum in org agenda
(add-hook 'org-agenda-mode-hook
          (lambda ()
              (linum-mode -1)))

;; Use speed commands
(setq org-use-speed-commands t)

;; Load all org files form these directories into agenda
(setq org-agenda-files (list (concat dcn/org-directory "todo.org")
                             (concat dcn/org-directory "inbox.org")
                             dcn/palantir-org-directory))

;; Org keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                        "CANCELLED(c@/!)"))))

;; Allow todo selection with keys
(setq org-use-fast-todo-selection t)

;; Allow use of shift and arrows to change state without log info
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Add and remove tags based on todo state
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-capture-templates
      '(("t" "Todo" entry
         (file (concat dcn/org-directory "inbox.org"))
         "* TODO %?\n%U\n\n%a\n" :clock-in t :clock-resume t)
        ("n" "Note" entry
         (file (concat dcn/org-directory "inbox.org"))
         "* %?  :NOTE:\n%U\n\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry
         (file+datetree (concat dcn/org-directory "journal.org"))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry
         (file (concat dcn/palantir-org-directory "meetings.org"))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("e" "Event" entry
         (file (concat dcn/org-directory "inbox.org"))
         "* %? %^{Time}T  :EVENT:\n%U\n")
        ("d" "Full-day event" entry
         (file (concat dcn/org-directory "inbox.org"))
         "* %? %^{Date}t  :EVENT:\n%U\n" :clock-in :clock-resume t)
        ("D" "Multi-day event" entry
         (file (concat dcn/org-directory "inbox.org"))
         "* %? %^{Start Time}t--%^{End Time}t  :EVENT:\n%U\n" :clock-in :clock-resume t)
        ("r" "Russian-English translation" plain
         (file (concat dcn/org-directory "russian.org"))
         "| %^{Russian word} | %^{English word} | %^{Notes} |"
         :immediate-finish t)))


;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@shiya" . ?s)
                            ("@journal" . ?j)
                            ("@reading" . ?r)
                            ("@phone" . ?p)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("EVENT" . ?E)
                            ("DAILY" . ?D)
                            ("DCN" . ?d)
                            ("SHIYA" . ?S)
                            ("FAMILY" . ?F)
                            ("PALANTIR" . ?W)
                            ("FLETCHER" . ?b)
                            ("FUNNY" . ?f)
                            ("GOAL" . ?g)
                            ("FEEDBACK" . ?l)
                            ("CONVERSATION" . ?t)
                            ("PROUD" . ?P)
                            ("crypt" . ?C)
                            ("IDEA" . ?i)
                            ("NOTE" . ?n)
                            ("IMPORTANT" . ?I)
                            ("QUESTION" . ?q)
                            ("CANCELLED" . ?c))))

;; Allow setting single tags without menu
(setq org-fast-tag-selection-single-key (quote expert))

;; When searching by tags ignore tasks with schedule or deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


;;; Refile configuration

;; Include current file and files contributing to agenda up to 9 levels
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Use Ido and full paths for refiling
(setq org-completion-use-ido t)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Exclude DONE from refile targets
(defun dcn/verify-refile-target ()
    "Exclude DONE todos from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'dcn/verify-refile-target)


;;; Agenda views

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda commands and definitions
(setq org-agenda-custom-commands
      (quote (("n" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "INBOX"
                      ((org-agenda-overriding-header "New Tasks")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header
                             "Stuck Projects")
                            (org-agenda-skip-function
                             'dcn/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header
                             (concat "Next Tasks"
                                     (if dcn/hide-scheduled-and-waiting-next-tasks
                                             ""
                                         " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-todo-ignore-scheduled
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-INBOX-CANCELLED-WAITING-HOLD/!-NEXT"
                           ((org-agenda-overriding-header
                             (concat "Standalone Tasks"
                                     (if dcn/hide-scheduled-and-waiting-next-tasks
                                             ""
                                         " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function
                             'dcn/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function
                             'dcn/skip-non-projects)
                            (org-tags-match-list-sublevels
                             'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header
                             (concat "Waiting and Postponed Tasks"
                                     (if dcn/hide-scheduled-and-waiting-next-tasks
                                             ""
                                         " (including WAITING and SCHEDULED tasks)")))
                            ;;(org-agenda-skip-function 'dcn/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled
                             dcn/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             dcn/hide-scheduled-and-waiting-next-tasks)))
                (tags "-INBOX/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function
                        'dcn/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

;; Agenda helper functions
(defvar dcn/hide-scheduled-and-waiting-next-tasks t)

(defun dcn/toggle-next-task-display ()
    (interactive)
    (setq dcn/hide-scheduled-and-waiting-next-tasks
          (not dcn/hide-scheduled-and-waiting-next-tasks))
    (when (equal major-mode 'org-agenda-mode)
        (org-agenda-redo))
    (message "%s WAITING and SCHEDULED NEXT Tasks"
             (if dcn/hide-scheduled-and-waiting-next-tasks
                     "Hide" "Show")))

(defun dcn/skip-non-stuck-projects()
    "Skip trees that are not stuck"
    (save-restriction
        (widen)
        (let ((next-headline
               (save-excursion (or (outline-next-heading)
                                   (point-max)))))
            (if (dcn/is-project-p)
                    (let* ((subtree-end
                            (save-excursion (org-end-of-subtree t)))
                           (has-next))
                        (save-excursion
                            (forward-line 1)
                            (while (and (not has-next)
                                        (< (point) subtree-end)
                                        (re-search-forward
                                         "^\\*+ NEXT " subtree-end t))
                                (unless (member "WAITING" (org-get-tags-at))
                                    (setq has-next t))))
                        (if has-next
                                next-headline
                            nil)) ; Stuck - has tasks but no next task
                next-headline))))

(defun dcn/skip-non-projects ()
    "Skip trees that are not projects."
    (if (save-excursion (dcn/skip-non-stuck-projects))
            (save-restriction
                (widen)
                (let ((subtree-end (save-excursion
                                       (org-end-of-subtree t))))
                    (cond ((dcn/is-project-p)
                           nil)
                          ((and (dcn/is-project-subtree-p)
                                (not (dcn/is-task-p)))
                           nil)
                          (t
                           subtree-end))))
        (save-excursion (org-end-of-subtree t))))

(require 'org-habit)
(defun dcn/skip-project-tasks ()
    "Show non-project tasks."
    (save-restriction
        (widen)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((dcn/is-project-p) subtree-end)
             ((org-is-habit-p) subtree-end)
             ((dcn/is-project-subtree-p) subtree-end)
             (t nil)))))

(defun dcn/is-project-p ()
    "Any task with a todo keyword subtask."
    (save-restriction
        (widen)
        (let ((has-subtask)
              (subtree-end (save-excursion (org-end-of-subtree t)))
              (is-a-task (member (nth 2 (org-heading-components))
                                 org-todo-keywords-1)))
            (save-excursion
                (forward-line 1)
                (while (and (not has-subtask)
                            (< (point) subtree-end)
                            (re-search-forward "^\*+ " subtree-end t))
                    (when (member (org-get-todo-state)
                                  org-todo-keywords-1)
                        (setq has-subtask t))))
            (and is-a-task has-subtask))))

(defun dcn/find-project-task ()
    "Move point to parent (project) task if any."
    (save-restriction
        (widen)
        (let ((parent-task (save-excursion
                               (org-back-to-heading
                                'invisible-ok)
                               (point))))
            (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components))
                              org-todo-keywords-1)
                    (setq parent-task (point))))
            (goto-char parent-task)
            parent-task)))

(defun dcn/is-project-subtree-p ()
    "Any task with a todo keyword that's in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
        (save-excursion
            (dcn/find-project-task)
            (if (equal (point) task)
                    nil
                t))))

(defun dcn/is-task-p ()
    "Any task with a todo keyword and no subtask."
    (save-restriction
        (widen)
        (let ((has-subtask)
              (subtree-end (save-excursion (org-end-of-subtree t)))
              (is-a-task (member (nth 2 (org-heading-components))
                                 org-todo-keywords-1)))
            (save-excursion
                (forward-line 1)
                (while (and (not has-subtask)
                            (< (point) subtree-end)
                            (re-search-forward "^\*+ " subtree-end t))
                    (when (member (org-get-todo-state)
                                  org-todo-keywords-1)
                        (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

;;; Archival settings
;; Allow archival of tasks not marked as done
(setq org-archive-mark-done nil)

;; Archive tasks in Archived Tasks section
(setq org-archive-location "%s_archive::* Archived Tasks")

;; THIS MAY BE BROKEN
(defun dcn/skip-non-archivable-tasks ()
    "Skip trees not available for archiving."
    (save-restriction
        (widen)
        (let ((next-headline (save-excursion
                                 (or (outline-next-heading)
                                     (point-max))))
              (subtree-end (save-excursion (org-end-of-subtree t))))
            (if (member (org-get-todo-state) org-todo-keywords-1)
                    (if (member (org-get-todo-state) org-done-keywords)
                    (let* ((daynr (string-to-int (format-time-string
                                                  "%d" (current-time))))
                           (a-month-ago (* 60 60 24 (+ daynr 1)))
                           (last-month (format-time-string
                                        "%Y-%m-" (time-subtract
                                                  (current-time)
                                                  (seconds-to-time
                                                   a-month-ago))))
                           (this-month (format-time-string
                                        "%Y-%m-" (current-time)))
                           (subtree-is-current
                            (save-excursion
                                (forward-line 1)
                                (and (< (point)
                                        subtree-end)
                                     (re-search-forward
                                      (concat last-month
                                              "\\|" this-month)
                                      subtree-end t)))))
                        (if subtree-is-current
                                subtree-end ; Skip if this month or last
                            nil)) ; Available to archive
                (or subtree-end (point-max)))
            next-headline))))

;; Enable narrowing to subtree
(defun dcn/org-todo (arg)
    (interactive "p")
    (if (equal arg 4)
            (save-restriction
                (dcn/narrow-to-org-subtree)
                (org-show-todo-tree nil))
        (dcn/narrow-to-org-subtree)
        (org-show-todo-tree nil)))

(defun dcn/widen ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
            (progn
                (org-agenda-remove-restriction-lock)
                (when org-agenda-sticky
                    (org-agenda-redo)))
        (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "W"
                           (lambda ()
                               (interactive)
                               (setq dcn/hide-scheduled-and-waiting-next-tasks t)
                               (dcn/widen))))
          'append)


;; Limit agenda to subtree
(defun dcn/set-agenda-restriction-lock (arg)
    "Set restriction lock to current task subtree or file if ARG."
    (interactive "p")
    (let* ((pom (dcn/get-pom-from-agenda-restriction-or-point))
           (tags (org-with-point-at pom (org-get-tags-at))))
        (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
            (save-restriction
                (cond
                 ((and (equal major-mode 'org-agenda-mode) pom)
                  (org-with-point-at pom
                      (org-agenda-set-restriction-lock restriction-type))
                  (org-agenda-redo))
                 ((and (equal major-mode 'org-mode)
                       (org-before-first-heading-p))
                  (org-agenda-set-restriction-lock 'file))
                 (pom
                  (org-with-point-at pom
                      (org-agenda-set-restriction-lock
                       restriction-type))))))))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "\C-c\C-x<"
                           'dcn/set-agenda-restriction-lock))
          'append)

;; Follow mode
(defun dcn/restrict-to-file-or-follow (arg)
    "Set agenda restrictionto 'file or with ARG follow mode."
    (interactive "p")
    (if (equal arg 4)
            (org-agenda-follow-mode)
        (widen)
        (dcn/set-agenda-restriction-lock 4)
        (org-agenda-redo)
        (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "F"
                           'dcn/restrict-to-file-or-follow))
          'append)

(defun dcn/narrow-to-org-subtree ()
    (widen)
    (org-narrow-to-subtree)
    (save-restriction
        (org-agenda-set-restriction-lock)))

(defun dcn/narrow-to-subtree ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
            (progn
                (org-with-point-at
                        (org-get-at-bol 'org-hd-marker)
                    (dcn/narrow-to-org-subtree))
                (when org-agenda-sticky
                    (org-agenda-redo)))
        (dcn/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "N"
                           'dcn/narrow-to-subtree))
          'append)

(defun dcn/narrow-up-one-org-level ()
    (widen)
    (save-excursion
        (outline-up-heading 1 'invisible-ok)
        (dcn/narrow-to-org-subtree)))

(defun dcn/get-pom-from-agenda-restriction-or-point ()
    (or (and (marker-position org-agenda-restrict-begin)
             org-agenda-restrict-begin)
        (org-get-at-bol 'org-hd-marker)
        (and (equal major-mode 'org-mode) (point))
        org-clock-marker))

(defun dcn/narrow-up-one-level ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
            (progn
                (org-with-point-at
                        (dcn/get-pom-from-agenda-restriction-or-point)
                    (dcn/narrow-up-one-org-level))
                (org-agenda-redo))
        (dcn/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "U"
                           'dcn/narrow-up-one-level))
          'append)

(defun dcn/narrow-to-org-project ()
    (widen)
    (save-excursion
        (dcn/find-project-task)
        (dcn/narrow-to-org-subtree)))

(defun dcn/narrow-to-project ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
            (progn
                (org-with-point-at
                        (dcn/get-pom-from-agenda-restriction-or-point)
                    (dcn/narrow-to-org-project)
                    (save-excursion
                        (dcn/find-project-task)
                        (org-agenda-set-restriction-lock)))
                (org-agenda-redo)
                (beginning-of-buffer))
        (dcn/narrow-to-org-project)
        (save-restriction
            (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "P"
                           'dcn/narrow-to-project))
          'append)

(defvar dcn/project-list nil)

(defun dcn/view-next-project ()
    (interactive)
    (let (num-project-left current-project)
        (unless (marker-position org-agenda-restrict-begin)
            (goto-char (point-min))
            ;; Clear existing markers on list
            (while dcn/project-list
                (set-marker (pop dcn/project-list) nil))
            (re-search-forward "New Tasks")
            (forward-visible-line 1))

        ;; Build a new project marker list
        (unless dcn/project-list
            (while (< (point) (point-max))
                (while (and (< (point) (point-max))
                            (or (not (org-get-at-bol 'org-hd-marker))
                                (org-with-point-at
                                        (org-get-at-bol
                                         'org-hd-marker)
                                    (or (not (dcn/is-project-p))
                                        (dcn/is-project-subtree-p)))))
                    (forward-visible-line 1))
                (when (< (point) (point-max))
                    (add-to-list 'dcn/project-list
                                 (copy-marker
                                  (org-get-at-bol 'org-hd-marker))
                                 'append))
                (forward-visible-line 1)))

        ;; Pop off first marker on list and display
        (setq current-project (pop dcn/project-list))
        (when current-project
            (org-with-point-at current-project
                (setq dcn/hide-scheduled-and-waiting-next-tasks nil)
                (dcn/narrow-to-project))
            ;; Remove marker
            (setq current-project nil)
            (org-agenda-redo)
            (beginning-of-buffer)
            (setq num-projects-left (length dcn/project-list))
            (if (> num-projects-left 0)
                    (message "%s projects left to view"
                             num-projects-left)
                (beginning-of-buffer)
                (setq dcn/hide-scheduled-and-waiting-next-tasks t)
                (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
               (org-defkey org-agenda-mode-map "V"
                           'dcn/view-next-project))
          'append)

;; Show next headline
(setq org-show-entry-below (quote ((default))))

;; Create an inactive timestamp with new header
(defvar dcn/insert-inactive-timestamp t)

(defun dcn/toggle-insert-inactive-timestamp ()
    (interactive)
    (setq dcn/insert-inactive-timestamp (not
                                         dcn/insert-inactive-timestamp))
    (message "Heading timestamps are %s"
             (if dcn/insert-inactive-timestamp "ON" "OFF")))

(defun dcn/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

(defun dcn/insert-heading-inactive-timestamp ()
    (save-excursion
        (when dcn/insert-inactive-timestamp
            (org-return)
            (org-cycle)
            (dcn/insert-inactive-timestamp))))

(defun dcn/insert-todo-heding-respect-content ()
    (interactive)
    (dcn/insert-heading-inactive-timestamp)
    (org-insert-todo-heading-respect-content))

(defadvice org-insert-todo-heading
        (after dcn/org-time-stamp-new-headline activate compile)
               (let ((previous-location (point)))
            (dcn/insert-heading-inactive-timestamp)
            (goto-char previous-location)))

;;; Clock configuration

;; Persist clocking when restart
;;(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-history-length 23)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state 'dcn/clock-in-to-next)
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-sound t)

;; Automatically resume task on start
(setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in reports
(setq org-clock-report-include-clocking-task t)

(setq dcn/keep-clock-running nil)

(defun dcn/clock-in-to-next (kw)
    "Switch task from TODO to NEXT when clocking in.  Skip capture
    tasks, project, and sub-projects. Projects and sub-projects are
    switched from NEXT to TODO."
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
        (cond
         ((and (member (org-get-todo-state) (list "TODO"))
               (dcn/is-task-p))
          "NEXT")
         ((and (member (org-get-todo-state) (list "NEXT"))
               (dcn/is-project-p))
          "TODO"))))

(defun dcn/punch-in (arg)
    "Start continuous clocking and set the default task to the
    selected task. If no task is selected set the Organization task as
    the default task."
    (interactive "p")
    (setq dcn/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
            ;; In agenda
            (let* ((marker (org-get-at-bol 'org-hd-marker))
                   (tags (org-with-pointer-at marker
                                              (org-get-tags-at))))
                (if (and (eq arg 4) tags)
                        (org-agenda-clock-in '(16))
                    (dcn/clock-in-organization-task-as-default)))
        ;; Not in Agenda
        (save-restriction
            (widen)
                                        ; Find tags on current task
            (if (and (equal major-mode 'org-mode) (not
                                                   (org-before-first-heading-p)) (eq arg 4))
                    (org-clock-in '(16))
                (dcn/clock-in-organization-task-as-default)))))

(defun dcn/punch-out ()
    (interactive)
    (setq dcn/keep-clock-running nil)
    (when (org-clock-is-active)
        (org-clock-out))
    (org-agenda-remove-restriction-lock))

(global-set-key (kbd "\C-ci") 'dcn/punch-in)
(global-set-key (kbd "\C-co") 'dcn/punch-out)

(defun dcn/clock-in-default-task ()
    (save-excursion
        (org-with-point-at org-clock-default-task
            (org-clock-in))))

(defun dcn/clock-in-parent-task ()
    "Move point to the parent (project) task if any and clock in."
    (let ((parent-task))
        (save-excursion
            (save-restriction
                (widen)
                (while (and (not parent-task) (org-up-heading-safe))
                    (when (member (nth 2 (org-heading-components))
                                  org-todo-keywords-1)
                        (setq parent-task (point))))
                (if parent-task
                        (org-with-point-at parent-task
                            (org-clock-in))
                    (when dcn/keep-clock-running
                        (dcn/clock-in-default-task)))))))

(defvar dcn/organization-task-id "organization-task-123")

(require 'org-id)
(defun dcn/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find dcn/organization-task-id 'marker)
        (org-clock-in '(16))))

(defun dcn/clock-out-maybe ()
    (when (and dcn/keep-clock-running
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
        (dcn/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'dcn/clock-out-maybe 'append)

;; Column mode for viewing tasks
(setq org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; Effort estimates
(setq org-global-properties (quote (("Effort_ALL" .
                                     "0:10 0:30 1:00 2:00 4:00 8:00")
                                    ("STYLE_ALL" . "habit"))))

;; Command to translate words in table.
;; Depends on https://github.com/soimort/translate-shell
(defun translate-insert-rows ()
    "Translates a plain word and inserts it and translation into new table."
    (interactive)
    (let (pos1 pos2 bds)
        (if (use-region-p)
                (setq pos1 (region-beginning) pos2 (region-end))
            (progn
                (setq bds (bounds-of-thing-at-point 'line))
                (setq pos1 (car bds) pos2 (cdr bds))))
        (setq num-lines (count-lines pos1 pos2))
        (goto-char pos1)
        (while (> num-lines 0)
            (setq line (replace-regexp-in-string "\n" ""
                                                 (thing-at-point 'line)))
            (setq result (replace-regexp-in-string "\n" ""
                                                   (shell-command-to-string
                                                    (message
                                                     "trans -b %s" line))))
            (kill-visual-line)
            (insert (message "|%s|%s||" line result))
            (beginning-of-line)
            (org-cycle)
            (next-line)
            (beginning-of-line)
            (setq num-lines (- num-lines 1)))))

(add-hook 'org-mode-hook
          (lambda ()
              (local-set-key "\C-cr" 'translate-insert-rows))
          'append)

;; Don't enter blank lines between headings
(setq org-cycle-separator-lines 0)

;; Don't export timestamps when publishing
(setq org-export-with-timestamps nil)

;; Display settings
(setq org-startup-indented t)

;; Use org-crypt to encrypt parts of org file
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key dcn/gpg-key)
;;(setq org-crypt-disable-auto-save nil)


;; Org Babel languages
(setq org-plantuml-jar-path
      "/usr/local/Cellar/plantuml/8015/plantuml.8015.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (haskell . t)
   (ledger . t)
   (sh . t)
   (plantuml . t)))

;; This is dangerous but convenient
(setq org-confirm-babel-evaluate nil)

(provide 'init-org)
;;; init-org.el ends here

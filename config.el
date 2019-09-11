(setq user-full-name "deceroainfinito"
      user-mail-address "raul@deceroainfinito.es"
      epa-file-encrypt-to user-mail-address)

(setq
 +doom-dashboard-banner-file (expand-file-name "red_dice_logo.png" doom-private-dir))

(setq
 doom-font (font-spec :family "Fira Code" :size 14)
 doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 18))

(def-package! doom-themes
  :init
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  :config
  (load-theme 'doom-one t))

(map! :ne "M-=" (λ! (text-scale-set 0))
      :ne "M-+" #'text-scale-increase
      :ne "M--" #'text-scale-decrease)

;; !!!! M stands for 'alt' key
(map! :g "M-º" (λ! (interactive) (insert "\\")))
(map! :ie "M-1" (λ! (interactive) (insert "|")))
(map! :ie "M-2" (λ! (interactive) (insert "@")))
(map! :ie "M-3" (λ! (interactive) (insert "#")))
(map! :ie "M-ñ" (λ! (interactive) (insert "~")))

(map! :leader :g "a" #'projectile-ag)

(after! org
  :config
  (setq org-link-frame-setup '((file . find-file-other-window)))
  (setq org-todo-keywords '((sequence "[!](i)" "[ ](E)" "[-](g)" "[?](q)" "|" "[X](x)")
                            (sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)" "DONE(d)")
                            (sequence "[READY FOR REVIEW](W)"
                                      "[REVIEW FAILED](F)"
                                      "[READY FOR TESTING](T)"
                                      "[URGENT](G)"
                                      "[TESTING FAILED](A)"
                                      "[DO NOT MERGE](N)"
                                      "[QA NOT REQUIRED](Q)"
                                      "[READY FOR PRODUCTION](P)"
                                      "|"
                                      "[MERGED](M)"
                                      "[DONE](D)"))))

(map! :map outline-mode-map
      :n "zw" #'widen)

(map! :map org-mode-map
      :n "gi" #'org-id-copy
      :n "ci" #'org-id-get-create)

(map! :map evil-org-mode-map
      :n "c" #'evil-change)

(after! org
  :init
  (add-to-list 'org-modules 'org-habit t)
  :config
  (setq org-agenda-files (list "~/ownbujo" "~/badibujo/"))
  (setq org-habit-show-all-today t)
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                    org-agenda-file-regexp))))

(def-package! org-super-agenda
  :after org-agenda
  ;; before the package is loaded
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due today"
                                         :deadline today)
                                  (:name "Important"
                                         :priority "A")
                                  (:name "Habits"
                                         :tags "habit")
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                  (:name "Big Outcomes"
                                         :tag "bo")))
  ;; after the package is loaded
  :config
  (org-super-agenda-mode))

(after! org
  :init
  (add-to-list 'org-modules 'org-journal t)
  :config
  (setq org-journal-dir "/Users/raulmp/Library/Mobile Documents/com~apple~CloudDocs/Badis/org/ownjournal"))

(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun afs/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))

(defun hunspell/check-english
            (interactive)
            (ispell-change-dictionary "en_GB"))
(defun hunspell/check-spanish
            (interactive)
            (ispell-change-dictionary "es_es"))

(after! ispell
  :config
  (setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_GB")
  ;; (map! :map org-mode-map
  ;;     :n "he" #'hunspell/check-spanish
  ;;     :n "hi" #'hunspell/check-english))

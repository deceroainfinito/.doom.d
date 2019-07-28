;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;; (setq doom-font (font-spec :family "Source Code" :size 20) doom-big-font (font-spec :family "Source Code" :size 36))

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
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                  (:name "Big Outcomes"
                                         :tag "bo")))
  ;; after the package is loaded
  :config
  (org-super-agenda-mode))

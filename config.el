;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;; (setq doom-font (font-spec :family "Source Code" :size 20) doom-big-font (font-spec :family "Source Code" :size 36))
;;

(setq

 +doom-dashboard-banner-file (expand-file-name "red_dice_logo.png" doom-private-dir)

 doom-font (font-spec :family "Fira Code" :size 22)
 doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 18)


 org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
                     (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
                     (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)")
                     (sequence "[READY FOR REVIEW](W)" "[REVIEW FAILED](F)" "[READY FOR TESTING](T)" "[URGENT](G)" "[TESTING FAILED](A)" "|" "[DONE](d)"))
 )

(map! :ne "M-=" (λ! (text-scale-set 0))
      :ne "M-+" #'text-scale-increase
      :ne "M--" #'text-scale-decrease)

;; M spanish keyboard symbols doesn't work so well with workspaces feature
;; !!!! M stands for 'alt' key
(map! :g "M-º" (λ! (interactive) (insert "\\")))
(map! :ie "M-1" (λ! (interactive) (insert "|")))
(map! :ie "M-2" (λ! (interactive) (insert "@")))
(map! :ie "M-3" (λ! (interactive) (insert "#")))



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
  (load-theme 'doom-peacock t))

;; Maximize on startup
(set-frame-parameter (selected-frame) 'fullscreen 'fullboth)

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

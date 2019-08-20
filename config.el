;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

 ;; fullscreen at startup
(set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
 ;; Literate init
(org-babel-load-file (expand-file-name "~/.doom.d/my-config.org"))

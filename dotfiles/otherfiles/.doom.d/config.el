;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "mneuss"
      user-mail-address "mneuss@no-reply.com")

(setq doom-theme 'doom-one)

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(after! org
  (setq org-directory "~/Documents/org")
  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXTSTEP(s)" "WAITING(w)" "|" "DONE(d)" "WONTDO(n)")))
  (setq org-agenda-files '("~/Documents/org"))
  (setq org-log-done t)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c i") 'org-capture-inbox)
  )


(after! org-capture
  (setq org-capture-templates
        '(("i" "Misc inbox" entry (file "inbox.org") "* TODO %?")
          ("p" "Programming" entry (file "programming.org") "* TODO %?")
          ("l" "Language Learning" entry (file "language.org") "* TODO %?"))
  )
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq auto-mode-alist
   (append
     '(
       ("\\.pl\\'" . prolog-mode)
       ;; ("\\.agda\\'" . agda2-mode)
       ;; ("\\.lagda.md\\'" . agda2-mode)
       )
     auto-mode-alist))
;; (add-to-list 'auto-mode-alist '("\\.g\\'" . gnuplot-mode))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq projectile-project-search-path '("~/Programming" "~/Documents/org" "~/Documents/agenda" "~/.xmonad"))

(setq lsp-haskell-server-path "haskell-language-server-wrapper")
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/zettelkasten/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-alias-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

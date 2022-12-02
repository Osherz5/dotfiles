;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

  
;; Startup optimizations
(when (boundp 'w32-pipe-read-delay)
(setq w32-pipe-read-delay 0))


;; UI settings
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
  
(column-number-mode)
(global-display-line-numbers-mode t)

(setq global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers
(global-auto-revert-mode 1) ; Revert buffers when the underlying file has changed


;; Set frame transparency
(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)
;; Make frame transparency overridable
(defvar efs/frame-transparency '(97 . 97))

(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; TODO: Install these automatically when missing
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; HELM
(use-package helm
  :bind (("M-x" . 'helm-M-x)
         ("C-x C-f" . 'helm-find-files))

  :config (helm-mode 1)
)

(use-package helm-ag
  :bind (("C-c C-g" . 'helm-ag))
)

(use-package lua-mode)
(use-package magit
  :init
  (setq magit-git-executable "C:/Program Files/Git/cmd/git.exe")
)

;; Projectile
(use-package projectile
  :bind (("C-c p" . 'projectile-command-map))
  :config (projectile-mode +1)
  )

(use-package solarized-theme)

;; Theme
(load-theme 'solarized-dark t)


(use-package transpose-frame)

(use-package lsp-mode)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))); or lsp-deferred
  :bind (:map python-mode-map ("C-M-l r" . lsp-workspace-restart)
              ("C-M-l R" . lsp-rename))
  )
;; TODO: Switch to elpy


(use-package company
  :config (global-company-mode t)
  )


; Org-Mode Timer
(setq org-clock-sound "~/.emacs.d/sounds/PauseEffect.wav")

; English dates in timestamps
(setq system-time-locale "C")

; Org-Mode log times for TODOs
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "CANCELED(c@/!)" )))


; Activate Org-Mode Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (C . t)
   )
)

; Add <s TAB style code block insertion
(require 'org-tempo)
(setq org-modules
      '(org-tempo)
      )

(add-to-list 'org-structure-template-alist '("p" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("r" . "src R"))

(setq org-babel-R-command "c:/Progra~1/R/R-4.2.1/bin/R --slave --no-save")

; Org handle tabs on src blocks
(setq org-src-tab-acts-natively t)

;; Enable cdlatex in org
(use-package cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
(plist-put org-format-latex-options :scale 1.5)

;; Org export options
(setq org-export-backends '(ascii html icalendar latex md odt))

; Agenda keybinds
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)


;; 4 Spaces instead of \t
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; Set indent to 4 spaces also in json.el
(setq json-encoding-default-indentation    "    ")

;; Prettify JSON
(defun prettify-json()
  (interactive)
  (json-pretty-print-buffer)
  (delete-trailing-whitespace)
)


(global-set-key (kbd "M-F") 'prettify-json)

;; Fix find in dired
(setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\"")


;; Fix python path in windows
(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")


;; CTAGS
(setq path-to-ctags "C:/Users/osherj/scoop/shims/ctags.exe") ;; <- your ctags path here
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -f TAGS -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;; Run python and pop-up its shell.
;; Kill process to solve the reload modules problem.
(defun my-python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
     (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
     (kill-process (get-buffer-process "*Python*"))
     ;; If you want to clean the buffer too.
     ;;(kill-buffer "*Python*")
     ;; Not so fast!
     (sleep-for 0.5))
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  ;; Pop new window only if shell isnt visible
  ;; in any frame.
  (unless (get-buffer-window "*Python*" t) 
    (python-shell-switch-to-shell)))

(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell))

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-p") 'my-python-shell-run)
     (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)))


; Set default browser
(setq browse-url-generic-program "c:/Program Files/Google/Chrome/Application/chrome.exe")
(setq browse-url-browser-function 'browse-url-generic)


; Set up my task management file on the work computer (Backed by SyncThing)
(when (equal system-name "OSHERJ-LP")
  (setq org-agenda-files
        (list "c:/synced/Work.org"
              ))
  (find-file "c:/synced/Work.org") ; It's also the default startup buffer
)


;; Utils
(defun decode-hex-string (hex-string)
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))

;; Increment number function
(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-+") 'increment-number-at-point)

;; TODO: Add encode hex


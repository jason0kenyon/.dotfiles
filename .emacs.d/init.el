  (add-to-list 'load-path (expand-file-name "pers-config" user-emacs-directory))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (setq custom-file "~/.emacs.d/custom.el")
  (require 'package)

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Initialize use-package
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t)

  (use-package auto-package-update
    :config
    (auto-package-update-maybe))
  (use-package org-auto-tangle
    :diminish
    :init
    (add-hook 'org-mode-hook 'org-auto-tangle-mode))

  (defvar jas/window-inc-size-hor 20)
  (defvar jas/window-inc-size-vert 10)
  (defvar jas/default-font-size 180)
  (defvar jas/default-variable-font-size 220)
  (defvar jas/default-fixed-font "Martian Mono")
  (defvar jas/default-variable-font "Cormorant Garamond")
  (setq inhibit-startup-message t)
(setq initial-scratch-message "")
  (setq tab-width 2)
  (blink-cursor-mode -1)
  (show-paren-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 10)
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (defun display-startup-echo-area-message ()
    (message ""))
  (use-package helpful)
  (use-package no-littering
    :init
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

    (use-package openwith
      :config
      (openwith-mode 1))
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp
                  '("pdf"))
                 "zathura"
                 '(file))))

    (use-package undo-tree
    :diminish)
    (global-undo-tree-mode)
    (use-package evil
      :init
      (setq evil-undo-system 'undo-tree)
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll t)
      (setq evil-want-C-i-jump t)
      (setq evil-want-C-w-delete nil)
      (setq evil-want-integration t)
      (setq evil-want-fine-undo t)
      (setq evil-shift-width tab-width)
      :config
      (evil-mode 1))

    (use-package evil-escape
      :diminish
      :init
      (setq-default evil-escape-key-sequence "jk")
      (setq-default evil-escape-delay 0.1)
      :config
      (evil-escape-mode 1))
    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))

  (use-package which-key
    :diminish 
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 1))
  (use-package general
    :config (general-evil-setup t))
  (general-create-definer jas/leader-def
    ;; :prefix my-leader
    :prefix "SPC")
  (general-define-key
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease) 
  (general-define-key
   :keymaps 'read-passwd-map
   "C-v" 'evil-paste-after)
  (jas/leader-def
    :states 'normal
    "." 'find-file
    "," 'consult-buffer
    "fp" (lambda () (interactive) (find-file (expand-file-name "init.org" user-emacs-directory)))
    "fr" 'consult-recent-file
    "hi" 'info
    "bn" 'switch-to-next-buffer
    "bv" 'switch-to-prev-buffer
    "bk" 'kill-buffer
    "ck" 'kill-compilation
    "oa" 'org-agenda
    "hf" 'helpful-callable
    "hv" 'helpful-variable
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "ww" 'evil-window-delete
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "w]" (lambda () (interactive) (enlarge-window-horizontally jas/window-inc-size-hor)) 
    "w[" (lambda () (interactive) (shrink-window-horizontally jas/window-inc-size-hor)) 
    "w}" (lambda () (interactive) (enlarge-window jas/window-inc-size-vert)) 
    "w{" (lambda () (interactive) (shrink-window jas/window-inc-size-vert)) 
    "w=" 'balance-windows
    "wt" 'tear-off-window

    )

  (set-face-attribute 'default nil :font jas/default-fixed-font :height jas/default-font-size )
  (set-face-attribute 'fixed-pitch nil :font jas/default-fixed-font :height jas/default-font-size )
  (set-face-attribute 'variable-pitch nil :font jas/default-variable-font :weight 'normal :height jas/default-variable-font-size )
  (use-package all-the-icons)
  (use-package doom-themes
    :config
    (setq doom-themes-enable-bold t    
          doom-themes-enable-italic t)) 
  (use-package kaolin-themes)
  (load-theme 'doom-city-lights t)

(use-package sudo-edit)
        (use-package doom-modeline)
(setq doom-modeline-height 60)
  ;; Define your custom doom-modeline
  (doom-modeline-def-modeline 'my-simple-line
    '(bar matches buffer-info remote-host  parrot selection-info)
    '(misc-info minor-modes input-method  major-mode process vcs checker))

  ;; Set default mode-line
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'my-simple-line 'default)))

  ;; Configure other mode-lines based on major modes
  (add-to-list 'doom-modeline-mode-alist '(my-mode . my-simple-line))
      (doom-modeline-mode)

  (use-package counsel)
  (recentf-mode 1)
    (use-package savehist
      :init
      (savehist-mode))
      (use-package vertico
        :init
        (vertico-mode))
      (use-package vertico-directory
        :after vertico
        :ensure nil
        ;; More convenient directory navigation commands
        :bind (:map vertico-map
                    ("RET" . vertico-directory-enter)
                    ("DEL" . vertico-directory-delete-char)
                    ("M-DEL" . vertico-directory-delete-word)
                    ("C-k" . previous-line-or-history-element)
                     ("C-j" . next-line-or-history-element))
        ;; Tidy shadowed file names
        :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
      (use-package consult)
      (use-package marginalia
        :init (marginalia-mode))
      (use-package embark)
      (use-package embark-consult)
      (use-package orderless
        :custom
        (completion-styles '(orderless basic))
        (completion-category-overrides '((file (styles basic partial-completion)))))
  (use-package consult-flycheck)

(use-package all-the-icons-dired)
(setq all-the-icons-dired-monochrome nil)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq org-src-preserve-indentation t)
  (use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (add-hook 'org-mode-hook 'org-superstar-mode)
        (setq org-src-tab-acts-natively t)
        (setq org-src-fontify-natively t)
        (require 'org-tempo)
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-hook 'org-mode-hook (lambda() (display-line-numbers-mode 0)))
        (add-hook 'org-mode-hook 'org-indent-mode)
        (add-hook 'org-mode-hook 'visual-line-mode)
        (add-hook 'org-mode-hook (lambda () (set-fringe-mode 10)))
    (setq org-ellipsis " ▼"
            org-superstar-remove-leading-stars t
            org-hide-emphasis-markers t
            org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
            org-superstar-item-bullet-alist '((?+ . ?◆) (?- . ?•))
            org-superstar-special-todo-items 'hide)
      (add-hook 'org-mode-hook 'variable-pitch-mode)
      (add-hook 'org-mode-hook 'visual-line-mode)
      (set-face-attribute 'org-document-title nil :font jas/default-fixed-font :weight 'bold :height 1.3)
      (dolist (face '((org-level-1 . 1.8)
                      (org-level-2 . 1.6)
                      (org-level-3 . 1.5)
                      (org-level-4 . 1.2)
                      (org-level-5 . 1.1)
                      (org-level-6 . 1.1)
                      (org-level-7 . 1.1)
                      (org-level-8 . 1.1))) (set-face-attribute (car face) nil :font jas/default-variable-font :weight 'medium :height (cdr face)))

      (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
      (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
      (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
      (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

(add-hook 'org-agenda-mode-hook (lambda () (display-line-numbers-mode 0) ))
  (setq org-directory "~/Dropbox/notes"
        org-agenda-files '("~/Dropbox/notes" "~/Dropbox/notes/daily"))
(setq org-id-locations-file (expand-file-name ".orgids" user-emacs-directory))
(setq org-insert-heading-respect-content t)
  (setq org-agenda-window-setup 'only-window)
  (setq
   org-agenda-block-separator ?\u25AA
   org-todo-keywords
   '((sequence
      "TODO(t)"
      "WAIT(w)"
      "|"
      "DONE(d)"
      "CANCELLED(c)"
      )))
  (setq org-agenda-remove-tags t)

    (setq org-clock-mode-line-entry nil)
    (use-package org-pomodoro
      :after org)
  (setq org-pomodoro-length 60)
  (setq org-pomodoro-short-break-length 30)
  (setq org-pomodoro-long-break-length 45)
      (jas/leader-def
   :states 'normal
        "op"  'org-pomodoro)

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
      (jas/leader-def
    :states 'normal
    :keymaps 'org-mode-map
     "mx" 'org-toggle-checkbox
     "mp" 'org-priority
     "mt" 'org-time-stamp)
    (general-define-key
   :keymaps 'org-mode-map
  "C-<return>" 'org-meta-return
  "M-<return>" 'org-insert-todo-heading
  )

      (use-package denote)
      (setq denote-directory "~/Dropbox/notes")
  
      (setq denote-templates '((daily . "* Journal\n\n* Tasks\n\n* Notes") (math-landing-page . "* meta-analysis\n* Source")))

      (defun daily-journal ()
        "Create an entry tagged 'journal' with the date as its title."
        (interactive)
        (denote
         (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
         '("daily")
         'org
         (concat denote-directory "/daily")
         nil
         'daily)) ; multiple keywords are a list of strings: '("one" "two")
(add-hook 'dired-mode-hook #'denote-dired-mode)

  (use-package citar
    :custom
    (citar-bibliography '("~/Dropbox/shared-notes/templates/refs.bib")))
  (use-package citar-denote
    :diminish
    :after citar denote
    :config
    (citar-denote-mode)
    (setq citar-open-always-create-notes t))
  (setq citar-library-paths '("~/Dropbox/library/papers" "~/Dropbox/shared-notes/bookshelf/papers"))
  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . "Notes on ${author editor}, ${title}")))
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . "📁")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . "🖋️")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . "🔗")))
  (setq citar-symbol-separator "  ")

(use-package cdlatex)
(require 'lazytab)
(set-display-table-slot standard-display-table 
                         'selective-display 
                         (string-to-vector " ▼"))
  (use-package mixed-pitch)
  (add-hook 'LaTeX-mode-hook 'mixed-pitch-mode)
        (general-define-key
    :keymaps 'LaTeX-mode-map
  "C-<return>" (lambda () (interactive) (LaTeX-insert-item) (TeX-fold-paragraph))
      )
(add-hook 'yas-after-exit-snippet-hook 'TeX-fold-buffer)
    (setq TeX-electric-sub-and-superscript t)
          (setq TeX-parse-self t)
          (setq-default TeX-master nil)
          (use-package tex-mode
            :ensure auctex)
          (add-hook 'TeX-mode-hook 'LaTeX-math-mode)
          (add-hook 'TeX-mode-hook 'electric-pair-mode)
          (add-hook 'TeX-mode-hook 'visual-line-mode)
          (add-hook 'TeX-mode-hook 'reftex-mode)
          (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
(add-hook 'LaTeX-mode-hook 
          (lambda ()
            (TeX-fold-mode 1)
            (add-hook 'find-file-hook 'TeX-fold-buffer t t)))
          (add-hook 'TeX-mode-hook 'prettify-symbols-mode)
          (add-hook 'TeX-mode-hook
                    (lambda ()
                      (push '("\\mathbb{C}" . ?ℂ) prettify-symbols-alist)
                      (push '("\\mathbb{F}" . ?𝔽) prettify-symbols-alist)
                      (push '("\\dots" . ?…) prettify-symbols-alist)
                      ))
(setq LaTeX-item-indent 2)
(setq LaTeX-indent-level 4)

(custom-set-variables
 '(TeX-fold-macro-spec-list
  '(("[f]"
    ("footnote" "marginpar"))
   ("[c]"
    ("cite"))
   ("[l]"
    ("label"))
   ("[r]"
    ("ref" "pageref" "eqref" "footref"))
   ("[i]"
    ("index" "glossary"))
   ("[1]:||🟍"
    ("item"))
   ("..."
    ("dots"))
;; tweaked defaults
        ("©" ("copyright"))
        ("®" ("textregistered"))
        ("™"  ("texttrademark"))
        ;; extra
        ("⬖{1}" ("begin"))
        ("⬗{1}" ("end"))
   (1
    ("part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "section*" "subsection*" "subsubsection*" "paragraph*" "subparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "mathbf" "textsc" "textup")))))
(general-define-key
:states 'normal
:keymaps 'LaTeX-mode-map
"<tab>" 'outline-toggle-children)

(setq LaTeX-electric-left-right-brace t)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
    (add-hook 'LaTeX-mode-hook 'electric-pair-mode)
    (use-package yasnippet)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (use-package yasnippet-snippets)
    (use-package aas
      :hook (LaTeX-mode . aas-activate-for-major-mode))
    (yas-global-mode 1)
(defun jas/enum-check ()
(string= (LaTeX-current-environment) "enumerate"))
    (use-package laas
      :hook ((LaTeX-mode . laas-mode))
      :config ; do whatever here
      (aas-set-snippets 'laas-mode
	  "mk" (lambda () (interactive)
		      (yas-expand-snippet "$$0$"))
	;; set condition!
	"pma" (lambda () (interactive)
                (orgtbl-mode)
		 (yas-expand-snippet
"\\begin{pmatrix}
$0
\\end{pmatrix}"))
	"spn" (lambda () (interactive)
		 (yas-expand-snippet "\\Span($1)$0"))

	"defin" (lambda () (interactive)
		 (yas-expand-snippet "\\int_{$1}^{$2} $0"))

	";R" (lambda () (interactive)
		 (yas-expand-snippet "\\mathbb{R}"))

	";C" (lambda () (interactive)
		 (yas-expand-snippet "\\mathbb{C}"))
	";<" (lambda () (interactive)
		 (yas-expand-snippet "\\langle "))

	";>" (lambda () (interactive)
		 (yas-expand-snippet "\\rangle"))
	"sum" (lambda () (interactive)
		 (yas-expand-snippet "\\sum_{$1}^{$2} $0"))


	"||" (lambda () (interactive)
		 (yas-expand-snippet "||$1||$0"))
	;; add accent snippets
	:cond #'laas-object-on-left-condition
	"qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;;(add-hook 'aas-post-snippet-expand-hook (lambda () (TeX-fold-buffer)))
(use-package powerthesaurus)
(defun my-hide-compilation-buffer (proc)
  "Hide the compile buffer `PROC' is ignored."
  (let* ((window (get-buffer-window "*compilation*"))
         (frame (window-frame window)))
    (ignore-errors
      (delete-window window))))

(add-hook 'compilation-start-hook 'my-hide-compilation-buffer)
(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pvc -pdf --view=none")))
  (setq dictionary-server "localhost")
       (use-package flycheck
    :diminish)
       (flycheck-add-mode 'tex-chktex 'LaTeX-mode)
       (add-hook 'LaTeX-mode-hook 'flycheck-mode)
       (add-hook 'LaTeX-mode-hook (lambda () (set-fringe-mode 30)))
       (jas/leader-def
      :states 'normal
    "<tab>" 'lazytab-orgtbl-replace
    "sf" 'consult-flycheck
    "sg" 'consult-ripgrep
    "sw" 'dictionary-search
    "cc" 'flyspell-correct-at-point
    "C"  'compile
    )
    (use-package flyspell-correct)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)

(server-start)
(use-package exwm)


(add-hook 'exwm-update-class-hook
      (lambda ()
        (exwm-workspace-rename-buffer exwm-class-name)))


(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 3456x2160 --pos 0x0 --rotate normal --output DP-1 --off --output DP-2 --off --output DP-3 --off")

  (defun jas/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (jas/run-in-background ,command)))
    (setq key (pop bindings)
          command (pop bindings))))
  (defun make-external-command (command)
    (lambda ()
      (interactive)
      (let ((buffer-name (car (split-string command))))
        (cond
         ((equal buffer-name (buffer-name))
          (switch-to-last-used-buffer))
         ((get-buffer buffer-name)
          (switch-to-buffer (get-buffer buffer-name)))
         (t (start-process-shell-command buffer-name nil command))))))
  (defun jas/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(setq exwm-input-prefix-keys
        '(?\M-x))
 (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)
          ([?\s-q] . exwm-workspace-delete)
          ([?\s-s] . evil-window-vsplit)
          ([?\s-v] . evil-window-split)
          ([?\s-p] . exwm-workspace-switch)
          ([?\s-w] . evil-window-delete)
          ([?\s-]] . (lambda () (interactive) (enlarge-window-horizontally jas/window-inc-size-hor)))
          ([?\s-[] . (lambda () (interactive) (shrink-window-horizontally jas/window-inc-size-hor)))

          ([?\s-}] . (lambda () (interactive) (enlarge-window jas/window-inc-size-vert)))
          ([?\s-{] . (lambda () (interactive) (shrink-window jas/window-inc-size-vert)))

          ;; Switch workspace
          ;;          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

(jas/bind-command
"<XF86AudioMute>" "amixer set Master toggle"
"<XF86AudioLowerVolume>" "amixer set Master 10%-"
"<XF86AudioRaiseVolume>" "amixer set Master 10%+"
"<XF86MonBrightnessUp>" "brightnessctl set 10%+"
"<XF86MonBrightnessDown>" "brightnessctl set 10%-"
"s-b" "qutebrowser"
"s-g" "gimp")
(exwm-input-set-key (kbd "s-x") 'counsel-linux-app)
(defun jas/setup-window-by-class ()
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("Brave-browser" (exwm-workspace-move-window 2))
    ("Inkscape" (setq-default mode-line-format nil))))
(add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))
(add-hook 'exwm-manage-finish-hook
            (lambda ()
              ;; Send the window where it belongs
              (jas/setup-window-by-class)))

(setq exwm-workspace-number 4)
(defun jas/exwm-init-hook ()
  (jas/run-in-background "dropbox")
  (jas/run-in-background "nm-applet")
  (jas/run-in-background "blueman-applet")
  (jas/run-in-background "pasystray")
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)
 ;; Start the Polybar panel
  (jas/start-panel)
  )
;; When EXWM starts up, do some extra confifuration
(add-hook 'exwm-init-hook #'jas/exwm-init-hook)
(exwm-enable)

(defvar jas/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun jas/kill-panel ()
  (interactive)
  (when jas/polybar-process
    (ignore-errors
      (kill-process jas/polybar-process)))
  (setq jas/polybar-process nil))

(defun jas/start-panel ()
  (interactive)
  (jas/kill-panel)
  (setq jas/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun jas/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))
(defun jas/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun jas/send-polybar-exwm-workspace ()
  (jas/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'jas/send-polybar-exwm-workspace)

(use-package bongo)
(setq bongo-default-directory "~/Dropbox/music")

(use-package password-store)
(use-package vterm)
  (add-hook 'vterm-mode-hook (lambda() (display-line-numbers-mode 0)))
  (add-hook 'eshell-mode-hook (lambda() (display-line-numbers-mode 0)))
(setq delete-by-moving-to-trash t)
(jas/leader-def
:states 'normal
"t" 'vterm)

(defun jas/denote-link-find-file ()
  "Use minibuffer completion to visit linked file. This is my version that works with arbitrary file types"
  (interactive)
  (if-let* ((current-file (buffer-file-name))
            (file-type (denote-filetype-heuristics current-file))
            (regexp (denote--link-in-context-regexp file-type))
            (files (denote-link--expand-identifiers regexp)))

  (let ((file-names (mapcar #'denote-get-file-name-relative-to-denote-directory
                            files)))
(find-file (completing-read "Jump to file: " file-names)))
    (user-error "No links found in the current buffer")))
(jas/leader-def
:states 'normal
"ms" 'jas/denote-link-find-file
"pm" 'exwm-layout-toggle-mode-line
"pk" 'jas/kill-panel
"pp" 'jas/start-panel
)
(use-package elfeed)
;; Somewhere in your .emacs file
(setq elfeed-feeds
      '("https://api.quantamagazine.org/feed/"
("http://arxiv.org/rss/math.DS" chaos)
("http://arxiv.org/rss/math.AG" alggeom)
("http://arxiv.org/rss/math.DG" difgeo)
))

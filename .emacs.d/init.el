(server-start)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq custom-file "~/.emacs.d/custom.el")
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defvar jas/default-font-size 180)
  (defvar jas/default-variable-font-size 180)
  (defvar jas/default-fixed-font "Hack")
  (defvar jas/default-variable-font "Cantarell")
  (setq inhibit-startup-message t)
  (blink-cursor-mode -1)
  (show-paren-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode 10)
  (use-package which-key
    :config
    (which-key-mode 1)
    (setq which-key-idle-delay 1))
  (set-face-attribute 'default nil :font jas/default-fixed-font :height jas/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font jas/default-fixed-font :height jas/default-font-size)
  (set-face-attribute 'variable-pitch nil :font jas/default-variable-font :height jas/default-variable-font-size :weight 'regular)
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
    "fp" (lambda () (interactive) (find-file user-emacs-directory))
    "hi" 'info
    "bn" 'switch-to-next-buffer
    "bv" 'switch-to-prev-buffer
    "oa" 'org-agenda
    "hf" 'helpful-callable
    "hv" 'helpful-variable)
  (use-package openwith
    :config
    (openwith-mode 1))
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file))))
  (use-package undo-tree)
 (global-undo-tree-mode)
 (customize-set-variable 'evil-undo-system 'undo-tree)
 (setq evil-want-fine-undo t)

(use-package diminish)
  (use-package all-the-icons)
  (use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t)) ; if nil, italics is universally disabled
    (load-theme 'doom-snazzy t)
    (use-package evil
      :init
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll t)
      (setq evil-want-integration t)

      :config
      (evil-mode 1))
    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))
    (use-package org-auto-tangle
      :init
      (add-hook 'org-mode-hook 'org-auto-tangle-mode))
    (use-package no-littering
      :init
      (setq auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
    (use-package auto-package-update
      :config
      (auto-package-update-maybe))
    (use-package helpful)
    (use-package evil-escape
      :init
      (setq-default evil-escape-key-sequence "jk")
      (setq-default evil-escape-delay 0.1)
      :config
      (evil-escape-mode 1))
    (setq org-src-tab-acts-natively t)
    (setq org-src-fontify-natively t)
    (setq-default tab-width 2)
    (setq-default evil-shift-width tab-width)
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(setq initial-scratch-message "")
(line-number-mode 0)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(use-package doom-modeline
  :config (doom-modeline-mode 1))
(setq doom-modeline-major-mode-icon nil)
(setq doom-modeline-height 10)
(setq doom-modeline-buffer-encoding nil)

(use-package ivy)
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

(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode 0)))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'visual-line-mode)
  (use-package org-bullets)
  (setq org-ellipsis " ▼"
        org-superstar-remove-leading-stars t
        org-hide-emphasis-markers t)
  (jas/leader-def
:states 'normal
:keymaps 'org-mode-map
 "mx" 'org-toggle-checkbox
 "mp" 'org-priority
 "mt" 'org-time-stamp)
  ;;  (add-hook 'org-mode-hook 'variable-pitch-mode)
  ;;  (add-hook 'org-mode-hook 'visual-line-mode)
  ;;  (set-face-attribute 'org-document-title nil :font jas/default-fixed-font :weight 'bold :height 1.3)
  ;;  (dolist (face '((org-level-1 . 2.0)
  ;;                  (org-level-2 . 2.0)
  ;;                  (org-level-3 . 1.5)
  ;;                  (org-level-4 . 1.2)
  ;;                  (org-level-5 . 1.1)
  ;;                  (org-level-6 . 1.1)
  ;;                  (org-level-7 . 1.1)
  ;;                  (org-level-8 . 1.1)))
  ;;    (set-face-attribute (car face) nil :font jas/default-variable-font :weight 'medium :height (cdr face)))
  ;;
  ;;  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  ;;  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  ;;  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  ;;  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  ;;  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  ;;  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;;  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;;  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;;  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  ;;  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  ;;  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(setq
 org-startup-folded 'content
 org-src-tab-acts-natively t
 org-src-fontify-natively t
 )
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(setq org-directory "~/projects/notes"
         org-agenda-files '("~/projects/notes"))
   (setq org-agenda-window-setup 'only-window)
   (use-package org-fancy-priorities)
   (setq org-fancy-priorities-list '("⚡" "⚠" "❗"))
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
  (setq org-agenda-prefix-format
'((agenda . " %i %-12:c%?-12t% s")
 (todo . " %i %-12:c")
 (tags . " %i %-12:c")
 (search . " %i %-12:c")))
   (setq org-agenda-custom-commands
         '(("v" "Main"
            ((tags-todo "+PRIORITY=\"A\""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("WAIT")))
                         (org-agenda-overriding-header "High Priority Tasks:")))
             (tags-todo "+PRIORITY=\"B\""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("WAIT")))
                         (org-agenda-overriding-header "Medium Priority Tasks:")))
             (tags-todo "+PRIORITY=\"C\""
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("WAIT")))
                         (org-agenda-overriding-header "Low Priority Tasks:")))
             (agenda "")
             (todo "WAIT"
                   ((org-agenda-overriding-header "On Hold:")))))
           ("l" "Waitlist"
            ((todo "WAIT"
                   ((org-agenda-overriding-header "On Hold:")))))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(use-package tex-mode
  :ensure auctex)
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
(add-hook 'TeX-mode-hook 'visual-line-mode)
(add-hook 'TeX-mode-hook 'reftex-mode)
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)
(add-hook 'TeX-mode-hook (lambda () (and (TeX-fold-mode 1) (TeX-fold-buffer))))
(add-hook 'TeX-mode-hook 'prettify-symbols-mode)
(add-hook 'TeX-mode-hook
          (lambda ()
            (push '("\\mathbb{C}" . ?ℂ) prettify-symbols-alist)
            (push '("\\mathbb{F}" . ?𝔽) prettify-symbols-alist)
            ))

(add-hook 'LaTeX-mode-hool 'electric-pair-mode)
(use-package yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(use-package yasnippet-snippets)
(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode))
(yas-global-mode 1)
(use-package laas
  :hook ((LaTeX-mode . laas-mode))
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
      "mk" (lambda () (interactive)
                  (yas-expand-snippet "$$0$"))
      "nk" (lambda () (interactive)
                  (yas-expand-snippet "\\[$0\\]"))
    ;; set condition!
    :cond #'texmathp ; expand only while in math
    "spn" (lambda () (interactive)
             (yas-expand-snippet "\\Span($1)$0"))
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package lsp-mode
:hook  (lsp-mode . lsp-enable-which-key-integration))
(use-package lsp-ui)
(add-hook 'LaTeX-mode-hook 'lsp-mode)

(use-package denote)
(setq denote-directory "~/projects/notes")

(setq denote-templates '((daily . "* Journal\n\n* Tasks\n** TODO [/]\n1. [ ] Mindfulness(10min)\n2. [ ] Journaling(5min)\n3. [ ] Check Out\n** Notes") (math-landing-page . "* meta-analysis\n* Source")))

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

(use-package with-editor)
(add-hook 'shell-mode-hook  'with-editor-export-editor)

(setq x-select-enable-clipboard t)
(use-package magit) 
(setq evil-move-cursor-back nil) 
(setq evil-esc-delay 0)

(add-to-list 'load-path "~/latex-notes") 
(require 'latex-notes)

(use-package citar
      :custom
      (citar-bibliography '("~/projects/writing/templates/refs.bib")))
  (use-package citar-denote
    :after citar denote
    :config
    (citar-denote-mode)
    (setq citar-open-always-create-notes t))
  (setq citar-library-paths '("~/library/papers/"))
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

(use-package exwm)
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

(setq exwm-input-prefix-keys
      '(?\M-x))

(defun jas/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))
(add-hook 'exwm-update-class-hook #'jas/exwm-update-class)
(defun jas/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(setq exwm-workspace-number 5)
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
          ([?\s-J] . +evil/window-move-down)
          ([?\s-K] . +evil/window-move-up)
          ([?\s-H] . +evil/window-move-left)
          ([?\s-L] . +evil/window-move-right)


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
"s-b" "qutebrowser")
(exwm-input-set-key (kbd "s-x") 'counsel-linux-app)

;; Make sure the server is started (better to do this in your main Emacs config!)
(server-start)

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

(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 3456x2160 --pos 0x0 --rotate normal --output DP-1 --off --output DP-2 --off --output DP-3 --off")
(defun jas/exwm-init-hook ()
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

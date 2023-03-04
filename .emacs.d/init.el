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

(defvar jas/default-font-size 180)
  (defvar jas/default-variable-font-size 180)
  (defvar jas/default-fixed-font "FiraMono")
  (defvar jas/default-variable-font "Genos")
  (setq inhibit-startup-message t)
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

(setq jas/window-inc-size-hor 20)
(setq jas/window-inc-size-vert 10)

(use-package which-key
       :diminish
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
           "fp" (lambda () (interactive) (find-file (expand-file-name "init.org" user-emacs-directory)))
           "hi" 'info
           "bn" 'switch-to-next-buffer
           "bv" 'switch-to-prev-buffer
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

(use-package openwith
  :config
  (openwith-mode 1))
(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("pdf"))
             "zathura"
             '(file))))

(use-package diminish)
    (use-package all-the-icons)
    (use-package doom-themes
      :config
      ;; Global settings (defaults)
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t)) ; if nil, italics is universally disabled
(use-package kaolin-themes)
      (load-theme 'kaolin-bubblegum t)
      (use-package evil
        :init
        (setq evil-want-keybinding nil)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump t)
        (setq evil-want-C-w-delete nil)
        (setq evil-want-integration t)
        (setq evil-want-fine-undo t)

        :config
        (evil-mode 1))
    (use-package undo-tree
:diminish)
   (global-undo-tree-mode)
   (customize-set-variable 'evil-undo-system 'undo-tree)
      (use-package evil-collection
  :diminish
        :after evil
        :config
        (evil-collection-init))
      (use-package org-auto-tangle
  :diminish
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
  :diminish
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
(diminish 'evil-collection-unimpaired-mode)
(diminish 'visual-line-mode)
(diminish 'yas-minor-mode)
(with-eval-after-load 'reftex
(diminish 'reftex-mode))
(with-eval-after-load 'eldoc
(diminish 'eldoc-mode))
(with-eval-after-load 'face-remap
(diminish 'buffer-face-mode))
(with-eval-after-load 'org-indent
(diminish 'org-indent-mode))

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

(add-hook 'org-mode-hook (lambda() (display-line-numbers-mode 0)))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(use-package org-bullets)
(setq org-ellipsis " ▼"
      org-superstar-remove-leading-stars t
      org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (set-face-attribute 'org-document-title nil :font jas/default-fixed-font :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 2.0)
                  (org-level-2 . 2.0)
                  (org-level-3 . 1.5)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font jas/default-variable-font :weight 'medium :height (cdr face)))

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

(setq org-directory "~/projects/notes"
         org-agenda-files '("~/projects/notes" "~/projects/notes/daily"))
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
"C-<return>" 'org-insert-todo-heading
)

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
    (use-package yasnippet
:config
 (diminish 'yas-minor-mode))
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (use-package yasnippet-snippets)
    (use-package aas
      :hook (LaTeX-mode . aas-activate-for-major-mode))
    (yas-global-mode 1)
    (use-package laas :hook ((LaTeX-mode . laas-mode))
      :config ; do whatever here
      (aas-set-snippets 'laas-mode
          "mk" (lambda () (interactive)
                      (yas-expand-snippet "$$0$"))
          "\[" (lambda () (interactive)
                      (yas-expand-snippet "\\[$0\\]"))

        ;; set condition!
        :cond #'texmathp ; expand only while in math
        "spn" (lambda () (interactive)
                 (yas-expand-snippet "\\Span($1)$0"))
          "int" (lambda () (interactive)
                      (yas-expand-snippet "\\int"))
          "sum" (lambda () (interactive)
                      (yas-expand-snippet "\\sum_{$1}^{$2}$0"))
        ;; add accent snippets
        :cond #'laas-object-on-left-condition
        "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

  (setq TeX-electric-sub-and-superscript t)

(use-package flycheck)
     (use-package lsp-mode
    :diminish
    :hook  (lsp-mode . lsp-enable-which-key-integration))
    (use-package lsp-ui
     :diminish)

    (add-hook 'LaTeX-mode-hook 'lsp-mode)
    (add-hook 'LaTeX-mode-hook 'electric-pair-mode)
    (diminish 'flycheck-mode)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (general-define-key
:keymaps 'LaTeX-mode-map
"C-<return>" 'LaTeX-insert-item)

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

(use-package citar
      :custom
      (citar-bibliography '("~/projects/writing/templates/refs.bib")))
  (use-package citar-denote
  :diminish
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

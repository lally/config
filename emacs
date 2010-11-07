;;
;; Some machine-generated customizations at top.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fringe-mode 0 nil (fringe))
 '(inhibit-startup-screen t)
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/jdk/instances/jdk1.6.0"))))
 '(org-agenda-files (list "/research/phd/planning.org" "/research/phd/torque/modeling.org" "~/public_html/blog.org" "/research/phd/researchdef/writing.org" "~/Work/School/DVEs/dve_course.org" "/research/phd/pubs.org"))
 '(org-enable-table-editor (quote optimized))
 '(org-export-latex-classes (quote (("vgtc" "\\documentclass{vgtc}
\\usepackage{mathptmx}
\\usepackage{graphicx}
\\usepackage{times}
\\onlineid{0}
\\vgtccategory{Research}
\\vgtcinsertpkg" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("article" "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt,a4paper]{report}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt,a4paper]{book}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-hide-leading-stars t)
 '(pr-print-using-ghostscript t)
 '(safe-local-variable-values (quote ((py-indent-offset . 4) (TeX-master . "../index") (TeX-master . t)))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(font-lock-builtin-face ((t (:foreground "ForestGreen" :background "Black"))))
 '(font-lock-comment-face ((t (:foreground "Yellow" :background "Black"))))
 '(font-lock-constant-face ((t (:foreground "LightGray" :background "Black"))))
 '(font-lock-default-face ((t (:foreground "White" :background "Black"))))
 '(font-lock-function-name-face ((t (:foreground "Red" :background "Black"))))
 '(font-lock-keyword-face ((t (:foreground "Gold" :background "Black"))))
 '(font-lock-negation-char-face ((t (:weight extra-bold))))
 '(font-lock-string-face ((t (:foreground "LimeGreen" :background "Black"))))
 '(font-lock-type-face ((t (:foreground "Red" :background "Black"))))
 '(font-lock-variable-name-face ((t (:foreground "SteelBlue" :background "Black"))))
 '(font-lock-warning-face ((t (:foreground "White" :background "Firebrick"))))
 '(region ((nil (:background "dark olive green"))))
 '(twit-author-face ((t (:weight bold :height 0.8 :family "fixed"))))
 '(twit-message-face ((default (:height 0.8 :family "fixed")) (nil nil)))
 '(twit-title-face ((t (:underline "DeepSkyBlue"))))
 '(twit-zebra-1-face ((t (:background "black"))))
 '(twit-zebra-2-face ((t (:background "dark olive green")))))

;;----------------------------------------------------------------------
;; GLOBAL Settings
;;----------------------------------------------------------------------

;; Always enable server mode
(server-mode 't)
(tool-bar-mode nil)

;; Fix the UI: scrollbars.
;(setq scroll-bar-mode-explicit t)
(setq comint-prompt-read-only t)
(set-scroll-bar-mode `nil)
;; colors
(set-foreground-color "white")
(set-background-color "black")
;; font
;(set-default-font
; "-*-profont-medium-r-normal--11-110-72-72-c-60-iso8859-1")
 ;(set-default-font
 ; "-misc-dejavu sans mono-medium-r-normal--10-100-72-72-m-60-iso8859-1")
; turn off blinking and beeping
(setq ring-bell-function 'ignore)


;; Emacs base bindings
(global-set-key "\r" 'newline-and-indent)
(global-set-key [f21] 'keyboard-quit)
(global-set-key [f22] 'goto-line)
(global-set-key "\C-\\" 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "M-k") 'manual-entry)

;; forward & backward window controls
(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

;(global-set-key [SunF36] 'other-window-backward)
;(global-set-key [SunF37] 'other-window)

;; Just undo a filled region, in case I have to let some other program
;; refill it later.
;; Note to self: apparently there's a longlines-mode which may
;; obviate the need for this.
(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs, 
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(set-face-background 'modeline          "grey30")
(set-face-background 'modeline-inactive "grey0")
(set-cursor-color "white")
(set-mouse-color "white")

(defun setup-frame (frame)
  (set-face-background 'modeline          "grey30")
  (set-face-background 'modeline-inactive "grey0")
  (set-face-background 'region "dark slate gray")
  (set-cursor-color "white")
  (set-mouse-color "white")
  (modify-frame-parameters frame '(
		   (background-color . "black")
		   (foreground-color . "white")
;		   (font . "-*-profont-medium-r-normal--12-120-72-72-c-60-iso8859-1")
		   )
	   )
  )
(add-hook 'after-make-frame-functions 'setup-frame)

;; Aliases.
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)
(defalias 'cr 'comment-region)
(defalias 'er 'eval-region)

;;----------------------------------------------------------------------
;; DTRACE support.
;;----------------------------------------------------------------------

(autoload 'd-mode "d-mode" () t)
(add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode))
(add-hook 'd-mode-hook 'imenu-add-menubar-index)
(add-hook 'd-mode-hook 'font-lock-mode)


;;----------------------------------------------------------------------
;; ORG MODE SETUP
;;----------------------------------------------------------------------

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-font-lock-mode 1)                     ; for all buffers
;(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
   (lambda()
     (turn-on-font-lock)
     (turn-on-auto-fill)))

; Setup some Org-mode agenda files.
(setq org-agenda-files (list "/research/phd/planning.org"
			     "/research/phd/torque/modeling.org"
		             "~/public_html/blog.org" 
			     "/research/phd/researchdef/writing.org"
			     "/research/phd/pubs.org"))

;;----------------------------------------------------------------------
;; LATEX
;;----------------------------------------------------------------------

(add-hook 'LaTeX-mode-hook
   (lambda()
     (turn-on-font-lock)
     (turn-on-auto-fill)))

;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
(global-set-key [(control \})] 'tex-close-latex-block)

;;---------------------------------------------------------------------- 
;; Haskell mode
;;----------------------------------------------------------------------

(load "~/local/haskell-mode-2.4/haskell-site-file")
(load "~/local/share/emacs/site-lisp/twit.el")
(add-hook 'haskell-mode-hook
	  (lambda()
	    (turn-on-haskell-doc-mode t)
	    (turn-on-haskell-simple-indent t)
	    (turn-on-font-lock t)
	    (imenu-add-menubar-index t)))

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;(add-hook 'haskell-mode-hook 'font-lock-mode)
;(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;----------------------------------------------------------------------
;; C/C++ Mode
;;----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (local-set-key (kbd "<f23>") 'hs-toggle-hiding)
    (set-variable 'tab-width 4)
    (set-variable 'c-basic-offset 4)
    (turn-on-auto-fill)
    (hs-minor-mode t)
    (flyspell-prog-mode)
    ))


;;----------------------------------------------------------------------
;; CEDET, for the Java dev environment.
;;----------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "/opt/local/share/emacs/site-lisp/jde/lisp"))
(add-to-list 'load-path (expand-file-name "/opt/local/share/emacs/site-lisp/elib"))
(load-file (expand-file-name "/opt/local/share/emacs/site-lisp/magit.el"))
(load-file (expand-file-name "~/.emacs.d/plugins/psvn.el"))

;; Load CEDET
(load-file "/opt/local/share/emacs/site-lisp/cedet/common/cedet.el")

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; * This turns on which-func support (Plus all other code helpers)
;;(semantic-load-enable-minimum-features)
;(semantic-load-enable-code-helpers)
;(semantic-load-enable-guady-code-helpers)
(semantic-load-enable-excessive-code-helpers)

;; enable JDE. 
;(require 'jde)
(require 'magit)

;; IDO, for my enhanced buffer management.
(require 'ido)
(ido-mode t)

;; MAGIT, for GIT support.
(require 'magit)

(global-set-key [f5] 'magit-status)

;;; Emacs/W3 Configuration
(setq load-path (cons "/opt/emacs/share/emacs/site-lisp" load-path))
(condition-case () (require 'w3-auto "w3-auto") (error nil))

;; Midnight mode, a GC for unused buffers.
(require 'midnight)


;; Shove the function name at the top of the buffer, in the 'HeaderLine'
;;(load "which-func")
;;(which-func-mode 1)

;;(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;;(setq which-func-header-line-format
;;              '(which-func-mode
;;                ("" which-func-format
;;                 )))
;(defadvice which-func-ff-hook (after header-line activate)
;  (when which-func-mode
;    (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;    (setq header-line-format which-func-header-line-format)))

;;==================================================================
;; Fullscreen support
;;==================================================================

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(global-set-key [SunF36] 'toggle-fullscreen)
(global-set-key [SunF37] 'menu-bar-mode)

(set-variable 'mouse-autoselect-window 1)
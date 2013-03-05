; (package-init)
(defvar *aquamacs-p* (boundp 'aquamacs-version))
(require 'haskell-mode)
(load-library "hideshow")
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(when *aquamacs-p*
  (progn
    (osx-key-mode -1) 
    (unless window-system   ;; in TTY (terminal) modee
      (normal-erase-is-backspace-mode nil)
      (set-face-inverse-video-p 'mode-line-inactive t)
      (define-key osx-key-mode-map "\C-z" 'suspend-emacs))
 
    (setq
     ns-command-modifier 'meta         ; Apple/Command key is Meta
     ns-alternate-modifier nil         ; Option is the Mac Option key
     ns-use-mac-modifier-symbols  nil  ; display standard Emacs (and not standard Mac) modifier symbols)
     )
 
    ;; Persistency and modes:
    (setq
     initial-major-mode 'emacs-lisp-mode              ; *scratch* shows up in emacs-lisp-mode
     ;; aquamacs-default-major-mode 'emacs-lisp-mode  ; new buffers open in emacs-lisp-mode
     )
 
    ; Frame and window management:
    (ido-mode 1)
    (tabbar-mode -1)		     ; no tabbar
    (one-buffer-one-frame-mode -1)       ; no one-buffer-per-frame
    (setq special-display-regexps nil)   ; do not open certain buffers in special windows/frames
    ; (tool-bar-mode 0) ; turn off toolbar
    ; (scroll-bar-mode -1)  ; no scrollbars
    ;; Appearance
    (aquamacs-autoface-mode -1) ; no mode-specific faces, everything in Monaco
    )
)

;; Google-specific configuration
(when (file-accessible-directory-p "/home/build")
  (progn 
    (load-file "/home/build/public/eng/elisp/google.el")
	 ;Some extra local packages are available that are not included by
	 ;google.el by default. You need to require those modules explicitly if
	 ;you want their functionality. These include:

    (require 'p4-google)                ;; g4-annotate, improves find-file-at-point
    (require 'compilation-colorization) ;; colorizes output of (i)grep
    (require 'rotate-clients)           ;; google-rotate-client
    (require 'rotate-among-files)       ;; google-rotate-among-files
    (require 'googlemenu)               ;; handy Google menu bar
    (require 'google-java)              ;; fast Java compilation code
    (require 'p4-files)                 ;; transparent support for
    ;; Perforce filesystem
    (require 'google3)                  ;; magically set paths for
    ;; compiling google3 code
    (require 'gsearch)                  ;; Search the whole Google code base.
    (require 'googlemenu)
    (add-to-list 'load-path "/usr/local/google/share/emacs/site-lisp")
    (p4-enable-file-name-handler)
    (global-set-key [f12] 'google-compile)
    (setq p4-use-p4config-exclusively t)
    (load-file "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.elc")
    ;(global-set-key [?\M-.] 'gtags-show-tag-locations)
    (global-set-key [M-*] 'gtags-pop-tag)
    ;;----------------------------------------------------------------------
    ;; CEDET, for the Java dev environment.
    ;;----------------------------------------------------------------------

    (add-to-list 'load-path (expand-file-name "/opt/local/share/emacs/site-lisp/jde/lisp"))
    (add-to-list 'load-path (expand-file-name "/opt/local/share/emacs/site-lisp/elib"))
    (load-file (expand-file-name "/opt/local/share/emacs/site-lisp/magit.el"))
    (load-file (expand-file-name "~/.emacs.d/plugins/psvn.el"))

    ;; Load CEDET
    (load-file "/opt/local/share/emacs/site-lisp/cedet/common/cedet.el")
;;; Emacs/W3 Configuration
    (setq load-path (cons "/opt/emacs/share/emacs/site-lisp" load-path))
    (condition-case () (require 'w3-auto "w3-auto") (error nil))
    ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
    ;; * This turns on which-func support (Plus all other code helpers)
;    (semantic-load-enable-minimum-features)
                                        ;(semantic-load-enable-code-helpers)
                                        ;(semantic-load-enable-guady-code-helpers)
;    (semantic-load-enable-excessive-code-helpers)
                                        ;(semantic-load-enable-gaudy-code-helpers)
))
;;
;; END HOST-SPECIFIC MODIFICATIONS
(global-set-key (kbd "C-M-<left>")  'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "<XF86Forward>")  'windmove-right)
(global-set-key (kbd "<XF86Back>") 'windmove-left)
(global-set-key (kbd "C-M-<up>")    'windmove-up)
(global-set-key (kbd "C-M-<down>")  'windmove-down)


(require 'cedet)
(require 'semantic)
;;
;; When the site-libs are present
(when (file-accessible-directory-p "~/config/libs")
  (progn
    (add-to-list 'load-path "~/config/libs/magit-0.8.2")))


;; IDO, for my enhanced buffer management.
(require 'ido)
(ido-mode t)
(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and
                                        ; smart completion
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; MAGIT, for GIT support.
(require 'magit)

(global-set-key [f5] 'magit-status)

;;---------------------------------------------------------------------- 
;; Haskell mode
;;----------------------------------------------------------------------

;(load "~/local/haskell-mode-2.4/haskell-site-file")
;(load "~/local/share/emacs/site-lisp/twit.el")
(require 'inf-haskell)
(add-hook 'haskell-mode-hook
          (lambda()
            (turn-on-haskell-doc-mode t)
            (turn-on-haskell-simple-indent t)
            (turn-on-font-lock t)
            (imenu-add-menubar-index t)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width condensed :foundry "unknown" :family "Anka/Coder Narrow")))))



;; Locally added - http://www.corp.google.com/eng/google_emacs.html
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/config/libs/site-lisp")

(require 'dired-details+)
(require 'dired-x)
(require 'column-marker)
(require 'fic-mode)
(require 'buff-menu+)
(require 'langtool)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(setq langtool-language-tool-jar "/usr/local/share/languagetool-1.6/LanguageTool.jar")
(setq tramp-default-method "ssh")

;(require 'light-symbol)
(fringe-mode 'minimal)
(add-hook 'c++-mode-hook 'turn-on-fic-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)
(defun my-py-mode-hook()
  (interactive)
  (c-subword-mode 1)
  (turn-on-fic-mode)
  (hs-minor-mode)
  (column-number-mode 1)
  (column-marker-1 79)
)
(add-hook 'py-mode-hook 'my-py-mode-hook)

;(add-hook 'python-mode-hook 'turn-on-fic-mode)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq bol "." (not (any "."))) ;; dot-files
              (seq "~" eol)                 ;; backup-files
              (seq bol "CVS" eol)           ;; CVS dirs
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(column-marker-1 78)

(server-start)
(display-time-mode)
;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)
(set-variable 'comint-prompt-read-only 't)

(setq cc-other-file-alist
      `(("\\cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".cc"))
        ("\\.hpp$" (".c" ".cpp" ".cc"))))

(defun other-window-backwards ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-x O") 'other-window-backwards)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General Window Setup
;;

;
; Normal configuration stuff
(scroll-bar-mode 'nil)
(tool-bar-mode 'nil)
(transient-mark-mode t)

(ido-mode t)
(set-fringe-mode '(1 . 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(color-theme-selection "Black" nil (color-theme))
 '(column-number-mode t)
 '(display-time-mode t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(org-capture-templates (quote (("p" "Python" entry (file "~/config/notes.org") "py: ") ("t" "TODO" entry (file "~/config/notes.org") "TODO "))))
 '(tool-bar-mode nil))

(set-background-color "black")
(set-foreground-color "white")
(set-face-background 'region "midnight blue")
(set-face-background 'highlight "grey15")
;(global-set-key [?\M-.] 'gtags-feeling-lucky)

;(semantic-enable-gaudy-code-helpers)


;; Yummy, from: http://stringofbits.net/2009/08/emacs-23-dbus-and-libnotify/
(require 'dbus)
(defun send-desktop-notification (summary body timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0
    "/usr/share/icons/hicolor/scalable/apps/emacs23.svg"
    summary
    body
    '(:array)
    '(:array :signature "{sv}")
    ':int32 timeout))

(defun pw/compile-notify (buffer message)
  (send-desktop-notification "emacs compile" message 30000))

(setq compilation-finish-function 'pw/compile-notify)
;(require 'icicles)
;(require 'fuzzy-match)
;(icy-mode 1)
(put 'narrow-to-region 'disabled nil)
;(set-variable 'icicle-show-Completions-initially t)
; (set-variable 'mouse-autoselect-window nil)
;(icicle-ido-like-mode 1)
(setq-default ido-default-file-method 'selected-window)
(setq-default display-buffer-reuse-frames 1)
(set-fill-column 79)
(column-marker-1 79)
(global-font-lock-mode 1)

;
; GIT SETUP
; https://wiki.corp.google.com/twiki/bin/view/Nonconf/GitAndEmacs
(global-auto-revert-mode)
(global-set-key [S-f12] 'magit-status)


;
; ORG MODE SETUP
;
(require 'org-install)
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
     (flyspell-mode 1)
     (turn-on-font-lock)
     (turn-on-auto-fill)))

;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
(global-set-key [(control \})] 'tex-close-latex-block)


;;----------------------------------------------------------------------
;; C/C++ Mode
;;----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
  (lambda()
    (set-variable 'tab-width 4)
    (set-variable 'c-basic-offset 4)
    (turn-on-auto-fill)
    (hs-minor-mode t)
    (flyspell-prog-mode)
    ))



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


(set-variable 'mouse-autoselect-window 1)


;; ORG Capture
(setq org-directory "~/config")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-remember)

(setq ido-max-directory-size 100000)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ct" 'org-time-stamp)
(set-variable 'org-hide-leading-stars t)
(setq org-default-notes-file "~/config/notes.org")
(define-key global-map "\C-cr" 'org-capture) ; 'r' for remember.
(global-set-key "\M-\C-\\" 'comment-region)
(global-set-key [C-tab] 'indent-region)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#330")

; g13-support

(defun filename-of-path (n)
  (last (split-string n "/" 't)))

(global-set-key "\C-\M-g" 'goto-line)

;(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
;(load "/usr/share/emacs/site-lisp/ess/ess-site")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(set-face-foreground 'mode-line-inactive "turquoise1")
(set-face-background 'mode-line-inactive "black")
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "turquoise1")
(defalias 'rs 'replace-string)

;;
;; Automatic Mode Selection
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(put 'set-goal-column 'disabled nil)

; (add-to-list 'package-archives
;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you enable git-gutter-mode for some modes
(add-hook 'LaTeX-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x r") 'git-gutter:revert-hunk)

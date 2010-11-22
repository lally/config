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

;; Locally added - http://www.corp.google.com/eng/google_emacs.html
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/icicles")
(add-to-list 'load-path "/usr/local/google/share/emacs/site-lisp")
(require 'googlemenu)
(require 'dired-details+)
(require 'dired-x)
(require 'column-marker)
(require 'fic-mode)
(require 'magit)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(load-file "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.elc")
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
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and
                                         ; smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu

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

(p4-enable-file-name-handler)
(server-start)
(display-time-mode)
(global-set-key [f12] 'google-compile)
;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)
(set-variable 'comint-prompt-read-only 't)

(setq cc-other-file-alist
      `(("\\cpp$" (".hpp" ".h"))
        ("\\.h$" (".c" ".cpp" ".cc"))
        ("\\.hpp$" (".c" ".cpp" ".cc"))))

(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General Window Setup
;;

;
; Normal configuration stuff
(scroll-bar-mode 'nil)
(tool-bar-mode 'nil)
(transient-mark-mode t)

(setq p4-use-p4config-exclusively t)
(ido-mode t)
(set-fringe-mode '(1 . 1))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(color-theme-selection "Black" nil (color-theme))
 '(display-time-mode t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window) t)
 '(menu-bar-mode t)
 ;; C-cX for X in [p,t] will enable the right template.
 '(org-capture-templates
   (quote (("p" "Python" entry (file "~/config/notes.org") "py: ")
           ("t" "TODO" entry (file "~/config/notes.org") "TODO ")))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black"
                :foreground "white" :inverse-video nil :box nil
                :strike-through nil :overline nil :underline nil
                :slant normal :weight normal :height 69 :width normal
                :foundry "unknown" :family "Droid Sans Mono")))))

(set-background-color "black")
(set-foreground-color "white")
(set-face-background 'region "midnight blue")
;(global-set-key [?\M-.] 'gtags-feeling-lucky)
(global-set-key [?\M-.] 'gtags-show-tag-locations)
(global-set-key [M-*] 'gtags-pop-tag)

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
(semantic-load-enable-gaudy-code-helpers)
;(require 'icicles)
;(require 'fuzzy-match)
;(icy-mode 1)
(put 'narrow-to-region 'disabled nil)
;(set-variable 'icicle-show-Completions-initially t)
(set-variable 'mouse-autoselect-window t)
;(icicle-ido-like-mode 1)
(setq-default ido-default-file-method 'selected-window)
(setq-default display-buffer-reuse-frames 1)
(set-fill-column 77)
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
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
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

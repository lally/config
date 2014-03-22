;;============================================================
;; AUTOSAVE CONFIGURATION
;;============================================================
;; LS: this is to prevent slow-ass NFS calls.  This is up front to prevent
;; the performance going to hell if another part of the file breaks.
;;
;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Another attempt to speed up emacs.
(setq vc-handled-backends nil)

(defun xauth-command (&rest args)
  "Calls xauth(1) with the commandline arguments ARGS, returning a
string containing the output"
  (with-output-to-string
    (with-current-buffer
   standard-output
      (apply 'call-process "xauth" nil t nil args))))

(defun xauth-list (&optional authority-file)
  "Returns a list of the current X authority tokens, each element of
the form (display key-protocol hex-string)"
  (mapcar 'split-string
     ;; eliminate blank lines
     (remove-if (lambda (s) (zerop (length s)))
           (split-string (apply 'xauth-command
                 (append (if authority-file
                        (list
                         "-f"
                         authority-file
                         "-i")
                      nil)
                    (list "list")))
               "[\n]"))))

(defun xauth-add (display key-protocol hex-string &optional authority-file)
  "Adds an X authority token to the data base."
  (apply 'xauth-command (append (if authority-file
                (list "-f" authority-file)
              nil)
            (list "add" display key-protocol hex-string))))


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

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 81 :width condensed :foundry "unknown" :family "Anka/Coder Narrow"))))
 '(ebrowse-root-class ((((min-colors 88)) (:foreground "white" :weight bold)))))


;; ALIASES
(defalias 'rs 'replace-string)
(defalias 'ar 'align-regexp)
(put 'scroll-left 'disabled nil)

;; GDB Setup
(setq gdb-command-name "gdb --nx")

(if (file-exists-p "/home/build")
    (progn
      ;;============================================================
      ;; GOOGLE SETUP
      ;;============================================================
      (load-file "/home/build/public/eng/elisp/google.el")
                                        ;Some extra local packages are available that are not included by
                                        ;google.el by default. You need to require those modules explicitly if
                                        ;you want their functionality. These include:
      (require 'google)
      (require 'compilation-colorization) ;; colorizes output of (i)grep
      (require 'rotate-clients)           ;; google-rotate-client
      (require 'rotate-among-files)       ;; google-rotate-among-files
      (require 'googlemenu)               ;; handy Google menu bar
      (require 'google3)                  ;; magically set paths for
      ;; compiling google3 code
      (require 'google-imports) ;; M-x google-imports-iwyu
      (load-library "gm-prepare")
      (require 'protobuf-mode)            ;; protocol buffers support
      )
)

;; Locally added - http://www.corp.google.com/eng/google_emacs.html
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/icicles")
; (add-to-list 'load-path "~/.emacs.d/site-lisp/org/lisp")
; (add-to-list 'load-path "~/.emacs.d/site-lisp/org/contrib/lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/local/share/emacs23/site-lisp")
(if (file-exists-p "/usr/local/google")
    (add-to-list 'load-path "/usr/local/google/share/emacs/site-lisp")
)
;; Stuff in config/libs/*
(add-to-list 'load-path "~/config/libs/site-lisp")
(add-to-list 'load-path "~/config/libs/site-lisp/haskell-mode")
;; magit now ships with emacs.
;(add-to-list 'load-path "~/config/libs/site-lisp/magit-0.8.2")
(require 'dired-details+)
(require 'dired-x)
(require 'column-marker)
(require 'fic-mode)
(require 'magit)
(require 'buff-menu+)
;(require 'dbgr)
(require 'vline)
(require 'org-install)
(require 'org-habit)
(require 'org-protocol)
(require 'haskell-mode)


(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

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
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


(if (file-exists-p "/usr/local/google/src/cedet-1.1/common/cedet.el")
    (load-file "/usr/local/google/src/cedet-1.1/common/cedet.el")
	;;============================================================
	;; SEMANTICDB SETUP
	;;============================================================
	(global-ede-mode 1)                      ; Enable the Project management system
	;(semantic-load-enable-gaudy-code-helpers)      ; Enable prototype help and
	;                                               ; smart completion
	(setq semantic-stickyfunc-mode 1)
	(setq semantic-decoration-mode 1)
	(setq semantic-idle-completion-mode nil)
	;(global-srecode-minor-mode 1)            ; Enable template insertion menu

	;(add-hook 'python-mode-hook 'turn-on-fic-mode)

	(require 'semantic/ia)
	; (require 'semantic/gcc)
	(require 'semantic/db)
	(global-semanticdb-minor-mode 1)
	(semanticdb-enable-gnu-global-databases 'c-mode)
	(semanticdb-enable-gnu-global-databases 'c++-mode)
	;; Remove semanticdb-save-all-db-idle from the auto-save-hook.  It looks
	;; to be my stalling problem.  I blame NFS.
	(remove-hook 'auto-save-hook 'semanticdb-save-all-db-idle)
	(set-variable 'semantic-idle-scheduler-max-buffer-size 4096) ; 4k max buffer to reparse
)

;(require 'light-symbol)
;(fringe-mode "left-only")
(add-hook 'c++-mode-hook 'turn-on-fic-mode)
(fringe-mode '(16 . 0))

(setq ido-max-directory-size 100000) ;; in _bytes_, not dirents.
(setq gtags-use-gtags-mixer nil)

;;============================================================
;; Custom Editing Functions
;;============================================================

;; http://www.emacswiki.org/emacs/IncrementNumber
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

;; https://github.com/benma/emacs.d/blob/a22f73ee26473bf94775f04c3f969523f6bbb145/init.el#L333
(defun whack-whitespace ()
  "Delete all white space from point to the next word."
  (interactive nil)
  (when (re-search-forward "[ \t\n]+" nil t)
    (replace-match "" nil nil)))

(defun reverse-other-window ()
  (interactive)
  (other-window -1))


;;============================================================
;; C++ SETUP
;;============================================================
(defun local-cpp-mode-hook()
  (interactive)
  (set-fill-column 77)
  (turn-on-fic-mode)
  (hs-minor-mode 1)
  (column-number-mode 1)
  (column-marker-1 79)
  (fringe-mode 'left-only)
  (linum-mode)
  (flyspell-prog-mode)
;
; Ooooh, this is nice, but I may get tired of it.
;
;  (glasses-mode 1)
)

(defun local-borg-mode-hook()
  (interactive)
  (hs-minor-mode 1)
)

(defun local-latex-mode-hook()
  (interactive)
  (set-fill-column 79)
  (flyspell-mode 1)
  (auto-fill-mode 1))


(add-hook 'c++-mode-hook 'local-cpp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)
(add-hook 'borg-mode-hook 'local-borg-mode-hook)
(add-hook 'latex-mode-hook 'local-latex-mode-hook)
;;============================================================
;; PYTHON SETUP
;============================================================
(defun my-py-mode-hook()
  (interactive)
;  (c-subword-mode 1)
  (turn-on-fic-mode)
  (hs-minor-mode 1)
  (column-number-mode 1)
  (column-marker-1 79)
  (flyspell-prog-mode)
  (orgtbl-mode)
)
(add-hook 'python-mode-hook 'my-py-mode-hook)
(set-variable 'python-indent 2)
; from: /home/build/nonconf/google3/third_party/py/pylint/elisp/pylint.el
(require 'compile)
;(grok-init)

;; adapted from pychecker for pylint
(defun pylint-python-hook ()
  (interactive)
  (defun pylint ()
    "Run pylint against the file behind the current buffer after
    checking if unsaved buffers should be saved."

    (interactive)
    (let* ((file (buffer-file-name (current-buffer)))
       (command (concat "gpylint --output-format=parseable \"" file "\"")))
      (save-some-buffers (not compilation-ask-about-save) nil) ; save  files.
      (compile-internal command "No more errors or warnings" "pylint")))
  (local-set-key [f1] 'pylint)
  )

(add-hook 'python-mode-hook 'pylint-python-hook)
;(global-set-key (kbd "C-M-,") gtags-show-callers)



;;
;; TODO(lally): Run through ~/gitwork and invoke this for all members.
;; I can use DESCRIPTION for :name.
;;
;(ede-cpp-root-project "qsi-cache-threadscape"
;                      :name "Cache and Threadscape Stats"
;                      :file "~/gitwork/qsi-cache-threadscape/google3/mustang/BUILD"
;)
;; See http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html for details
;; on what's in here.


;(setq-mode-local c-mode semanticdb-find-default-throttle
;                 '(project unloaded system recursive))



(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;============================================================
;; DIRED SETUP
;;============================================================
(add-hook 'dired-load-hook (lambda ()
                             (progn
                               (require 'dired-sort-menu)
                               (require 'dired-sort-menu+)
                               (require 'dired-sort-map))))
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

;;============================================================
;; LINT, COMPILE AND TEST
;;============================================================
(defun lint-cl ()
  (interactive) (compile "git5 --no-pager lint -d -v"))
(defun get-cl-comments ()
  (interactive) (compile "git5 comments -q"))
(set-fill-column 77)
(server-start)
(display-time-mode)


(set-variable 'comint-prompt-read-only 't)

(setq-default cc-other-file-alist
              '(
                ("\\.cc$"  (".hh" ".h"))
                ("\\.hh$"  (".cc" ".C"))

                ("\\.c$"   (".h"))
                ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

                ("\\.C$"   (".H"  ".hh" ".h"))
                ("\\.H$"   (".C"  ".CC"))

                ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
                ("\\.HH$"  (".CC"))

                ("\\.cxx$" (".hh" ".h"))
                ("\\.cpp$" (".hpp" ".hh" ".h"))
                ("\\.hpp$" (".cpp"))
                ))


;;============================================================
;; GENERAL WINDOW SETUP
;;============================================================

;
; Normal configuration stuff
(scroll-bar-mode -1)
(tool-bar-mode 'nil)
(transient-mark-mode t)

;(setq p4-use-p4config-exclusively t)
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
 '(gdb-find-source-frame t)
 '(gdb-many-windows t)
 '(gdb-show-changed-values t)
 '(gdb-speedbar-auto-raise t)
 '(haskell-program-name "cabal-dev ghci")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/org/toplevel/habits.org" "~/org/toplevel/incoming.org" "~/org/toplevel/learning.org" "~/org/toplevel/monitoring.org" "~/org/toplevel/optimization.org" "~/org/toplevel/pending.org" "~/org/toplevel/research.org" "~/org/toplevel/unsorted.org" "~/org/project/onegig.org" "~/org/project/scoreboard.org" "~/org/project/thwack.org" "~/org/project/twenty.org")))
 '(org-enforce-todo-dependencies t)
 '(org-modules (quote (org-bbdb org-bibtex org-crypt org-ctags org-docview org-id org-jsinfo org-habit org-inlinetask org-irc org-w3m org-mouse org-git-link org-learn org-panel)))
 '(safe-local-variable-values (quote ((org-use-property-inheritance . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(defun my-window-setup-hook (frame)
  "Set window parameters, for those that don't seem to stick."
  (set-fill-column 79)
  (set-default-font
   "-unknown-Anka/Coder Narrow-normal-normal-condensed-*-*-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :height 93)
  (set-background-color "black")
  (set-foreground-color "white")
  (set-cursor-color "white")
  (set-face-background 'region "midnight blue")
  (show-paren-mode 1)
  ; (set-face-background 'hl-line "#330")
  (global-hl-line-mode 1)
  )
(add-hook 'after-make-frame-functions 'my-window-setup-hook)
(my-window-setup-hook nil)

(put 'narrow-to-region 'disabled nil)
(set-variable 'mouse-autoselect-window nil)
(setq-default ido-default-file-method 'selected-window)
(setq-default display-buffer-reuse-frames 1)
(set-fill-column 79)
(column-marker-1 81)
(global-font-lock-mode 1)
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")
(set-face-background 'region "midnight blue")
(show-paren-mode 1)
(set-face-background 'hl-line "#330")
(global-hl-line-mode 1)

;(require 'icicles)
;(require 'fuzzy-match)
;(icy-mode 1)
;(set-variable 'icicle-show-Completions-initially t)
;(icicle-ido-like-mode 1)

;;============================================================
;; COMPILATION SETUP
;;============================================================
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

;;============================================================
;; GIT SETUP
;;============================================================
; https://wiki.corp.google.com/twiki/bin/view/Nonconf/GitAndEmacs
;(global-auto-revert-mode)


;;============================================================
;; ORG MODE SETUP
;;============================================================
;(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;;-------------------------
;; This should be per-file!
;;-------------------------
;; Use      #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@) (as an example)
;; (setq org-todo-keywords
;;       '((sequence "WAITING(w)" "OPTIONAL(o)" "BEGIN(b)" "UGLY(u)"a
;;                   "CODEREVIEW(r)" "UPDATE(u)" "PERIPHERAL(p)" "TESTING(t)"
;;                   "TOSUBMIT(s)" "DONE(d)")))

(set-variable 'org-hide-leading-stars t)

;; ORG CAPTURE
;; -----------
(setq org-default-notes-file "~/org/unsorted.org")
(define-key global-map "\C-cr" 'org-capture) ; 'r' for remember.
(setq org-protocol-default-template-key "l")
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/unsorted.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("l" "Link" entry (file+olp "~/org/intel/unsorted.org" "Web Links")
        "* %a\n %?\n %i")))


;; ORG MOBILE
;; ----------
(setq org-mobile-directory "/scpc:lally@lal.ly:/home/lally/org")
(setq org-log-into-drawer t)

;; ORG AGENDA
;; ----------
(setq org-agenda-files (nconc (file-expand-wildcards "~/org/toplevel/*.org")
                              (file-expand-wildcards "~/org/project/*.org")))
(setq org-agenda-time-grid
      '((weekly today require-timed remove-match)
        "----------------"
        (0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359))
      )
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down) ;; agenda should ignore category
        (todo category-keep priority-down)
        (tags category-keep priority-down)
        (search category-keep))
      )
(setq org-agenda-to-appt t) ;; add appointments to the agenda view
(setq org-agenda-window-setup 'current-window) ;; don't kill my window setup
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . nil) (C . t) (R . t)))
; (setq org-src-fontify-natively t)

;; ORG EDIT HOOK
;; -------------
(defun my-org-hook ()
  (interactive)
  (auto-fill-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1)
  (column-number-mode 1)
  (column-marker-1 79)
)

(add-hook 'org-mode-hook 'my-org-hook)

;; ORG TEXT EXPORT
;; ----------------
;; Ref: http://orgmode.org/worg/org-contrib/org-export-generic.html
;; NOTE: This doesn't work.
;; (org-set-generic-type
;;  "header-file-documentation"
;;  '(:file-suffix     ".h"
;;    :key-binding     ?D
;;    :title-prefix    "//"
;;    :title-suffix    "?="
;;    :body-header-section-numbers nil
;;    :body-section-header-prefix  "//\n"
;;    :body-section-header-suffix ("?=" "?-")
;;    :body-line-format "//  %s\n"
;;    :body-line-wrap   75
;;    ))


;;============================================================
;; g13-support
;;============================================================
(defun filename-of-path (n)
  (last (split-string n "/" 't)))

;;============================================================
;; EXPERIMENTAL
;;============================================================
;;(load-file "/home/build/eng/elisp/gfs.el")
;;(gfs-enable-file-name-handler)

;;  Keep these at the end, so that any failures here don't propagate
;;  to other preferences above.


(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")
(set-face-background 'region "midnight blue")

;; Package-Manager stuff, Emacs 24+ only
(if (> emacs-major-version 24)
    (progn
	(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
				 ("marmalade" . "http://marmalade-repo.org/packages/")
				 ("melpa" . "http://melpa.milkbox.net/packages/")))
	(package-initialize)
	(require 'git-gutter)
	;;; This was installed by package-install.el.
	;;; This provides support for the package system and
	;;; interfacing with ELPA, the package archive.
	;;; Move this code earlier if you want to reference
	;;; packages in your .emacs.
	(require 'package)
	;; Any add to list for package-archives (to add marmalade or melpa) goes here
	(add-to-list 'package-archives
	    '("marmalade" .
	      "http://marmalade-repo.org/packages/"))
	(package-initialize)
))
;;============================================================
;; KEYBINDINGS
;;============================================================

(global-set-key (kbd "C-x O") 'reverse-other-window)
(global-set-key "\C-\M-g" 'goto-line)

(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<left>") 'windmove-left)

(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "C-x g l") 'lint-cl)
(global-set-key (kbd "C-x g c") 'get-cl-comments)

(global-set-key [f2] 'previous-error)
(global-set-key [f3] 'next-error)
(global-set-key [f12] 'compile)
(global-set-key [S-f12] 'magit-status)

;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)
(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key (kbd "C-c o") 'ff-find-other-file)

(global-set-key (kbd "C-c i") 'my-increment-number-decimal)
(global-set-key "\M-\C-\\" 'comment-region)
(global-set-key [C-tab] 'indent-region)
(global-set-key [?\M-.] 'gtags-feeling-lucky)
(global-set-key [?\M-.] 'gtags-show-tag-locations)
(global-set-key [?\M-*] 'gtags-pop-tag)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ct" 'org-time-stamp)

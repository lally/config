;; -*-  mode: Lisp; eval: (orgstruct++-mode); eval: (setq orgstruct-heading-prefix-regexp ";; ");  -*-

;;============================================================
;; * AUTOSAVE CONFIGURATION
;;============================================================
;; LS: this is to prevent slow-ass NFS calls.  This is up front to prevent
;; the performance going to hell if another part of the file breaks.
;;
;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar autosave-dir (expand-file-name "~/tmp/emacs_autosaves/"))
(make-directory autosave-dir t)
(set-language-environment "UTF-8")

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

;;============================================================
;; * LOAD PATH SETUP
;;============================================================
;; ALL OF IT
;;

(if (file-exists-p "/usr/share/emacs24/site-lisp/emacs-google-config/devtools/editors/emacs/google.el")
;;"/home/build/public/eng/elisp/google.el")
    (progn
;;============================================================
;; ** GOOGLE SETUP
;;============================================================
      (load-file
       "/usr/share/emacs24/site-lisp/emacs-google-config/devtools/editors/emacs/google.el")

      ;(load-file "/home/build/public/eng/elisp/google.el")
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
      ; (load-library "gm-prepare")
      (require 'protobuf-mode)            ;; protocol buffers support
      ))

;; Locally added - http://www.corp.google.com/eng/google_emacs.html
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(if (file-exists-p "/usr/local/google")
    (add-to-list 'load-path "/usr/local/google/share/emacs/site-lisp"))
;; Stuff in config/libs/*
(add-to-list 'load-path "~/config/libs/site-lisp")
;(if (file-exists-p "~/config/libs/site-lisp/haskell-mode")
;    (add-to-list 'load-path "~/config/libs/site-lisp/haskell-mode")
;)

;; Package-Manager stuff, Emacs 24+ only
(if (>= emacs-major-version 24)
    (progn
      (package-initialize)
	;;; This was installed by package-install.el.
	;;; This provides support for the package system and
	;;; interfacing with ELPA, the package archive.
	;;; Move this code earlier if you want to reference
	;;; packages in your .emacs.
      (require 'package)
      ;; Any add to list for package-archives (to add marmalade or melpa) goes here
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
      (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
      (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
      (package-initialize)
      (edit-server-start)
      )
  )


;;============================================================
;; * General package load-up
;;============================================================

(require 'column-marker)
(require 'fic-mode)
(require 'org-install)
(require 'org-habit)
(require 'org-protocol)
(require 'haskell-mode)
(require 'elisp-format)  ; in config/libs/site-lisp.
(require 'git-gutter-fringe+)


;; IDO, for my enhanced buffer management.
(require 'ido)
(ido-mode t)
(global-ede-mode 1)                      ; Enable the Project management system
(filesets-init)  ; Enable Filesets

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 68 :width normal :foundry "unknown" :family "PragmataPro"))))
 '(ebrowse-root-class ((((min-colors 88)) (:foreground "white" :weight bold))) t)
 '(mode-line ((t (:background "#212931" :foreground "#eeeeec" :box (:line-width -1 :style released-button) :height 1.0 :family "PragmataPro")))))


;; ALIASES
(defalias 'rs 'replace-string)
(defalias 'ar 'align-regexp)
(put 'scroll-left 'disabled nil)

;; GDB Setup
(setq gdb-command-name "gdb --i=mi2 --nx")
(setq gdb-create-source-file-list nil)

(defun my-gdb-mode ()
  (hl-line-mode 't)
  (fring-mode '(8 . 0)))

(add-hook 'gdb-mode-hook 'my-gdb-mode)

;; ** IRC Custom loads
(if (file-exists-p "~/config/libs/site-lisp/irc")
    (add-to-list 'load-path "~/config/libs/site-lisp/irc")
)

;;============================================================
;; * BBDB Setup
;;============================================================
;;; bbdb
(require 'bbdb)
(require 'bbdb-autoloads)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq
 bbdb-file "~/org/bbdb"
 bbdb-offer-save 'auto
 bbdb-notice-auto-save-file t
 bbdb-expand-mail-aliases t
 bbdb-canonicalize-redundant-nets-p t
 bbdb-always-add-addresses t
 bbdb-complete-name-allow-cycling t
 )


;;============================================================
;; * Mail Setup
;;============================================================
; (require 'auth-source)
; (require 'secrets)
; (setq secrets-enabled 't)
(setenv "EMAIL" "lally@google.com")
(setenv "NAME" "Lally Singh")
(setenv "SMTPSERVER" "smtp.gmail.com")
(require 'gnus)
(setq user-email-address "lally@google.com")
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)))
(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq-default
     gnus-summary-line-format "%U%R%z %(%-12,12&user-date;%16=|%-25,25f| %B%s%)\n"
;     gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
     gnus-thread-sort-functions '((not gnus-thread-sort-by-date))
     gnus-sum-thread-tree-false-root ""
     gnus-sum-thread-tree-indent " "
     gnus-sum-thread-tree-root ""
     gnus-sum-thread-tree-leaf-with-other "├► "
     gnus-sum-thread-tree-single-leaf     "╰► "
     gnus-sum-thread-tree-vertical "│")
;(setq gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
; see 'format-time-string
(setq gnus-user-date-format-alist '(((gnus-seconds-today). "Today, %H:%M")
                                    (604800 . "%a %H:%M")
;                                    (t . "%a, %b %d %Y")
                                    ((gnus-seconds-month). "%a, %b %d")
;                                    ((gnus-seconds-year). "%b %d")
                                    (t . "%b %d %Y")
                                    ))
; https://eschulte.github.io/emacs24-starter-kit/starter-kit-gnus.html
(when window-system
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-display-arrow t)


;;============================================================
;; * SEMANTICDB SETUP
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


;;============================================================
;; * Haskell mode
;;============================================================

;(load "~/local/haskell-mode-2.4/haskell-site-file")
;(load "~/local/share/emacs/site-lisp/twit.el")
(require 'mmm-mode)
(require 'inf-haskell)
(defun my-haskell-mode ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-simple-indent)
  (turn-on-font-lock)
  (imenu-add-menubar-index))

(add-hook 'haskell-mode-hook 'my-haskell-mode)

(mmm-add-classes
 '((literate-haskell-bird
    :submode text-mode
    :front "^[^>]"
    :include-front true
    :back "^>\\|$")
   (literate-haskell-latex
    :submode literate-haskell-mode
    :front "^\\\\begin{code}"
    :front-offset (end-of-line 1)
    :back "^\\\\end{code}"
    :include-back nil
    :back-offset (beginning-of-line -1))))

(defun my-mmm-mode ()
  ;; go into mmm minor mode when class is given
  (make-local-variable 'mmm-global-mode)
  (setq mmm-global-mode 'true))

(setq mmm-submode-decoration-level 0)
(add-hook 'haskell-mode-hook 'my-mmm-mode)

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
; (add-hook 'haskell-mode-hook 'font-lock-mode)
; (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;(require 'light-symbol)
;(set-fringe-style "left-only")
(add-hook 'c++-mode-hook 'turn-on-fic-mode)
(fringe-mode '(18 . 0))

(setq ido-max-directory-size 100000) ;; in _bytes_, not dirents.
(setq gtags-use-gtags-mixer nil)

;;============================================================
;; * Custom Editing Functions
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
;; (defun whack-whitespace ()
;;   "Delete all white space from point to the next word."
;;   (interactive nil)
;;   (when (re-search-forward "[ \t\n]+" nil t)
;;     (replace-match "" nil nil)))

(defun reverse-other-window ()
  (interactive)
  (other-window -1))

;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   ;; TODO: fix the color scheme (blue's ugly) 
    (if (not dedicated)
        (face-remap-add-relative 'mode-line '(:foreground "blue"))
      (face-remap-add-relative 'mode-line '(:foreground "black")))
    (force-mode-line-update)
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))



;;============================================================
;; * C++ SETUP
;;============================================================
(defun local-cpp-mode-hook()
  (interactive)
  (set-fill-column 77)
  (turn-on-fic-mode)
  (hs-minor-mode 1)
  (column-number-mode 1)
  (column-marker-1 79)
  (set-fringe-mode '(1 . 1))
  (c-set-offset 'arglist-intro 4)
  (c-set-offset 'tomost-intro '-)
  (c-set-offset 'innamespace  [0])
  (linum-mode)
  (flyspell-prog-mode)
;
; Ooooh, this is nice, but I may get tired of it.
;
;  (glasses-mode 1)
)

(defun local-borg-mode-hook()
  (interactive)
  (hs-minor-mode 1))

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
;; * PYTHON SETUP
;============================================================
(defun my-py-mode-hook()
  (interactive)
;  (c-subword-mode 1)
  (turn-on-fic-mode)
  (hs-minor-mode 1)
  (column-number-mode 1)
  (column-marker-1 79)
  (flyspell-prog-mode)
  (orgtbl-mode))
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
  (local-set-key [f1] 'pylint))

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



(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;============================================================
;; * DIRED SETUP
;;============================================================
;; (add-hook 'dired-load-hook (lambda ()
;;                              (progn
;;                                (require 'dired-sort-menu)
;;                                (require 'dired-sort-menu+)
;;                                (require 'dired-sort-map))))
;; (setq dired-omit-files
;;       (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
;;               (seq bol "." (not (any "."))) ;; dot-files
;;               (seq "~" eol)                 ;; backup-files
;;               (seq bol "CVS" eol)           ;; CVS dirs
;;               )))
;; (setq dired-omit-extensions
;;       (append dired-latex-unclean-extensions
;;               dired-bibtex-unclean-extensions
;;               dired-texinfo-unclean-extensions))
;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(column-marker-1 78)

;;============================================================
;; * LINT, COMPILE AND TEST
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
                ("\\.hpp$" (".cpp"))))


;;============================================================
;; * GENERAL WINDOW SETUP
;;============================================================

;
;; ** Normal configuration stuff
(scroll-bar-mode -1)
(tool-bar-mode 'nil)
(transient-mark-mode t)

;(setq p4-use-p4config-exclusively t)
(ido-mode t)
(set-fringe-mode '(12 . 8))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(color-theme-selection "Black" nil (color-theme))
 '(column-number-mode t)
 '(custom-safe-themes (quote ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "f21caace402180ab3dc5157d2bb843c4daafbe64aadc362c9f4558ac17ce43a2" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" default)))
 '(display-time-mode t)
 '(ede-project-directories (quote ("/usr/local/google/home/lally")))
 '(epg-gpg-program "/usr/bin/gpg2")
 '(gdb-find-source-frame t)
 '(gdb-many-windows t)
 '(gdb-show-changed-values t)
 '(gdb-speedbar-auto-raise t)
 '(global-hl-line-mode t)
 '(gnus-select-method (quote (nil "news")))
 '(haskell-program-name "cabal repl")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(inhibit-startup-screen t)
 '(org-agenda-custom-commands (quote (("n" "Agenda and all TODO's" ((agenda "" nil) (alltodo "" nil)) nil) ("x" "@CURRENT_WORK" tags-todo "@CURRENT_WORK|@READY_WORK|@BLOCKED_WORK" nil) ("d" "Dashboard" ((tags-todo "@CURRENT_WORK|@READY_WORK|@BLOCKED_WORK") (tags-todo "@WORK_CTX|@TODO"))))))
 '(org-agenda-files (quote ("~/org/project/uproxy/tls.org" "~/org/capture.org" "~/org/from-mobile.org" "~/org/perf.org" "~/org/personal.org" "~/org/plan-scratchpad.org" "~/org/productivity.org" "~/org/speculation.org" "~/org/unsorted.org" "~/org/project/uproxy/china.org" "~/org/project/uproxy/code.org" "~/org/project/uproxy/design-manual.org" "~/org/project/uproxy/docker.org" "~/org/project/uproxy/ignored-tickets.org" "~/org/project/uproxy/sctp.org" "~/org/project/uproxy/security.org" "~/org/project/uproxy/toplevel.org" "~/org/project/uproxy/uproxy.org" "~/org/project/uproxy/webrtc.org")))
 '(org-enforce-todo-dependencies t)
 '(org-modules (quote (org-bbdb org-bibtex org-crypt org-ctags org-docview org-id org-jsinfo org-habit org-inlinetask org-mouse org-git-link org-learn org-panel)))
 '(org-tags-exclude-from-inheritance (quote ("@CURRENT_WORK" "@READY_WORK" "@BLOCKED_WORK")))
 '(safe-local-variable-values (quote ((eval setq orgstruct-heading-prefix-regexp ";; ") (org-use-property-inheritance . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t)))
 '(typescript-indent-level 2))

(defun my-window-setup-hook (frame)
  "Set window parameters, for those that don't seem to stick."
  (modify-frame-parameters frame (list '(fill-column . 79)
                                       '(background-color . "black")
                                       '(foreground-color . "white")
                                       '(cursor-color . "white")))
  (set-fill-column 79)
; THIS NEVER WORKED:  (set-face '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 68 :width normal :foundry "unknown" :family "PragmataPro"))))
;  (set-face-attribute 'default nil :height 93)
  (message "Setting window parameters for new frame")
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
;(set-background-color "black")
;(set-foreground-color "white")
; (set-cursor-color "white")
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
;; * COMPILATION SETUP
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
;; * GIT SETUP
;;============================================================
; https://wiki.corp.google.com/twiki/bin/view/Nonconf/GitAndEmacs
;(global-auto-revert-mode)


;;============================================================
;; * ORG MODE SETUP
;;============================================================
;(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;-------------------------
;; This should be per-file!
;;-------------------------
;; Use      #+TODO: TODO(t) WAIT(w@/!) | DONE(d!) CANCELED(c@) (as an example)
;; (setq org-todo-keywords
;;       '((sequence "WAITING(w)" "OPTIONAL(o)" "BEGIN(b)" "UGLY(u)"a
;;                   "CODEREVIEW(r)" "UPDATE(u)" "PERIPHERAL(p)" "TESTING(t)"
;;                   "TOSUBMIT(s)" "DONE(d)")))

(set-variable 'org-hide-leading-stars t)

;; ** ORG CAPTURE
(setq org-default-notes-file "~/org/unsorted.org")
(define-key global-map "\C-cr" 'org-capture) ; 'r' for remember.
(setq org-protocol-default-template-key "l")
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/unsorted.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("p" "Planning Journal Entry" entry (file "~/org/plan-scratchpad.org") "* %T Plan for today")
   ("m" "Meta (Productivity) Entry" entry (file "~/org/productivity.org") "* %T Meta")
   ("l" "Link" entry (file+olp "~/org/intel/unsorted.org" "Web Links")
        "* %a\n %?\n %i")))


;; ** ORG MOBILE
(setq org-mobile-directory "/scpc:lally@lal.ly:/home/lally/org")
(setq org-log-into-drawer t)

;; ** ORG AGENDA
(let ((relevant-orgfiles (nconc (file-expand-wildcards "~/org/*.org")
                                (file-expand-wildcards
                                 "~/org/project/uproxy/*.org"))))
  (progn
    (setq org-agenda-files relevant-orgfiles))
  )
; Targets include this file and any file contributing to the agenda - up to 4 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 2))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

; (org-agenda-files)
(setq org-agenda-time-grid
      '((weekly today require-timed remove-match)
        "----------------"
        (0400 0600 0800 1000 1200 1400 1600 1800 2000 2200 2359)))
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down) ;; agenda should ignore category
        (todo category-keep priority-down)
        (tags category-keep priority-down)
        (search category-keep)))
(setq org-agenda-to-appt t) ;; add appointments to the agenda view
(setq org-agenda-window-setup 'current-window) ;; don't kill my window setup
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . nil) (C . t) (R . t)))
; (setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")


;; ** ORG EDIT HOOK
;; -------------
(defun my-org-hook ()
  (interactive)
  (auto-fill-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1)
  (flyspell-mode 1)
  (column-number-mode 1)
  (set-fill-column 79)
  (column-marker-1 79))

(add-hook 'org-mode-hook 'my-org-hook)

;; ** ORG TEXT EXPORT
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
;; * g13-support
;;============================================================
(defun filename-of-path (n)
  (last (split-string n "/" 't)))

;;============================================================
;; * EXPERIMENTAL
;;============================================================
;;(load-file "/home/build/eng/elisp/gfs.el")
;;(gfs-enable-file-name-handler)

;;  Keep these at the end, so that any failures here don't propagate
;;  to other preferences above.


;(set-background-color "black")
;(set-foreground-color "white")
; (set-cursor-color "white")
; (set-face-background 'region "midnight blue")

;; ** HELM
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(helm-mode 1)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;; helm-dash

(require 'helm-dash)
(setq helm-dash-common-docsets '("C" "C++" "JavaScript" "Bash"))
(require 'w3m)
(setq w3m-home-page "http://emacs-w3m.namazu.org/info/")
(setq browse-url-browser-function 'w3m-browse-url)

;; ** POWERLINE
;(require 'powerline)
;(powerline-default-theme)
;; ** Smart Mode Line
; Screw powerline, go smart-mode-line, it's more customizable, apparently.
(require 'smart-mode-line)
(setq sml/theme 'dark)
(sml/setup)

;; ** sr-speedbar
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "PragmataPro-7")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))



;; ** IRC
;; (add-hook 'rcirc-mode-hook
;;           (lambda ()
;;             (if (string-match (regexp-opt '("irc.freenode.net"
;;                                             "irc.google.com")
;;                               (buffer-name))
;;                 (rcirc-reconnect-mode 1)))))

;;============================================================
;; * Developer tool support
;;============================================================
;; ** Git
(require 'magit)
(require 'git-gutter)

;; ** cscope
(require 'xcscope)
(cscope-setup)
;;============================================================
;; * KEYBINDINGS
;;============================================================

(global-set-key (kbd "C-x o") 'other-window)
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
(global-set-key [f5] 'magit-status)
(global-set-key [f12] 'compile)

; cscope
(global-set-key [(control f3)]  'cscope-set-initial-directory)
(global-set-key [(control f4)]  'cscope-unset-initial-directory)
(global-set-key [(control f5)]  'cscope-find-this-symbol)
(global-set-key [(control f6)]  'cscope-find-global-definition)
(global-set-key [(control f7)]  'cscope-find-global-definition-no-prompting)
(global-set-key [(control f8)]  'cscope-pop-mark)
(global-set-key [(control f9)]  'cscope-history-forward-line)
(global-set-key [(control f10)] 'cscope-history-forward-file)
(global-set-key [(control f11)] 'cscope-history-backward-line)
(global-set-key [(control f12)] 'cscope-history-backward-file)
(global-set-key [(meta f9)]  'cscope-display-buffer)
(global-set-key [(meta f10)] 'cscope-display-buffer-toggle)

;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)
(global-set-key (kbd "M-R") 'revert-buffer)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key (kbd "C-c o") 'ff-find-other-file)

(global-set-key (kbd "C-c i") 'my-increment-number-decimal)
(global-set-key "\M-\C-\\" 'comment-region)
(global-set-key [C-tab] 'indent-region)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ct" 'org-time-stamp)
(global-set-key [pause] 'toggle-current-window-dedication)
; TODO: find module for highlighting the current column, and add it in
; a keystroke here.

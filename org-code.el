;; org-code.el -- Programmer's tools for org-mode

;; Copyright (C) 2014 Lally Singh <yell@lal.ly>

;; Author: Lally Singh <yell@lal.ly>
;; Keywords: unittests, test server, version control
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cl))

(require 'org)

; Start a test of a project.
;
;  project - identifying the .org file / path inside for manipulation
;  test-id - a unique test identifier for this run.  Or a constant if
;     there's only ever one outstanding at a time.
;  targets - a list of string identifiers for individual targets'
;     status to be tracked
(defun org-code-begin-test (project test-id targets)
  "Make an entry in the appropriate .org file for this test-id, with sub-entries for each target. 
|project| - identifying the .org file / path inside for manipulation
|test-id| - a unique test identifier for this run.  Or a constant if there's only ever one outstanding at a time.
|targets| - a list of string identifiers for individual targets' status to be tracked"
  (interactive)
  ; find the right project
  (with-current-buffer (find-file-noselect (expand-file-name (concat "~/org/project/" project "/code.org")))
    ; put in new notes about the test, and all targets
    (save-excursion
      (goto-char (point-min))
      ; find the start of the 'Tests' top-level heading
      (re-search-forward "^\\* Tests")
      (let ((heading-start (match-beginning 0))
            (heading-end (match-end 0))
            (section-start (+ 1 (match-end 0)))
            ; generate the test command
            (test-command (mapconcat 'identity (append (append '("echo" "test.sh") (cons test-id targets)) '("&")) " ")))
        (goto-char section-start)
        
        ; find the end of the section 'Tests'.
        (let ((search-result (re-search-forward "^\\* " (point-max) 1 1)))
          (let ((section-end (cond ((not search-result) (point-max))
                                   ('t (match-beginning 0)))))
            (goto-char section-end)
            ; put in a new subsection at the end, for this test.
            (org-insert-time-stamp (current-time) t nil (concat "\n** Test " test-id " ") "\n")
            (insert (concat "    :PROPERTIES:\n    :COMMAND:  " test-command "\n    :END:\n"))
            (forward-line -3)
            (org-cycle)
            (forward-line 3)
            (dolist (target targets)
              (insert (concat "*** " "RUN " target "\n")))
            ; run the command in a new buffer (ala *compilation*)
            (shell-command test-command (generate-new-buffer "*run-test*"))
            )
          )
        )
      )
    )
  )


; Update the status of a test.
;
; org-code-mark-test will update the status of a test-id/target pair
;   to |status|.
(defun org-code-mark-test (project test-id target status)
  "Update the status of a started test."
  (interactive)
  (with-current-buffer (find-file-noselect (expand-file-name (concat "~/org/project/" project "/code.org")))
    ; put in new notes about the test, and all targets
    (save-excursion
      (goto-char (point-min))
      ; find the start of the 'Tests' top-level heading
      (re-search-forward "^\\* Tests")
      (let ((heading-start (match-beginning 0))
            (heading-end (match-end 0))
            (section-start (+ 1 (match-end 0))))
        (goto-char section-start)
        
        ; find the end of the section 'Tests'.
        (let ((search-result (re-search-forward "^\\* " (point-max) 1 1)))
          (let ((section-end (cond ((not search-result) (point-max))
                                   ('t (match-beginning 0)))))
            (goto-char section-end)
            ; search backwards for the last project with this test-id.
            (re-search-backward (concat "^\\*\\* Test " test-id))
            (re-search-forward (concat "^\\*\\*\\* .* " target))
            (replace-match (concat "*** " status " " target))
            )
          )
        )
      )
    )
  )


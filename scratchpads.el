;;; scratchpads.el --- Persistent scratchpad handling  -*- lexical-binding: t -*-

;; Author: Spyros Roum <spyros.roum@posteo.net>
;; Maintainer: Spyros Roum <spyros.roum@posteo.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/SpyrosRoum/emacs-scratchpads
;; Keywords: tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Scratchapads helps you handle multiple persistant scratch files
;; on a project-by-project or global basis.
;; This can be useful for anything from jotting some quick notes down,
;; to writing API calls for your project with Org mode and Verb.

;;; Code:

(defgroup scratchpads nil
  "Handle persistent scratchpads by the project."
  :group 'tools)

(defcustom scratchpads-base-dir
  (expand-file-name "scratches" user-emacs-directory)
  "The base directory for scratchpads.
A sub-directory is created for each project, along with one
more for all scratchpads that are created outside of one."
  :group 'scratchpads)

(defvar scratchpads-projectless-dir "misc--scratches"
  "Directory for projectless scratchpads.
A \"projectless\" scratchpad is any scratchpad that was created
outside of any project.
This directory is created inside the base-dir.")

(defun scratchpads--dir-with-num (dir num)
  "Add the given `NUM' in the `DIR', respecting file extensions."
  (if-let ((ext (file-name-extension dir 't)))
    (concat (file-name-sans-extension dir) (format "%s" num) ext)
    (concat dir (format "%s" num))))

(defun scratchpads--generate-unique-name (name dir)
  "Generate a unique name for the scratchpad based on `NAME'.
Returns an expanded `DIR'+`NAME' which is certain not to exist.
When the given `NAME' already exists a unique one is generated
by appending a number starting from 2, until one that doesn't
exist is found.
The number is appended at the end of the name unless it
includes a dot (`.'), then it's added before the dot."
  (let ((num 1))
    (while (f-exists? (expand-file-name name dir))
      (setq num (1+ num))
      (setq name (scratchpads--dir-with-num name num)))
    (expand-file-name name dir)))

;;;###autoload
(defun scratchpad-new (name &optional projectless)
  "Opens a new scratchpad with a name based on `NAME'.
If a file already exists with the given name then it's altered by appending
a number to the end of it, but before the file extension if it exists.
The number starts from `2' and increments until a unique file name is found.

For example if the given name is `foo.py', but there already is a scratchpad
named `foo.py', the final file name could be `foo2.py'.

If no extension is given then `initial-major-mode' is used as the mode of the
file.

If `PROJECTLESS' is non-nill (a prefix argument is given), then the scratchpad
will be created as projectless even if there is an active project.
When there is no active project this argument is ignored.

Returns the resulting buffer object."
  (interactive
    (list
      (read-string "Enter a name: " "scratch")
      current-prefix-arg))
  (let*
    (
      (scratch-dir
        (if-let
          (
            (proj (project-current))
            (_ (not projectless)))
          (project-name proj)
          scratchpads-projectless-dir))
      (base-dir (expand-file-name scratch-dir scratchpads-base-dir))
      (file (scratchpads--generate-unique-name name base-dir)))
    (mkdir base-dir 't)
    (let ((buff (find-file file)))
      (when (string= (symbol-name major-mode) "fundamental-mode")
        (funcall initial-major-mode))
      buff)))

(defun scratchpads--potential-pads (&optional include-projectless)
  "Return a list of petential scratchpads.
Only scratchpads that belong to the current project are returned by default.
When `INCLUDE-PROJECTLESS' is non-nill then projectless scratchpads are included
in the results, otherwise only scratchpads related to the current project
are returned.
If there is no project open then `include-projectless' has no effect as only
the projectless scratchpads are returned every time."
  (let*
    (
      (scratch-dirs
        (if-let ((proj (project-current)))
          (if include-projectless
            (list (project-name proj) scratchpads-projectless-dir)
            (list (project-name proj)))
          (list scratchpads-projectless-dir)))
      (all-files-expanded
        (mapcan
          (lambda (dir-name)
            (directory-files-recursively
              (expand-file-name dir-name scratchpads-base-dir)
              ".*"))
          scratch-dirs))
      (all-files-relative
        (mapcar
          (lambda (p) (file-relative-name p scratchpads-base-dir))
          all-files-expanded)))
    all-files-relative))

;;;###autoload
(defun scratchpad-open (name)
  "Open a scratchpad for editing.
If a prefix argument is given then projectless scratchpads are included
in the search regardless of if a project is active or not.
`NAME' must be a file name relative to `scrtachpad-base-dir'"
  (interactive
    (list
      (completing-read
        "Select a scratchpad: "
        (scratchpads--potential-pads current-prefix-arg)
        nil
        't)))
  (find-file (expand-file-name name scratchpads-base-dir)))

(defun scratchpads--current-file-if-scratchpad ()
  "Return the name of the current file only if it's a scratchpad.
When the current file is a scrtachpad,
its name is returned relative to `scratchpads-base-dir'."
  (when buffer-file-name
    (when (file-in-directory-p buffer-file-name scratchpads-base-dir)
      (file-relative-name buffer-file-name scratchpads-base-dir))))

;;;###autoload
(defun scratchpad-delete (name)
  "Delete a scratchpad forever.
Delete a scratchpad related to the current project.
If a prefix argument is given then projectless scratchpads are
included as options.
`NAME' must be a file name relative to `scratchpads-base-dir'."
  (interactive
    (list
      (completing-read
        "Select a scratchpad for deletion: "
        (scratchpads--potential-pads current-prefix-arg)
        nil
        't
        (scratchpads--current-file-if-scratchpad))))
  (let*
    (
      (prompt (format "Are you sure you want to delete %s? " name))
      (confirmed (yes-or-no-p prompt))
      (file (expand-file-name name scratchpads-base-dir)))
    (when confirmed
      (delete-file file)
      (when (string= buffer-file-name file)
        (kill-buffer)))))

(provide 'scratchpads)

;;; scratchpads.el ends here

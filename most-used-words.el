;;; most-used-words.el --- Display most used words in buffer
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Udyant Wig
;;
;; Author: Udyant Wig <udyant.wig@gmail.com>
;; URL: https://github.com/udyantw/most-used-words
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience, wp

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Identify the most frequently used words in a buffer.
;;
;; The command
;;   M-x most-used-words-buffer
;; goes through the current buffer keeping a count of words, as defined
;; by the default syntax table.  Once done, it displays a buffer of the
;; most frequently used words.
;;
;; I wrote the following functions, and the wonderful posters at
;; gnu.emacs.help reviewed it.
;; Part of a discussion thread on gnu.emacs.help follows the
;; development:
;; https://groups.google.com/d/topic/gnu.emacs.help/nC8IsIuNeek/discussion
;;
;; Special thanks are due to Stefan Monnier and Eric Abrahamsen, who
;; made suggestions that greatly improved performance; Emmanuel Berg
;; reminded me to prefix functions from CL-LIB; Bob Proulx,
;; Ben Bacarisse, Nick Dokos, Eli Zaretskii shared of their Unix wisdom
;; and experience; and Bob Newell tested it for his purposes.

;;; Code:

(require 'cl-lib)

(cl-defmacro with-view-buffer (buffer &body body)
  `(progn
     (with-current-buffer ,buffer
       ,@body)
     (view-buffer-other-window ,buffer
			       nil
			       #'kill-buffer)))

(defun most-used-words-buffer-1 (n)
  "Make a list of the N most used words in buffer."
  (let ((counts (make-hash-table :test #'equal))
	sorted-counts
	start
	end)
    (save-excursion
      (goto-char (point-min))
      (skip-syntax-forward "^w")
      (setf start (point))
      (cl-loop until (eobp)
	 do
	   (skip-syntax-forward "w")
	   (setf end (point))
	   (cl-incf (gethash (buffer-substring start end) counts 0))
	   (skip-syntax-forward "^w")
	   (setf start (point))))
    (cl-loop for word being the hash-keys of counts
       using (hash-values count)
       do
	 (push (list word count) sorted-counts)
       finally (setf sorted-counts (cl-sort sorted-counts #'>
					  :key #'cl-second)))
    (mapcar #'cl-first (cl-subseq sorted-counts 0 n))))

;;;###autoload
(cl-defun most-used-words-buffer (&optional n)
  "Determine the N (default 3) most used words in the current
buffer."
  (interactive (list (completing-read
		      "How many words? (default 3) "
		      nil
		      nil
		      nil
		      nil
		      nil
		      3
		      nil)))
  (unless (numberp n)
    (setf n (string-to-number n)))
  (let ((most-used-words-buffer (get-buffer-create "*Most used words*"))
	most-used)
    (setf most-used (most-used-words-buffer-1 n))
    (with-view-buffer most-used-words-buffer
      (dolist (word most-used)
	(insert (format "%s" word))
	(newline)))))

(provide 'most-used-words)

;;; most-used-words.el ends here

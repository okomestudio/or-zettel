;;; or-zettel.el --- A Zettels extension for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/or-zettel
;; Version: 0.3.1
;; Keywords: org-roam, convenience
;; Package-Requires: ((emacs "31.1") (org "9.8.10") (org-roam "2.3.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A minor mode for using zettels in Org Roam.
;;
;;; Code:

(require 'seq)
(require 'org)
(require 'org-roam)

(defgroup or-zettel nil
  "Settings for `or-zettel'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/or-zettel"))

(defcustom or-zettel-mode-prefix "C-c n z"
  "Prefix key sequence for `or-zettel-mode' commands."
  :type 'string
  :group 'or-zettel)

(defcustom or-zettel-tags '("z")
  "List of Org Roam tags that identify a buffer as a zettel."
  :type '(repeat string)
  :group 'or-zettel)

(defcustom or-zettel-relationship-section "Links"
  "Name of the Org heading under which zettel relationships are stored."
  :type 'string
  :group 'or-zettel)

(defcustom or-zettel-relationship-format "%s:"
  "Format string used when inserting relationship keywords."
  :type 'string
  :group 'or-zettel)

(defcustom or-zettel-relationship-keywords
  '("example-of"
    "extends"
    "follows"
    "generalizes"
    "informs"
    "kind-of"
    "objects-to"
    "obsoletes"
    "opposes"
    "qualifies"
    "questions"
    "replaces"
    "responds-to"
    "see-also"
    "supports")
  "List of available relationship keywords for zettels."
  :type '(repeat string)
  :group 'or-zettel)

(defcustom or-zettel-meta-keywords '("Usage Logs" "Links" "References" "Notes")
  "TBD."
  :type '(repeat :tag "Keyword" string)
  :group 'or-zettel)

(defcustom or-zettel-meta-heading-tags '("meta")
  "List of heading tags to treat as meta sections."
  :type '(repeat :tag "List of heading tags" string)
  :group 'or-zettel)

(defcustom or-zettel-meta-heading-text-re nil
  "Regex for heading texts to treat as meta sections.
The value is specified as a regexp string or a list of heading titles."
  :type '(choice (string :tag "Regexp")
                 (repeat :tag "List of heading texts" string))
  :group 'or-zettel)

(defcustom or-zettel-meta-section-fontify t
  "Non-nil to enable custom fontification overlays for meta sections."
  :type 'boolean
  :group 'or-zettel)

(defcustom or-zettel-footnote-fontify nil
  "Non-nil to enable custom fontification for footnotes."
  :type 'boolean
  :group 'or-zettel)

(defface or-zettel-meta-section-text '((t :height 0.85))
  "Face applied to the body text of Org meta section text."
  :group 'or-zettel)

(defface or-zettel-footnote-text '((t :height 0.85))
  "Face applied to the body text of Org footnote text."
  :group 'or-zettel)

;;; Meta Headings

(defun or-zettel-meta-insert (keyword)
  "Insert a headline with KEYWORD and `or-zettel-meta-heading-tags` at the end of the buffer."
  (interactive
   (list (completing-read "Meta keyword: " or-zettel-meta-keywords nil t)))
  (let ((existing-headings nil) ; list of (keyword . buffer-position)
        keyword-exists-p move-to)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        ;; The meta heading already exists.
        (while (re-search-forward org-complex-heading-regexp nil t)
          (let* ((title (string-trim (match-string 4)))
                 (pos (match-beginning 0))
                 (tags (string-split (string-trim (match-string 5)) ":" t)))
            (when (and title
                       (seq-intersection or-zettel-meta-heading-tags tags))
              (when (string= title keyword)
                (setq keyword-exists-p t))
              (when-let* ((idx (seq-position or-zettel-meta-keywords title #'string=)))
                (push (cons title pos) existing-headings)))))

        ;; The meta heading does not already exist.
        (unless keyword-exists-p
          (let* ((target-idx (seq-position or-zettel-meta-keywords keyword #'string=))
                 insert-before-pos insert-after-pos)
            (unless target-idx
              (user-error "Keyword '%s' not found in `or-zettel-meta-keywords`" keyword))

            (setq existing-headings
                  (sort existing-headings (lambda (a b) (< (cdr a) (cdr b)))))

            ;; Find the first existing heading that appears AFTER target.
            (catch 'found
              (dolist (item existing-headings)
                (let* ((item-kw (car item))
                       (item-pos (cdr item))
                       (item-idx (seq-position or-zettel-meta-keywords item-kw #'string=)))
                  (when (> item-idx target-idx)
                    (setq insert-before-pos item-pos)
                    (throw 'found t)))))

            ;; If no succeeding heading exists, find the last existing heading that appears BEFORE target
            (unless insert-before-pos
              (let ((prev-pos nil))
                (dolist (item existing-headings)
                  (let* ((item-kw (car item))
                         (item-pos (cdr item))
                         (item-idx (seq-position or-zettel-meta-keywords item-kw #'string=)))
                    (when (< item-idx target-idx)
                      (setq prev-pos item-pos))))
                (when prev-pos
                  ;; Position at the end of that previous heading's subtree
                  (goto-char prev-pos)
                  (org-end-of-subtree t t)
                  (setq insert-after-pos (point)))))

            ;; Navigate to insertion point.
            (cond (insert-before-pos (goto-char insert-before-pos))
                  (insert-after-pos (goto-char insert-after-pos))
                  (t (goto-char (point-max)) (unless (bolp) (insert "\n"))))

            ;; Format and insert the new heading with tags.
            (let ((tags-str (if or-zettel-meta-heading-tags
                                (concat ":" (string-join or-zettel-meta-heading-tags ":") ":")
                              "")))
              (unless (bolp) (insert "\n"))
              (insert "* " keyword " " tags-str "\n\n")
              (setq move-to (point)))))))
    (goto-char move-to)))

;;; Zettel Relationships

(defun or-zettel-relationship-insert (tag)
  "Interactively select TAG from a list and insert the relationship.
If relationship entries already exist, appends after a blank line.
Leaves point ready for typing at the end of the new tag."
  (interactive
   (list (completing-read "Choose relationship: " or-zettel-relationship-keywords nil t)))
  (let ((re-heading (concat "^\\*+[ \t]+" or-zettel-relationship-section))
        (relationship (format or-zettel-relationship-format tag))
        (re-any-rel (concat "^" (format or-zettel-relationship-format "[^]]+"))))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (re-search-forward re-heading nil t)
        (goto-char (point-max))
        (insert (concat "\n* " or-zettel-relationship-section)))
      (org-end-of-meta-data nil)
      (let* ((start-pos (point))
             (end-pos (save-excursion
                        (if (re-search-forward "^\\*+" nil t)
                            (match-beginning 0)
                          (point-max))))
             (last-match nil))
        (goto-char start-pos)
        (while (re-search-forward re-any-rel end-pos t)
          (setq last-match (line-end-position)))
        (if last-match
            (progn
              (goto-char last-match)
              (insert (concat "\n\n" relationship " ")))
          (goto-char start-pos)
          ;; (unless (bolp) (insert "\n\n"))
          (insert (concat "\n" relationship " "))))))
  (org-reveal))

;;; Section Overlays

(defvar-local or-zettel--overlays-timer nil
  "Timer used to defer overlay updates during rapid typing.")

(defun or-zettel--overlays-delete-all ()
  "Delete all meta section overlays in the buffer."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'or-zettel-meta-section)
        (delete-overlay ov)))))

(defun or-zettel--overlays-update (&rest _)
  "Refresh meta section overlays across the buffer."
  (or-zettel--overlays-delete-all)
  (let ((title-re (cond ((listp or-zettel-meta-heading-text-re)
                         (concat "\\("
                                 (regexp-opt or-zettel-meta-heading-text-re)
                                 "\\)"))
                        (t or-zettel-meta-heading-text-re))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and org-complex-heading-regexp
                    (re-search-forward org-complex-heading-regexp nil t))
          (let ((stars (match-string 1))
                (title (match-string 4))
                (tags  (match-string 5)))
            (when (or (and title-re title
                           (string-match-p title-re title))
                      (when tags
                        (seq-intersection or-zettel-meta-heading-tags
                                          (string-split tags ":" t))))
              (let* ((level (length stars))
                     (body-start (1+ (line-end-position)))
                     (boundary-regex (format "^\\*\\{1,%d\\}[ \t]" level))
                     (body-end (save-excursion
                                 (if (re-search-forward boundary-regex nil t)
                                     (match-beginning 0)
                                   (point-max)))))
                (when (< body-start body-end)
                  (let ((ov (make-overlay body-start body-end)))
                    (overlay-put ov 'or-zettel-meta-section t)
                    (overlay-put ov 'face 'or-zettel-meta-section-text)))))))))))

(defun or-zettel--overlays-update-trigger (_beg _end _len)
  "Schedule a delayed overlay refresh after buffer edits."
  (when or-zettel--overlays-timer
    (cancel-timer or-zettel--overlays-timer))
  (setq or-zettel--overlays-timer
        (run-with-idle-timer 0.3 nil #'or-zettel--overlays-update)))

(defun or-zettel--overlays-setup ()
  "Enable dynamic section overlays in the current buffer."
  (or-zettel--overlays-update)
  (add-hook 'after-change-functions #'or-zettel--overlays-update-trigger nil t)
  (add-hook 'kill-buffer-hook
            (lambda () (when or-zettel--overlays-timer (cancel-timer or-zettel--overlays-timer)))
            nil t))

;;; Minor Mode

(defvar-keymap or-zettel-mode-map
  :doc "Keymap for `or-zettel-mode'.")

(defvar-keymap or-zettel-mode-prefix-map
  :doc "Keymap for `or-zettel-mode' under prefix."
  "h" #'or-zettel-meta-insert
  "r" #'or-zettel-relationship-insert)

(keymap-set or-zettel-mode-map or-zettel-mode-prefix or-zettel-mode-prefix-map)

(define-minor-mode or-zettel-local-mode
  "Buffer-local minor mode for individual zettel buffers."
  :lighter " zettel"
  :keymap or-zettel-mode-map
  (if or-zettel-local-mode
      (progn
        (when or-zettel-meta-section-fontify
          (or-zettel--overlays-setup))
        (when or-zettel-footnote-fontify
          (font-lock-add-keywords
           'org-mode
           '(("^[ \t]*\\[fn:[^]]+\\][ \t]+\\(.*\\)$"
              (1 'or-zettel-footnote-text prepend)))
           'append)))
    (or-zettel--overlays-delete-all)))

(defun or-zettel--turn-on ()
  "Enable `or-zettel-local-mode' if the current buffer is a zettel node."
  (when (derived-mode-p 'org-mode)
    (when-let* ((node (save-excursion
                        (goto-char (point-min))
                        (org-roam-node-at-point)))
                (tags (org-roam-node-tags node))
                (_ (seq-intersection or-zettel-tags tags #'string=)))
      (or-zettel-local-mode 1))))

;;;###autoload
(define-globalized-minor-mode or-zettel-mode or-zettel-local-mode
  or-zettel--turn-on
  :group 'org-roam)

(provide 'or-zettel)
;;; or-zettel.el ends here

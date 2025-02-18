;;; shift-number.el --- Increase/decrease the number at point -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (c) 2016–2017 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>

;; URL: https://github.com/alezost/shift-number.el
;; Keywords: convenience
;; Created: 12 Apr 2016
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Increase or decrease the number at point with `shift-number-up' and
;; `shift-number-down' commands.

;; To install the package manually, add the following to your init file:
;;
;; (add-to-list 'load-path "/path/to/shift-number-dir")
;; (autoload 'shift-number-up "shift-number" nil t)
;; (autoload 'shift-number-down "shift-number" nil t)

;; For more verbose description and a gif demonstration, see
;; <https://github.com/alezost/shift-number.el>.

;;; Code:

(defgroup shift-number nil
  "Increase or decrease the number at point."
  :group 'convenience)

(defcustom shift-number-regexp (rx (group (one-or-more num)))
  "Regexp for `shift-number' function.
The first parenthesized expression must match the number."
  :type 'regexp
  :group 'shift-number)

(defcustom shift-number-negative t
  "If non-nil, support negative numbers."
  :type 'boolean
  :group 'shift-number)

(defun shift-number--replace-in-region (str beg end)
  "Utility to replace region from BEG to END with STR.
Return the region replaced."
  (declare (important-return-value nil))

  (let ((len (length str))
        (i-beg nil)
        (i-end nil)
        (i-end-ofs nil))

    ;; Check for skip end.
    (let ((i 0))
      (let ((len-test (min (- end beg) len)))
        (while (< i len-test)
          (let ((i-next (1+ i)))
            (cond
             ((eq (aref str (- len i-next)) (char-after (- end i-next)))
              (setq i i-next))
             (t ; Break.
              (setq len-test i))))))
      (unless (zerop i)
        (setq i-end (- len i))
        (setq len (- len i))
        (setq end (- end i))
        (setq i-end-ofs i)))

    ;; Check for skip start.
    (let ((i 0))
      (let ((len-test (min (- end beg) len)))
        (while (< i len-test)
          (cond
           ((eq (aref str i) (char-after (+ beg i)))
            (setq i (1+ i)))
           (t ; Break.
            (setq len-test i)))))
      (unless (zerop i)
        (setq i-beg i)
        (setq beg (+ beg i))))

    (when (or i-beg i-end)
      (setq str (substring str (or i-beg 0) (or i-end len))))

    (goto-char beg)
    (delete-region beg end)
    (insert str)
    (when i-end-ofs
      ;; Leave the cursor where it would be if the end wasn't clipped.
      (goto-char (+ (point) i-end-ofs)))
    (cons beg (+ beg (length str)))))

(defun shift-number--in-regexp-p (regexp)
  "Return non-nil, if point is inside REGEXP on the current line."
  ;; The code originates from `org-at-regexp-p'.
  (save-excursion
    (let ((pos (point))
          (end (line-end-position))
          found
          exit)
      (beginning-of-line)
      (while (and (not (or exit found)) (re-search-forward regexp end t))
        (cond
         ((> (match-beginning 0) pos)
          (setq exit t))
         ((>= (match-end 0) pos)
          (setq found t))))
      found)))

(defun shift-number--impl (n)
  "Change the number at point by N.
If there is no number at point, search forward till the end of
the current line and change it."
  ;; The whole number is removed and a new number is inserted in its
  ;; place, so `save-excursion' is not used, as it will put the point at
  ;; the beginning of the number.  Instead, the point is saved and
  ;; restored later.
  (let ((old-pos (point))
        (num-bounds
         (save-match-data
           (cond
            ((or (shift-number--in-regexp-p shift-number-regexp)
                 (re-search-forward shift-number-regexp (line-end-position)))
             (cons (match-beginning 1) (match-end 1)))
            (t
             nil)))))

    (unless num-bounds
      (error "No number on the current line"))

    (let* ((beg (car num-bounds))
           (end (cdr num-bounds))
           (sign
            (and shift-number-negative
                 (cond
                  ((eq ?- (char-before beg))
                   -1)
                  (t
                   1))))
           (replace-bounds
            (cons
             (cond
              ((eq sign -1)
               (1- beg))
              (t
               beg))
             end))

           (old-num-str (buffer-substring-no-properties beg end))
           (old-num (string-to-number old-num-str))
           (new-num (+ old-num (* sign n)))

           (new-num-sign-str "")
           (new-num-leading-str "")
           (new-num-str (number-to-string (abs new-num))))

      ;; Handle sign flipping & negative numbers.
      (when (< new-num 0)
        (setq sign (- sign)))
      (when (eq sign -1)
        (setq new-num-sign-str "-"))

      ;; If there are leading zeros, preserve them keeping the same
      ;; length of the original number.
      (when (string-match-p "\\`0" old-num-str)
        (let ((len-diff (- (length old-num-str) (length new-num-str))))
          (when (> len-diff 0)
            (setq new-num-leading-str (make-string len-diff ?0)))))

      ;; Prefer this over delete+insert so as to reduce the undo overhead
      ;; when numbers are mostly the same.
      (shift-number--replace-in-region
       (concat new-num-sign-str new-num-leading-str new-num-str)
       (car replace-bounds)
       (cdr replace-bounds))

      (cond
       ;; If the point was exactly at the end, keep it there.
       ((eq old-pos end)
        (setq old-pos (point)))
       ;; Prevent the change causing the cursor to "leave" the number,
       ;; allowing for further adjustments.
       (t
        (setq old-pos (min (point) old-pos))))

      (goto-char old-pos))))

;;;###autoload
(defun shift-number-up (&optional arg)
  "Increase the number at point (or on the current line) by ARG."
  (interactive "p")
  (shift-number--impl arg))

;;;###autoload
(defun shift-number-down (&optional arg)
  "Decrease the number at point (or on the current line) by ARG."
  (interactive "p")
  (shift-number--impl (- arg)))

(provide 'shift-number)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; shift-number.el ends here

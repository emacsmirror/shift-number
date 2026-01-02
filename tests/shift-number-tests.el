;;; shift-number-tests.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; See: `shift-number-tests.sh' for launching this script.

(require 'ert)
(require 'rect)

;;; Code:

(defvar shift-number-tests-basedir
  (concat (file-name-directory load-file-name) "..")
  "Base directory for shift-number tests.")
(add-to-list 'load-path shift-number-tests-basedir)
(require 'shift-number)

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defun buffer-reset-text (initial-buffer-text)
  "Initialize buffer with INITIAL-BUFFER-TEXT."
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text)))

(defmacro with-shift-number-test (initial-buffer-text &rest body)
  "Run BODY with messages inhibited, setting buffer text to INITIAL-BUFFER-TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((inhibit-message t))
       (buffer-reset-text ,initial-buffer-text)
       ,@body)))

(defun cursor-marker ()
  "Insert a | character at point to mark cursor position for test verification."
  (insert "|"))

(defmacro should-error-with-message (form error-type expected-message)
  "Assert FORM signals an error of ERROR-TYPE with EXPECTED-MESSAGE."
  (declare (indent 1))
  (let ((err-sym (make-symbol "err")))
    `(let ((,err-sym (should-error ,form :type ,error-type)))
       (should (equal ,expected-message (error-message-string ,err-sym))))))


;; ---------------------------------------------------------------------------
;; Basic Integer Tests

(ert-deftest simple-increment ()
  "Check a single number increments."
  (let ((text-initial "1")
        (text-expected "|2"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-decrement ()
  "Check a single number decrements."
  (let ((text-initial "2")
        (text-expected "|1"))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-increment-by-amount ()
  "Check incrementing by a specific amount."
  (let ((text-initial "5")
        (text-expected "|15"))
    (with-shift-number-test text-initial
      (shift-number-up 10)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-decrement-by-amount ()
  "Check decrementing by a specific amount."
  (let ((text-initial "15")
        (text-expected "|5"))
    (with-shift-number-test text-initial
      (shift-number-down 10)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest delta-zero ()
  "Check that delta of zero leaves number unchanged."
  (let ((text-initial "42")
        (text-expected "|42"))
    (with-shift-number-test text-initial
      (shift-number-up 0)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest zero-increment ()
  "Check that single digit zero increments correctly."
  (let ((text-initial "0")
        (text-expected "|1"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-delta-up ()
  "Check that negative delta decrements when using shift-number-up."
  (let ((text-initial "10")
        (text-expected "|7"))
    (with-shift-number-test text-initial
      (shift-number-up -3)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-delta-down ()
  "Check that negative delta increments when using shift-number-down."
  (let ((text-initial "10")
        (text-expected "|13"))
    (with-shift-number-test text-initial
      (shift-number-down -3)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Negative Number Tests

(ert-deftest simple-to-negative ()
  "Check a number can decrement to negative."
  (let ((text-initial "0")
        (text-expected "|-1"))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-increment ()
  "Check a negative number increments correctly."
  (let ((text-initial "-5")
        (text-expected "|-4"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-to-positive ()
  "Check a negative number can become positive."
  (let ((text-initial "-1")
        (text-expected "|1"))
    (with-shift-number-test text-initial
      (shift-number-up 2)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-to-zero ()
  "Check a negative number can become zero."
  (let ((text-initial "-1")
        (text-expected "|0"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-two-digit-to-one-digit ()
  "Check that incrementing -10 to -9 reduces digit count correctly."
  (let ((text-initial "-10")
        (text-expected "|-9"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-disabled-at-zero ()
  "Check that decrementing below zero is prevented when shift-number-negative is nil."
  (let ((text-initial "0")
        (text-expected "|0")
        (shift-number-negative nil))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-disabled-existing-negative ()
  "Check that existing negative is treated as unsigned when shift-number-negative is nil.
The minus sign is ignored, so -5 becomes -6 (5 incremented to 6)."
  (let ((text-initial "-5")
        (text-expected "|-6")
        (shift-number-negative nil))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest negative-disabled-existing-negative-decrement ()
  "Check that decrementing with shift-number-negative nil treats number as unsigned.
The minus sign is ignored, so -5 is treated as 5, decrementing gives 4, result is -4."
  (let ((text-initial "-5")
        (text-expected "|-4")
        (shift-number-negative nil))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest positive-sign-preserved ()
  "Check that explicit + sign is preserved."
  (let ((text-initial "+5")
        (text-expected "|+6"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest positive-sign-to-negative ()
  "Check that explicit + sign becomes - when crossing zero."
  (let ((text-initial "+1")
        (text-expected "|-1"))
    (with-shift-number-test text-initial
      (shift-number-down 2)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Leading Zeros / Padding Tests

(ert-deftest leading-zeros-preserved ()
  "Check that leading zeros are preserved when incrementing."
  (let ((text-initial "007")
        (text-expected "|008"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest leading-zeros-preserved-decrement ()
  "Check that leading zeros are preserved when decrementing."
  (let ((text-initial "010")
        (text-expected "|009"))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest leading-zeros-overflow-padded ()
  "Check that leading zeros are lost when number overflows padded width."
  (let ((text-initial "099")
        (text-expected "|100"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest leading-zeros-width-gain ()
  "Check that leading zeros are preserved when number gains a digit within width."
  (let ((text-initial "009")
        (text-expected "|010"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest leading-zeros-negative ()
  "Check leading zeros with negative numbers."
  (let ((text-initial "-007")
        (text-expected "|-006"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest leading-zeros-negative-to-zero ()
  "Check that negative number with leading zeros can become zero."
  (let ((text-initial "-001")
        (text-expected "|000"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest leading-zeros-to-negative ()
  "Check that positive number with leading zeros can become negative."
  (let ((text-initial "001")
        (text-expected "|-001"))
    (with-shift-number-test text-initial
      (shift-number-down 2)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Width Change Tests

(ert-deftest width-gain ()
  "Check behavior when number gains a digit."
  (let ((text-initial "99")
        (text-expected "|100"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest width-reduction ()
  "Check behavior when number loses a digit."
  (let ((text-initial "100")
        (text-expected "|99"))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest width-reduction-negative ()
  "Check behavior when negative number loses a digit."
  (let ((text-initial "-100")
        (text-expected "|-99"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Cursor Position Tests

(ert-deftest cursor-in-middle ()
  "Check that number is found when cursor is in the middle."
  (let ((text-initial "12345")
        (text-expected "12|346"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '3'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-at-end ()
  "Check that number is found when cursor is at the end."
  (let ((text-initial "123")
        (text-expected "12|4"))
    (with-shift-number-test text-initial
      (goto-char (point-max))
      (backward-char 1) ; Position cursor on '3'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-before-number ()
  "Check that number ahead on line is found when cursor is before it."
  (let ((text-initial "abc 42")
        (text-expected "|abc 43"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-between-numbers ()
  "Check that next number is found when cursor is between numbers."
  (let ((text-initial "10 20 30")
        (text-expected "10 |21 30"))
    (with-shift-number-test text-initial
      (forward-char 3) ; Position cursor on space after '10'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-on-first-of-multiple ()
  "Check that first number is incremented when cursor is on it."
  (let ((text-initial "10 20 30")
        (text-expected "|11 20 30"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-on-negative-sign ()
  "Check that number is found when cursor is on the negative sign."
  (let ((text-initial "-42")
        (text-expected "|-41"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-on-positive-sign ()
  "Check that number is found when cursor is on the positive sign."
  (let ((text-initial "+42")
        (text-expected "|+43"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest cursor-after-last-digit ()
  "Check that number is found when cursor is immediately after last digit."
  (let ((text-initial "123")
        (text-expected "124|"))
    (with-shift-number-test text-initial
      (goto-char (point-max)) ; Position cursor after '3'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; No Operation / Edge Case Tests

(ert-deftest nop-no-number ()
  "Error when there's no number on the line."
  (let ((text-initial "abc")
        (text-expected "|abc"))
    (with-shift-number-test text-initial
      (should-error-with-message
          (shift-number-up 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-non-number-signed ()
  "Error when minus sign is followed by non-digit."
  (let ((text-initial "-X")
        (text-expected "|-X"))
    (with-shift-number-test text-initial
      (should-error-with-message
          (shift-number-up 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-cursor-after-trailing-space ()
  "Error when cursor is after number with trailing space at end of buffer."
  (let ((text-initial "123 ")
        (text-expected "123 |"))
    (with-shift-number-test text-initial
      (goto-char (point-max))
      (should-error-with-message
          (shift-number-up 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-cursor-after-trailing-space-decrement ()
  "Error when decrementing with cursor after trailing space."
  (let ((text-initial "123 ")
        (text-expected "123 |"))
    (with-shift-number-test text-initial
      (goto-char (point-max))
      (should-error-with-message
          (shift-number-down 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-blank-line ()
  "Error on a blank line."
  (let ((text-initial "")
        (text-expected "|"))
    (with-shift-number-test text-initial
      (should-error-with-message
          (shift-number-up 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-blank-line-decrement ()
  "Error on a blank line when decrementing."
  (let ((text-initial "")
        (text-expected "|"))
    (with-shift-number-test text-initial
      (should-error-with-message
          (shift-number-down 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-different-line ()
  "Error - do not increment number on a different line."
  (let ((text-initial
         ;; format-next-line: off
         (concat "\n"
                 "42"))
        (text-expected
         ;; format-next-line: off
         (concat "|\n"
                 "42")))
    (with-shift-number-test text-initial
      (should-error-with-message
          (shift-number-up 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest nop-cursor-after-all-numbers ()
  "Error when cursor is positioned after all numbers on the line."
  (let ((text-initial "10 20 30 ")
        (text-expected "10 20 30 |"))
    (with-shift-number-test text-initial
      (goto-char (point-max)) ; Position cursor after trailing space.
      (should-error-with-message
          (shift-number-up 1)
        'error
        "No number on the current line")
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Sign Context Tests (avoid false negatives in expressions)

(ert-deftest expression-subtraction ()
  "Check that subtraction expressions don't create false negatives."
  (let ((text-initial "123-456")
        (text-expected "123-|457"))
    (with-shift-number-test text-initial
      (forward-char 4) ; Position cursor on '4'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest expression-subtraction-first-number ()
  "Check that first number in subtraction is incremented independently."
  (let ((text-initial "123-456")
        (text-expected "|124-456"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest expression-addition ()
  "Check that addition expressions work correctly."
  (let ((text-initial "123+456")
        (text-expected "123+|457"))
    (with-shift-number-test text-initial
      (forward-char 4) ; Position cursor on '4'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Region Tests

(ert-deftest region-multiple-numbers ()
  "Check that all numbers in a region are incremented."
  (let ((text-initial "1 2 3")
        (text-expected "2 3 |4"))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest region-decrement-multiple ()
  "Check that all numbers in a region are decremented."
  (let ((text-initial "5 6 7")
        (text-expected "4 5 |6"))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest region-partial ()
  "Check that only numbers within the region are modified.
The 1 is unchanged, 2 becomes 3, 3 becomes 4, original 4 and 5 unchanged."
  (let ((text-initial "1 2 3 4 5")
        (text-expected "1 3 4 |4 5"))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char 3) ; Position before '2'.
      (set-mark (point))
      (forward-char 4) ; Position after '3'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest region-no-numbers ()
  "Check that region with no numbers leaves buffer unchanged."
  (let ((text-initial "abc def ghi")
        (text-expected "abc def ghi|"))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest region-mixed-signs ()
  "Check that region with mixed positive and negative numbers works."
  (let ((text-initial "-5 0 +5")
        (text-expected "-4 1 +|6"))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest region-multiple-lines ()
  "Check that region spanning multiple lines increments all numbers."
  (let ((text-initial
         ;; format-next-line: off
         (concat "1 2\n"
                 "3 4\n"
                 "5 6"))
        (text-expected
         ;; format-next-line: off
         (concat "2 3\n"
                 "4 5\n"
                 "6 |7")))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest region-reversed ()
  "Check that region works when mark is after point."
  (let ((text-initial "1 2 3")
        (text-expected "|2 3 4"))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-max))
      (set-mark (point))
      (goto-char (point-min))
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Rectangle Mode Tests

(ert-deftest rectangle-single-column ()
  "Check rectangle mode increments a column of numbers."
  (let ((text-initial
         ;; format-next-line: off
         (concat "1 0 0\n"
                 "1 0 0\n"
                 "1 0 0"))
        (text-expected
         ;; format-next-line: off
         (concat "2 0 0\n"
                 "2 0 0\n"
                 "|2 0 0")))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (backward-char 4) ; Position at column 0 of last line.
      (rectangle-mark-mode 1)
      (shift-number-up 1)
      (rectangle-mark-mode 0)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest rectangle-with-negatives ()
  "Check rectangle mode works with negative numbers."
  (let ((text-initial
         ;; format-next-line: off
         (concat "-5 0\n"
                 "-5 0\n"
                 "-5 0"))
        (text-expected
         ;; format-next-line: off
         (concat "-4 0\n"
                 "-4 0\n"
                 "-|4 0")))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (backward-char 2) ; Position at column 0 of last line.
      (rectangle-mark-mode 1)
      (shift-number-up 1)
      (rectangle-mark-mode 0)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest rectangle-decrement ()
  "Check rectangle mode decrements a column of numbers."
  (let ((text-initial
         ;; format-next-line: off
         (concat "5 0 0\n"
                 "5 0 0\n"
                 "5 0 0"))
        (text-expected
         ;; format-next-line: off
         (concat "4 0 0\n"
                 "4 0 0\n"
                 "|4 0 0")))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (backward-char 4) ; Position at column 0 of last line.
      (rectangle-mark-mode 1)
      (shift-number-down 1)
      (rectangle-mark-mode 0)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest rectangle-middle-column ()
  "Check rectangle mode increments a middle column of numbers."
  (let ((text-initial
         ;; format-next-line: off
         (concat "1 2 3\n"
                 "1 2 3\n"
                 "1 2 3"))
        (text-expected
         ;; format-next-line: off
         (concat "1 3 3\n"
                 "1 3 3\n"
                 "1 |3 3")))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (forward-char 2) ; Position at column 2 (the '2').
      (set-mark (point))
      (goto-char (point-max))
      (backward-char 2) ; Position at column 2 of last line.
      (rectangle-mark-mode 1)
      (shift-number-up 1)
      (rectangle-mark-mode 0)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest rectangle-last-column ()
  "Check rectangle mode increments the last column of numbers."
  (let ((text-initial
         ;; format-next-line: off
         (concat "1 2 3\n"
                 "1 2 3\n"
                 "1 2 3"))
        (text-expected
         ;; format-next-line: off
         (concat "1 2 4\n"
                 "1 2 4\n"
                 "1 2 |4")))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (forward-char 4) ; Position at column 4 (the '3').
      (set-mark (point))
      (goto-char (point-max))
      (rectangle-mark-mode 1)
      (shift-number-up 1)
      (rectangle-mark-mode 0)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Motion Tests

(ert-deftest motion-enabled ()
  "Check that shift-number-motion moves cursor and sets mark at number start."
  (let ((text-initial "abc 123 def")
        (text-expected "abc 124| def")
        (mark-expected 5) ; Position of "124".
        (shift-number-motion t))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string)))
      (should (equal mark-expected (mark))))))

(ert-deftest motion-enabled-down ()
  "Check that shift-number-motion works with shift-number-down."
  (let ((text-initial "abc 123 def")
        (text-expected "abc 122| def")
        (mark-expected 5) ; Position of "122".
        (shift-number-motion t))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string)))
      (should (equal mark-expected (mark))))))

(ert-deftest motion-enabled-region ()
  "Check that shift-number-motion works with region operations."
  (let ((text-initial "1 2 3")
        (text-expected "2 3 4|")
        (mark-expected 1) ; Position of "2" (first number in region).
        (shift-number-motion t))
    (with-shift-number-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string)))
      (should (equal mark-expected (mark))))))

(ert-deftest motion-disabled ()
  "Check that cursor position is maintained and mark is not set when motion is disabled."
  (let ((text-initial "123")
        (text-expected "|124")
        (shift-number-motion nil))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string)))
      (should-not (mark)))))

;; ---------------------------------------------------------------------------
;; Multi-digit and Large Number Tests

(ert-deftest large-number ()
  "Check that large numbers are handled correctly."
  (let ((text-initial "999999999")
        (text-expected "|1000000000"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest large-negative ()
  "Check that large negative numbers are handled correctly."
  (let ((text-initial "-999999999")
        (text-expected "|-1000000000"))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Context Tests (number in various positions)

(ert-deftest number-at-line-start ()
  "Check number at the start of a line."
  (let ((text-initial "42 is the answer")
        (text-expected "|43 is the answer"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest number-at-line-end ()
  "Check number at the end of a line."
  (let ((text-initial "answer is 42")
        (text-expected "|answer is 43"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest number-surrounded-by-parens ()
  "Check number surrounded by parentheses."
  (let ((text-initial "foo(42)")
        (text-expected "foo(|43)"))
    (with-shift-number-test text-initial
      (forward-char 4) ; Position cursor on '4'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest number-in-array ()
  "Check number in array-like syntax."
  (let ((text-initial "[1, 2, 3]")
        (text-expected "[|2, 2, 3]"))
    (with-shift-number-test text-initial
      (forward-char 1) ; Position cursor on '1'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest number-embedded-in-word ()
  "Check number embedded within identifier-like text."
  (let ((text-initial "var1name")
        (text-expected "var|2name"))
    (with-shift-number-test text-initial
      (forward-char 3) ; Position cursor on '1'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Decimal Number Tests

(ert-deftest decimal-integer-part ()
  "Check that cursor on integer part of decimal increments integer."
  (let ((text-initial "3.14")
        (text-expected "|4.14"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest decimal-fractional-part ()
  "Check that cursor on fractional part of decimal increments fraction."
  (let ((text-initial "3.14")
        (text-expected "3.|15"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '1'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest decimal-fractional-overflow ()
  "Check that fractional part can overflow its width."
  (let ((text-initial "3.99")
        (text-expected "3.|100"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '9'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest decimal-cursor-on-point ()
  "Check that cursor on decimal point finds the integer part."
  (let ((text-initial "3.14")
        (text-expected "4|.14"))
    (with-shift-number-test text-initial
      (forward-char 1) ; Position cursor on '.'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-mantissa ()
  "Check that scientific notation mantissa is incremented independently."
  (let ((text-initial "1e10")
        (text-expected "|2e10"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-exponent ()
  "Check that scientific notation exponent is incremented independently."
  (let ((text-initial "1e10")
        (text-expected "1e|11"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '1' in exponent.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-uppercase ()
  "Check that uppercase E in scientific notation works."
  (let ((text-initial "1E10")
        (text-expected "|2E10"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-negative-exponent ()
  "Check that negative exponent in scientific notation is handled."
  (let ((text-initial "1e-10")
        (text-expected "1e-|9"))
    (with-shift-number-test text-initial
      (forward-char 3) ; Position cursor on '1' in exponent.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-positive-exponent ()
  "Check that explicit positive exponent in scientific notation is handled."
  (let ((text-initial "1e+10")
        (text-expected "1e+|11"))
    (with-shift-number-test text-initial
      (forward-char 3) ; Position cursor on '1' in exponent.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-cursor-on-exponent-sign ()
  "Check that cursor on + in exponent finds the exponent."
  (let ((text-initial "1e+10")
        (text-expected "1e|+11"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '+'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-cursor-on-e ()
  "Check that cursor on 'e' finds the mantissa."
  (let ((text-initial "1e10")
        (text-expected "2|e10"))
    (with-shift-number-test text-initial
      (forward-char 1) ; Position cursor on 'e'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest underscore-separator ()
  "Check that underscore-separated numbers are treated as separate numbers."
  (let ((text-initial "1_000")
        (text-expected "|2_000"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest hexadecimal-prefix ()
  "Check that hexadecimal prefix is treated as separate number."
  (let ((text-initial "0x10")
        (text-expected "|1x10"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest multiple-decimal-points ()
  "Check behavior with multiple decimal points like version numbers."
  (let ((text-initial "1.2.3")
        (text-expected "|2.2.3"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest multiple-decimal-points-middle ()
  "Check incrementing middle component of version number."
  (let ((text-initial "1.2.3")
        (text-expected "1.|3.3"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '2'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest multiple-decimal-points-last ()
  "Check incrementing last component of version number."
  (let ((text-initial "1.2.3")
        (text-expected "1.2.|4"))
    (with-shift-number-test text-initial
      (forward-char 4) ; Position cursor on '3'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest all-zeros ()
  "Check that all zeros increment correctly."
  (let ((text-initial "000")
        (text-expected "|001"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest all-zeros-to-negative ()
  "Check that all zeros can decrement to negative with leading zeros."
  (let ((text-initial "000")
        (text-expected "|-001"))
    (with-shift-number-test text-initial
      (shift-number-down 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-decimal-mantissa ()
  "Check scientific notation with decimal in mantissa."
  (let ((text-initial "1.5e10")
        (text-expected "|2.5e10"))
    (with-shift-number-test text-initial
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest scientific-notation-decimal-mantissa-fractional ()
  "Check incrementing fractional part of decimal mantissa in scientific notation."
  (let ((text-initial "1.5e10")
        (text-expected "1.|6e10"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '5'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(ert-deftest tab-separated-numbers ()
  "Check that tab-separated numbers work correctly."
  (let ((text-initial "1\t2\t3")
        (text-expected "1\t|3\t3"))
    (with-shift-number-test text-initial
      (forward-char 2) ; Position cursor on '2'.
      (shift-number-up 1)
      (cursor-marker)
      (should (equal text-expected (buffer-string))))))

(provide 'shift-number-tests)
;;; shift-number-tests.el ends here

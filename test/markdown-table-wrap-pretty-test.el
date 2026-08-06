;;; markdown-table-wrap-pretty-test.el --- Tests for markdown-table-wrap-pretty -*- lexical-binding: t; -*-

;; Copyright (C) 2026 SayreBlades

;; Author: SayreBlades
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the display companion package `markdown-table-wrap-pretty'.
;;
;; These tests cover the PURE helpers that the display layer is built from:
;; separator detection, org normalization, table parsing, row/separator
;; rendering (plain and Unicode-pretty), inline-span styling, and
;; prefix-aware display-group construction.  The overlay/toggle behavior
;; itself is not exercised here because it needs a live window/display;
;; the pure helpers are the high-value, deterministic surface.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'markdown-table-wrap)
(require 'markdown-table-wrap-pretty)

(defmacro markdown-table-wrap-pretty-test--with-prettify (prettify &rest body)
  "Evaluate BODY with `markdown-table-wrap-pretty-prettify' bound to PRETTIFY.
Bodies that call the renderers depend on this defcustom; bind it
locally so tests are deterministic and order-independent."
  (declare (indent 1))
  `(let ((markdown-table-wrap-pretty-prettify ,prettify))
     ,@body))

;;;; Separator detection

(ert-deftest markdown-table-wrap-pretty-test-separator-gfm ()
  "A plain GFM separator row is recognized."
  (should (markdown-table-wrap-pretty--separator-line-p "|---|---|")))

(ert-deftest markdown-table-wrap-pretty-test-separator-gfm-aligned ()
  "A GFM separator with alignment colons is recognized."
  (should (markdown-table-wrap-pretty--separator-line-p "|:--|:-:|--:|")))

(ert-deftest markdown-table-wrap-pretty-test-separator-org ()
  "An org separator with `+' joins is recognized."
  (should (markdown-table-wrap-pretty--separator-line-p "|---+---|")))

(ert-deftest markdown-table-wrap-pretty-test-separator-not-a-row ()
  "A data row is not a separator."
  (should-not (markdown-table-wrap-pretty--separator-line-p "| a | b |")))

;;;; Org normalization

(ert-deftest markdown-table-wrap-pretty-test-normalize-strips-cookies ()
  "Org width cookies are stripped regardless of major mode."
  (should (equal "|  |  |"
                 (markdown-table-wrap-pretty--normalize-org-line
                  "| <l5> | <5> |"))))

(ert-deftest markdown-table-wrap-pretty-test-normalize-org-hline-in-org ()
  "In `org-mode' the `+' join in a separator becomes GFM `|'."
  (with-temp-buffer
    (org-mode)
    (should (equal "|---|---|"
                   (markdown-table-wrap-pretty--normalize-org-line
                    "|---+---|")))))

(ert-deftest markdown-table-wrap-pretty-test-normalize-gfm-sep-unchanged ()
  "An already-GFM separator is left untouched in `org-mode'."
  (with-temp-buffer
    (org-mode)
    (should (equal "|---|---|"
                   (markdown-table-wrap-pretty--normalize-org-line
                    "|---|---|")))))

;;;; Parsing

(ert-deftest markdown-table-wrap-pretty-test-parse-gfm ()
  "Parse a GFM table into headers, aligns, and rows."
  (should (equal '(("A" "B") (nil nil) (("1" "2")))
                 (markdown-table-wrap-pretty--parse-table
                  (split-string "| A | B |\n|---|---|\n| 1 | 2 |" "\n")))))

(ert-deftest markdown-table-wrap-pretty-test-parse-org-hline-cookie ()
  "Parse an org table, normalizing its hline and stripping cookies."
  (with-temp-buffer
    (org-mode)
    (should (equal '(("A" "B") (nil nil) (("1" "2")))
                   (markdown-table-wrap-pretty--parse-table
                    (split-string "| A | B |\n|---+---|\n| 1 | 2 |" "\n"))))))

(ert-deftest markdown-table-wrap-pretty-test-parse-no-separator ()
  "A block without a separator row parses to nil."
  (should-not (markdown-table-wrap-pretty--parse-table
               (split-string "| a | b |\n| c | d |" "\n"))))

(ert-deftest markdown-table-wrap-pretty-test-parse-separator-first ()
  "A block whose first line is the separator (no header) parses to nil."
  (should-not (markdown-table-wrap-pretty--parse-table
               (split-string "|---|---|\n| 1 | 2 |" "\n"))))

;;;; Separator-line rendering

(ert-deftest markdown-table-wrap-pretty-test-render-separator-plain ()
  "Plain separator renders standard markdown dashes."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal "| - | - |"
                   (markdown-table-wrap-pretty--render-separator-line
                    '(1 1) '(nil nil))))))

(ert-deftest markdown-table-wrap-pretty-test-render-separator-aligned ()
  "Plain separator preserves GFM alignment colons."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal "| :-- | --: | :-: |"
                   (markdown-table-wrap-pretty--render-separator-line
                    '(3 3 3) '(left right center))))))

(ert-deftest markdown-table-wrap-pretty-test-render-separator-pretty ()
  "Pretty separator renders a Unicode box-drawing rule."
  (markdown-table-wrap-pretty-test--with-prettify t
    (should (equal "├───┼───┤"
                   (markdown-table-wrap-pretty--render-separator-line
                    '(1 1) '(nil nil))))))

;;;; Row rendering

(ert-deftest markdown-table-wrap-pretty-test-render-row-plain ()
  "A single short row renders as one plain markdown line."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal '("| A | B |")
                   (markdown-table-wrap-pretty--render-row-lines
                    '("A" "B") '(1 1) '(nil nil))))))

(ert-deftest markdown-table-wrap-pretty-test-render-row-pretty ()
  "A single short row renders with Unicode verticals when prettify is on."
  (markdown-table-wrap-pretty-test--with-prettify t
    (should (equal '("│ A │ B │")
                   (markdown-table-wrap-pretty--render-row-lines
                    '("A" "B") '(1 1) '(nil nil))))))

(ert-deftest markdown-table-wrap-pretty-test-render-row-right-align ()
  "Right alignment pads on the left."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal '("|   A |")
                   (markdown-table-wrap-pretty--render-row-lines
                    '("A") '(3) '(right))))))

(ert-deftest markdown-table-wrap-pretty-test-render-row-center-align ()
  "Center alignment splits padding, favoring the left on odd widths."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal '("|  A  |")
                   (markdown-table-wrap-pretty--render-row-lines
                    '("A") '(3) '(center))))))

(ert-deftest markdown-table-wrap-pretty-test-render-row-wraps ()
  "A cell wider than its column wraps across multiple visual rows."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal '("| hello |" "| world |")
                   (markdown-table-wrap-pretty--render-row-lines
                    '("hello world") '(5) '(nil nil))))))

;;;; Inline-span styling

(ert-deftest markdown-table-wrap-pretty-test-span-bold ()
  "`**x**' folds to the text in the `bold' face."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "**bold**")))
    (should (equal "bold" (substring-no-properties r)))
    (should (eq 'bold (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-italic ()
  "`*x*' folds to the text in the `italic' face."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "*it*")))
    (should (equal "it" (substring-no-properties r)))
    (should (eq 'italic (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-bold-italic ()
  "`***x***' folds before `**x**' and uses `bold-italic'."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "***bi***")))
    (should (equal "bi" (substring-no-properties r)))
    (should (eq 'bold-italic (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-strikethrough ()
  "`~~x~~' folds to the text with a strike-through face."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "~~st~~")))
    (should (equal "st" (substring-no-properties r)))
    (should (equal '(:strike-through t) (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-code-single ()
  "A single-backtick code span uses the code face."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "`code`")))
    (should (equal "code" (substring-no-properties r)))
    (should (eq 'markdown-table-wrap-pretty-code-face
                (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-code-double ()
  "A double-backtick code span trims one surrounding space (CommonMark)."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "`` c d ``")))
    (should (equal "c d" (substring-no-properties r)))
    (should (eq 'markdown-table-wrap-pretty-code-face
                (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-link ()
  "`[text](url)' folds to the text in the `link' face."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "[t](u)")))
    (should (equal "t" (substring-no-properties r)))
    (should (eq 'link (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-image ()
  "`![alt](url)' folds to the alt text in the `link' face."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "![a](u)")))
    (should (equal "a" (substring-no-properties r)))
    (should (eq 'link (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-escaped-pipe ()
  "An escaped pipe `\\|' unescapes to a literal pipe (GFM table level)."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "a \\| b")))
    (should (equal "a | b" (substring-no-properties r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-code-literal-markup ()
  "Markup inside a code span is literal, not folded."
  (let ((r (markdown-table-wrap-pretty-render-inline-spans "`**x**`")))
    (should (equal "**x**" (substring-no-properties r)))
    (should (eq 'markdown-table-wrap-pretty-code-face
                (get-text-property 0 'face r)))))

(ert-deftest markdown-table-wrap-pretty-test-span-in-cell-passthrough ()
  "With `prettify' nil, cells pass through unchanged (valid source)."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (equal "**bold**"
                   (markdown-table-wrap-pretty--render-inline-spans-in-cell
                    "**bold**")))))

;;;; Display groups (prefix-aware)

(ert-deftest markdown-table-wrap-pretty-test-display-groups-plain-fits ()
  "A plain table that fits yields one group per source line."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (should (= 3 (length
                  (markdown-table-wrap-pretty--table-display-groups
                   (split-string "| A | B |\n|---|---|\n| 1 | 2 |" "\n")
                   80))))))

(ert-deftest markdown-table-wrap-pretty-test-display-groups-blockquote-prefix ()
  "A blockquote prefix is repeated on every wrapped continuation line."
  (markdown-table-wrap-pretty-test--with-prettify nil
    (let* ((lines (split-string
                   "> | Name | Age |\n> |------|-----|\n> | Alexander | 30 |"
                   "\n"))
           (groups (markdown-table-wrap-pretty--table-display-groups lines 14))
           (all-lines (apply #'append groups)))
      (should all-lines)
      (should (cl-every (lambda (l) (string-prefix-p "> " l)) all-lines)))))

(provide 'markdown-table-wrap-pretty-test)
;;; markdown-table-wrap-pretty-test.el ends here

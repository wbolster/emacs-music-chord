;;; music-chord --- major mode for text files containing musical chords

;;; Commentary:

;; todo

;;; Code:

(require 'rx)

;; todo defvar instead of setq

(defgroup music-chord nil
  "Text with music chords"
  :group 'text
  :prefix "music-chord-")

(defface music-chord-face
  '((t :inherit font-lock-constant-face))
  "Face for base chords."
  :group 'music-chord)
(defvar music-chord-face 'music-chord-face)

(defface music-chord-addition-face
  '((t :inherit font-lock-variable-name-face))
  "Face for chord additions."
  :group 'music-chord)
(defvar music-chord-addition-face 'music-chord-addition-face)

(defface music-chord-bass-face
  '((t :inherit font-lock-keyword-face))
  "Face for custom bass notes."
  :group 'music-chord)
(defvar music-chord-bass-face 'music-chord-bass-face)

(defface music-chord-header-face
  '((t :inherit font-lock-comment-face))
  "Face for hearders in music-chord."
  :group 'music-chord)
(defvar music-chord-header-face 'music-chord-header-face)

(defface music-chord-repetition-face
  '((t :inherit font-lock-warning-face))
  "Face for repetition marks in music-chord."
  :group 'music-chord)
(defvar music-chord-repetition-face 'music-chord-repetition-face)

(defconst music-chord--font-lock-keywords
  `((,(rx symbol-start
          ;; root note
          (group
           (any "ABCDEFG") (? (any "b♭#♯")))

          ;; chord quality
          (group
           (? (or "Maj" "M" "maj" "min" "m"))
           (? (or "+" "aug" "0" "dim")))

          ;; additions
          (group
           (*
            (? (any "(/"))
            (? (or "add" "no" "M" "Maj" "maj"))
            (? (any "b♭#♯+"))
            (or "2" "3" "5" "6" "7" "9" "11" "13")
            (? ")")))

          ;; suspended chord
          (group
           (? (or "sus2" "sus4")))

          ;; alternative bass note
          (group
           (? "/" (any "ABCDEFG") (? (any "b♭#♯"))))

          symbol-end)
     (1 music-chord-face)
     (2 music-chord-face)
     (3 music-chord-addition-face)
     (4 music-chord-addition-face)
     (5 music-chord-bass-face))

    ;; header lines, e.g. [verse]
    (,(rx
       bol
       (any "([")
       (1+ print)
       (any ")]")
       eol)
     . music-chord-header-face)

    ;; repetition marks, e.g. 4x or ×2
    (,(rx
       (1+ digit)
       (any "Xx×"))
     (0 music-chord-repetition-face t))
    (,(rx
       (any "Xx×")
       (1+ digit))
     (0 music-chord-repetition-face t)))
  "Font lock keywords for music-chord mode.")

(setq music-chord-mode-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?\( "_" table)
        (modify-syntax-entry ?\) "_" table)
        (modify-syntax-entry ?/ "_" table)
        (modify-syntax-entry ?+ "_" table)
        table))

(define-derived-mode music-chord-mode text-mode "music-chord"
  "Major mode for editing text files with musical chords in them."
  (setq font-lock-defaults '((music-chord--font-lock-keywords) nil nil)))

(provide 'music-chord)
;;; music-chord.el ends here

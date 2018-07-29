(require 'rx)

;; todo defvar instead of setq
;; todo define faces inheriting from font-lock-*-face

(setq music-chord--font-lock-keywords
      `((,(rx word-boundary
              ;; root note
              (any "ABCDEFG") (? (any "b♭#♯"))

              ;; chord quality
              (? (or "Maj" "M" "maj" "min" "m"))
              (? (or "+" "aug" "0" "dim"))

              ;; additions
              (*
               (? (any "(/"))
               (? (or "add" "no" "M" "Maj" "maj"))
               (? (any "b♭#♯+"))
               (or "2" "3" "5" "6" "7" "9" "11" "13")
               (? ")")
               )

              ;; suspended chord
              (? (or "sus2" "sus4"))

              ;; alternative bass note
              (? "/" (any "ABCDEFG") (? (any "b♭#♯")))
              word-boundary)
         . font-lock-constant-face)

        ;; header lines, e.g. [verse]
        (,(rx
           bol
           (any "([")
           (1+ print)
           (any ")]")
           eol)
         . font-lock-comment-face)

        ;; repetition marks, e.g. 4x or ×2
        (,(rx
           (1+ digit)
           (any "Xx×"))
         . font-lock-warning-face)
        (,(rx
           (any "Xx×")
           (1+ digit))
         . font-lock-warning-face)
        ))

(setq music-chord-mode-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?\( "w" table)
        (modify-syntax-entry ?\) "w" table)
        (modify-syntax-entry ?/ "w" table)
        (modify-syntax-entry ?+ "w" table)
        table))

(define-derived-mode music-chord-mode text-mode "music-chord"
  "Major mode for editing text files with musical chords in them."
  (setq font-lock-defaults '(music-chord--font-lock-keywords)))

(defun xah-abbrev-h-f ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “ahf” stand for abbrev hook function.
Version 2016-10-24"
  t)

(put 'xah-abbrev-h-f 'no-self-insert t)

(when (boundp 'java-mode-abbrev-table)
  (clear-abbrev-table java-mode-abbrev-table))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("main" "public static void main(String[] args) {

}" xah-abbrev-h-f)
    ("printf" "System.out.printf();" xah-abbrev-h-f)
    )
  )

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

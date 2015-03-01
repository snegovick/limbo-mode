(defvar limbo-mode-hook nil)

(defvar limbo-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for limbo major mode")

(add-to-list 'auto-mode-alist '("\\.b\\'" . limbo-mode))

(defconst limbo-font-lock-keywords
  (list
   '("\\<\\(=>\\|c\\(?:ase\\|on\\)\\|else\\|for\\|i\\(?:f\\|mp\\(?:\\(?:lemen\\|or\\)t\\)\\|nclude\\)\\|nil\\|re\\(?:f\\|turn\\)\\|while\\)\\>" . font-lock-keyword-face)
   '("\\<\\(a\\(?:dt\\|rray\\)\\|byte\\|int\\|list\\|string\\)\\>" . font-lock-type-face)
   '("^[ \t]*\\([a-zA-Z]+[[:alnum:]]*\\)(.*)[^;]" (1 'font-lock-function-name-face))
   '("\\('\\w*'\\)" . font-lock-variable-name-face)
   )
  )

(defun limbo-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) (looking-at-open-paren nil) cur-indent)
      (if (looking-at "^[ \t]*}")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) default-tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion 
          (while not-indented
            (if (looking-at "^[ \t]*{")
                (setq looking-at-open-paren t))
            (forward-line -1)
            (if (looking-at "[ \t]*\\(=>\\|case\\|else\\|for\\|if\\|while\\)[ \t]*{")
                (progn
                  (setq cur-indent (+ (current-indentation) default-tab-width))
                  (setq not-indented nil))
              (if (looking-at "[ \t]*\\(=>\\|case\\|else\\|for\\|if\\|while\\)")
                  (progn
                    (if (equal looking-at-open-paren t)
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
                      (progn
                        (setq cur-indent (+ (current-indentation) default-tab-width))
                        (setq not-indented nil))
                      ))
                (if (looking-at "^[ \t]*}")
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
                  (if (looking-at "^[ \t]*{")
                      (progn
                        (setq cur-indent (+ (current-indentation) default-tab-width))
                        (setq not-indented nil))
                    (if (bobp)
                        (setq not-indented nil)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar limbo-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for limbo-mode")


(defun limbo-mode ()
  "Major mode for editing Limbo sources"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table limbo-mode-syntax-table)
  (use-local-map limbo-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(limbo-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'limbo-indent-line)
  (setq major-mode 'limbo-mode)
  (setq mode-name "Limbo")
  (run-hooks 'limbo-mode-hook)
  )

(provide 'limbo-mode)

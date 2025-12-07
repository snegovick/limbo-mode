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
   '("^[ \t]*\\([a-zA-Z\\_]+[a-zA-Z0-9\\_]*\\)(.*)[^;]" (1 'font-lock-function-name-face))
   '("\\('\\w*'\\)" . font-lock-variable-name-face)
   '(" *include\\(?:_next\\)?\\s-+\\(\".*\"\\)\\)" . (1 font-lock-string-face))
   )
  )

(defun limbo-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          (looking-at-open-paren nil)
          (looking-at-keyword nil)
          (looking-at-keyword-open-paren nil)
          (looking-at-close-paren nil)
          (looking-at-start nil)
          (looking-at-regular-line nil)
          (looking-at-comment-line nil)
          (looking-at-empty-line nil)
          (had-empty-line nil)
          (distance 0)
          cur-indent)
      (save-excursion
        (if (looking-at "^[ \t]*$")
            (progn
              (message "looking at empty line")
              (setq looking-at-empty-line t))
          (if (looking-at "^[ \t]*#")
              (progn
                (message "looking at comment")
                (setq looking-at-comment-line t))
            (if (looking-at "^[ \t]*{")
                (progn
                  (message "looking at open paren")
                  (setq looking-at-open-paren t))
              (if (looking-at "[ \t]*\\(=>\\|case\\|else\\|for\\|if\\|while\\).*[ \t]*{")
                  (progn
                    (message "looking at keyword open paren")
                    (setq looking-at-keyword-open-paren t))
                (if (looking-at "[ \t]*\\(=>\\|case\\|else\\|for\\|if\\|while\\)")
                    (progn
                      (message "looking at keyword")
                      (setq looking-at-keyword t))
                  (if (looking-at "^[ \t]*}")
                      (progn
                        (message "looking at close paren")
                        (setq cur-indent (- (current-indentation) tab-width))
                        (setq looking-at-close-paren t))
                    (if (bobp)
                        (progn
                          (message "looking at start")
                          (setq looking-at-start t))
                      (progn
                        (message "looking at regular line")
                        (setq looking-at-regular-line t))
                      )
                    )
                  )
                )
              )
            )
          )
        (while not-indented
          (forward-line -1)
          (setq distance (+ distance 1))
          (if (looking-at "^[ \t]*$")
              (progn (message "Has empty line")
                     (setq had-empty-line t)))
          (if (looking-at "^[ \t]*include")
              (progn
                (message "found include")
                (if (equal looking-at-empty-line t)
                    (progn
                      (setq cur-indent 0)
                      (setq not-indented nil))
                  (if (equal had-empty-line t)
                      (progn
                        (message "Had empty line, discard indent")
                        (setq not-indented nil))
                    (progn
                      (setq cur-indent (+ (current-indentation) tab-width))
                      (setq not-indented nil)))))
            (if (looking-at "[ \t]*\\(case\\|else\\|for\\|if\\|while\\).*[ \t]*{")
                (progn
                  (message "in open paren keyword")
                  (if (equal looking-at-close-paren t)
                      (progn
                        (setq cur-indent (current-indentation))
                        (setq not-indented nil))
                    (progn
                      (setq cur-indent (+ (current-indentation) tab-width))
                      (setq not-indented nil))))
              (if (looking-at "[ \t]*\\(case\\|else\\|for\\|if\\|while\\)")
                  (if (equal looking-at-open-paren t)
                      (progn
                        (message "open paren in keyword")
                        (setq cur-indent (current-indentation))
                        (setq not-indented nil))
                    (if (equal looking-at-close-paren t)
                        (progn
                          (message "close paren in keyword")
                                        ;(setq cur-indent (current-indentation))
                                        ;(setq cur-indent (- (current-indentation) tab-width))
                                        ;(setq not-indented nil)
                          )
                      (if (= distance 1)
                          (progn
                            (message "distance in keyword")
                            (setq cur-indent (+ (current-indentation) tab-width))
                            (setq not-indented nil))
                        (progn
                          (message "else in keyword")
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil)))))
                (if (looking-at ".*{.*}")
                    ()
                  (if (looking-at "^[ \t]*#")
                      ()
                    (if (looking-at ".*{")
                        (progn
                          (message "in open paren")
                          (if (equal looking-at-close-paren t)
                              (progn
                                (setq cur-indent (current-indentation))
                                (setq not-indented nil))
                            (progn
                              (setq cur-indent (+ (current-indentation) tab-width))
                              (setq not-indented nil))))
                      (if (looking-at "^[ \t]*}")
                          (progn
                            (message "in close paren")
                            (if (equal looking-at-close-paren t)
                                (progn
                                  (setq cur-indent (- (current-indentation) tab-width))
                                  (setq not-indented nil))
                              (progn
                                (setq cur-indent (current-indentation))
                                (setq not-indented nil))))
                        (if (bobp)
                            (progn
                              (message "in start")
                              (setq not-indented nil))
                          )
                        )
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
        (progn
          (message "no hint")
          (indent-line-to 0)))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar limbo-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st)
  "Syntax table for limbo-mode")

(define-derived-mode limbo-mode prog-mode "Limbo mode"
  "Major mode for editing limbo files."
  :syntax-table limbo-mode-syntax-table
  (setq-local font-lock-defaults '(limbo-font-lock-keywords))
  (setq-local indent-line-function 'limbo-indent-line)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  )


(provide 'limbo-mode)


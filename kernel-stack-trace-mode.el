(setq kst/keywords
      '(("^\\[[. [:digit:]]+\\] \\[<[[:xdigit:]]+>\\]" . font-lock-constant-face)))

(defalias 'kst/read-file-name (if (fboundp 'ido-read-file-name)
			      'ido-read-file-name
			    'read-file-name)
  "Function to use to find files. Uses `ido-read-file-name' if it
exists, else `read-file-name'")

(defun kst/find-next-addr ()
  "Places point on the next address in the stack trace"
  (interactive)
  (search-forward-regexp "^\\[[. [:digit:]]+\\] \\[<[[:xdigit:]]+>\\]")
  (search-backward "<")
  (forward-char))

(defun kst/find-prev-addr ()
  "Places point on the previous address in the stack trace"
  (interactive)
  (search-backward-regexp "^\\[[. [:digit:]]+\\] \\[<[[:xdigit:]]+>\\]")
  (search-forward "<"))

(defun kst/next-error (&optional argp reset)
  "function to jump to the next item in the stack trace. suitable
  for use as a `next-error-function'"
  (interactive "p")
  ;; we need to run occur-find-match from within the Occur buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
	  (current-buffer)
	(next-error-find-buffer nil nil
				(lambda ()
				  (eq major-mode 'kernel-stack-trace-mode))))
    (kst/find-next-addr)
    (kst/visit-current-addr)))

(defun kst/get-current-elf ()
  "Gets the value of `kst/current-elf' if not nil, else asks
  the user to pick a location for the current vmlinux."
  (interactive)
  (if kst/current-elf
      kst/current-elf
    (kst/set-current-elf)))

(defun kst/set-current-elf ()
  "Sets and returns `kst/current-elf' using
`kst/read-file-name'. TODO: optional arg to just set the elf
directly."
  (interactive)
  (setq kst/current-elf (kst/read-file-name "Select an elf: "))
  kst/current-elf)

(defun kst/visit-current-addr ()
  "Visits the address at point by invoking addr2line and jumping
  to the result"
  (interactive)
  (let* ((addr (thing-at-point 'word))
	 (output (shell-command-to-string (format "addr2line -e %s %s"
						  (kst/get-current-elf)
						  addr)))
	 (filename (substring output
			      0
			      (string-match-p ":" output)))
	 (line-number (string-to-number (substring output
						   (+ 1 (string-match-p ":" output))
						   -1))))
    (find-file-other-window filename)
    (goto-line line-number)
    (recenter)
    (other-window 1)))

(defvar kernel-stack-trace-mode-map nil "Keymap for kernel-stack-trace-mode")

(when (not kernel-stack-trace-mode-map)
  (setq kernel-stack-trace-mode-map (make-sparse-keymap))
  (define-key kernel-stack-trace-mode-map (kbd "n") 'kst/find-next-addr)
  (define-key kernel-stack-trace-mode-map (kbd "p") 'kst/find-prev-addr)
  (define-key kernel-stack-trace-mode-map (kbd "q") 'bury-buffer)
  (define-key kernel-stack-trace-mode-map (kbd "<return>") 'kst/visit-current-addr))

(define-derived-mode kernel-stack-trace-mode fundamental-mode
  (setq font-lock-defaults '(kst/keywords))
  (setq mode-name "kst")
  (setq next-error-function 'kst/next-error)
  (setq kst/current-elf nil)
  (use-local-map kernel-stack-trace-mode-map))

(provide 'kernel-stack-trace-mode)

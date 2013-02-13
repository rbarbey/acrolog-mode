;; allow users to run their code when this mode is run
(defvar acrolog-mode-hook nil)

;; define keymap, if few entries, change to 'make-sparse-keymap'
(defvar acrolog-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'acrolog-next-failure)
    map)
  "Keymap for Acrolinx Logfile major mode")

;; autoload
(add-to-list 'auto-mode-alist '("\\(coreserver\\|ls-[0-9]\\{2\\}\\)\\.log\\(\\.[0-9]+\\)?\\'" . acrolog-mode))

(defvar failure-log-levels-regexp 
  "\\[\\(WARN \\|ERROR\\|FATAL\\|CRITICAL\\)\\].*$"
  "Regexp for failure log messages")

;;;; syntax-highlighting

;; error conditions
(defconst acrolog-font-lock-errors
  (list
   '("\\[\\(WARN \\|ERROR\\|FATAL\\|CRITICAL\\)\\].*$" . font-lock-warning-face)
   '("\\bat [.a-zA-Z():_0-9 \\$]+$" . font-lock-warning-face)
   '("\\b[\\.a-zA-Z]+Exception" . font-lock-warning-face)
   '("TERM trapped.*$" . font-lock-warning-face))
  "Error condition expressions")

;; syntax highlighting of log levels
(defconst acrolog-font-lock-log-levels
  (list
   '("\\[request [0-9]+\\]" . font-lock-keyword-face)
   '("\\(Language Server\\)? ls-[0-9]\\{2\\}-[a-f0-9]+" . font-lock-keyword-face)
   '("^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\} |" . font-lock-preprocessor-face)
   '("Available memory.*$" . font-lock-doc-face)
   '("\\(-->\\|<--\\) Wrapper St.*$" . font-lock-doc-face)
   '("\\[\\(DEBUG\\|INFO \\)\\]" . font-lock-builtin-face)
   )
  "Minimal highlighting expressions for Acrolinx Logfile mode")

(defconst acrolog-font-locks
  (append acrolog-font-lock-errors
	  acrolog-font-lock-log-levels)
  "Default highlighting expression for Acrolog mode")

;; functions

;; next error
(defun acrolog-next-failure ()
  "Jump to next WARN, ERROR, CRITICAL, FATAL log message"
  (interactive)
  (message "Next failure log message")
  (re-search-forward "\\[\\(WARN \\|ERROR\\|FATAL\\|CRITICAL\\)\\].*$")
  (point))

;; 'main' function
(defun acrolog-mode ()
  "Major mode for Acrolinx Core and Language Server log files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acrolog-mode-map)
  
  (set (make-local-variable 'font-lock-defaults) '(acrolog-font-locks))
  
  (setq major-mode 'acrolog-mode)
  (setq mode-name "Acrolog")
  (run-hooks 'acrolog-mode-hook))

(provide 'acrolog-mode)

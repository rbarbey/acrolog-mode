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

(defvar acrolog-mode-syntax-table
  (make-syntax-table text-mode-syntax-table)
  ;;(let ((st (make-syntax-table)))
  ;;  st)
  "Syntax table for acrolog-mode")

;;;; syntax-highlighting

;; error conditions
(defconst acrolog-font-lock-errors
  (list
   '("\\[?\\(WARN \\|ERROR\\|FATAL\\|CRITICAL\\)\\]?.*$" . font-lock-warning-face)
   '("\\s-\\{2,\\}at [.a-zA-Z():_0-9<> \\$]+$" . font-lock-warning-face)
   ;; Exception in thread "qtp-1142714968-33" java.lang.OutOfMemoryError: Java heap space
   '("Exception in thread.*$" . font-lock-warning-face) 
   ;; Stacktraces like
   ;;      at com.ibm.oti.util.Msg.<clinit>(Msg.java:46)
   '("\\b[\\.a-zA-Z]+Exception" . font-lock-warning-face)
   '("TERM trapped.*$" . font-lock-warning-face)
   ;; JVMDUMP039I Processing dump event "systhrow", detail "java/lang/OutOfMemoryError" at 2013/02/12 15:42:26 - please wait.
   ;; JVMPORT030W /proc/sys/kernel/core_pattern setting "|/usr/share/apport/apport %p %s %c"
   ;; Pinging the JVM took 8411 seconds to respond.
   ;; JVM appears hung: Timed out waiting for signal from JVM.  Restarting JVM.
   ;; JVM did not exit on request, terminated
   ;; JVM received a signal SIGKILL (9).
   '("\\(Pinging the \\)?JVM\\( \\|DUMP\\|PORT\\).*$" . font-lock-warning-face)
   )
  "Error condition expressions")

;; syntax highlighting of log levels
(defconst acrolog-font-lock-log-levels
  (list
   ;; access point "2376fe58-7298-4eff-ab69-34ea1374eae1"
   '("access point \"[-[:xdigit:]]+\"" . font-lock-keyword-face)
   ;; session 458152c83fb01402, session id "458152c83fb01402"
   '("session \\(id \"\\)?[[:xdigit:]]+\"?" . font-lock-keyword-face)
   ;; user x, user "x", user x (y)
   '("[uU]ser \"?[a-zA-Z@.0-9]+\"?" . font-lock-keyword-face)
   ;; [request 123], request 123, request id 123, request "123"
   '("\\[?request \\(id \\)?\"?[0-9]+\"?\\]?" . font-lock-keyword-face)
   ;; authentication token r1Y94pijD6z8pDnJ/GZcpj920co61m0FA+7Hwm1WtQfJg9gpEYmW6zpaGexNp7qyjFAAvIvCSUBljmtjVt7R7TEzYb/DIakk/A64YB/M9XIk0cJJj68IdFu9pv2Egzhm7emdI/Ds0Yw0fyNa0L6GqA==
   '("\\(authentication\\|authorization\\) token [a-zA-Z0-9=/+]+" . font-lock-keyword-face)

   ;; Acrolinx Server "0154cbda0717a9eb"
   '("Acrolinx Server \"[[:xdigit:]]+\"" . font-lock-keyword-face)

   '("\\(Language Server \\)?ls-[-[:xdigit:]]\\{3,\\}" . font-lock-keyword-face)
   ;; 2013-02-04 19:03:37,579, 2013-02-04 19:03:37.579
   '("^[-/0-9 :,.]+ |" . font-lock-preprocessor-face)
   '("Available memory.*$" . font-lock-doc-face)
   ;; --> Wrapper Started <-- Wrapper Stopped
   '("\\(-->\\|<--\\) Wrapper St.*$" . font-lock-doc-face)
   '("\\[?\\(DEBUG\\|INFO \\)\\]?" . font-lock-builtin-face)
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
  (re-search-forward "\\[?\\(WARN \\|ERROR\\|FATAL\\|CRITICAL\\)\\]?.*$")
  (point))

;; 'main' function
(defun acrolog-mode ()
  "Major mode for Acrolinx Core and Language Server log files"
  (interactive)
  (kill-all-local-variables)

  (defvar acrolog-mode-syntax-table 
    (let ((st (make-syntax-table text-mode-syntax-table)))
      (modify-syntax-entry ?@ "w" st)
      st))
  
  (set-syntax-table acrolog-mode-syntax-table)
  (use-local-map acrolog-mode-map)
  
  (set (make-local-variable 'font-lock-defaults) '(acrolog-font-locks))
  
  (setq major-mode 'acrolog-mode)
  (setq mode-name "Acrolog")
  (run-hooks 'acrolog-mode-hook))

(provide 'acrolog-mode)

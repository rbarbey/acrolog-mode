acrolog-mode
============

Major mode for Acrolinx Core and Language Server log files

Open tasks (not in any order)

* Code folding (using hs-minor-mode) of 
  * Exception stacktraces
  * check request
  * server restarts?
  * timestamps?
* Better syntax highlighting
* Navigation functions that allow you to jump between
  * server restarts
  * warnings/errors
  * check requests
  * LS initialization requests
* Timestamp-correct merging of core server and language server log files
* Support for SEO server logs
* automatically reloading logs if they have changed on disk (using `revert buffer`[http://www.emacswiki.org/emacs/RevertBuffer])

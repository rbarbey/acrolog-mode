acrolog-mode
============

Major mode for Acrolinx Core and Language Server log files

Installation
------------
To use this majore mode, add the following to your `init.el` file

       (add-to-list 'load-path "~/.emacs.d/acrolog-mode")
       (require 'acrolog-mode)


Open tasks (not in any order)
-----------------------------

* Code folding (using [`hs-minor-mode`](http://www.emacswiki.org/emacs/HideShow)) of 
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
* automatically reloading logs if they have changed on disk (using [`revert buffer`](http://www.emacswiki.org/emacs/RevertBuffer))

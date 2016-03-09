;;; auto-yasnippet-persist-plus -- persistence extension for auto-yasnippet

;;; Author: Jason Milkins <jasonm23@gmail.com>

;;; Package-Requires: ((auto-yasnippet "0.3"))

;;; Commentary:

;;      M-x describe-function RET aya-persist-snippet-plus

;;  Note: This package requires auto-yasnippet,

;;; Code:

(require 'auto-yasnippet)

;;;###autoload
(defun aya-persist-snippet-plus ( name key &optional group)
  "Persist the current snippet, NAME KEY and GROUP can be provided by the user.
The customizable variable, `aya-persist-snippets-dir' will be
used as the root directory for saving snippets.  You will need to
ensure that yasnippet is also configured to scan this directory
for snippets.

The current `major-mode` name will be used to determine the snippets
sub-directory to store the snippet.

For example, if you are in `c-mode`, the snippet will be saved to
~/.emacs.d/snippets/c/name, (assuming the default snippets dir.)

When preceded by `universal-argument' (C-u) you'll be prompted to
supply a snippet group name.  See
https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
for more on groups.

Currently it is necessary to use `yas/reload-all' to load/use newly
persisted snippets."
  (interactive (if (eq aya-current "")
                   (list nil nil nil)
                 (list
                  (read-from-minibuffer "Snippet name: ")
                  (read-from-minibuffer "Snippet key: ")
                  (unless (equal current-prefix-arg nil)
                    (read-string "Snippet group [leave blank for none]: ")))))
  (catch 'exit-clause
    (when (eq aya-current "")
      (message "Aborting: You don't have a current auto-snippet defined.")
      (throw 'exit-clause nil))
    (let* ((snippet nil)
           (mode-snippets-dir nil)
           (snippet-filename nil)
           (groupstring nil)
           (snippet-dir aya-persist-snippets-dir)
           (modename (format "%s" major-mode))
           (mode-snippets-dir (format "%s/%s"
                                      snippet-dir modename))
           (snippet-filename (format "%s/%s.yasnippet"
                                     mode-snippets-dir name)))
      (if (eq group nil)
          (setq groupstring "")
        (setq groupstring (format  "# group: %s\n" group)))
      (setq snippet
            (format (concat
                     "# -*- mode: snippet -*-\n"
                     "# contributor: %s\n"
                     "# name: %s\n"
                     groupstring
                     "# key: %s\n"
                     "# --\n"
                     "%s") user-full-name name key aya-current))
      (unless (file-exists-p mode-snippets-dir)
        (make-directory mode-snippets-dir t))
      (if (file-exists-p snippet-filename)
          (message
           "A snippet called %s already exists in %s, try again, using a different name."
           name mode-snippets-dir)
        (append-to-file snippet nil snippet-filename)))))

(provide 'auto-yasnippet-persist-plus)

;;; auto-yasnippet-persist-plus.el ends here

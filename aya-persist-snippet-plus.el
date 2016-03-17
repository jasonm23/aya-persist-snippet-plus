;;; aya-persist-snippet-plus -- persistence extension for auto-yasnippet

;;; Author: Jason Milkins <jasonm23@gmail.com>

;;; Package-Requires: ((auto-yasnippet "0.3"))

;;; Commentary:
;;
;;      M-x describe-function RET aya-persist-snippet-plus
;;
;; ## Documentation
;;
;; Persist the current aya-snippet to a file, using KEY, GROUP and DESCRIPTION.
;;
;; The custom var, `aya-persist-snippets-dir' is used as root
;; for saved snippets.  Yasnippet should also be configured
;; to scan this directory.
;;
;; The current `major-mode` name will be used to determine the
;; sub-directory to store the snippet.
;;
;; e.g. when in `c-mode`, a aya-snippet will be saved to
;; ~/.emacs.d/snippets/c/{key}
;;
;; When preceded by `universal-argument' you'll be prompted to
;; supply a snippet group name.  See
;; https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
;; for more on groups.
;;
;; If preceded by double `universal-argument', you'll be
;; prompted to supply a group and snippet description.
;;
;; Convenience functions to make group and or description mandatory,
;; are included in this package.
;;
;; Set `aya-persist-snippets-autoreload' to non-nil to have new snippets
;; available for use, immediately after saving.

;;; Code:

(require 'auto-yasnippet)

;;;###autoload
(defun aya-persist-snippet-plus (key &optional group description)
  "Persist the current aya-snippet to a file, using KEY, GROUP and DESCRIPTION.

The custom var, `aya-persist-snippets-dir' is used as root
for saved snippets.  Yasnippet should also be configured
to scan this directory.

The current `major-mode` name will be used to determine the
sub-directory to store the snippet.

e.g. when in `c-mode`, a aya-snippet will be saved to
~/.emacs.d/snippets/c/{key}

When preceded by `universal-argument' you'll be prompted to
supply a snippet group name.  See
https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
for more on groups.

If preceded by double `universal-argument', you'll be
prompted to supply a group and snippet description.

Convenience functions to make group and or description mandatory,
are included in this package.

Set `aya-persist-snippets-autoreload' to non-nil to have new snippets
available for use, immediately after saving."

  (interactive (if (eq aya-current "")
                   (list nil nil nil)
                 (list
                  (read-from-minibuffer "Snippet name: ")
                  (read-from-minibuffer "Snippet key: ")
                  (when current-prefix-arg
                    (read-string "Snippet group [blank for none]: ")))))
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

(provide 'aya-persist-snippet-plus)

;;; aya-persist-snippet-plus.el ends here

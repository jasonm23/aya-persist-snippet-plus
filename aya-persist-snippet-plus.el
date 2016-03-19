;;; aya-persist-snippet-plus -- persistence extension for auto-yasnippet

;;; Author: Jason Milkins <jasonm23@gmail.com>

;;; Version: 0.0.1

;;; Package-Requires: ((auto-yasnippet "0.3"))

;;; Commentary:
;;
;; Persist the current aya-snippet to a file, using KEY, NAME and GROUP.
;;
;; The custom var, `aya-persist-snippets-dir' is used as root
;; for saved snippets.  Yasnippet should also be configured
;; to scan this directory.
;;
;; The current `major-mode` name will be used to determine the
;; sub-directory to store the snippet.
;;
;; e.g. when in `c-mode' the current `aya-snippet' will be saved to
;; ~/.emacs.d/snippets/c/{key}
;;
;; When preceded by `universal-argument' you'll be prompted to
;; supply a snippet name.
;;
;; If twice preceded by `universal-argument', you'll also be
;; prompted to supply a snippet group (ie.  ctrl-U twice)
;;
;; See:
;; https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
;; for more on groups.
;;
;; Set `aya-persist-snippets-autoreload' as t to have new persisted snippets
;; loaded immediately for use.
;;
;;; Code:

(require 'auto-yasnippet)
(require 'cl)

;;;###autoload
(defun aya-current-active-p ()
  "When aya-current is active return t."
  (and (boundp 'aya-current) (not (string= aya-current ""))))

(defvar aya-last-persisted-snippet-file nil
  "Path filename of the last persisted filename.")

(defcustom aya-persist-snippets-autoreload t
  "Automatic loading of new persisted auto yasnippets.")

(defcustom aya-contributor-name nil
  "Contributor name for persisted snippets.")

;;;###autoload
(defun aya-persist-snippet-plus (key &optional name group)
"Persist the current aya-snippet to a file, using KEY, NAME and GROUP.

The custom var, `aya-persist-snippets-dir' is used as root
for saved snippets.  Yasnippet should also be configured
to scan this directory.

The current `major-mode` name will be used to determine the
sub-directory to store the snippet.

e.g. when in `c-mode` the current `aya-snippet' will be saved to
~/.emacs.d/snippets/c/{key}

When preceded by `universal-argument' you'll be prompted to
supply a snippet name.

If twice preceded by `universal-argument', you'll also be
prompted to supply a snippet group (ie.  ctrl-U twice)

See:
https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
for more on groups.

Set `aya-persist-snippets-autoreload' as t to have new persisted snippets
loaded immediately for use."
  (interactive (if (aya-current-active-p)
                   (list
                    (read-from-minibuffer "Snippet key: ")
                    (when current-prefix-arg
                      (read-from-minibuffer "Snippet name: "))
                    (when (>= (prefix-numeric-value current-prefix-arg) 16)
                      (read-from-minibuffer "Snippet group: ")))
                 (list nil nil nil)))
  (unless (aya-current-active-p)
    (message "Aborting: You don't have a current auto-snippet defined") (return-from aya-persist-snippet-plus))
  (let* ((user-full-name (or aya-contributor-name user-full-name))
         (snippet nil)
         (groupstring nil)
         (snippet-dir aya-persist-snippets-dir)
         (mode-snippets-dir (format "%s/%s" snippet-dir major-mode))
         (snippet-filename (format "%s/%s" mode-snippets-dir key)))
    (setq snippet
          (concat
           "# -*- mode: snippet -*-\n"
           (when user-full-name
             (format "# contributor: %s\n" user-full-name))
           (when name
             (format "# name: %s\n" name))
           (when group
             (format  "# group: %s\n" group))
           "# --\n"
           aya-current))
          (unless (file-exists-p mode-snippets-dir) (make-directory mode-snippets-dir t))
          (if (file-exists-p snippet-filename)
              (message "A snippet called %s already exists in %s, try again, using a different key." key mode-snippets-dir)
            (progn (append-to-file snippet nil snippet-filename)
                   (setq aya-current "") ;; reset the current autosnippet
                   (setq aya-last-persisted-snippet-file snippet-filename)
                   (when aya-persist-snippets-autoreload (yas-reload-all))))))

;;;###autoload
  (defun* aya-persist-open-last-snippet ()
    "Open the last persisted snippet in a buffer.

Uses `aya-last-persisted-snippet-file'"
    (interactive)
    (when (string= aya-last-persisted-snippet-file "") (return-from aya-persist-snippet-open-last))
    (let ((find-file-wildcards nil))
      (find-file aya-last-persisted-snippet-file)))

  (provide 'aya-persist-snippet-plus)

;;; aya-persist-snippet-plus.el ends here

# Aya-persist-snippet-plus

An extension for [abo-abo/auto-yasnippet](https://github.com/abo-abo/auto-yasnippet)

## Description

Persist the current aya-snippet to a file, using KEY, GROUP and DESCRIPTION.

The custom var, `aya-persist-snippets-dir` is used as root
for saved snippets.  Yasnippet should also be configured
to scan this directory.

The current `major-mode` name will be used to determine the
sub-directory to store the snippet.

e.g. when in `c-mode`, a aya-snippet will be saved to
~/.emacs.d/snippets/c/{key}

When preceded by `universal-argument` you'll be prompted to
supply a snippet group name.  See [documentation](https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4)
for more on groups.

If preceded by double `universal-argument`, you'll be
prompted to supply a group and snippet description.

Convenience functions to make group and or description mandatory,
are included in this package.

Set `aya-persist-snippets-autoreload` to non-nil to have new snippets
available for use, immediately after saving.

# Aya-persist-snippet-plus

An extension for [abo-abo/auto-yasnippet](https://github.com/abo-abo/auto-yasnippet)

## Description

Persist the current aya-snippet to a file, using `KEY`, `NAME` and `GROUP`.

The custom var, `aya-persist-snippets-dir` is used as root
for saved snippets.  Yasnippet should also be configured
to scan this directory.

The current `major-mode` name will be used to determine the
sub-directory to store the snippet.

e.g. when in `c-mode` the current `aya-snippet` will be saved to
`~/.emacs.d/snippets/c/{key}`

When preceded by `universal-argument` you'll be prompted to
supply a snippet name.

If twice preceded by `universal-argument`, you'll also be
prompted to supply a snippet group (ie.  ctrl-U twice)

See:
https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
for more on groups.

Set `aya-persist-snippets-autoreload` as `t` to have new persisted snippets
loaded immediately for use.

# Emacs auto yasnipppet persist plus

In Emacs

     M-x describe-function RET aya-persist-snippet-plus

## Aya-persist-snippet-plus

Persist the current snippet, NAME KEY and GROUP can be provided by the user.
The customizable variable, `aya-persist-snippets-dir` will be
used as the root directory for saving snippets.  You will need to
ensure that yasnippet is also configured to scan this directory
for snippets.

The current `major-mode` name will be used to determine the snippets
sub-directory to store the snippet.

For example, if you are in `c-mode`, the snippet will be saved to
`~/.emacs.d/snippets/c/{name}`, (assuming the default snippets dir.)

When preceded by `universal-argument` (C-u) you'll be prompted to
supply a snippet group name.  See
https://capitaomorte.github.io/yasnippet/snippet-development.html#sec-2-4
for more on groups.

Currently it is necessary to use `yas/reload-all' to load/use newly
persisted snippets.

(This is likely to change as optional, soon)

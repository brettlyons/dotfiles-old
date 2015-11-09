This library configures Magit and Evil to play well with each
other. For some background see https://github.com/magit/evil-magit/issues/1.

Installation and Use
====================

Everything is contained in evil-magit.el, so you may download and load that file
directly. The recommended method is to use MELPA via package.el (`M-x
package-install RET evil-magit RET`).

Evil and Magit are both required. After requiring those packages, the following
will setup the new key bindings for you.

optional: this is the evil state that evil-magit will use
(setq evil-magit-state 'motion)
(require 'evil-magit)

Use `evil-magit-revert` to revert changes made by evil-magit to the default
evil+magit behavior.

Key Bindings
============

The basic key binding scheme is described in the following tables.

   Command              | Old  | New
  ----------------------|----- |------
   cherry pick          | a/A  |
   branch               | b    |
   bisect               | B    |
   commit               | c    |
   diff                 | d/D  |
   help                 | h/?  |
   ediff                | e/E  |
   fetch                | f    |
   pull                 | F    |
   ignore               | i/I  |
   jump                 | j    | g
   delete               | k    | x
   untrack              | K    | X
   log                  | l/L  |
   merge                | m    |
   remote               | M    |
   next section         | n    | C-j
   next section sibling | M-n  | gj or ]
   submodule            | o    | C-o
   prev section         | p    | C-k
   prev section sibling | M-p  | gk or [
   push                 | P    |
   rebase               | r    |
   refresh              | g    | gr/gR
   rename               | R    |
   stage                | s/S  |
   tag                  | t    |
   notes                | T    |
   unstage              | u/U  |
   revert               | v/V  | o/O
   am                   | w    |
   patch                | W    |
   reset                | x    | C-r
   show-refs            | y    |
   cherry               | Y    |
   stash                | z/Z  |
   git-cmd              | :    | \ |
   run                  | !    |

Evil-specific commands and more

   Command                     | New
  -----------------------------|--------
   evil-goto-line              | G
   evil-next-visual-line       | j
   evil-previous-visual-line   | k
   evil-search-next            | n
   evil-search-previous        | N
   set-mark-command            | v or V
   evil-ex                     | :
   evil-search-forward         | /
   evil-scroll-page-up         | C-b
   evil-scroll-down            | C-d
   evil-scroll-page-down       | C-f
   evil-scroll-up              | C-u (if C-u scroll enabled)
   evil-emacs-state            | C-z

Any other bindings are meant to be consistent with these.

Disclaimer
==========

Given the complexity of magit key bindings combined with the complexity of git
itself, it is possible that there are some rough edges where the current binding
is not the expected one in a buffer. It will be very helpful for you to report
any such instances.

maps changed

1. git-commit-mode-map
2. git-rebase-mode-map
3. magit-mode-map
4. magit-blame-mode-map
5. magit-blob-mode-map
6. magit-diff-mode-map
7. magit-log-mode-map
8. magit-log-select-mode-map
9. magit-popup-mode-map
10. magit-reflog-mode-map
11. magit-status-mode-map

S1. magit-commit-section-map
S2. magit-file-sections-map
S3. magit-hunk-section-map
S4. magit-staged-section-map

maps unchanged

12. magit-cherry-mode-map
13. magit-file-mode-map
14. magit-log-read-revs-map
15. magit-minibuffer-local-ns-map
16. magit-process-mode-map
17. magit-refs-mode-map
18. with-editor-mode-map

S5. magit-branch-section-map
S6. magit-module-commit-section-map
S7. magit-remote-section-map
S8. magit-stash-section-map
S9. magit-stashes-section-map
S10. magit-tag-section-map
S11. magit-unpulled-section-map
S12. magit-unpushed-section-map
S13. magit-unstaged-section-map
S14. magit-untracked-section-map

TODO

#!/bin/bash
# Output the full #[bg=COLOR] tmux style directive for the current second's
# parity. Used by .tmux.conf's window-status-format to pulse a window's bg
# while the window has the @cc_running user-option set.
#
# This must live in a script (rather than inline in .tmux.conf) because
# tmux's #() parser counts [ ] inside the source format, which breaks if
# the perl one-liner contains literal #[bg=...] in its argument.
exec perl -e 'print time%2 ? q!#[bg=colour94]! : q!#[bg=colour22]!'

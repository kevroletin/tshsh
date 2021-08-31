# Demo

Implemented features.

## Switch between interactive shells

* we hit `Ctrl+x s s` to open shh shell;
* we hit `Ctrl+x s z` to open zsh shell;
* we hit `Ctrl+x s p` to open python3 shell;
* we hit `Ctrl+z` a couple of times to quickly switch between python and zsh.

![Ctrl-z](../assets/demo_c_z.svg?raw=true "Title")

## Copy output of a previous command

Copying the output of the previous command is useful:

* we hit `Ctrl+x s p` to open python shell and evaluate an expression there;
* we hit `Ctrl+z` to switch back to zsh;
* we hit `Ctrl+p` paste output of a previous command
* then we repeat a simi# Demo

![Clipboard](../assets/demo_copy_out.svg?raw=true "Title")

## Synchronize current working directory

We change the current working directory and switch between python and zsh. After
each switch tshsh evaluates `cd` command for us.

![Sync cwd](../assets/demo_sync_cd_shell.svg?raw=true "Title")

## Synchronize current working directory using a TUI app

This demo is similar to a previous one but using a tui app ranger. Currently
TUI apps don't work well enough because TUI apps might change terminal state
which they usually restore after exiting. tshsh's switching logic doesn't track
terminal state changes and hence might leave terminal "dirty" (one example is
image preview in rxvt).

![Sync cwd ranger](../assets/demo_sync_cd_ranger.svg?raw=true "Title")

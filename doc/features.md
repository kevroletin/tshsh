# Demo

[Almost] implemented features.

## C-z to switch between interactive shells

Here we hit Ctrl+z a couple of times to switch between python and zsh.

By itself, this feature is not very impressive. Usually, one can press
`Ctrl+z` to suspend python repl and then type `%` to resume it afterward.

However, `tshsh` can capture repl's output and can send input. That's why
it can build more sophisticated features which make switching useful and time
saving.

![Ctrl-z](../assets/demo_c_z.svg?raw=true "Title")

## Copy output of a previous command

Copying the output of the previous command is useful:

* to "quickly compute" something using a programming language repl and return
  back to a shell with the result;
* to "quickly execute" a shell command while working in a repl (for example, to
  change a current directory or perform a file search);

Step by step demo walkthrough:

* execute `[x**2 for x in range(1,10)]` in python repl;
* hit `Ctrl-z` and switch to `zsh`;
* clipboard contains output from the python command, so we can insert it from a clipboard;
* execute `echo $TERM` in `zsh`;
* hit `Ctrl-z` and switch back to `python`;
* clipboard contains `rxvt-unicode-256color` (which is my terminal emulator).

![Clipboard](../assets/demo_copy_out.svg?raw=true "Title")

## Synchronize current working directory

It might be hard to understand what is going on in this demo because `tshsh`
executes `cd` commands and they clutter output (we will declutter the output
once `tshsh` comes to a certain level of maturity). So step by step explanation:

* in `python` repl execute `os.getcwd()` to print current directory;
* hit `Ctrl-z` and switch to `zsh`;
* `tshsh` executes `cd` command for us, to make sure `zsh` has the same working directory as `python`;
* we execute `cd /tmp`;
* hit `Ctrl-z` and switch to `python`;
* `tshsh` executes `os.chdir('/tmp')` command for us
* we execute `os.chdir('/home')`
* hit `Ctrl-z` and switch to `zsh`;
* `tshsh` executes `cd /home` command for us

![Sync cwd](../assets/demo_sync_cd_shell.svg?raw=true "Title")

## Synchronize current working directory using a TUI app

Conceptually it's the same idea of synchronizing a working directory but using
a TUI app. Making this work in the general case for any TUI app is a complicated
task. This demo doesn't always work well, and we cut only a good part
out of a long recording ^_^

![Sync cwd ranger](../assets/demo_sync_cd_ranger.svg?raw=true "Title")

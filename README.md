# tshsh

tshsh (**T**erminal, **SH**ell, **SH**ell) lets you switch between two interactive shells
while working in a single terminal.

## Problem statement

High-level languages 

* are great for scripting and interactive programming using a repl;
* **aren't great** as a replacement for an interactive shell such as `bash`, `zsh`, `fish` etc.

`tshsh` helps the user:

* to simultaneously work with two interactive shells (and get the best of the two worlds);
* automates synchronizing *(copy-pasting)* values from one shell to the other.

An example. I like Haskell, and I often use it for small automation tasks.
However, a Haskell interactive repl `ghci` lacks many features which my `zsh`
setup has. So each time I am hacking on Haskell code, I keep two terminals, one
for `ghci` and the other for `zsh`. Then I manually copy-paste values from one
terminal to the other one. Instead of keeping two terminals, I might press
Ctrl+z to suspend `ghci` and then resume it by using `%` but I still need to
copy-paste between two shells `¯\_(ツ)_/¯`.

## Features

Here is one of the demos to demonstrate: 
* switching between `python` and `sh` by pressing Ctrl-z;
* automatic synchronization of a working directory
* copying of a previous command output to clipboard;

First, we switch between `python` and `sh` by pressing Ctrl-z. Then we change
directory and discover that after switching `tshsh` evaluates `cd` command to
synchronize the current working directory. Then we evaluate "1+2" in python, switch
to `sh` and discover that the clipboard contains the result of the last
expression evaluated in `python`:

![Demo](./assets/main_page_demo.svg?raw=true "Demo")

For more details see [Features](./doc/features.md) and [Roadmap](./doc/features.md).

## How does it work

```
.-----------.
| sh-4.4$ _ | <--- tshsh ---> sh
|           |                 python
'-----------'

^Z

.-----------.
| Type "help| <--- tshsh      sh
| >>>       |           \---> python
'-----------'
```

Under the hood, tshsh spawns two shell processes and attaches them to two
virtual tty devices. Then it marshals input/output, signals back and forth and
synchronizes tty state, terminal state, and optionally chosen shells state (pwd,
environment variables, etc). It's very similar to how
[script](https://man7.org/linux/man-pages/man1/script.1.html) works; except that
script spawns only one puppet shell but tshsh spawns two. One also can think of
tshsh as a very primitive terminal multiplexer (i.e.
[screen](https://man7.org/linux/man-pages/man1/screen.1.html)); except that
tshsh doesn't draw any tui.

For more details see [Design](./doc/design.md).

## Goals 

* non-intrusiveness: no source code or config modifications of puppet shells
* responsiveness (one can safely drop the program into ~/.bashrc without
  worrying about shell initialization time, responsiveness, and memory
  consumption).

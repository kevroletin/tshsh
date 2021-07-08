# tshsh

tshsh (Terminal, SHell, SHell) - is an "ctrl+z" implementation which works for
shells. It allows to switch between two interactive shells while working in the
single terminal.


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

Under the hood tshsh spawns two shell processes and attaches them to a two
virtual tty devices. Then it marshals input/output, signals back and forth and
synchronizes tty state, terminal state, and optionally chosen shells state (pwd,
environment variables, etc). It's very similar to how
[script](https://man7.org/linux/man-pages/man1/script.1.html) works; except that
script spawns only one puppet shell but tshsh spawns two. One also can think of
tshsh as very primitive terminal multiplexer (i.e.
[screen](https://man7.org/linux/man-pages/man1/screen.1.html)); except that
tshsh doesn't draw any tui.

# Terminology

For simplicity we'll refer to a gui terminal as `xterm` or a `terminal`. We'll
refer to a interactive programs spawned and managed by tshsh as `puppets` or
`shells`. When we say `tty` we refer to a pair of file descriptors in `/dev/*`
directory or/and kernel logic which reacts/transform inputs from mentioned
files.

# Implementation

User input
----------
         (1)              (2)
xterm -> tty -> zshzsh -> pup_tty -> sh
          :                :
       raw mode           mode configured by sh

A user types commands into xterm, a puppet is supposed to receive text input and
signals.

We set tty(1) into raw-mode without echo and flow control (TODO: check it in the
code); but ask it to generate some of interrupts which we care about. So that
tty doesn't do any unwanted modifications to the input from a user. Hence we can
just pass it to a puppet's pup_tty(2). The only expected modifications are
generating signals when user presses ^C, ^Z, etc. (see how we handle signals).
Puppet's virtual tty is configured by a puppet itself, so it migh do some
modifications or echo, but it's expected by a puppet itself.

In theory we handle signals in two ways: parse raw input from user or ask tty to
parse it and generate signals for us. (TODO: what do we choose and why?).

Except from typing commands user also can paste data from a X clipboard. This is
a special case because of bracketed paste mode. bracketed paste mode is an
option of xterm but it's configured by a puppet using ansi escape codes. We have
a dedicated section for it.

Output
------

         (1)              (2)
xterm <- tty <- zshzsh <- pup_tty <- sh
  :
  configured by sh
  using ansi escape seq.

There are several dark corners in handling output from a puppet:
* TODO: unicode: while switching puppets we might accidentally output a half of
  unicode character; it seems like xterm doesn't die from this;
* xterm state;

  when puppet outputs ansi escape sequence then it can modify the state of
  xterm; The sequences are terminal specific (run `infocmp -L $TERM` to check
  yours) and aren't handled by tty;

Switch
------

On switch we need to:
1) synchronize relevant state (TODO: what is relevant):
   * tty state
   * xterm state
   * cwd (current working directory)
   * environment variables
   for each part of the state we need a way to read and write it
2) restore user interface to a usable state:
   * detect and remove not cleaned tui
   * show prompt or restore tui of a new puppet

Providing "smooth" user experience required understanding of behavior and current
state of a puppet.

| Prev                    | Next                    | Comment                                                            |
|-------------------------|-------------------------|--------------------------------------------------------------------|
| shell waiting for input | shell waiting for input | need to draw a prompt of a new shell                               |
| actively outputing cmd  |                         | -                                                                  |
| partly inputted command |                         | -                                                                  |
|                         | partly inputted command | restore prompt, command and cursor position                        |
|                         | actively outputing cmd  | continue dumping output
| tui                     |                         | clean parts of tui                                                 |
|                         | tui                     | readraw tui or allocate enough new lines and ask the app to redraw |

Parsing
-------

As described previously, after switch we want to restore a user interface to a
sensible state. To do this we want to understand a state of a puppet shell. In
particular, we want to distinct between:
* text-cmd :: a command is running; it writes plain text as output;
* tui-cmd :: a tui program is running; it output text mixed with ansi-escape sequences to
  render it's interface;
* prompt-init :: shell has written a prompt and it's waiting for user input;
* prompt-in-progress :: user is typing a command; shell interacts with a user and draws auto
  completion, suggestions, etc.

Unfortunately, most of the shells don't provide structured output. Instead they
just output a stream of characters. Moreover there is a special case, when
background jobs write output while running in background; hence output from
several commands might be mixed.

Nevertheless, humans are able to make sense of what is happening in the
terminal. Hence software should be able to parse shell output and figure out
boundaries between commands, associate output with a command, figure out if it's
a tui app or a command which outputs plain text.

TODO: humans are confused when background jobs output. Require `stty tostop`.

Conceptually input and output happen asynchronously. That makes it a little
harder to associate user commands with a command output. We can simplify things
a little by assuming that shell (or tty) will echo user input. Hence instead of
parsing both input and output we can parse only output and take both user input
and command output from a same stream of characters.

```
input  -> .-----.
          | tty |
output <- '-----'
```

Ansi codes which change XTerm state
-----------------------------------

TODO: bracketed paste mode and maybe others; mention terminfo

# Goals 

* non intrusiveness: no source code or config modifications of puppet shells
* responsiveness

# Non-goals

* portability

# Planned features

* switch shells using a key binding
* synchronize cwd and env variables on switch
* restore partially typed command
* capture text output of a previous command
* handle tui in a meaningful way (maybe send WINCH with a hope that tui app will
  redraw itself, send ^R, ^L)

# Current problems

* works only for non-tui shells; switching back/forth vim will not work
* signals processing is incomplete:
  * termination of puppets is not handled; so if one stops a puppet with ctrl+d
    then they need to close tshsh with ctrl+c;
  * ctrl+c kills tshsh while it should propagate to children;
* something with sessions management is wrong: ps from forked zsh shows too many
  processes
* unicode handling is broken (we receive/send bytestrings so there is
  possibility that during switch we will send only half of unicode character
  from one puppet and then switch to the other);
* output from an inactive puppet is just discarded; as a result we can miss ansi
  escaped sequences which try to configure terminal (especially bracket paste
  mode);
* bracket paste mode was a surprise for us; we need to investigate more, which
  modes can be set using ansi escaped sequences;

# Links

* https://www.linusakesson.net/programming/tty/
* https://blog.nelhage.com/2009/12/a-brief-introduction-to-termios/

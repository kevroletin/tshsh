# tshsh

Currently, a toy project to play with Haskell memory management, profiling, Linux
system programming and simple DSLs.

---

tshsh (Terminal, SHell, SHell) - is a "ctrl+z" implementation that works for
shells. It allows switching between two interactive shells while working in a
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

Under the hood, tshsh spawns two shell processes and attaches them to two
virtual tty devices. Then it marshals input/output, signals back and forth and
synchronizes tty state, terminal state, and optionally chosen shells state (pwd,
environment variables, etc). It's very similar to how
[script](https://man7.org/linux/man-pages/man1/script.1.html) works; except that
script spawns only one puppet shell but tshsh spawns two. One also can think of
tshsh as a very primitive terminal multiplexer (i.e.
[screen](https://man7.org/linux/man-pages/man1/screen.1.html)); except that
tshsh doesn't draw any tui.

## Terminology

For simplicity, we'll refer to a GUI terminal as `xterm` or a `terminal`. We'll
refer to an interactive program spawned and managed by tshsh as `puppets` or
`shells`. When we say `tty` we refer to a pair of file descriptors in `/dev/*`
the directory or/and kernel logic which reacts/transform inputs from mentioned
files.

## Implementation

### User input


```
         (1)              (2)
xterm -> tty -> zshzsh -> pup_tty -> sh
          :                :
       raw mode           mode configured by sh
```

A user enters commands into xterm, a puppet is supposed to receive text input and
signals.

We set tty(1) into raw-mode without echo and flow control, but ask it to
generate some of the interrupts which we care about. So that tty doesn't do any
unwanted modifications to the input from a user. Hence we can just pass raw user
input to a puppet's pup_tty(2). The only expected modifications that we asked for from
tty(1) are generating certain signals when a user presses ^C, ^Z, etc. (see how we
handle signals). Puppet's virtual tty is configured by a puppet itself; puppet
will get expected behavior because tty(1) is in raw mode, so all the
modifications of input happen in pup_tty(2) according to the puppet's requested
configuration; it seems like tty doesn't change the output from a puppet
and just passes it to an xterm (TODO: find a proof).

In theory, we can handle signals in two ways: parse raw input from a user or ask
tty to parse it and generate signals for us. For now, we ask tty to generate
signals for simplicity.

Except for typing commands, users also can paste data from an X clipboard. This is
a special case because of bracketed paste mode. bracketed paste mode is an
option of xterm but it's configured by a puppet using ANSI escape codes. We have
a dedicated section for it.

### Puppet output

```
         (1)              (2)
xterm <- tty <- zshzsh <- pup_tty <- sh
  :
  configured by sh
  using ANSI escape seq.
```

There are several dark corners in handling output from a puppet:
* Unicode: currently we don't parse Unicode so while switching puppets we might
  accidentally output half of Unicode character; it seems like xterm doesn't
  die from this and the worst thing which we might observe is 1 broken character
  which seems acceptable (TODO: find a proof);

* xterm state;

  when puppet outputs ANSI escape sequence then it can modify the state of an
  xterm; The sequences are terminal-specific and aren't handled by tty (run
  `infocmp -L $TERM` to check your terminal using terminfo database); ANSI
  sequences might change background/foreground color, window title, toggle
  bracket paste mode, ask a terminal to display a picture, change the cursor
  position, etc. Some shells set many xterm properties on each prompt, while
  others don't.
  
  Ideally, we should track xterm state changes from each puppet and restore them
  after switching. For example, one puppet might change foreground text style,
  we might cancel this style on a switch and then set it once more why switching
  back again. For now, we don't have these features

### Background puppet output

For now, we just ignore the output from a background puppet. And alternative
would be to accumulate it in a temporal file and make it available by user
request.

### Switching

On the switch we need to:
1) synchronize relevant state (TODO: what is relevant):
   * tty state
   * xterm state
   * cwd (current working directory)
   * environment variables
   for each part of the state, we need a way to read and write it
2) restore user interface to a usable state:
   * detect and remove tui
   * show prompt or restore tui of a new puppet

Providing a "smooth" user experience required an understanding of behavior and current
state of a puppet.

| Prev                    | Next                    | Comment                                                            |
|-------------------------|-------------------------|--------------------------------------------------------------------|
| shell waiting for input | shell waiting for input | need to draw a prompt of a new shell                               |
| actively outputting cmd  |                         | -                                                                  |
| partly inputted command |                         | -                                                                  |
|                         | partly inputted command | restore prompt, command and cursor position                        |
|                         | actively outputting cmd  | continue dumping output                                            |
| tui                     |                         | clean parts of tui                                                 |
|                         | tui                     | redraw tui or allocate enough new lines and ask the app to redraw |

### Parsing

As described previously, after the switch we want to restore a user interface to a
sensible state. To do this we want to understand a state of a puppet shell. In
particular, we want to distinct between:
* text-cmd :: a command is running; it writes plain text as output;
* tui-cmd :: a tui program is running; it output text mixed with ANSI-escape sequences to
  render its interface;
* prompt-init :: shell has written a prompt and it's waiting for user input;
* prompt-in-progress :: the user is typing a command; shell interacts with a user and draws auto
  completion, suggestions, etc.

Unfortunately, most of the shells don't provide structured output. Instead, they
just output a stream of characters. Moreover, there is a special case, when
background jobs write output while running in the background; hence output from
several commands might be mixed.

Nevertheless, humans can make sense of what is happening in the
terminal. Hence software should be able to parse shell output and figure out
boundaries between commands, and associate output with a command, figure out if
it's a tui app or a command which outputs plain text.

TODO: humans are confused when background jobs output. Require `stty tostop`.

Conceptually input and output happen asynchronously. That makes it a little
harder to associate user commands with command output. We can simplify things
a little by assuming that shell (or tty) will echo user input. Hence instead of
parsing both input and output we can parse the only output and take both user input
and command output from the same stream of characters.

### ANSI codes that change XTerm state

TODO: bracketed paste mode, background/foreground color, and maybe others; mention terminfo

## Goals 

* non-intrusiveness: no source code or config modifications of puppet shells
* responsiveness (one can safely drop the program into ~/.bashrc without
  worrying about shell initialization time, responsiveness, and memory
  consumption).

## Non-goals

* portability

## Planned features

* [Done] switch shells using a key binding
* [Done] synchronize cwd 
* synchronize env variables after switching
* synchronize xterm state (only a selected subset)
* config file, the default configuration for popular shells
* interactive configuration dialog, help writing a parser for a prompt
* restore partially typed command
* capture text output of a previous command
* handle tui in a meaningful way (maybe send WINCH with the hope that the tui app will
  redraw itself, send ^R, ^L)

## Current problems

* no config file
* works only for non-tui shells; switching back/forth vim will not work
* signals processing is incomplete:
  * termination of puppets is not handled; so if one stops a puppet with ctrl+d
    then they need to close tshsh with ctrl+c;
  * ctrl+c kills tshsh while it should propagate to children;
* something with sessions management is wrong: ps from forked zsh shows too many
  processes
* output from an inactive puppet is just discarded; as a result, we can miss ANSI
  escaped sequences which try to configure terminal (especially bracket paste
  mode);
* a few hard-coded hacks like disabling bracket paste mode.

## Links

* https://www.linusakesson.net/programming/tty/
* https://blog.nelhage.com/2009/12/a-brief-introduction-to-termios/

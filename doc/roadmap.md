# Roadmap

## Planned features

* [Done] switch shells using a key binding
* [Done] synchronize cwd 
* [Done] capture text output of a previous command
* [Done] lazy puppet startup
* [Done] restart a puppet after it terminates
* synchronize env variables after switching
* synchronize xterm state (cursor visibility, background/foreground colors,
  etc.)
* config file, the default configuration for popular shells
* interactive configuration dialog, help writing a parser for a prompt
* [Maybe] restore partially typed command
* handle tui in a meaningful way (maybe send WINCH with the hope that the tui app will
  redraw itself, send ^R, ^L)

## Current problems

* no config file
* switching back/forth to TUI apps works for VIM and ranger but might not work
  for other apps
* output from an inactive puppet is just discarded; as a result, we can miss ANSI
  escaped sequences which try to configure terminal (especially bracket paste
  mode);
* a few hard-coded hacks like disabling bracket paste mode.
* no packaging procedure; executing shell commands like stty, xclip, kill and our
  acquire_tty_wrapper
* no user guide

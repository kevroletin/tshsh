# Roadmap

## Planned features

* [Done] switch shells using a key binding
* [Done] synchronize cwd 
* [Done] lazy puppet startup
* [Done] capture text output of a previous command
* synchronize env variables after switching
* synchronize xterm state (cursor visibility, background/foreground colors,
  etc.)
* config file, the default configuration for popular shells
* interactive configuration dialog, help writing a parser for a prompt
* [Maybe] restore partially typed command
* handle tui in a meaningful way (maybe send WINCH with the hope that the tui app will
  redraw itself, send ^R, ^L)
* restart a puppet after it terminates

## Current problems

* no config file
* switching back/forth to TUI apps works for VIM and ranger but might not work
  for other apps
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
* no packaging procedure; executing shell commands like stty, xclip, kill
* no user guide

# Roadmap

## Planned features

### alpha1
* [Done] switch shells using a key binding
* [Done] synchronize cwd 
* [Done] capture text output of a previous command
* [Done] lazy puppet startup
* [Done] restart a puppet after it terminates
* [Done] handle tui in a meaningful way
* copy-paste previous command output using a key binding
  * maybe we should ignore empty command output
* edit previous command output in an editor
  * make command configurahble
* config file, default configuration for popular shells
* limit amount of buffered command output

### Following releases
* e2e tests for TUI apps
* multiple puppets
* configurable leader-key keybindings
* tui for commands and puppet statuses
* escape sequences debugger: turn escape sequences into human-readable explanation
* interactive configuration dialog, help writing a parser for a prompt
* synchronize env variables after switching
* track xterm state (window title, cursor visibility, background/foreground colors, etc.)

## Current problems

* output from an inactive puppet is just discarded; as a result, we can miss ANSI
  escaped sequences which try to configure terminal (especially bracket paste
  mode);
* window title is not cleaned after switching

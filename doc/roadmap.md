# Roadmap

## Planned features

### alpha1
* [Done] switch shells using a key binding
* [Done] synchronize cwd 
* [Done] capture text output of a previous command
* [Done] lazy puppet startup
* [Done] restart a puppet after it terminates
* [Done] handle tui in a meaningful way
* [Done] copy-paste previous command output using a key binding
* [Done] edit previous command output in an editor
* [Done] multiple predefined puppets
* regular expressions for prompt matcher
* limit amount of buffered command output
* config file, default configuration for popular shells
* fix TODOs:
  * WaitInput should have timeouts
  * implement Sleep to replace threadDelay

### Following releases
* e2e tests for TUI apps
* spawn arbitrary amount of puppets
* configurable leader-key keybindings
* tui for commands and puppet statuses
* escape sequences debugger: turn escape sequences into human-readable explanation
* interactive configuration dialog, help writing a parser for a prompt
* synchronize env variables after switching
* synchronize chosen repl variables after switching
* track xterm state (window title, cursor visibility, background/foreground colors, etc.)
* http api
  * stream output of a selected puppet
  * execute a shell command in a selected puppet via api
  * return last command output via api

## Current problems

* window title is not cleaned after switching;
* output from an inactive puppet is just discarded; as a result, we can miss
  ANSI escaped sequences which try to configure terminal (bracket paste mode,
  images preview, etc);
* terminal state is not cleared properly; as a result switching from a ranger
  might leave image preview in a terminal.

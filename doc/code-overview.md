# tshsh code overview

Half-year ago I've started a personal project
[tshsh](https://github.com/kevroletin/tshsh).
[README.md](https://github.com/kevroletin/tshsh/blob/master/README.md) describes
its purpose and
[features.md](https://github.com/kevroletin/tshsh/blob/master/doc/features.md)
presents it's features.

I've started `tshsh` to play with programming after a long break, and also to
get some fun and to feel productive once again. At this point (after two months
of active development and a half year of returning from time to time) I am
pretty happy about the resulting software. I am using it on daily basis and I
have plans to add new features.

Getting **fun** was one of the **main motivations** to start working. That meant
writing and testing code instead of spending 90% of the time reading
documentation and choosing between different libraries. That lead to a situation
that certain decisions were made just to try things out. I also wanted to try
**not** to use monads in some places and to see if it's practical. I think
experimenting is normal and pretty common in Haskell community ¯\_(ツ)_/¯. As a
result EDSL implementation seems overly complex :(

Here is a short description of the main difficulties and components:

* System programming - dealing with terminal tools brings difficulties;
* Memory management - there is a simple optimization to reduce pressure on
  garbage collector while reading a stream of binary data into memory;
* EDSL - it's a reasonable design decision to have DSL but the implementation of
  our DSL is overly complex (mostly due to desire to play with things and
  reinvent the wheel);
* Parsing - it has one good optimization and an idea for parsing combinators
  optimized to our use-case;
* Muxer - execution of EDSL programs and handling input/output is implemented by
  interleaving executing EDSL programs until they block and handling
  input/output.

## Overview

As [README.md](https://github.com/kevroletin/tshsh/blob/master/README.md) says, `tshsh` is:
* a shell automation tool
* a simple terminal multiplexer.

I would say, the shell automation tool is the most important part. It means that
`tshsh` parses the output of command-line shells and repls (such as `sh` or
`python`) and understands the boundaries of commands and command outputs. Let's
check an example below:

```sh
$  ~  date
Tue Feb 22 12:47:31 +10 2022
$  ~  echo 123
123
```

`tshsh` can understand that there were 2 commands: `date` and `echo 123`. And
also that each command produced its output. To understand boundaries between
commands `tshsh`
* uses user-defined config (see
  [ShellConfig.hs](https://github.com/kevroletin/tshsh/blob/master/app/ShellConfig.hs)
* parses commands output (see Parsing section).

## System programming

Dealing with terminals and command-line tools turned out to be quite a nuanced
task. Terminal tools aren't a popular topic, so there is not much information
available online. I learned about terminals mostly by experimentation and
debugging and later I've written 4 posts describing how things work, see [How
terminal
works](https://kevroletin.github.io/terminal/2021/12/11/how-teArminal-works-in.html).

## Memory management

TLDR;
* we allocate big buffers
* continuously read small chunks of data into a buffer
* allocate new buffer when the previous one is full
* history is a linked list of buffers.

A user might accidentally dump a large text file into the terminal or execute a
command like `tail -f` which continuously outputs data over a long time. The
simple approach of reading input by small chunks in a loop would produce many
small allocations and put pressure on the garbage collector which will increase
CPU usage and decrease responsiveness.

To reduce the number of allocations we:
1. allocate big buffers 
2. continuously read small chunks of input into buffers

```
  buffer
  .------------------------------------------------------------.
  | chunk1 |      chunk2     |  empty                           |
  `------------------------------------------------------------'
                              ^
                              `next write will go here
```

To keep history we accumulate output of arbitrary length by keeping a list of
buffers:

```
  buffer1
  .------------------------------------------------------------.
  | chunk1 |      chunk2     |  chunk3                         |
  `------------------------------------------------------------'

  buffer2
  .------------------------------------------------------------.
  | chunk4 | empty                                             |
  `------------------------------------------------------------'
```

Let's discuss the accumulation of history in detail. Let's say we've just
received `chunk1` and the next output is `chink2`. `chunk1` and `chink2` are
slices of the same buffer and they are located one after another. We can
concatenate them in a new slice without copying memory. Before such
functionality was possible with `ByteString` because a ByteString was a record
of buffer, offset, and size. Newer releases use `SharedPtr` which doesn't store
a link to a buffer and instead points to a slice. To implement functionality of
merging slices we've implemented
[Tshsh/Data/BufferSlice.hs](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Data/BufferSlice.hs).

[Tshsh/ReadLoop.hs](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/ReadLoop.hs)
Contains logic or reading binary data into buffers and allocating new buffers if
needed.

## EDSL

TLDR; each Haskell program should contain EDSL.

One of the design goals of `tshsh` is "non-intrusiveness: no source code or
config modifications of puppet shells". Because of that, we expect `tshsh` to
contain a fair amount of customization for each interactive shell or repl it
supports. An example of simple customization is "regular expressions" for
detecting shell prompts (so that `tshsh` can figure out boundaries between
command outputs).

But there might be more complicated logic for getting/setting the state of a
repl. For example, there is an idea to automate synchronizing variables between
different repls (say ruby and python). Getting/setting values might be nuanced
and might contain several steps. For such customization, one needs to write
code. There should be a library or EDSL to help write such a logic using `tshsh`
shell automation facilities.

There are numerous ways to write such a DSL. Basic examples would be:
* writing MTL-based code
* using Final Tagless style (writing monadic code with type class constraints
  similar to MTL)
* Free Monads.
* ...

To choose DSL we must also think about how we will execute its programs. In the
case of `tshsh` while executing DSL programs we would send commands to shells
which would produce outputs that our DSL programs would consume. This is a
concurrent process:

```

   DSL program               Shell process
 .--------------------.    .--------------------.
 | send_string "pwd"  |--->| $  /tmp  pwd       |
 | pwd <- wait_line   |<---| /tmp               |
 | ...                |    |                    |
 | send_string "date" |--->| $  /tmp  date      |
 | pwd <- wait_line   |<---| Tue Feb 22 15:45:30|
 '--------------------'    '--------------------'

```

One way to execute such DSL programs is to execute them concurrently in a
separate thread. That way DSL program might send commands concurrently and wait
on input without blocking the rest of the program. At the same time, another
part of `tshsh` would continue running and can handle shell process
input/output.

The second approach is to use coroutines. With coroutines, we might interleave
execution of DSL programs and handling of processes IO.

We've chosen to implement coroutines just for fun ^_^.

### Type

First, what do we have? We have a DSL, which is based on a tree-like data
structure, which represents a program. It has no Monad instance, so one has to
construct a program by combining constructors without the help of do-notation.

```
data Program st i o m r where
```

Although it's not based on a Monad, it has some pretty common features: State,
Lift, evaluating `Program` returns a value and it has operations to send/receive
messages (similarly to streaming libraries like Conduit). And also there is a
feature to abort execution with an error message.

* st - state (similarly to State Monad)
* i, o - input/output types (similarly to streaming libraries like Conduit)
* m - base Monad for Lift (similar to Monad transformers)
* r - returned value (similar to any Monad)

There are operations to combine `Program` values
* `AdapterAll`, `AdapterSt`, `Adapter` - change types associated with streaming and state features
* `Pipe` - connect corresponding outputs to input in a list of `Program`s.

### Implementation

I was inspired by Free Monads but I don't like Free Monads because of O^2
composition complexity. The alternative would be a Freer Monad which, I think
would work. But I had the motivation to experiment and I've come up with the
following solution.

My mental image for Free Monads is that a monadic action is a list of actions
that can be extended by appending a new action to its end. Let's say, we have
Free Monads, then the code like this:

```
step1 >>= step2
```

would cause bind operator `>>=` to "reconstruct" the entire `step1` structure to append `step2` to its end.

OK, let's change this by introducing the `AndThen` constructor which would
concatenate two actions in O(1):

```
step1 `AndThen` step2
```

Now if we want to evaluate the resulting action we first need to pattern match
on `AndThen` to get `step1`. If `step1` contains `AndThen` then we need to
pattern match again. If there are many nested `AndThen` then, evaluating one
node of a tree had O(h) complexity, where `h` is the height of the tree. In
general, it's not necessarily a problem, because, depending on the interpreter,
walking from the root of such `AndThen` tree might happen only once.

In our case, it was a problem, because we wanted coroutines-like behavior, so
our interpreter evaluates DSL programs step by step. There are comments in the
code on how we solve this problem, See [source code
](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Lang/Coroutine/CPS/Internal.hs#L27)
comment for more explanations. In a few words, while walking the tree down, we
re-balance the tree.

### Composition

1. CPS

   Naming: values composed using CPS style has `C` suffix; if they produce
   `Program` with `()` state parameter, then it's `C_` suffix.

   * Pros:
     + naturally comes from `Program` tree-like structure
   * Cons:
     + hard to combine values of different types (for example computation with
       `()` State should combine with any computation)
     + hard to store polymorphic types (when we, for example, don't care about
       State type because we don't use State).

   All `Program` constructors except for `Finish` have a parameter that stores
   the next action. So combining `Programs` is just nesting constructors. With
   such an approach we can make reusable subprograms using CPS:
   
   ```
   outputGreetingC_ next = Output "Hello" next
   ```
   
   This approach seems simple but has serious limitations. It's hard to combine
   continuations of different types. Let's consider an example. We have two
   actions that have incompatible `st`(State) parameters. We can easily combine
   them using the `AdaptSt` constructor because `Program ()`, obviously doesn't
   change state, so we can turn it into a `Program st` with any state.
   
   ```
   act1 :: Program () i o m r
   act2 :: Program Int i o m r
   ```
   
   TODO: explain better
   
   The problem comes from two aspects:
   * we write functions in CPS style
   * some parameters are polymorphic
   * we want to store those functions in config data structures.
   
   As a result types in config data structures become complicated because
   * we want some type parameters to be polymorphic
   * because we would create a type class to convert values
   * which leads to existential types because to store polymorphic value
     compiler also wants to store type class dictionaries.

2. `AndThen`

   Naming: values composed using `AndThen` have `P` or `P_` suffix depending on their `State` type parameter.

    * Pros: easier composition
    * Cons: writing `AndThen` between every two statements clutters the code.

    Combining values is easier than CPS programs:
    * we can combine `Program`s of different types using `Adapter`s
    * we can store `Program` in config data structures.

### Evaluation

[`stepUnsafe`](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Lang/Coroutine/CPS/Internal.hs#L175)
evaluates `Program` value until `Program` either:
1. it outputs a single value
2. it cannot be further evaluated (because `Program` waits for inputs or just
   sleeps until a certain time).

If `Program` produced output then `stepUnsafe` returns a pair: output value and
remaining evaluated part of original `Program` which might output again. So it
can/should be further evaluated. If `Program` produced no results, that means
that the resulting `Program` cannot be further evaluated without new inputs.

`stepUnsafe` has one **important** requirement: all `Program` outputs should be
consumed before `Program` can receive input. To reflect the fact that `Program`
might produce more outputs, or it's ready to receive inputs, there is a
[`ProgramEv`](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Lang/Coroutine/CPS/Internal.hs#L152)
wrapper.

## Parsing

[This
comment](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Matcher/Seq.hs#L159)
describes motivation for implementing ~~(trying to implement)~~ own parsers.

It turned out that in the case of `ByteString`, searching for a single character
works fast because it compiles down to `memchr` processor instruction. That
observation leads to a hypothesis that we can write a library that will utilize:
* efficient processors instructions
* will favor no backtracking/buffering.

At this point, the library has only two parsers.

## Muxer

That [piece of
code](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Muxer.hs#L39-L49)
describes `Muxer`s main loop. It waits for one of several events:
* signal arrived
* input from the terminal available
* input from one of the puppets available
* current `Program` can be further evaluated because it's lat `Wait` has
  finished.

Then it executed the appropriate handler from
[Tshsh/Muxer/Handlers.hs](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Muxer/Handlers.hs).

### Synchronization

TLDR; we just execute one `Program` at a time, all synchronization is assumed to
happen inside a `Program` (but there aren't primitives in `Program` for
synchronization yet).

Some `tshsh` features require synchronization between different concurrent tasks
associated with different puppets. For example, synchronizing the current
working directory might be done in 2 steps:

* execute a command to get the current working directory in Puppet#1
* execute a command to set the current working directory in Puppet#2.

Currently, there might be only one running `Program` which can send commands to
different puppets [see
MuxState](https://github.com/kevroletin/tshsh/blob/master/src/Tshsh/Muxer/Types.hs#L68).
That's enough for existing and planned features.

### Testing

We have:
* unit tests 

  mostly to make sure that `Program` is evaluated as intended and the
  interpreter doesn't hang;

* end unit tests

  [tshsh/test_scripts](/home/behemoth/Scratch/haskell/tshsh/test_scripts);
  testing performance gave me an idea, that it's a good idea to collect heap
  profiles for the end-user tests in CI server and maybe in some cases they
  might be analyzed automatically.
  
  

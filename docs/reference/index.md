---
title: Reference
---

## Extempore reference manual

Use Cian's stuff here, e.g.

``` org
* Introduction
** Why Extempore?
Why create another language.
+ Live Coding
+ Creative Coding
+ Close to the Metal
+ It can do everything!
+ Scheme is a wonderful language
** What is Extempore?
+ Two languages
+ Scheme
+ XTLang
+ Why Two Languages
** XTLang Overview
+ Scheme Like
+ High level of abstraction (macros, typing, etc)
+ C Like
+ High Performance
+ Easy Access to C libraries
+ Flexible
+ REPL access to the compiler and runtime
* Basic XTLang
** Setup
*** Installation
*** Editors
*** Hello Worlds
+ Hello World!
+ Hello Sine
+ Hello Triangle
** Getting Started
*** How to run and load code
+ Starting XTLang
  + Windows/OSX/Linux
+ Calling Code
  + Emacs/Vim/other editors
+ Where output goes
*** Scheme fundamentals
+ Brackets
+ Some very basic commands (addition/subtraction)
*** XTLang fundamentals
+ Intro to how you call XTLang from Scheme
** Writing Simple Programs
*** Input/Output
+ Print
+ Command line input
*** Control Flow
+ If
+ Case
+ While
+ Anything else...
*** Variables
+ Local Variables (Let)
+ Global Variables
** Basic Types
*** Static Typing
+ Explain idea
+ Often can be inferred
*** Integers
*** Floats
*** Boolean
*** Character
*** Pointers
+ Introduce idea and the basic types for completion.
+ Explain that a pointer is a reference to a memory location that contains data.
  + This will be returned to (and repeated) later
*** C String
*** 'Better' String
*** Symbols
*** Enum
*** Literal Types
** Functions
*** Lambda
+ Function Definitions
+ Returning a Lamda
*** Function Types
+ Type Inference
+ Actual typing
*** Passing around functions
+ Include symbols here
*** Polymorphic Functions
*** Closures
+ Definition
+ Accessing
+ Returning a Closure
* Intermediate XTLang
** Memory Management
*** Pointers
+ What is a pointer
+ Memory
+ Memory addition and different types.
*** Creating Memory
**** Stack Allocation
**** Zone Allocation
**** Global Allocation
*** Functions, Closures and Memory
*** Strategies for Managing Memory
** Advanced Types
*** Arrays
*** Tuples
+ Access
+ Memory allocation and tuples
+ Length is part of the type
+ Pointer types for Tuples
+ Recursive Definitions
*** ADTs
+ Concrete Types only. Polymorphic Types will be discussed later.
+ Memory Allocations and ADTs
+ Pointer Types
+ Recursive Definitions.
+ Pattern Matching
+ Include some examples (tree, linked list maybe).
+ Show that ADTs and Tuples can be combined easily
*** Named Types
**** Bind-Alias
+ Discourage Use for the General Case
**** Bind-Type
Combing ADTs and Tuples 
** Generic Types
*** Tuples
*** ADTs
*** Type Classes
* XTLang Libraries
** Modules/Name Spaces
** Common Collections
Hashtables, etc
** Input/Output
print, input, etc
** Other Important Libraries
+ Introduce Namespaces, and how to load libraries here formally
** Creating a Library
* Advanced XTLang
** Time & Event Scheduling
How to schedule events. Currently vague, but would include discussions
of things like scheduling on beats, and approaches to having multiple clocks.

An argument could be made for having this later, but I think this is more useful
than Generic Types
** Concurrency
This is vague as I don't know much about XTLang's options here
*** Threads
*** Data Sharing
*** Processor Allocation
** Error Handling
** Macros
* Scheme-XTLang Interop
* C-XTLang Interop
** Memory Management
** Structs
+ Discuss work arounds
```



# 

![](img/erlang_logo_with_dpi.png)


#
\centering
\begin{Huge}
What is Erlang,

and how does it work?
\end{Huge}

# About
\centering
\Large

* Programming Language from 1987 by Ericsson
* Functional & Concurrent
* Strong dynamicaly typed
* Influencing
    * From: Prolog
    * To: Clojure, Scala, Elixir
* Running on `BEAM` Virtual Machine
* Using `OTP Framework`
* Named after _Agner Krarup Erlang_ or __Er__icsson __lang__uage



# Erlang Applications

* membase
* riak
* rabbitmq
* CouchDB
* eJabberD


# Comparing

\centering
\large

| Erlang 	| Java 	| NodeJS 	| PHP 	|
|-----------------	|-----------------	|----------------	|----------------	|
| compiled 	| compiled 	| interpreted 	| interpreted 	|
| multithreaded 	| multithreaded 	| singlethreaded 	| singlethreaded 	|
| parallel, async 	| parallel, async 	| async 	| sequential 	|


# 
\centering

\begin{Huge}
Comparing Handling \\ of HTTP Requests
\end{Huge}


# PHP: HTTP Request

\centering

![](img/phpfcgi_with_dpi.png)



# NodeJS: HTTP Request

\centering

```javascript
var http = require("http");

var handler = function(request, response) {
  response.writeHead(200, {"Content-Type": "text/plain"});
  response.write("Hello World!");
  response.end();
};

var server = http.createServer(handler);
 
server.listen(80);
```

# Java: HTTP Request

\centering

![](img/tomcat_architecture_with_dpi.png)


# Erlang: HTTP Request

\centering

![](img/erlang_system_with_dpi.png)
<!--
Erlang itself is the server, like NodeJS does.
Either a new process is spawned or one is used from a worker pool.
The Process starts with a empty state, each data has to be fetched from other processes, compareable to PHP.
At the end of the request the process can stop.
Processes of Requests could talk to each other.
-->

# BEAM Virtual Machine

\centering
\Large

* Compareable to Java VM
* But can create _way more_ Processes
* Processes scheduled inside BEAM, not OS

* Processes send and receive Messages
* Processes can have State and Name for PID



# Features of BEAM VM

\centering
\huge

* Microprocesses
* Pattern Matching _(Not Regex)_
* Immutable Variables


# Let it crash

Supervisors can restart crashed processes
  
NO defense code

Any error in code will crash process

-> shorter cleaner code
-> faster implem
-> Robust code  
-> handling all errors


# Garbage Collection

Each Process has its own GC

# 
\centering

\begin{Huge}
Thinking in Processes
\end{Huge}


# Elevator example

* 3 Elevators
* 10 Floors

How many Processes?

# Process State and Messages

* Each Process as its own state
* Data exchange via Messages
* Messages handled like Transactions
* Removing Deadlocks and Race Conditions

# 
\centering

\begin{Huge}
Thinking Functional
\end{Huge}


# Functional Erlang

\centering

* Not _fully_ functional
    * No complex Type System
    * No currying
    * ...
* Immutable Variables
* Small Functions

# Not free of Side Effects

* Messages between Processes
* Global Registry
* Database
* External Services


# Alan Kay about OOP

> "OOP to me means only messaging, local retention and protection and hiding of state-process, and extreme late-binding of all things."


> [_Dr. Alan Kay_](http://userpage.fu-berlin.de/~ram/pub/pub_jf47ht81Ht/doc_kay_oop_en)



# 
\centering

\begin{Huge}
Coding Erlang
\end{Huge}

# Coding

* Variables start with capitals
* No type declarations
* Short functions
* Pattern matched function heads
* Multiple function bodies
* Commas, semicolons, dots, arrows
* Small, stable (,easy) declarative Syntax

# Hello World!
\centering

```erlang
io:format("Hello World!").
```

# Fibonacci
\centering

```erlang
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).
```

# Pattern Matching

\centering

```erlang
% Matching on lists
[First, Second, Third] = [1,2,3].

% Matching deep strcutures
{ok, {SomeName, [FirstValue | MoreValues]}} = do_something().
```

# Immutable Variables

\centering

```erlang
A = 42.
A = A + 8. % will crash
```

```erlang
B = 42.
A = B + 8.
```


# Hello World Module
\centering

```erlang
-module(hello).
-export([start/0]).

start() ->
    io:format("Hello World!").
```

```shell
$ erlc hello.erl
$ erl -s hello
```

# Hello World Process
\centering

```erlang
NEEDED? page 108
```

# 
\centering

\begin{Huge}
OTP
\end{Huge}

# Learning Erlang

\centering

* erlang.org
* Learn _You Some Erlang_ Book and Website
* Book _Programming Erlang_
* IRC Freenode #erlang and #erlounge
* Erlang Mailing List
* Erlang Factory & Conferences




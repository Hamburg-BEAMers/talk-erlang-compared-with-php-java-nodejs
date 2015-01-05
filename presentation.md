

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

* Programming Language from 1987 at Ericsson
* Functional & Concurrent
* Strong dynamicaly typed
* Influenced
    * By: Prolog
    * To: Clojure, Scala, Elixir
* Running on `BEAM` Virtual Machine
* Using `OTP Framework`
* Named after _Agner Krarup Erlang_ or __Er__icsson __lang__uage


# Comparing

\centering

| Erlang 	| Java 	| NodeJS 	| PHP 	|
|-----------------	|-----------------	|----------------	|----------------	|
| compiled 	| compiled 	| interpreted 	| interpreted 	|
| multithreaded 	| multithreaded 	| singlethreaded 	| singlethreaded 	|
| parallel, async 	| parallel, async 	| async 	| sequential 	|


# Comparision: HTTP Requests

\centering
\begin{Huge}
How are they handled in these Languages?
\end{Huge}


# PHP: HTTP Request

\centering

![](img/phpfcgi_with_dpi.png)



# NodeJS: HTTP Request

\centering

![](img/phpfcgi_with_dpi.png)



# BEAM Virtual Machine

\centering

* Compareable to Java VM
* But can create _way more_ Processes
* Processes send and receive Messages
* Processes can have State and Name
* Processes scheduled inside BEAM, not OS


# Alan Kay about OOP

> "OOP to me means only messaging, local retention and protection and hiding of state-process, and extreme late-binding of all things."


> [_Dr. Alan Kay_](http://userpage.fu-berlin.de/~ram/pub/pub_jf47ht81Ht/doc_kay_oop_en)


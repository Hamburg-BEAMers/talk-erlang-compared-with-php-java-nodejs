-module(hello_process).
-export([start/0, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    register(hello, Pid),
    timer:sleep(1000), whereis(hello) ! hello,
    timer:sleep(1000), whereis(hello) ! {hello, "Julius"},
    timer:sleep(1000), whereis(hello) ! ["Foo", bar].

loop() ->
    receive
        hello ->
			hello();
		{hello, Name} ->
			hello(Name);
        Message ->
			io:format("Unknown message: ~p\n", [Message])
    end,
    loop().

hello() ->
	hello("World").

hello(Name) ->
	io:format("Hello ~s!\n", [Name]).

# ecall

Erlang library for group remote calls and casts.

I tried to keep API as simple as possible.

I appreciate any pull requests for bug fixing, tests or extending the functionality.

CASTS
-----
    Casts always return ok and do not wait for results. So they are the fastest.
    
    ----------------cast_one----------------------------------
    If you want to cast any random node from Nodes call:

    ok = ecall:cast_one(Nodes, Module, Function, Arguments )
    
    and at the choosed node Module:Function(...Arguments ) I mean Module:Function(A1,A2,...)  
    will be applied

    ----------------cast_all----------------------------------
    If you want to cast all nodes from Nodes call:

    ok = ecall:cast_all(Nodes, Module, Function, Arguments )

CALLS
-----
    Calls are a bit more complicated. Calls wait for reply. 
    
    Called Module:Function( ...Arguments ) has to reply either:

    { ok, Result }  or {error, Error}

    ----------------call_one----------------------------------
    Use this policy if it is enough if at least one node From nodes replied {ok, Result}

    {ok, {Node,Result}} | {error, NodesErrors} = 
        ecall:call_one(Nodes, Module, Function, Arguments )
    
    The algorithm takes one random node from Nodes and calls it with Module:Function( ...Arguments )
    If the node replied with {ok, Result} it's returned to you. 
    If the node replied with {error, Error} this node is exluded from the Nodes 
    and the next random node is called and so on until {ok, Result} or
    no one left. If none was ok then the list of errors is return to you as:
        
    {error, [{node1,Error1}, {node2,Error1}|_AndSoOn] }
    
    There is one more thing. The call to a node can return {badrpc, Result}. 
    It means that the node either not alive or the 
    Module:Function( Arguments ) crashed. 
    If you want to be aware of it call with _RpcErr = true:
    
    {ok, Result} | {error, NodesErrors} = 
        ecall:call_one(Nodes, Module, Function, Arguments, _RpcErr = true )
    
    then if the node didn't replied his error will look like {badrpc, Reason}

    ----------------call_any----------------------------------
    This policy is almost the same as call one but with one usefull difference. 
    All the Nodes are called at the same time.
    It means the algoritm doesn't wait for reply from the node and calls the next and next...
    then it waits for at least one {ok. Result} if it comes it's returned to you others are ignored
    If no one replied ok then the {error, NodesErrors} is returned to you the same as in call_one 
    Don't forget set _RpcErr = true if you are interested in badrpc errors:

    {ok,{Node,Result}} | {error, NodesErrors} = ecall:call_any(Nodes, Module, Function, Arguments )

    ----------------call_all----------------------------------
    This is the most strict policy. The {ok,NodesResults} will be returned to you
    only if all the Nodes replied with {ok,Result}. If at least one replied with {error,Error} then
    {error,{Node,Error}} is returned

    {ok,[{Node1,Result1},{Node2,Result2} | _AndSoOn]} | {error, {Node,Error}} = 
        ecall:call_all(Nodes, Module, Function, Arguments )

    ----------------call_all wait----------------------------------
    This is the slowest policy but for some needs useful. 
    If you ask:
        
    {Replies, Rejects} = ecall:call_all_wait(Nodes, Module, Function, Arguments )

    You will get who replied with {ok,Result} and who with {error,Error} or {badrpc,Reason}

    [{Node1,Result1},{Node2,Result2}| _AndSoOn] = Replies
    
    [{Node1,Reject1},{Node2,Reject2}| _AndSoOn] = Rejects


    If you need live use examples take a look at source code:



    https://github.com/vzroman/elock.git
    https://github.com/vzroman/esubscribe.git

    A little advirtisement :-)
    

BUILD
-----
    Add it as a dependency to your application and you are ready (I use rebar3)
    {deps, [
        ......
        {ecall, {git, "git@github.com:vzroman/ecall.git", {branch, "main"}}}
    ]}.

TODO
-----
    Tests!!!
    
#!/usr/bin/env escript
%% -*- erlang -*-

main(Keys) ->
    FilePath = filename:absname(filename:dirname(escript:script_name()) ++ "/../config/env.config"),
    {ok, AllKeyVal} = file:consult(FilePath),
    AllVal = get_keys_val(Keys, AllKeyVal),
    ArrayStr = output_to_string(AllVal),
    io:format("~ts", [ArrayStr]),
    init:stop(0);
main(_) ->
    init:stop(1).

get_keys_val(Keys, AllKeyVal) ->
    [proplists:get_value(erlang:list_to_atom(Key), AllKeyVal, "")|| Key <- Keys].

output_to_string(List) ->
    string:join([to_list(Data) || Data<-List], " ").

to_list(Data) when erlang:is_atom(Data) -> erlang:atom_to_list(Data);
to_list(Data) when erlang:is_binary(Data) -> erlang:binary_to_list(Data);
to_list(Data) when erlang:is_integer(Data) -> erlang:integer_to_list(Data);
to_list(Tuple) when erlang:is_tuple(Tuple) -> 
    string:join([to_list(Data) || Data<-erlang:tuple_to_list(Tuple)], " ");
to_list(Map) when erlang:is_map(Map) ->
    string:join([to_list(Data) || {_,Data}<-maps:to_list(Map)], " ");
to_list(List) when erlang:is_list(List) ->
    case lists:all(fun(C) -> C > 0 andalso C =< 255 end, List) of
        true -> List;
        _ -> string:join([to_list(Data) || Data<-List], " ")
    end.

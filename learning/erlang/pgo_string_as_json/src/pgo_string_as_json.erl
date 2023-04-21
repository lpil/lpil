-module(pgo_string_as_json).

-include_lib("eunit/include/eunit.hrl").

% Experimenting to try and understand a bug in the Gleam bindings to pgo.

insert_json_string_test() -> 
    application:ensure_all_started(pgo),
    pgo:start_pool(my_pool, #{
        pool_size => 5,
        host => "127.0.0.1",
        database => "postgres",
        user => "postgres",
        password => "postgres"
    }),
    pgo:transaction(
        fun() ->
        #{command := create} = pgo:query(
            "create temporary table things (id serial primary key, stuff jsonb not null)",
            [],
            #{pool => my_pool}
        ),
        erlang:display(pgo:query(
            "insert into things (stuff) values ($1) returning *",
            [<<"{\"foo\": \"bar\"}">>],
            #{pool => my_pool}
        )),
        erlang:display(pgo:query(
            "insert into things (stuff) values ($1) returning *",
            ["{\"foo\": \"bar\"}"],
            #{pool => my_pool}
        ))
    end, #{pool => my_pool}).

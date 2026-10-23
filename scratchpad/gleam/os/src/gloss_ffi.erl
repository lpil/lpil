-module(gloss_ffi).

-export([
    is_windows/0, environment_get/1, environment_set/2, environment_unset/1,
    environment_all/0, system_name/0, write/2, delete/1, read/1, cd/1
]).

system_name() ->
    case os:type() of
        {win32, _} -> ~"win32";
        {_, Name} -> erlang:atom_to_binary(Name)
    end.

is_windows() ->
    case os:type() of
        {win32, _} -> true;
        _ -> false
    end.

environment_get(Key) ->
    case os:getenv(unicode:characters_to_list(Key)) of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

environment_set(Key, Value) ->
    os:putenv(unicode:characters_to_list(Key), unicode:characters_to_list(Value)),
    nil.

environment_unset(Key) ->
    os:unsetenv(unicode:characters_to_list(Key)),
    nil.

environment_all() ->
    BinVars = lists:map(fun(VarString) ->
        [VarName, VarVal] = string:split(VarString, "="),
        {unicode:characters_to_binary(VarName), unicode:characters_to_binary(VarVal)}
    end, os:getenv()),
    maps:from_list(BinVars).

write(Path, Contents) ->
    posix_result(prim_file:write_file(Path, Contents)).

delete(Path) ->
    posix_result(file:del_dir_r(Path, [raw])).

read(Path) ->
    posix_result(prim_file:read_file(Path)).

cd(Path) ->
    posix_result(prim_file:change_directory(Path)).

posix_result(Result) ->
    case Result of
        ok -> {ok, nil};
        {ok, Value} -> {ok, Value};
        {error, eacces} = E -> E;
        {error, eagain} = E -> E;
        {error, ebadf} = E -> E;
        {error, ebadmsg} = E -> E;
        {error, ebusy} = E -> E;
        {error, edeadlk} = E -> E;
        {error, edeadlock} = E -> E;
        {error, edquot} = E -> E;
        {error, eexist} = E -> E;
        {error, efault} = E -> E;
        {error, efbig} = E -> E;
        {error, eftype} = E -> E;
        {error, eintr} = E -> E;
        {error, einval} = E -> E;
        {error, eio} = E -> E;
        {error, eisdir} = E -> E;
        {error, eloop} = E -> E;
        {error, emfile} = E -> E;
        {error, emlink} = E -> E;
        {error, emultihop} = E -> E;
        {error, enametoolong} = E -> E;
        {error, enfile} = E -> E;
        {error, enobufs} = E -> E;
        {error, enodev} = E -> E;
        {error, enoent} = E -> E;
        {error, enolck} = E -> E;
        {error, enolink} = E -> E;
        {error, enomem} = E -> E;
        {error, enospc} = E -> E;
        {error, enosr} = E -> E;
        {error, enostr} = E -> E;
        {error, enosys} = E -> E;
        {error, enotblk} = E -> E;
        {error, enotdir} = E -> E;
        {error, enotsup} = E -> E;
        {error, enxio} = E -> E;
        {error, eopnotsupp} = E -> E;
        {error, eoverflow} = E -> E;
        {error, eperm} = E -> E;
        {error, epipe} = E -> E;
        {error, erange} = E -> E;
        {error, erofs} = E -> E;
        {error, espipe} = E -> E;
        {error, esrch} = E -> E;
        {error, estale} = E -> E;
        {error, etxtbsy} = E -> E;
        {error, exdev} = E -> E;
        {error, badarg} -> {error, einval};
        {error, no_translation} -> {error, einval} 
    end.

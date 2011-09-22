-module(cereal).

-export([
         set_raw_tty_mode/1,
         set_tty_speed/3,
         set_tty_flow/2,
         open_tty/1,
         test/1
        ]).

-on_load(init/0).

set_raw_tty_mode(_) ->
    not_loaded(?LINE).

set_tty_speed(_, _, _) ->
    not_loaded(?LINE).

set_tty_flow(_, _) ->
    not_loaded(?LINE).

open_tty(_) ->
    not_loaded(?LINE).

test(Device) ->
    Fd = open_tty(Device),
    erlang:open_port({fd, Fd, Fd}, [binary, stream]),
    set_raw_tty_mode(Fd).

init() ->
    SoName = case code:priv_dir(cereal) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, cereal]);
                _ ->
                    filename:join([priv, cereal])
            end;
        Dir ->
            filename:join(Dir, cereal)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%% Copyright 2011, Joe Williams <joe@joetify.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(cereal).

-export([
         set_raw_tty_mode/1,
         set_tty_speed/3,
         set_tty_flow/2,
         open_tty/1,
         close_tty/1,
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

close_tty(_) ->
    not_loaded(?LINE).

test(Device) ->
    {ok, Fd} = open_tty(Device),
    ok = set_raw_tty_mode(Fd),
    erlang:open_port({fd, Fd, Fd}, [binary, stream]).

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

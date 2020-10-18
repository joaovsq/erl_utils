-module(file_utils).

-export([read_all/1, read_raw/1]).

%% Reads a file and returns a string with all content.
%% This function should not be used to read large files
read_raw(FileName) ->
    StringList = read_all(FileName),
    lists:flatten(StringList).

%% Reads all lines of a file and returns a list with each line (including lines with only the \n char).
%% This function should not be used to read large files.
read_all(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try
        get_lines(Device)
    after
        file:close(Device)
    end.

%% Returns a list containing all lines of a file. Be careful when using this to read large files.
get_lines(Device) ->
    case io:get_line(Device, "") of
        eof -> [];
        Line -> [Line] ++ get_lines(Device)
    end.

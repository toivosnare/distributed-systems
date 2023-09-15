-module(task1_1).
-export([next_date/1]).

is_leap_year(Year) ->
    if
        Year rem 400 == 0 -> true;
        Year rem 100 == 0 -> false;
        Year rem 4   == 0 -> true;
        true              -> false
    end.

is_last_day_of_month({Year,Month,Day}) ->
    case Month of
        1 -> Day == 31;
        2 -> case is_leap_year(Year) of false -> Day == 28; true -> Day == 29 end;
        3 -> Day == 31;
        4 -> Day == 30;
        5 -> Day == 31;
        6 -> Day == 30;
        7 -> Day == 31;
        8 -> Day == 31;
        9 -> Day == 30;
        10 -> Day == 31;
        11 -> Day == 30;
        12 -> Day == 31
    end.

next_date({Year, Month, Day}) ->
    case is_last_day_of_month({Year, Month, Day}) of
        true ->
            Y = case Month of
                12 when Year == -1 -> 1;
                12 -> Year + 1;
                _ -> Year
            end,
            M = (Month rem 12) + 1,
            D = 1,
            {Y, M, D};
        false -> {Year, Month, Day + 1}
    end.

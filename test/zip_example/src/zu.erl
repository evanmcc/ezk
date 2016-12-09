-module(zu).

-compile(export_all).

%% fake up a terrible delay distribution
pretend() ->
    case rand:uniform(1000) of
        1000 ->
            timer:sleep(2500);
        N when N > 900 ->
            timer:sleep(250 + (rand:uniform(100) - 50));
        N when N > 750 ->
            timer:sleep(25 + (rand:uniform(10) - 5));
        _N ->
            Delay = rand:uniform(10),
            timer:sleep(Delay)
    end.

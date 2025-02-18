%This prolog program finds a list of places for tariffs from WordNet.

:- dynamic cycle_counter/1.

% Initialize cycle counter
cycle_counter(1).

% Define target hypernyms for places
target_hypernym("location").
target_hypernym("region").
target_hypernym("country").
target_hypernym("city").
target_hypernym("state").
target_hypernym("continent").
target_hypernym("geographical_area").

% Check if a SynsetId has one of the target hypernyms
has_target_hypernym(SynsetId) :-
    hyp(SynsetId, HypernymId),
    g(HypernymId, Gloss),
    target_hypernym(Gloss).

% Find all place names with the desired hypernyms
find_places(Places) :-
    setof(Word, SynsetId^SenseNum^TagCount^(
        s(SynsetId, _, Word, n, SenseNum, TagCount),
        has_target_hypernym(SynsetId)
    ), Places).

% Main loop that stops when all places are processed
process_places_in_cycles :-
    cycle_counter(Cycle),
    format("Starting cycle ~w...\n", [Cycle]),

    % Generate filename dynamically
    format(atom(Filename), "places_3min~w.txt", [Cycle]),

    % Find places
    (   find_places(Places)
    ->  
        % Write to file if there are places
        tell(Filename),
        forall(member(P, Places), writeln(P)),
        told,
        format("Cycle ~w complete. Results saved to ~w\n", [Cycle, Filename]),

        % Increment cycle counter
        NextCycle is Cycle + 1,
        retract(cycle_counter(Cycle)),
        assertz(cycle_counter(NextCycle)),

        % Pause for 30 seconds
        writeln("Pausing for 30 seconds..."),
        sleep(30),

        % Start next cycle
        process_places_in_cycles
    ;   
        % Stop when no places remain
        writeln("All places have been processed. Stopping."),
        retract(cycle_counter(Cycle))
    ).

%Call by using the line below.
%swipl -s places_extraction.pl -g process_places_in_cycles -t halt.

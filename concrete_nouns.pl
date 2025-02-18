:- dynamic cycle_counter/1.

% Initialize cycle counter
cycle_counter(1).

% Define target hypernyms
target_hypernym("entity").
target_hypernym("object").
target_hypernym("physical_entity").
target_hypernym("artifact").
target_hypernym("animal").
target_hypernym("plant").

% Check if a SynsetId has one of the target hypernyms
has_target_hypernym(SynsetId) :-
    hyp(SynsetId, HypernymId),
    g(HypernymId, Gloss),
    target_hypernym(Gloss).

% Find all nouns with the desired hypernyms
find_concrete_nouns(Nouns) :-
    setof(Word, SynsetId^SenseNum^TagCount^(
        s(SynsetId, _, Word, n, SenseNum, TagCount),
        has_target_hypernym(SynsetId)
    ), Nouns).

% Main loop that stops when all nouns are processed
process_nouns_in_cycles :-
    cycle_counter(Cycle),
    format("Starting cycle ~w...\n", [Cycle]),

  %This is an untested prolog program to find concrete nouns from WordNet to comply with tariffs, by fetching country specific data for these words.  
  
  % Generate filename dynamically
    format(atom(Filename), "3min~w.txt", [Cycle]),

    % Find nouns
    (   find_concrete_nouns(Nouns) 
    ->  
        % Write to file if there are nouns
        tell(Filename),
        forall(member(N, Nouns), writeln(N)),
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
        process_nouns_in_cycles
    ;   
        % Stop when no nouns remain
        writeln("All concrete nouns have been processed. Stopping."),
        retract(cycle_counter(Cycle))
    ).

 %Call using
 %swipl -s concrete_nouns.pl

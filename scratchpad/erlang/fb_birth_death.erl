-module(thing).

-record(person, {birth, death}).

-record(loop, {highest_population_year,
               highest_population_count,
               current_population,
               deaths}).

main() ->
  People = [
    #person{birth = "1932", death = "1973"},
    #person{birth = "1900", death = "1995"},
    #person{birth = "1932", death = "1938"},
    #person{birth = "1820", death = "1895"},
    #person{birth = "1932", death = "1953"},
    #person{birth = "1922", death = "1971"},
  ],
  SortedPeople = lists:sort_by(fun(Person) -> Person#person.birth end),
  State = #loop{highest_population_year = 0,
                highest_population_count = 0,
                current_population = 0,
                deaths = #{}},
  loop(SortedPeople, hd(People)#person.birth, State).

loop([], _Year, State) ->
  State#loop.highest_population_year;
loop(People, Year, State) ->
  State1 = handle_deaths(Year, State),
  {State2, RemainingPeople} = handle_births(People, Year, State),
  loop(RemainingPeople, Year + 1, State).

handle_deaths(Year, State) ->
  DeathsForYear = maps:get(Year, 0, State#loop.deaths),
  CurrentPopulation = State#loop.current_population - DeathsForYear,
  State#loop{current_population = CurrentPopulation}.

handle_births([], _Year, State) ->
  {State, []};
handle_births([#person{birth = Birth, death = Death} = Person | RemainingPeople], Year, State)
  when Birth == Year ->
  State1 = register_death_year(Death, State),
  State2 = increment_population(State, Year),
  {State2, RemainingPeople}.

increment_population(State, Year) ->
  CurrentPopulation = State#loop.current_population + 1,
  HighestPopulation = State#loop.highest_population_year,
  State1 = State#loop{current_population = CurrentPopulation},
  if
    CurrentPopulation > HighestPopulation ->
      State1#loop{highest_population_year = Year,
                  highest_population_count = CurrentPopulation};
    true ->
      State1
  end.

register_death_year(Death, LoopState) ->
  NewDeaths = maps:update_with(Death, fun(Y) -> Y + 1 end, 0, LoopState#loop.death),
  LoopState#loop{deaths = NewDeaths}.

% 1. Sort them from lowest to highest by birth year
% 2. Start with the first year and iterate forwards by year
%   1. Store a variable for the number of people alive
%   2. Store a variable for the number for the year where the most people were
%      seen alive
% 3. For each year, check if the next oldest person has been born this year
%   1. If so:
%     1, increment the number of people currently alive
%     2. Check if this means there have been more people born this year than our
%        previous max, and record the year as the new max if so
%   3
%
%   Yay my editor crashed and I lost the rest of these notes
%
% N = length(People)
% Y = number of years from lowest birth year to highest death year, inclusive
%
% O(N log N + Y log N)


%
%
%
%
%
%
%
%
%
%
%

-module(thing).

-record(person, {birth, death}).
-record(loop, {highest_population_year, highest_population_count}).

main() ->
  People = [
    #person{birth = "1932", death = "1973"},
    #person{birth = "1900", death = "1995"},
    #person{birth = "1932", death = "1938"},
    #person{birth = "1820", death = "1895"},
    #person{birth = "1932", death = "1953"},
    #person{birth = "1922", death = "1971"},
  ],
  Events = [Event || Person <- People,
                     Event <- [{Person#person.birth, 1}, {Person#person.death, -1}]],
  SortedEvents = lists:sort(Events),
  count(Events, 0, #loop{highest_population_year = 0, highest_population_count = 0}).

count([], Population, State) ->
  State#loop.highest_population_year.
count([{Year, Delta} | Events], Population, State) ->
  NewPopulation = Population + Delta,
  NewState = if
    NewPopulation > State#loop.highest_population_count ->
      State#loop{highest_population_count = NewPopulation, highest_population_year = Year};
    true ->
      State
  end,
  count(Events, NewPopulation, NewState).

% N = number of peopl
%
% O(N log N)
%





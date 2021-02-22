-module(main).
-export[index/1].

index(File) ->
  ets:new(indexTable, [ordered_set, named_table]),
  processFile(File),
  prettyIndex().

processFile(File) ->
  {ok, IoDevice} = file:open(File, [read]),
  processLines(IoDevice, 1).

processLines(IoDevice, N) ->
  case io:get_line(IoDevice, "") of
    eof ->
      erlang:display(IoDevice),
      ok;
    Line ->
      processLine(Line, N),
      processLines(IoDevice, N+1)
  end.

-define(Punctuation, "(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line, N) ->
  case re:split(Line, ?Punctuation) of
    {ok, Words} ->
      erlang:display(N),
      processWords(Words, N);
    _ -> []
  end.

processWords(Words, N) ->
  case Words of
    [] ->
      erlang:display("Empty coso"),
      ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          ets:insert(inexTable, {{Normalise, N}});
        true -> ok
      end,
      processWords(Rest, N)
  end.

prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' ->
      ok;
    First ->
      case First of
        {Word, N} ->
          IndexEntry = {Word, [N]}
      end,
      prettyIndexNext(First, IndexEntry)
  end.

prettyIndexNext(Entry, {Word, Lines} = IndexEntry) ->
  Next = ets:next(indexTable, Entry),
  case Next of
    '$end_of_table' ->
      prettyEntry(IndexEntry);
    {NextWord, M} ->
      if NextWord == Word ->
        prettyIndexNext(Next, {Word, [M|Lines]});
      true ->
        prettyEntry(IndexEntry),
        prettyIndexNext(Next, {NextWord, [M]})
      end
  end.

prettyEntry({Word, [N]}) ->
  erlang:display(Word),
  erlang:display(N),
  ok.

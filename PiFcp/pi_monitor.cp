-monitor(serve).
-language(compound).

serve(In)+(UID = 1, Dash = 45 /* ascii("-", Dash) */) :-
  In ? unique_sender(String, Name),
  string_to_dlist(String, Head, [Dash | Tail]),
  convert_to_string(UID++, Suffix),
  string_to_dlist(Suffix, Tail, []),
  list_to_string(Head, Concatenated) :
    Name = Concatenated? |
      self;
  In ? Other,
  otherwise |
      self,
      fail(Other, unknown);
  In = [] | true.

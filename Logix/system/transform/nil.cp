/* $Header: /home/qiana/Repository/Logix/system/transform/nil.cp,v 1.1 1999/07/09 07:03:17 bill Exp $ */
-export([transform/5]).
-mode(trust).

transform(Attributes1, Terms1, Attributes2, Terms2, Errs) :-

    true : Terms1 = _,
      Attributes2 = Attributes1,
      Terms2 = [boot(`"_", `"_")],
      Errs = [] .

/* $Header: /home/qiana/Repository/Logix/system/ndg/phase0/self.cp,v 1.1.1.1 1999/07/09 07:03:06 bill Exp $ */
-language([compound]).
-scope(ndg).
-export([preproc/2,flow_analysis/2]).
-mode(trust).

procedure preproc(Procedures, Procs).

preproc(Procedures, Procs) :- preproc#preproc(Procedures, Procs).

procedure flow_analysis(Procs, Procs1).

flow_analysis(Procs, Procs1) :- flow_analysis#flow_analysis(Procs, Procs1).




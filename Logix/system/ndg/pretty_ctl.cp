/* $Header: /home/qiana/Repository/Logix/system/ndg/pretty_ctl.cp,v 1.1.1.1 1999/07/09 07:02:57 bill Exp $ */
-export([pretty/3]).
-mode(interrupt).
-language(compound).

procedure pretty(Any, String, String).

pretty(Context, File, Ctl) :-
	utils#append_strings([File, '.',Ctl], CtlFile),
	file # get_file(Context # CtlFile, CtlF, [string], Ok),
	ctl_file(Ok, Context, CtlFile, CtlF).
	

ctl_file(not_found, _, CtlFile, _) :-
	utils # append_strings(['ERROR - 996 - Error reading file ', CtlFile],
				Msg
		),
	computation # display(term, Msg, type(ground)).
ctl_file(true, Context, CtlFile, CtlF) :-
    true :
      ascii(lf, CR) |
	utils#append_strings([CtlFile,'.pretty'], PrettyFile),
	melt(CtlF, Ctls, CtlLs),
	label_names(CtlLs, 1),
	processor # interface(date_time(_, D, T)),
	procedures(Ctls, PrettyCtls, Open, Written, Written'),
	format([{1, ''(D, T)} | PrettyCtls], Terms, Pretty),
	terms_to_string # Terms,
	file # put_file(Context # PrettyFile, [Open | Pretty], characters(CR),
			Written
		),
	flat_context(Context, Context1),
	pretty_end(Written', Context1, PrettyFile).

flat_context(Context, ContOut)
:-
	context_to_list(Context, List),
	utils#append_strings(List, ContOut).

context_to_list(Context, List)
:-
	Context = {'#',A,Context'} : List = [A,' # '|List'] | context_to_list ;
	string(Context) : List = [Context].

pretty_end(Status, Context, File) :-

    Status = true |
	utils # append_strings(
		['MESSAGE 000 - File ', Context, ' # ', File,
			' has been written.'], Msg),
	computation # display(term, Msg, type(ground));

    otherwise : Context = _ |
	utils # append_strings(
		['MESSAGE 995 - Error Writing File ', File, ' - ', Status],
				Msg
		),
	computation # display(term, Msg, type(ground)).

label_names(Ls, I) :-

    Ls ? VL,
    convert_to_string(I, S),
    string_to_dlist(S, L, []),
    I' := I + 1 :
      ascii('V', V) |
	list_to_string([V | L], VL),
	label_names;

    Ls = [] : I = _ .


format(Ls, Terms, Pretty) + (Status = true) :-

    Status = true,
    Ls ? {Indent, Ctl},
    Width := 78 - Indent :
      Terms ! acyclic_grounded_terms_to_string(Ctl, Indent, Width, String),
      Pretty ! {String, Status'} |
	format;

    Status = true, Ls = [] :
      Terms = [],
      Pretty = [].


procedures(Ctls, PrH, Open, W1, W2) :- 

    Open = open :
      W2 = W1 |
	procedures(Ctls, PrH, Summary, Summary);

    Open =\= open : Ctls = _, W1 = _,
      W2 = open,
      PrH = [] .

procedures(Ctls, PrH, PrT, Summary) :- 

    Ctls ? {Proc1,Ctl} :
      PrH ! {0,' '},
      PrH' ! {0,{Proc1,length(Length)}},
      Summary ! {0,' '},
      Summary' ! {0,{Proc1,length(Length)}} |
	print_ctl(Ctl, PrH'', PrH''', 0, Length),
	procedures;

    Ctls = [] :
      PrH = [{0,' '} | PrT],
      Summary = [] .


print_ctl(Ctl, PrH, PrT, ILength, Length) :-

    Ctl ? Inst,
    Inst = label(_) : 
      PrH ! {1, Inst} |
	print_ctl;

    Ctl ? Inst,
    Inst =\= label(_),
    ILength' := ILength + 1 :
      PrH ! {4, Inst} |
	print_ctl;

    Ctl = [] :
      PrH = PrT,
      Length = ILength.


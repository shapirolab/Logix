-mode(trust).
-language(compound).
-export(dlv).

dlv(Server,Port,Ok) :-

	processor#dlv(stream(room(Server,Port,Ok,DlvIn,DlvOut,DlvHT))),
	computation#display(stream,DlvIn,put("/tmp/dlvin")),
	computation#display(stream,DlvOut,put("/tmp/dlvout")),
	computation#display(stream,DlvHT,put("/tmp/dlvht")).

-language(spifcp).

    Send(N, c) + I ::= {I := 0} | SendAndCount.

    SendAndCount(I, N, c) ::=
        {I++ < N}, SendNil(c) | self;
	{I >= N }, screen#display(sent - I*[])
    .

    SendNil(c) ::= c ! [], 0.

-module(epop_test).
-author('tobbe@serc.rmit.edu.au').
%%----------------------------------------------------------------------
%% File    : epop_test.erl
%% Created : 11 Mar 1998 by tobbe@serc.rmit.edu.au
%% Function: Some code for storing mail
%%----------------------------------------------------------------------
-vc('$Id$ ').
-export([store_mail/2]).

store_mail(User,Id) ->
    {Y,M,D} = date(),
    {Ms,S,Us} = erlang:now(),
    MsgID = lists:concat([Y,M,D,Ms,S,Us]),
    epop_dets:store_mail(User,MsgID,mail(User,Id)).

mail(User,1) ->
"
From tobbe@universe.serc.rmit.edu.au  Wed Mar 11 15:31:38 1998
Received: from universe.serc.rmit.edu.au (localhost [127.0.0.1])
        by universe.serc.rmit.edu.au (8.8.5/8.8.5) with ESMTP id PAA06105
        for <tobbe>; Wed, 11 Mar 1998 15:31:38 +1100 (EST)
Message-Id: <199803110431.PAA06105@universe.serc.rmit.edu.au>
From: Torbjorn Tornkvist <tobbe@serc.rmit.edu.au>
X-Mailer: MH 6.8.3
X-Erlang: http://www.erlang.se
To: " ++ User ++ 
"Subject: test
Date: Wed, 11 Mar 1998 15:31:38 +1100
Sender: tobbe@serc.rmit.edu.au


Hejsan pojkar !

För en vecka sedan fixade jag så att tjejen (Lotta)
kunde läsa mail hemifrån via en POP3 server i Svedala
(se www.home.se). Jag blev (som vanligt) lite nyfiken
på POP3 (Post Office Protocol version 3) protokollet
och strax så hamnade jag på ett stickspår (som vanligt ;-).

Så nu har jag implementerat ett full-blown (som Klacke brukar
säga) POP3 client/server paket. Så nu kan ni bli era
egna email-providers. Antar att nästa steg blir att implementera
en SMTP server (sendmail.... ;-).

Hur som helst, innan jag lägger upp det hela på arkivet
så har ni nu chansen att prova. Ni har alla fått ett
mail konto hos mig + ett par mail i era maildrops.
Ni har samma userid som på CSLab och password är detsamma
som userid'et. Gör så här för att prova om ni kan läsa mail:

 ...etc...

/Tobbe
";
mail(User,2) ->
"
From tobbe@universe.serc.rmit.edu.au  Wed Mar 11 15:31:38 1998
Received: from universe.serc.rmit.edu.au (localhost [127.0.0.1])
        by universe.serc.rmit.edu.au (8.8.5/8.8.5) with ESMTP id PAA06105
        for <tobbe>; Wed, 11 Mar 1998 15:31:38 +1100 (EST)
Message-Id: <199803110431.PAA06105@universe.serc.rmit.edu.au>
From: Torbjorn Tornkvist <tobbe@serc.rmit.edu.au>
X-Mailer: MH 6.8.3
X-Erlang: http://www.erlang.se
To: " ++ User ++
"Subject: hello
Date: Wed, 11 Mar 1998 15:31:38 +1100
Sender: tobbe@serc.rmit.edu.au


This is yet another test

Bye ! /Tobbe
".


/*
Group Members:
Sanveer Gill, 500953525, Section 5
Devrajsinh Chudasama, 500975539, Section 7
*/

flight(aa300,americanAirlines,ny,austin).
flight(ua95,unitedAirlines,chicago,toronto).
flight(ac987,airCanada,edmonton,montreal).
flight(ac087,airCanada,vancouver,shanghai).
flight(ua11,unitedAirlines,vancouver,ny).

flight(ac000,airCanada,toronto,shanghai).

flight(aa315,americanAirlines,austin,brampton).
flight(ua65,unitedAirlines,losAngeles,vegas).
flight(ac900,airCanada,barrie,mississauga).
flight(ac901,airCanada,toronto,vancouver).
flight(ac001,airCanada,toronto,beijing).
flight(ac888,airCanada,toronto,losAngeles).


flight(aa400,americanAirlines,austin,madrid).
flight(ua90,unitedAirlines,barcelona,london).
flight(ac100,airCanada,pittsburgh,calgary).
flight(ac200,airCanada,ottawa,
       losAngeles).
flight(aa999,americanAirlines,montreal,pittsburgh).

flight(ac905,airCanada,toronto,vancouver).
flight(ac906,airCanada,toronto,calgary).
flight(ac907,airCanada,toronto,edmonton).
flight(ac432, airCanada, toronto, london).
flight(ac009, airCanada, toronto, london).
flight(ua15, unitedAirlines, austin, miami).



dtime(ua15, austin, 1000).
dtime(ac009, toronto, 0900).
dtime(ac432, toronto, 0900).

dtime(ac888, toronto, 1300).
dtime(ac905, toronto, 0900).
dtime(ac906, toronto, 0800).
dtime(ac907, toronto, 1100).


dtime(ua11,vancouver,0510).
dtime(ac001,toronto,1845).
dtime(aa300,ny,1130).
dtime(ua95,chicago,1215).
dtime(ac987,edmonton,0900).
dtime(ac087,vancouver,1700).
dtime(aa315,austin,1100).
dtime(ua65,losAngeles,1730).
dtime(ac900,barrie,1900).
dtime(ac901,toronto,0800).
dtime(aa400,austin,0900).
dtime(ac000,toronto,0100).
dtime(aa999,montreal,0800).


dtime(ua90,barcelona,1300).
dtime(ac100,pittsburgh,1430).
dtime(ac200,ottawa,1930).


atime(ua15, miami, 1300).
atime(ac009, london, 1900).
atime(ac432, london, 1800).

atime(ac888, losAngeles, 1800).
atime(ac905, vancouver, 1300).
atime(ac906, calgary, 1100).
atime(ac907, edmonton, 1400).


atime(ua11,ny, 1200).
atime(ac001,beijing, 2400).
atime(aa999,pittsburgh,1540).
atime(ac000,shanghai,1330).
atime(aa300,austin,1330).
atime(ua95,toronto,1400).
atime(ac987,montreal,1300).
atime(ac087,shanghai,2000).
atime(aa315,brampton,1200).
atime(ua65,vegas,2230).
atime(ac900,mississauga,2100).
atime(ac901,vancouver,1600).
atime(aa400,madrid,1100).
atime(ua90,london,1700).
atime(ac100,calgary,1645).
atime(ac200,losAngeles,2330).

location(toronto,canada).		location(shanghai,china).	
location(austin, usa).			location(ny, usa).

location(chicago, usa).			location(edmonton, canada).
location(montreal, canada).		location(vancouver, canada).

location(brampton, canada).		location(losAngeles, usa).	
location(vegas, usa).			location(barrie, canada).

location(mississauga, canada).	location(calgary, canada).
location(madrid, spain).		location(barcelona, spain).

location(paris, france).		location(pittsburgh, usa).
location(calgary, canada).		location(ottawa, canada).

location(london, england). 		location(beijing,china).
location(london, uk). 			location(miami, usa).

article(an).
article(the).
article(a).
article(any).

common_noun(flight,FN):- flight(FN, _Airline, _City1, _City2).
common_noun(flight,T) :- dtime(_Flight, _City, T).
common_noun(flight,T) :- atime(_Flight, _City, T).

common_noun(city,X) :- location(X, _Country).
common_noun(country,X) :- location(_City, X).

common_noun(time, X) :- dtime(_Flight, _City, X).
common_noun(time, X) :- atime(_Flight, _City, X).

common_noun(time, X) :- flight(X, _Airline, _City1, _City2).

preposition(to, FN, Country) :- flight(FN, _Airline, _City, City2), location(City2, Country).
preposition(to, FN, City2) :- flight(FN, _Airline, _City, City2).

preposition(from, FN, Country) :- flight(FN, _Airline, City, _City2), location(City, Country).
preposition(from, FN, City) :- flight(FN, _Airline, City, _City2).

preposition(in, X, Y) :- location(X, Y).

preposition(with, Flight, DTime) :- flight(Flight, _Airline, City1, _City2), 
    dtime(Flight, City1, DTime).

preposition(with, Flight, _ATime) :- flight(Flight, _Airline, City1, _City2), 
    atime(Flight, City1, _DTime).

preposition(with, Flight, F) :- F = Flight.

preposition(between, X, Y) :- preposition(from, X, Y).
preposition(and, X, Y) :- preposition(to, X, Y).

preposition(of, Time, Flight) :- dtime(Flight, _City, Time).
preposition(of, Time, Flight) :- atime(Flight, _City, Time).

proper_noun(toronto).		proper_noun(austin).
proper_noun(losAngeles).	proper_noun(usa).
proper_noun(ny).			proper_noun(chicago).
proper_noun(england). 		proper_noun(edmonton).
proper_noun(montreal).
proper_noun(miami).
proper_noun(canada).
proper_noun(shanghai).
proper_noun(china).

proper_noun(vancouver).
proper_noun(brampton).
proper_noun(vegas).
proper_noun(barrie).
proper_noun(mississauga).
proper_noun(calgary).
proper_noun(madrid).
proper_noun(barcelona).
proper_noun(uk).

proper_noun(spain).
proper_noun(paris).
proper_noun(france).
proper_noun(pittsburgh).

proper_noun(ottawa).
proper_noun(london).
proper_noun(beijing).

adjective(domestic, F) :- flight(F, _Airline, City1, City2), location(City1, Country), 
    location(City2, Country).

adjective(international, F) :- flight(F, _Airline, City1, City2), location(City1, Country), 
     location(City2, Country2), \+ Country = Country2.

adjective(airCanada, F) :- flight(F, airCanada, _City1, _City2).
adjective(americanAirlines, F) :- flight(F, americanAirlines, _City1, _City2).  
adjective(unitedAirlines, F) :- flight(F, unitedAirlines, _City1, _City2).  

adjective(long, F) :- flight(ShortFlight, _Airline1, City1, City2), 
    flight(F, _Airline2, City1, City2), dtime(ShortFlight, City1, D), 
    atime(ShortFlight, City2, A), dtime(F, City1, D2), atime(F, City2, A2),
    (A-D) < (A2-D2).

adjective(shortest, F) :- flight(F, _Airline1, City1, City2), \+ (flight(F2, _Airline2, City3, City4),
    \+ F=F2, dtime(F,City1,D), atime(F,City2,A), dtime(F2,City3,D2), atime(F2,City4,A2),
                    (A2 - D2) < (A - D)).

adjective(shortest, F) :- flight(F, _Airline1, City1, City2), \+ (flight(F2, _Airline2, City3, City4),
    \+ F=F2, dtime(F,City1,D), atime(F,City2,A), dtime(F2,City3,D2), atime(F2,City4,A2),
       (A2 - D2) < (A - D), location(City1, Country), location(City2, Country2), location(City3, Country),
       location(City4, Country2), \+ Country = Country2).

adjective(shortest, F) :- flight(F, _Airline1, City1, City2), \+ (flight(F2, _Airline2, City1, City2),
    \+ F=F2, dtime(F,City1,D), atime(F,City2,A), dtime(F2,City1,D2), atime(F2,City2,A2),
                    A2 - D2 < A - D).
    


adjective(arrival, F) :- atime(F, _City, _Time).   
adjective(departure, F) :- dtime(F, _City, _Time).

adjective(arrival, T) :- atime(_Flight, _City, T).  
adjective(departure, T) :- dtime(_Flight, _City, T). 

adjective(morning, F) :- dtime(F, _City, T), T >= 500, T < 900. 
adjective(day, F) :- dtime(F, _City, T), T >= 900, T < 1200. 
adjective(afternoon, F) :- dtime(F, _City, T), T >= 1200, T < 1700. 
adjective(evening, F) :- dtime(F, _City, T), T >= 1700, T =< 2200.

adjective(morning, F) :- atime(F, _City, T), T >= 500, T < 900. 
adjective(day, F) :- atime(F, _City, T), T >= 900, T < 1200. 
adjective(afternoon, F) :- atime(F, _City, T), T >= 1200, T < 1700. 
adjective(evening, F) :- atime(F, _City, T), T >= 1700, T =< 2200.

%TIME ANSWER
adjective(morning, T) :- dtime(_F, _City, T), T >= 500, T < 900. 
adjective(day, T) :- dtime(_F, _City, T), T >= 900, T < 1200. 
adjective(afternoon, T) :- dtime(_F, _City, T), T >= 1200, T < 1700. 
adjective(evening, T) :- dtime(_F, _City, T), T >= 1700, T =< 2200.

adjective(morning, T) :- atime(_F, _City, T), T >= 500, T < 900. 
adjective(day, T) :- atime(_F, _City, T), T >= 900, T < 1200. 
adjective(afternoon, T) :- atime(_F, _City, T), T >= 1200, T < 1700. 
adjective(evening, T) :- atime(_F, _City, T), T >= 1700, T =< 2200.
                                                                         
adjective(american, Country) :- Country = usa.
adjective(american, City) :- location(City,usa).

adjective(chinese, Country) :- Country = china.
adjective(chinese, City) :- location(City,china).

adjective(canadian, Country) :- Country = canada.
adjective(canadian, City) :- location(City,canada).
/******************* parser **********************/

what(Words, Ref) :- np(Words, Ref).

/* Noun phrase can be a proper name or can start with an article */

np([Name],Name) :- proper_noun(Name).
np([the|Rest], Who) :- article(the), np2(Rest, Who).
np([Art|Rest], Who) :- article(Art), np2(Rest, Who).


/* If a noun phrase starts with an article, then it must be followed
   by another noun phrase that starts either with an adjective
   or with a common noun. */

np2([Adj|Rest],Who) :- adjective(Adj,Who), np2(Rest, Who).
np2([Noun|Rest], Who) :- common_noun(Noun, Who), mods(Rest,Who).


/* Modifier(s) provide an additional specific info about nouns.
   Modifier can be a prepositional phrase followed by none, one or more
   additional modifiers.  */

mods([], _).
mods(Words, Who) :-
	appendLists(Start, End, Words),
	prepPhrase(Start, Who),	mods(End, Who).

prepPhrase([Prep|Rest], Who) :-
	preposition(Prep, Who, Ref), np(Rest, Ref).

appendLists([], L, L).
appendLists([H|L1], L2, [H|L3]) :-  appendLists(L1, L2, L3).

% Tu trzeba dać sortowanie drzewiaste (narazie jest jakies z neta)
quick_sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):- pivoting(H,T,L1,L2),
    q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G):-X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X>H,pivoting(H,T,L,G).

%-	Ciag jest graficzny jesli jego podciag jest graficzny
%-	Podciągiem jest ciąg z wykluczeniem największego wierzchołka n i z odjeciem od pierwych
% n wierzołków po nim liczby 1
%-	Podciąg nie jest graficzny jeśli jego długość jest mniejsza od liczby n oraz lub któryś
% element ciągu jest mniejszy od 0
%-	Podciąg jest minimalny, gdy składa się z 0
%-	Podciąg minimalny jest graficzny

dec(X,Y) :- Y is X-1.

czy_lista_nie_ma_ujemnej([]).
czy_lista_nie_ma_ujemnej([H|T]) :- H>=0 , czy_lista_nie_ma_ujemnej(T) .

dlugosc_listy([],0).
dlugosc_listy([_|T],N) :- dlugosc_listy(T,N1), N is N1+1.

dolacz( [],L,L ).
dolacz( [H|T],L,[H|X] ) :- dolacz( T,L,X ).

zmniejsz_n_ele_o_1([],_,[]).
zmniejsz_n_ele_o_1(L,0,L).
zmniejsz_n_ele_o_1([H|T],N,[X|W]) :- X is H-1,dec(N,N1), zmniejsz_n_ele_o_1(T,N1,W).

minimalizuj([H|T], W) :-  quick_sort([H|T],[H1|T1]) ,dlugosc_listy(T1,N), N >= H ,
    zmniejsz_n_ele_o_1(T1,H1,W), czy_lista_nie_ma_ujemnej(W).

czy_minimalny([]).
czy_minimalny([H|T]) :- H == 0 , czy_minimalny(T).

suma_ele([],S) :- S = 0.
suma_ele([H|T],S) :- suma_ele(T,S1), S is S1 + H.

czy_graficzny(L) :- 
    czy_minimalny(L),! ;
    minimalizuj(L,M),!,
    czy_graficzny(M).

czy_graficzny(L,X) :- czy_graficzny(L) ,!, X = "Ciag jest graficzny";
    X = "Ciag nie jest graficzny".

:- use_module(library(random)).
:- use_module(library(filesex)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T],[0|T1],[X|T2]):-
    lanzamiento(T,T1,T2).
lanzamiento([_|T],[1|T1],[X1|T2]):-
    tiro_dado(X1),
    lanzamiento(T,T1,T2).

% Lanza un dado
tiro_dado(X):-
    random(1,7,X).


% Categoria Aces
puntaje(Dados, aces, Puntos) :-
    length(Dados, 5),
    findall(X, (member(X, Dados), X =:= 1), Aces),
    sum_list(Aces, Puntos).

% Categoria Twos 
puntaje(Dados, twos, Puntos) :-
    length(Dados, 5),
    findall(X, (member(X, Dados), X =:= 2), Aces),
    sum_list(Aces, Puntos).

% Categoria Threes 
puntaje(Dados, threes, Puntos) :-
    length(Dados, 5),
    findall(X, (member(X, Dados), X =:= 3), Aces),
    sum_list(Aces, Puntos).

% Categoria Fours 
puntaje(Dados, fours, Puntos) :-
    length(Dados, 5),
    findall(X, (member(X, Dados), X =:= 4), Aces),
    sum_list(Aces, Puntos).

% Categoria Fives
puntaje(Dados, fives, Puntos) :-
    length(Dados, 5),
    findall(X, (member(X, Dados), X =:= 5), Aces),
    sum_list(Aces, Puntos).

% Categoria Sixes
puntaje(Dados, sixes, Puntos) :-
    length(Dados, 5),
    findall(X, (member(X, Dados), X =:= 6), Aces),
    sum_list(Aces, Puntos).

% Categoria Three of a kind
puntaje(Dados, three_of_a_kind, Puntos) :-
    length(Dados, 5),
    cant_ocurrencias(_, Dados, Count),
    (Count >= 3 -> sum_list(Dados, Puntos) ; Puntos is 0).

% Categoria Four of a kind
puntaje(Dados, four_of_a_kind, Puntos) :-
    length(Dados, 5),
    cant_ocurrencias(_, Dados, Count),
    (Count >= 4 -> sum_list(Dados, Puntos) ; Puntos is 0).

% Categoria Full House
puntaje(Dados, full_house, Puntos) :-
    length(Dados, 5),
    msort(Dados, DadosOrdenados),
    ( (DadosOrdenados = [A, A, B, B, B] ; DadosOrdenados = [A, A, A, B, B])
    -> Puntos = 25
    ;  Puntos = 0
    ).

puntaje(Dados, small_straight, Puntos) :-
    sort(Dados, DadosOrdenados),
    ((sublist([1, 2, 3, 4], DadosOrdenados);
    sublist([2, 3, 4, 5], DadosOrdenados);
    sublist([3, 4, 5, 6], DadosOrdenados))
    -> Puntos = 30
    ;  Puntos = 0
    ).

puntaje(Dados, large_straight, Puntos) :-
    sort(Dados, DadosOrdenados),
    ((sublist([1, 2, 3, 4, 5], DadosOrdenados);
    sublist([2, 3, 4, 5, 6], DadosOrdenados))
    -> Puntos = 40
    ;  Puntos = 0
    ).

% Categoria Full House
puntaje(Dados, yahtzee, Puntos) :-
    length(Dados, 5),
    ( (Dados = [A, A, A, A, A])
    -> Puntos = 50
    ;  Puntos = 0
    ).

puntaje(Dados, chance, Puntos) :-
    length(Dados, 5),
    sum_list(Dados, Puntos).

cant_ocurrencias(_, [], 0).  
cant_ocurrencias(X, [X|T], Count) :- cant_ocurrencias(X, T, TailCount), Count is TailCount + 1.  
cant_ocurrencias(X, [H|T], Count) :- X \= H, cant_ocurrencias(X, T, Count). 

sublist(SubL, L) :-
    append(_, L2, L),
    append(SubL, _, L2).

%puntaje_tablero(+Tablero, -Puntaje) -> Tablero contiene un tablero con todos los slots completos. El segundo argumento contiene el puntaje para ese tablero
puntaje_tablero(Tablero, Puntaje) :-
    sumar_puntaje(Tablero, 0, 0, PuntajeSuperior, PuntajeInferior),
    SumaPuntaje is PuntajeSuperior + PuntajeInferior,
    (PuntajeSuperior >= 63 
    -> Puntaje is SumaPuntaje + 35
    ; Puntaje is SumaPuntaje
    ).

% Caso base: cuando la lista está vacía, el puntaje acumulado es el puntaje final
sumar_puntaje([], PuntajeFinalSuperior, PuntajeFinalInferior, PuntajeFinalSuperior, PuntajeFinalInferior).

% Caso recursivo: sumar el puntaje del primer slot al puntaje acumulado y procesar el resto de la lista
sumar_puntaje([s(Categoria, P)|Resto], PuntajeSuperiorAcumulado, PuntajeInferiorAcumulado, PuntajeFinalSuperior, PuntajeFinalInferior) :-
    member(Categoria, [aces, twos, threes, fours, fives, sixes])
    -> NuevoPuntajeSuperior is PuntajeSuperiorAcumulado + P, sumar_puntaje(Resto, NuevoPuntajeSuperior,PuntajeInferiorAcumulado, PuntajeFinalSuperior, PuntajeFinalInferior)
    ; NuevoPuntajeInferior is PuntajeInferiorAcumulado + P, sumar_puntaje(Resto, PuntajeSuperiorAcumulado, NuevoPuntajeInferior, PuntajeFinalSuperior, PuntajeFinalInferior).


ajustar_tablero(Tablero, Categoria, Puntaje, TableroSalida) :-
    nth1(Index, Tablero, s(Categoria, _)), % Encuentra el índice de la categoría en el tablero
    replace_nth1(Index, Tablero, s(Categoria, Puntaje), TableroSalida).

% Predicado auxiliar para reemplazar el elemento en la posición N de una lista.
replace_nth1(1, [_|T], X, [X|T]).
replace_nth1(N, [H|T], X, [H|R]) :-
    N > 1,
    N2 is N - 1,
    replace_nth1(N2, T, X, R).


% Estrategia para el jugador humano
cambio_dados(Dados, Tablero, humano, Patron) :-
    writeln('Dados actuales:'),
    writeln(Dados),
    writeln('Tablero actual:'),
    imprimir_tablero(Tablero),
    writeln('Ingrese el patrón de dados a cambiar (ej. [1,0,0,1,0]):'),
    read(Patron).

% Estrategia para ia_det
% Para esta estrategia considere tomar las Categorias desde la más probable a la menos probable
% donde retorno el patron de la categoria menos probable disponible haciendo re-roll de los dados
% menos frecuentes en la lista.
cambio_dados(Dados, Tablero, ia_det, Patron) :-
    length(Dados, 5), % Verificar que la lista de dados tenga 5 elementos
    yahtzee_chance(Dados, Tablero, Patron), !;
    large_straight_chance(Dados, Tablero, Patron), !;
    four_of_a_kind_chance(Dados, Tablero, Patron), !;
    sixes_chance(Dados, Tablero, Patron), !;
    small_straight_chance(Dados, Tablero, Patron), !;
    full_house_chance(Dados, Tablero, Patron), !;
    three_of_a_kind_chance(Dados, Tablero, Patron), !;
    number_chance(Dados, Tablero, Patron, fives, 5), !;
    number_chance(Dados, Tablero, Patron, fours, 4), !;
    number_chance(Dados, Tablero, Patron, threes, 3), !;
    number_chance(Dados, Tablero, Patron, twos, 2), !;
    number_chance(Dados, Tablero, Patron, aces, 1), !;
    Patron = [0,0,0,0,0].

% Estrategia para ia probabilista pra juego yahtzee
cambio_dados(Dados, Tablero, ia_prob, Patron) :-
    length(Dados, 5), % Verificar que la lista de dados tenga 5 elementos
    frecuencia(Dados, Frecuencias),
    max_member(MaxFreq, Frecuencias),
    (   Dados = [A,A,A,A,A],
        member(s(yahtzee, nil), Tablero)
    ->  Patron = [0,0,0,0,0], !

    ;   Dados = [A,A,A,A,A] % aca se pueden evaluar otros casos
    ->  Patron = [0,0,0,0,0], !

    ;   MaxFreq =:= 4,
        member(s(yahtzee, nil), Tablero)
    ->  max_frequencia(Frecuencias, MaxNum),
        patron_segun_frecuencia(Dados, MaxNum, Patron), !

    ;   MaxFreq =:= 4,
        member(s(sixes, nil), Tablero),
        msort(Dados, DadosOrdenados),
        sublist([6,6,6,6], DadosOrdenados)
    ->  patron_segun_frecuencia(Dados, 6, Patron), !
    
    ;   MaxFreq =:= 4,
        member(s(four_of_a_kind, nil), Tablero),
        max_frequencia(Frecuencias, MaxNum),
        nth1(MinNum, Frecuencias, 1),
        (MinNum =:= 1; MinNum =:= 2)
    ->  patron_segun_frecuencia(Dados, MaxNum, Patron), !

    ;   MaxFreq =:= 4,
        member(s(four_of_a_kind, nil), Tablero)
    ->  Patron = [0,0,0,0,0], !

    ;   MaxFreq =:= 4,
        member(s(three_of_a_kind, nil), Tablero),
        max_frequencia(Frecuencias, MaxNum),
        nth1(MinNum, Frecuencias, 1),
        (MinNum =:= 1; MinNum =:= 2)
    ->  patron_segun_frecuencia(Dados, MaxNum, Patron), !

    ;   MaxFreq =:= 4,
        member(s(three_of_a_kind, nil), Tablero)
    ->  Patron = [0,0,0,0,0], !

    ;   MaxFreq =:= 3,
        member(s(full_house, nil), Tablero),
        member(2, Frecuencias)
    ->  Patron = [0,0,0,0,0], !

    ;   (MaxFreq =:= 3; MaxFreq =:= 2),
        (
            % En ListaFiltrada \= [] falla si ninguna de las categorias que se evaluan en problog
            % estan vacias.
            % Lo mejor seria checkear que alguna de las categorias que se evaluan en problog este
            % vacia porque sino no tendria sentido evaluar en prolog porque igual no nos sirve
            % ninguna de esas categorias
            member(s(yahtzee, nil), Tablero);
            member(s(four_of_a_kind, nil), Tablero);
            member(s(full_house, nil), Tablero);
            member(s(three_of_a_kind, nil), Tablero);
            member(s(small_straight, nil), Tablero);
            member(s(large_straight, nil), Tablero)
        )
    ->  findall(Cat, member(s(Cat, nil), Tablero), CategoriasLibres),
        max_frequencia(Frecuencias, MaxNum),
        patron_segun_frecuencia(Dados, MaxNum, NewPatron),
        evaluar_patron(Dados, NewPatron, ResultadoModelo),
        include(es_categoria_libre(CategoriasLibres), ResultadoModelo, ListaFiltrada),
        ListaFiltrada \= [],
        categoria_mayor_probabilidad(ListaFiltrada, MejorCategoria, _),
        elegir_mejor_patron(Dados, Frecuencias, MaxFreq, MaxNum, MejorCategoria, Tablero, Patron), !

    ;   large_straight_chance(Dados, Tablero, Patron), !;
        patron_con_dados_menores(Dados, 4, Patron), !
        %Patron = [0,0,0,0,0] % en chance se pueden rollear los dados menores
    ).


elegir_mejor_patron(Dados, Frecuencias, _MaxFreq, MaxNum, MejorCategoria, Tablero, Patron) :-
    categorias(Categorias),
    member(MejorCategoria, Categorias),
    (   MejorCategoria = three_of_a_kind
    ->  patron_segun_frecuencia(Dados, MaxNum, Patron)
    ;   MejorCategoria = four_of_a_kind
    ->  patron_segun_frecuencia(Dados, MaxNum, Patron)
    ;   MejorCategoria = full_house
    ->  patron_full_house(Dados, Frecuencias, Patron)
    ;   MejorCategoria = small_straight
    ->  small_straight_chance(Dados, Tablero, Patron)
    ;   MejorCategoria = large_straight
    ->  large_straight_chance(Dados, Tablero, Patron)
    ;   MejorCategoria = yahtzee
    ->  patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

categoria_mayor_probabilidad([p(Categoria, Probabilidad)], Categoria, Probabilidad).

categoria_mayor_probabilidad([p(Categoria, Probabilidad) | T], CategoriaMaxProbablidad, MaxProbabilidad) :-
    categoria_mayor_probabilidad(T, TailCat, TailMax),
    (Probabilidad > TailMax ->
        (CategoriaMaxProbablidad = Categoria, MaxProbabilidad = Probabilidad)
    ;
        (CategoriaMaxProbablidad = TailCat, MaxProbabilidad = TailMax)
    ).

es_categoria_libre(CategoriasLibres, p(Cat, _)) :-
    member(Cat, CategoriasLibres).
        
% evaluar_patrones(Dados, Patrones, MejorPatron) :-
%     maplist(evaluar_patron(Dados), Patrones, Resultados),
%     encontrar_mejor_patron(Resultados, MejorPatron).

evaluar_patron(Dados, Patron, Resultado) :-
    write('Evaluando patron:'),
    writeln(Patron),
    agregar_evidencias_a_modelo(Dados, Patron),
    ejecutar_problog_queries(Resultado).

% Encuentra el mejor patron basado en los resultados
% encontrar_mejor_patron(_Resultados, _MejorPatron) :-
%     writeln('Aqui debe encontrar el mejor patron').

% Genera todos los patrones posibles de cambio de dados
generar_patron(Patron) :-
    generar_patron_aux(5, Patron).

generar_patron_aux(0, []).
generar_patron_aux(N, [0|Resto]) :-
    N > 0,
    N1 is N - 1,
    generar_patron_aux(N1, Resto).
generar_patron_aux(N, [1|Resto]) :-
    N > 0,
    N1 is N - 1,
    generar_patron_aux(N1, Resto).

    

generar_combinaciones([], _, [[]]).

generar_combinaciones([Num|Rest], Target, Combinations) :-
    member(Num, Target),
    generar_combinaciones(Rest, Target, SubCombinations),
    maplist(prepend_zero, SubCombinations, Combinations).

generar_combinaciones([Num|Rest], Target, Combinations) :-
    \+ member(Num, Target),
    generar_combinaciones(Rest, Target, SubCombinations),
    maplist(prepend_zero, SubCombinations, ZeroCombinations),
    maplist(prepend_one, SubCombinations, OneCombinations),
    append(ZeroCombinations, OneCombinations, Combinations).

prepend_zero(List, [0|List]).
prepend_one(List, [1|List]).
    

ejecutar_problog_queries(ListaValores) :-
    absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),
    absolute_file_name(modelo_problog, Modelo, [file_type(prolog)]),
    process_create(Problog, [Modelo], [stdout(pipe(In))]),
    read_string(In, _, Result),
    writeln(Result),
    split_string(Result, "\n\t", "\r ", Lines),
    append(Lines1, [_], Lines), % Quito último elemento de la lista
    lista_valores(Lines1, ListaValores).

lista_valores([X,Y|T],[TermValor|T1]):-
    split_string(X,"",":",[X1|_]), % Saco los dos puntos del final
    term_string(TermX,X1), % Convierto a término
    number_string(NumberY,Y), % Convierto a número
    TermValor =.. [p,TermX,NumberY], % Creo el término
    lista_valores(T,T1).
lista_valores([],[]).

    

agregar_evidencias_a_modelo(Dados, Patron) :-
    % Abrir el modelo original para lectura
    open('modelo_problog.pl', read, OriginalStream),
    leer_lineas(OriginalStream, Lines),
    close(OriginalStream),

    % Separar las queries y evidencias de las demás lineas
    exclude(linea_es_query, Lines, NonQueryLines),
    include(linea_es_query, Lines, QueryLines),
    exclude(linea_es_evidencia, NonQueryLines, NonQueryNonEvidenceLines),
    
    % Abrir el archivo modelo para escritura
    open('modelo_problog.pl', write, Stream),
    maplist(writeln(Stream), NonQueryNonEvidenceLines),
    maplist(escribir_linea_evidencia(Stream, Patron), Dados, [1, 2, 3, 4, 5]),
    maplist(writeln(Stream), QueryLines),
    close(Stream).


leer_lineas(Stream, Lineas) :-
    read_line_to_string(Stream, Linea),
    (   Linea = end_of_file
    ->  Lineas = []
    ;   Lineas = [Linea | RestoLineas],
        leer_lineas(Stream, RestoLineas)
    ).

linea_es_query(Linea) :-
    sub_string(Linea, 0, _, _, "query(").

linea_es_evidencia(Linea) :-
    sub_string(Linea, 0, _, _, "evidence(").

% Escribir linea de evidencia en archivo
escribir_linea_evidencia(Stream, Patron, DadoValor, DadoIndex) :-
    nth1(DadoIndex, Patron, Keep),
    (   Keep =:= 0
    ->  format(atom(Evidencia), 'evidence(dado~w(~w)).', [DadoIndex, DadoValor]),
        writeln(Stream, Evidencia)
    ;   true
    ).

yahtzee_chance(Dados, Tablero, Patron) :-
    member(s(yahtzee, nil), Tablero),
    (   Dados = [A,A,A,A,A]
    ->  Patron = [0,0,0,0,0]
    ;   frecuencia(Dados, Frecuencias),
        max_frequencia(Frecuencias, MaxNum),
        patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

large_straight_chance(Dados, Tablero, Patron) :-
    member(s(large_straight, nil), Tablero),
    (   subset([1,2,3,4,5], Dados)
    ->  Patron = [0,0,0,0,0]
    ;   subset([2,3,4,5,6], Dados)
    ->  Patron = [0,0,0,0,0]
    ;   encontrar_numeros_faltantes(Dados, Faltantes1, [1,2,3,4,5]),
        encontrar_numeros_faltantes(Dados, Faltantes2, [2,3,4,5,6]),
        length(Faltantes1, L1),
        length(Faltantes2, L2),
        (   L1 < L2
        ->  patron_large_straight(Dados, Patron, [1,2,3,4,5])
        ;   patron_large_straight(Dados, Patron, [2,3,4,5,6])
        )
           
    ).
    
four_of_a_kind_chance(Dados, Tablero, Patron) :-
    member(s(four_of_a_kind, nil), Tablero),
    frecuencia(Dados, Frecuencias),
    (   member(4, Frecuencias)
    ->  Patron = [0,0,0,0,0]
    ;   max_frequencia(Frecuencias, MaxNum),
        patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

sixes_chance(Dados, Tablero, Patron) :-
    member(s(sixes, nil), Tablero),
    frecuencia(Dados, Frecuencias),
    max_frequencia(Frecuencias, MaxNum),
    MaxNum = 6, 
    (   Frecuencias = [_,_,_,_,_,5] 
    ->  Patron=[0,0,0,0,0]
    ;   patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

small_straight_chance(Dados, Tablero, Patron) :-
    member(s(small_straight, nil), Tablero),
    (   (subset([1,2,3,4], Dados); subset([2,3,4,5], Dados); subset([3,4,5,6], Dados))
    ->  Patron = [0,0,0,0,0]
    ;   encontrar_numeros_faltantes(Dados, Faltantes1, [1,2,3,4]),
        encontrar_numeros_faltantes(Dados, Faltantes2, [2,3,4,5]),
        encontrar_numeros_faltantes(Dados, Faltantes3, [3,4,5,6]),
        length(Faltantes1, L1),
        length(Faltantes2, L2),
        length(Faltantes3, L3),
        min_list([L1, L2, L3], MinL),
        (   MinL =:= L1
        ->  patron_large_straight(Dados, Patron, [1,2,3,4])
        ;   MinL =:= L2
        ->  patron_large_straight(Dados, Patron, [2,3,4,5])
        ;   patron_large_straight(Dados, Patron, [3,4,5,6])
        )
    ).

full_house_chance(Tablero, Dados, Patron) :-
    member(s(full_house, nil), Tablero),
    frecuencia(Dados, Frecuencias),
    (   member(3, Frecuencias), 
        member(2, Frecuencias)
    ->  Patron = [0,0,0,0,0]
    ;   patron_full_house(Dados, Frecuencias, Patron)
    ).

three_of_a_kind_chance(Dados, Tablero, Patron) :-
    member(s(three_of_a_kind, nil), Tablero),
    frecuencia(Dados, Frecuencias),
    (   member(3, Frecuencias)
    ->  Patron = [0,0,0,0,0]
    ;   max_frequencia(Frecuencias, MaxNum),
        patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

number_chance(Dados, Tablero, Patron, Category, Number) :-
    member(s(Category, nil), Tablero),
    frecuencia(Dados, Frecuencias),
    max_frequencia(Frecuencias, MaxNum),
    MaxNum = Number,
    (   Frecuencias = [_,_,_,_,5,_] 
    ->  Patron=[0,0,0,0,0]
    ;   patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

% eliminar primer ocurrencia de Elemento de lista
eliminar_primer_ocurrencia(_, [], []) :- !.
eliminar_primer_ocurrencia(Elemento, [Elemento|Resto], Resto) :- !.
eliminar_primer_ocurrencia(Elemento, [Primer|Resto], [Primer|RestoResultado]) :-
    eliminar_primer_ocurrencia(Elemento, Resto, RestoResultado).

patron_large_straight([], [], _).
patron_large_straight([Num|Resto], [P|PatronResto], Target) :-
    (   member(Num, Target)
    ->  P = 0,
        eliminar_primer_ocurrencia(Num, Target, NewTarget)
    ;   P = 1,
        NewTarget = Target
    ),
    patron_large_straight(Resto, PatronResto, NewTarget).

patron_full_house(Dados, Frecuencias, Patron) :-
    findall(Num, (nth1(Idx, Frecuencias, F), F > 0, nth1(Idx, [1,2,3,4,5,6], Num)), Numbers),
    length(Numbers, Len),
    (   Len = 3
    ->  % caso ejemplo: 1,1,1,2,3 (quedarnos con los 1's y cambiar el 2 y 3)
        max_frequencia(Frecuencias, MaxNum),
        subtract(Dados, [MaxNum], Resto),
        Resto = [_|Numero],
        patron_reroll_si_pertenecen_a_lista(Dados, Numero, Patron)
    ;   Len = 4
    ->  % caso ejemplo: 1,2,3,4,4 (quedarnos con los 4's y cambiar el 1, 2 y 3)
        max_frequencia(Frecuencias, MaxNum),
        subtract(Dados, [MaxNum], Resto),
        Resto = [_|DosNumeros],
        patron_reroll_si_pertenecen_a_lista(Dados, DosNumeros, Patron)
    ;   Len = 5
    ->  % caso ejemplo: 1,2,3,4,5 (cambiar todos los dados)
        Patron = [1,1,1,1,1]
    ;   max_frequencia(Frecuencias, MaxNum),
        patron_segun_frecuencia(Dados, MaxNum, Patron)
    ).

patron_reroll_si_pertenecen_a_lista([], _, []).
patron_reroll_si_pertenecen_a_lista([Num|Resto], Target, [P|PatronResto]) :-
    (   member(Num, Target)
    ->  P = 1
    ;   P = 0
    ),
    patron_reroll_si_pertenecen_a_lista(Resto, Target, PatronResto).


encontrar_numeros_faltantes(Dados, Faltantes, Target) :-
    findall(X, (member(X, Target), \+ member(X, Dados)), Faltantes).

frecuencia(Dados, Frecuencias) :-
    findall(F, (between(1, 6, Num), cant_ocurrencias(Num, Dados, F)), Frecuencias).
    
% Encontrar en que dado se da la maxima frecuencia
max_frequencia(Frecuencias, MaxNum) :-
    max_member(MaxFrec, Frecuencias),
    nth1(MaxNum, Frecuencias, MaxFrec).

max_frecuencias(Frecuencias, MaxNums) :-
    max_member(MaxFrec, Frecuencias),
    findall(Index, (nth1(Index, Frecuencias, MaxFrec)), MaxNums).


patron_segun_frecuencia([], _, []).
patron_segun_frecuencia([Num|Resto], MaxNum, [P|PatronResto]) :-
    (   Num =:= MaxNum
    ->  P = 0
    ;   P = 1
    ),
    patron_segun_frecuencia(Resto, MaxNum, PatronResto).

% Devuelve un patron con 1s en los dados con valor menor a MaximoValor
patron_con_dados_menores([], _, []).
patron_con_dados_menores([Num|Resto], MaximoValor, [P|PatronResto]) :-
    (   Num >= MaximoValor
    ->  P = 0
    ;   P = 1
    ),
    patron_con_dados_menores(Resto, MaximoValor, PatronResto).

% Estrategia para humano
eleccion_slot(Dados, Tablero, humano, Categoria) :-
    writeln('Dados actuales:'),
    writeln(Dados),
    writeln('Tablero actual:'),
    imprimir_tablero(Tablero),
    writeln('Ingrese la categoría para asignar el puntaje:'),
    read(Categoria).

% Estrategia para ia_det
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
    findall(Cat, member(s(Cat, nil), Tablero), CategoriasLibres),
    mejor_categoria(Dados, CategoriasLibres, Categoria).

eleccion_slot(Dados, Tablero, ia_prob, Categoria) :-
    findall(Cat, member(s(Cat, nil), Tablero), CategoriasLibres),
    mejor_categoria(Dados, CategoriasLibres, Categoria).


mejor_categoria(_, [Cat], Cat) :- !.
mejor_categoria(Dados, [Cat|Resto], MejorCat) :-
    puntaje(Dados, Cat, Puntaje1),
    mejor_categoria(Dados, Resto, MejorResto),
    puntaje(Dados, MejorResto, Puntaje2),
    (Puntaje1 >= Puntaje2 -> MejorCat = Cat ; MejorCat = MejorResto).


imprimir_tablero(Tablero) :-
    write('-------------------------------------'), nl,
    write('|   Categoria      |   Puntaje      |'), nl,
    write('-------------------------------------'), nl,
    imprimir_filas(Tablero),
    writeln('-------------------------------------').

imprimir_filas([]).
imprimir_filas([s(Categoria, Puntaje)|Resto]) :-
    format('| ~w~t~30| | ~w~t~15|', [Categoria, Puntaje]), nl,
    imprimir_filas(Resto).




% **********************************************************
% === LOS SIGUIENTE PREDICADOS SON PARA PROBAR EL JUEGO ===
% **********************************************************


% Inicializo el tablero a partir de la lista de categorías
tablero_inicial([],[]).
tablero_inicial([Cat|Cats],[s(Cat,nil)|T1]):-
        tablero_inicial(Cats,T1).

yahtzee(Estrategia,Seed) :-
    yahtzeelog(Estrategia,Seed).

% Jugador yahtzee
% Jugador puede ser humano o ia
yahtzeelog(Estrategia,Seed):-
    yahtzeelog(Estrategia, Seed, _).

yahtzeelog(Estrategia, Seed, PuntajeFinal):-
    set_random(seed(Seed)),
    partida(Estrategia,TableroFinal),
    writeln('Termino el juego'),
    % Termina el juego, calculo los resultados.
    writeln(TableroFinal),
    puntaje_tablero(TableroFinal,PuntajeFinal),
    write('Puntaje obtenido:'),writeln(PuntajeFinal),
    imprimir_tablero(TableroFinal).

simular_varios_juegos(Estrategia, Archivo, Desde, Hasta) :-
    simular_varios_juegos([], Estrategia, Archivo, Desde, Hasta, _).
simular_varios_juegos(Acumulador, Estrategia, Archivo, Seed, Hasta, SeedsPuntajes) :-
    write('HASTA AHORA: '),
    writeln(Acumulador),
    Seed =< Hasta
    -> ( yahtzeelog(Estrategia, Seed, Puntaje)
         -> open(Archivo, append, Stream),
            write(Stream, Seed),
            write(Stream, ","),
            write(Stream, Puntaje),
            write(Stream, "\n"),
            close(Stream)
         ;  open(Archivo, append, Stream),
            write(Stream, Seed),
            write(Stream, ","),
            write(Stream, "Fallo"),
            write(Stream, "\n"),
            close(Stream)
       ),
       NuevaSeed is Seed + 1,
       simular_varios_juegos([par(Seed, Puntaje)|Acumulador], Estrategia, Archivo, NuevaSeed, Hasta, SeedsPuntajes)
    ;  SeedsPuntajes = Acumulador.

% Esto es simplemente para no utilizar ronda1 como sinónimo de juego
partida(Estrategia,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,Estrategia,Tablero,TableroFinal).

% Ronda de juego
% NumRonda es el número de ronda
% Tablero es el Tablero hasta el momento
% TableroSalida es el Tablero una vez finalizada la ronda
ronda(L1,_,Tablero,Tablero):-
    categorias(C),
    length(C,L),
    L1 =:= L+1.

ronda(NumRonda,Estrategia,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    imprimir_tablero(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,Estrategia,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,Estrategia,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,Estrategia,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1, 
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,Estrategia,Tablero2,TableroSalida).



%[s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10),s(sixes,18), s(three_of_a_kind,20), s(four_of_a_kind,22),s(full_house,0), s(small_straight,0), s(large_straight,40),s(yahtzee,50),s(chance,10)]

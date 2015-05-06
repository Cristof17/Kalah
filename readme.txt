Conținutul arhivei este următorul:

* Board.hs, BoardTest.hs: implementarea tablei de joc și a mutărilor,
	alături de testele aferente

* AISimple.hs, AISimpleTest.hs: implementarea euristicii simple de joc,
	alături de testele aferente

* AIMinimax.hs, AIMinimaxTest.hs: implementarea algoritmului minimax
	alături de testele aferente (pentru bonus)

* Tests.hs: toate testele de mai sus, cumulate

* Interactive.hs: mecanisme de I/O, ce permit jocul între doi utilizatori
    umani sau între un utilizator uman și calculator, pe baza euristicilor.

Testele pot fi rulate individual sau toate o dată, pe baza fișierului
'Tests.hs'. Pentru rularea globală, invocați din terminalul sistemului
de operare:

    > runhaskell Tests

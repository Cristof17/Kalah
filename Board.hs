{-
    Tabla de joc și mutările posibile.

    Modulul exportă numai funcțiile enumerate mai jos, ceea ce înseamnă
    că doar acestea vor fi vizibile din alte module. Justificarea vizează
    prevenirea accesului extern la structura obiectelor 'Board', și a eventualei
    coruperi a consistenței interne a acestora.
-}
module Board
    ( Board
    , Player (..)  -- Exportăm și constructorii de date 'You' și 'Opponent'.
    , House
    , build
    , yourSeeds
    , oppsSeeds
    , who
    , isOver
    , initialBoard
    , move
    , scores
    , successors
    ) where

import Consecutive

{-
    Jucătorul care urmează să mute.
-}
data Player = You | Opponent deriving (Eq, Show)

{-
    Tipul caselor, definit pentru lizibilitate.
-}
type House = Int

{-
    *** TODO ***

    Definiți tipul 'Board', astfel încât să rețină informație despre starea
    jocului, inclusiv tabla de joc.

    Observați că viitorii constructori de date și eventualele câmpuri pe care
    le veți specifica nu vor apărea în lista de funcții exportate de modul
    (vezi explicația de la începutul fișierului).
-}
data Board = Board (([House] , Int) , ([House]  ,Int)) Player deriving (Eq, Show)

{-
    *** TODO ***

    Instanțiați clasa 'Show' cu tipul 'Board'. Exemplu de reprezentare,
    unde scorurile sunt aferent jucătorilor 'You', respectiv 'Opponent':

       4  4  4  4  4  4
     0                  0    Next: You, Playing, Score: (0,0)
       4  4  4  4  4  4
-}

{-instance Show Board where
    show = undefined
-}
    
    
{-
    *** TODO BONUS ***

    Instanțiați clasa 'Consecutive', pentru a putea determina dacă în două
    configurații ale tablei trebuie să mute același jucător.
-}
instance Consecutive Board where
    b1 >< b2 = undefined

{-
    *** TODO ***

    Construiește tabla de joc.

    Funcția trebuie să determine dacă jocul este deja încheiat, pe baza
    conținutului caselor.
-}
build :: ([Int], Int)  -- Conținutul caselor și al depozitului utilizatorului
      -> ([Int], Int)  -- Conținutul caselor și al depozitului adversarului
      -> Player        -- Jucătorul aflat la rând
      -> Board         -- Tabla construită
build (house1 , score1) (house2 , score2 ) player  = (Board ((house1 , score1 ) , (house2 , score2)) player)

{-
    *** TODO ***

    Întoarce conținutul caselor și al depozitului utilizatorului.
-}
yourSeeds :: Board -> ([Int], Int)
yourSeeds (Board ((house1 , score1) , (house2  , score2)) player) = (house1 , score1 )

{-
    *** TODO ***

    Întoarce conținutul caselor și al depozitului adversarului.
-}
oppsSeeds :: Board -> ([Int], Int)
oppsSeeds (Board ((house1 , score1) , (house2  , score2)) player) = (house2 , score2)

{-
    *** TODO ***

    Întoarce jucătorul aflat la rând.
-}
who :: Board -> Player
who (Board ((house1 , score1) , (house2  , score2)) player) = player

{-
    *** TODO ***

    Întoarce 'True' dacă jocul s-a încheiat.
-}
isOver :: Board -> Bool
isOver (Board ((house1 , score1) , (house2  , score2)) player)= (null (filter (\x -> x > 0) house1)) || (null (filter (\x -> x > 0) house2)) 

{-
    *** TODO ***

    Tabla inițială.
-}
initialBoard :: Board
initialBoard = (Board (([4,4,4,4,4,4] , 0) , ([4,4,4,4,4,4] , 0)) You)

{-
    *** TODO ***

    Realizează o mutare pornind de la casa furnizată ca parametru, în funcție
    de configurația actuală a tablei și de jucătorul aflat la rând.

    Întoarce aceeași configurație dacă mutarea nu poate fi efectuată
    din diverse motive, precum numărul eronat al casei, sau casa goală.
-}

move :: House -> Board -> Board -- modified Board return with [House]
move house (Board ((house1 , score1) , (house2  , score2)) player)  = if house > 6 || house <= 0 then board
																	  else 
																		if player == You then 
																			if (house1 !! (house -1)) == 0 then board --casa eronata
																				else
																					if(result_player_you == Opponent) then -- am furat scoici de la oponent (am trecut de la user YOU la OPPONENT)
																						processBoardFromResponseYou score2 result_list_you result_player_you
																					else
																						if isOver (processBoardFromResponseYou score2 result_list_you player) then
																							 (processBoardFromResponseYou score2 result_list_you You)
																						else
																							 (processBoardFromResponseYou score2 result_list_you (keepPlayer board house))
																		else 
																			if (house2 !! (house - 1)) == 0 then board
																				else
																					if(result_player_opponent == You) then
																						processBoardFromResponseOpponent score1 result_list_opponent result_player_opponent
																					else
																						if isOver (processBoardFromResponseOpponent score1 result_list_opponent player) then
																							processBoardFromResponseOpponent score1 result_list_opponent Opponent
																						else
																							processBoardFromResponseOpponent score1 result_list_opponent (keepPlayer board house)
																	where
																		result_you = (increment_helper (house +1 ) (house1 !! (house -1)) ((replaceAt (house-1) 0 house1) ++ [score1] ++ reverse house2 ) You)
																		result_opponent = (increment_helper (6 - house + 1 + 1) (house2 !! (house - 1)) ((replaceAt (6 - house) 0 (reverse house2)) ++ [score2] ++ house1) Opponent)
																		result_list_you = (fst result_you)
																		result_list_opponent = (fst result_opponent)
																		result_player_you = (snd result_you)
																		result_player_opponent = (snd result_opponent)
																		board = (Board ((house1 , score1) , (house2  , score2)) player) 
																		
keepPlayer :: Board -> Int -> Player    -- check to see if the player keeps his turn 
keepPlayer board position =	if (who board) == You then								
								if (rem (((fst (yourSeeds board)) !! (position -1)) + position) 13) == 7 then
										You
								else
									if isOver board then You 
									else
										Opponent
							else
								if (rem(((fst (oppsSeeds board)) !! (position -1)) + position) 13) == 7 then
										Opponent
								else
									if isOver board then Opponent
									else
										You
									
processBoardFromResponseYou :: Int -> [House] -> Player -> Board
processBoardFromResponseYou score2 response player =(Board (((take 6 response) , response !! 6) , (reverse (drop 7 response) , score2)) player)

processBoardFromResponseOpponent :: Int -> [House] -> Player -> Board
processBoardFromResponseOpponent score1 response player =(Board (( drop 7 response , score1 ) , ((reverse (take 6 response)) , response !! 6)) player)

increment_helper :: Int -> Int-> [Int] -> Player ->([House] , Player)
increment_helper house count houses player = if count == 0 then (houses , player)  --sfarsitul move-ului
									  else if (count == 1) then --ultimul pas al modificarii (poate ajunge pe o casuta goala)
										   if((houses !! (house -1)) == 0)then  -- adauga scoicile oponentului 
											  if(house == 7) then --daca este pe casuta de scor
												increment_helper (house + 1) (count-1) (replaceAt (house -1 ) ((houses !! (house - 1)) +1) houses ) player --if it's score
											  else
												if (houses !! opposite == 0) then --daca ma mut pe casuta goala , iar oponentul nu are scoici
													increment_helper (house + 1) (count -1 ) (replaceAt opposite 0 (replaceAt (house-1) ((houses !! (house -1)) + 1 + (houses !! opposite )) houses)) player-- pun scoicile furate la scor 
												else -- oponentul are scoici si trebuie sa i le iau pentru a schimba si jucatorul
													if(player == You) then
														increment_helper (house + 1) (count -1 ) (replaceAt opposite 0 (replaceAt 6 ((houses !! (house -1)) + 1 + (houses !! opposite )) houses)) Opponent -- pun scoicile furate la scor 
													else 
														increment_helper (house + 1) (count -1 ) (replaceAt opposite 0 (replaceAt 6 ((houses !! (house -1)) + 1 + (houses !! opposite )) houses)) You -- pun scoicile furate la scor 
										   else
											  increment_helper (house + 1) (count-1) (replaceAt (house -1 ) ((houses !! (house - 1)) +1) houses ) player
									  else 
									  if house == ((length houses)+1)  then 
										increment_helper 1 (count) (replaceAt (house -1 ) ((houses !! (house - 1))) houses ) player
									  else 
										increment_helper (house + 1) (count-1) (replaceAt (house -1 ) ((houses !! (house - 1)) +1) houses ) player
									  where opposite = (house + ((((6 - house) + 1) * 2)) -1)
replaceAt _ _ []     = []
replaceAt 0 x (_:ys) = x:ys
replaceAt n x (y:ys) = y:replaceAt (n - 1) x ys

getOverScore :: Board -> (Int , Int)
getOverScore (Board ((house1 , score1) , (house2  , score2)) player) = (score1 + (foldl (+) 0 house1 ) , score2 + (foldl (+) 0 house2 ))

{-
    *** TODO ***

    Întoarce scorurile (utilizator, adversar).

    Calculul trebuie să țină cont de eventuala încheiere a jocului.
-}
scores :: Board -> (Int, Int)
scores (Board ((house1 , score1) , (house2  , score2)) player)  = if (isOver board) == True then getOverScore board else (score1, score2) where	
																board = Board ((house1 , score1 ) , (house2 , score2)) player



{-
    *** TODO ***

    Întoarce perechile casă-configurație, reprezentând mutările care pot fi
    făcute într-un singur pas, pornind de la configurația actuală.
-}

successors :: Board -> [(House, Board)]
successors = undefined

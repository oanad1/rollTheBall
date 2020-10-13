{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell Char deriving Ord

instance Eq Cell where 
    Cell type1  == Cell type2 = (type1 == type2)

{-
    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level (A.Array (Position) Cell)
    deriving (Eq, Ord)

{-
    Instanțiere Level pe Show. 
    Fiecare linie este urmată de \n (endl in Pipes).
-}
instance Show Cell 
    where show (Cell c_type) =  [c_type]

instance Show Level 
    where show (Level lv) =  let rows = fst (snd  (A.bounds lv))
                                 cols = snd (snd  (A.bounds lv)) 
                                 getCh (Cell cell) = [cell]
            in  [endl] ++ (unlines [foldl (\c row -> c ++ row) "" 
            [getCh (lv A.! (i,j)) | j <- [0..cols]] | i <- [0..rows]]) 

{-
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level (A.array ((0,0), pos) [((i,j), (Cell emptySpace)) | 
                 i <- [0..(fst pos)], j <- [0..(snd pos)]])


{-
    Verifica daca o celula contine emptySpace.
-}
isSpaceEmpty :: Cell -> Bool
isSpaceEmpty (Cell cell_type) = cell_type == emptySpace


{-
    Verifica daca o pozitie este valida (se afla in interiorul tablei).
-}
isPosValid :: Position -> Level -> Bool
isPosValid pos (Level lv) = let nr_rows = fst (snd  (A.bounds lv)) + 1
                                nr_cols = snd (snd  (A.bounds lv)) + 1
    in (fst pos >= 0) && (snd pos >= 0) && (fst pos < nr_rows) && 
    (snd pos < nr_cols)


{-
    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}
addCell :: (Char, Position) -> Level -> Level
addCell cell (Level lv) = if((isPosValid (snd cell) (Level lv)) &&
                          isSpaceEmpty (lv A.! (snd cell))) then
                                Level (lv A.// [((snd cell), (Cell (fst cell)))])
                          else 
                                Level lv


{-
    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel max_pos cells = foldr (\cell lv_map -> addCell cell lv_map) 
                            (emptyLevel max_pos) cells


{-
    Verifica daca o celula este imutabila. Celulele imutabile sunt:
    * [startUp, startDown, startLeft, startRight]
    * [winUp, winDown, winLeft, winRight]
    * emptySpace
-}
isCellImmutable :: Cell -> Bool
isCellImmutable (Cell cell_type) = (elem cell_type startCells) || 
                                   (elem cell_type winningCells) || 
                                   cell_type == emptySpace 


{-
    Primeste coordonatele in care se doreste a se face mutarea si
    coordonatele celulei curente si verifica daca sunt valide, daca 
    celula curenta nu este imutabila si daca celula vecina in care se
    face mutarea este emptySpace. 
    Intoarce o noua stare cu cele 2 celule interschimbate.
-}
changePos :: Position -> Position -> Level -> Level
changePos pos neigh_pos (Level lv) = if(isPosValid neigh_pos (Level lv)) 
    then 
        let cell = (lv A.! pos)
            neigh = (lv A.! neigh_pos) in            
        if((isCellImmutable cell == False) && (isSpaceEmpty neigh == True)) then 
            Level (lv A.// [(neigh_pos, cell), (pos, (Cell emptySpace))])
        else 
            (Level lv)
    else 
        (Level lv)    


{-
    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.
    Dacă nu se poate face mutarea nivel ramane neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos North (Level lv) = changePos pos (fst pos - 1, snd pos) (Level lv)

moveCell pos South (Level lv) = changePos pos (fst pos + 1, snd pos) (Level lv)

moveCell pos West (Level lv) = changePos pos (fst pos, snd pos - 1)  (Level lv)

moveCell pos East (Level lv) = changePos pos (fst pos, snd pos + 1) (Level lv)

{-
    Verifică dacă două celule se pot conecta.
    Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă). Primeste cele 
    2 celule si directia relativa a primei fata
    de a doua.

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell c1) (Cell c2) East 
    | elem c1 [horPipe, botRight, topRight, startLeft, winLeft] = 
        elem c2 [topLeft, botLeft, winRight, horPipe, startRight]
    | otherwise = False

connection (Cell c1) (Cell c2) West 
    | elem c1 [horPipe, topLeft, botLeft, startRight, winRight] = 
        elem c2 [horPipe, botRight, topRight, startLeft, winLeft]
    | otherwise = False

connection (Cell c1) (Cell c2) North 
    | elem c1 [verPipe, topLeft, topRight, startDown, winDown] = 
        elem c2 [verPipe, botLeft, botRight, startUp, winUp]
    | otherwise = False

connection (Cell c1) (Cell c2) South 
    | elem c1 [verPipe, botLeft, botRight, startUp, winUp] = 
        elem c2 [verPipe, topLeft, topRight, startDown, winDown]
    | otherwise = False


{-
    Primeste o celula si o pozitie vecina si verifica daca celula
    este conectata cu vecinul ei apeland functia connection.
    Verifica in plus daca pozitia primita este valida si daca a
    fost adaugata anterior in path.
-}
checkNeigh :: (Int, Int) -> Cell -> Directions -> Level -> [(Int, Int)] -> Bool
checkNeigh neigh_pos cell dir (Level lv) path = if(isPosValid neigh_pos (Level lv))
    then 
      if(elem neigh_pos path == False && (connection cell (lv A.! neigh_pos) dir)) 
        then True
        else False
    else False


{-
    Verifica toti vecinii unei celule pentru a vedea care
    sunt conectati cu celula curenta. Intoarce un nou path
    in care se adauga toti vecinii conectati ai celulei date.
-}
connectedNeigh :: (Int, Int) -> Cell -> [(Int, Int)] -> Level -> [(Int, Int)]
connectedNeigh pos c path lv
   | checkNeigh (fst pos + 1, snd pos) c North lv path = (fst pos + 1, snd pos):path
   | checkNeigh (fst pos - 1, snd pos) c South lv path = (fst pos - 1, snd pos):path
   | checkNeigh (fst pos, snd pos + 1) c West lv path = (fst pos, snd pos + 1):path
   | checkNeigh (fst pos, snd pos - 1) c East lv path = (fst pos, snd pos - 1):path
   | otherwise = []


{-
    Verifica daca o celula data este pipe.
-}
isPipe :: Cell -> Bool
isPipe (Cell c) = c /= emptySpace && c /= emptyCell


{-
    Construieste o cale pornind de la o celula de start data.
    Returneaza True daca ajunge la o celula de tip win. 
    Returneaza False daca ajunge la o celula care nu e pipe.
    Apeleaza functia connectedNeigh pentru a adauga in path
    toti vecinii conectati ai celulei curente, dupa care 
    se apeleaza recursiv cu prima celula din noul path ca
    celula de pornire (adica ultima celula conectata).
-}
existsPath :: (Int,Int) -> Cell -> [(Int, Int)] -> Level -> Bool
existsPath pos (Cell c) path (Level lv) = if(elem c winningCells) 
    then True
    else if(isPipe (Cell c) == False) 
         then False 
         else let new_path = connectedNeigh pos (Cell c) path (Level lv) in
             if(new_path == []) 
             then False 
             else existsPath (head new_path) (lv A.! (head new_path)) 
             new_path (Level lv)


{-
    Verifica daca o celula data este de start. 
-}
isStart :: Cell -> Bool
isStart (Cell c) = elem c startCells


{-
    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Cauta celula de start sub forma (Position, Cell) si apeleza functia 
    existsPath pronind de la aceasta pentru a determina daca se poate forma
    un drum complet.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel (Level lv) =  let 
    rows = fst (snd (A.bounds lv))
    cols = snd (snd  (A.bounds lv))
    positions = [((i,j), (lv A.! (i,j))) | i <- [0..rows], j <- [0..cols]]
    start = foldr (\cell first -> if(isStart (snd cell)) then cell else first) 
            ((-1,-1), (Cell emptySpace)) positions 
    in existsPath (fst start) (snd start) [(fst start)] (Level lv)


{-
    Verifica daca o mutare data poate fi efectuata (daca pozitia este
    valida si daca celula nu este imutabila). Intoarce mutarea sub 
    forma de lista cu un singur element, sau o lista vida in caz contrar.
-}  
checkMove :: Position -> Level -> Directions -> [((Position, Directions), Level)]
checkMove pos (Level lv) dir = if(isPosValid pos (Level lv) && 
                               (isCellImmutable (lv A.! pos) == False)) 
                               then [((pos, dir), moveCell pos dir (Level lv))]
                               else []


{-
    Returneaza o lista cu toate mutariile posibile asociate cu o 
    celula goala. Elementele din lista sunt de tip 
    ((Position, Directions), Level) si indica pozitia curenta 
    a vecinului care poate fi mutat pentru a ajunge in celula 
    goala, directia in care trebuie mutat si starea in care se 
    ajunge in urma mutarii.
-}  
getMoves :: Position -> Level -> [((Position, Directions), Level)]
getMoves pos lv = let
         upMove = checkMove (fst pos - 1, snd pos) lv South
         downMove = checkMove (fst pos + 1, snd pos) lv North
         leftMove = checkMove (fst pos, snd pos - 1) lv East
         rightMove = checkMove (fst pos, snd pos + 1) lv West
    in upMove ++ downMove ++ leftMove ++ rightMove

 {-
    Creeaza o instanta ProblemState pentru Level.
-}   
instance ProblemState Level (Position, Directions) where
    successors (Level lv) = let 
        rows = fst (snd (A.bounds lv))
        cols = snd (snd  (A.bounds lv))
        positions = [((i,j), (lv A.! (i,j))) | i <- [0..rows], j <- [0..cols]]
        in foldr (\cell succs -> if(isSpaceEmpty (snd cell)) 
            then (getMoves (fst cell) (Level lv)) ++ succs else succs) [] positions 

    isGoal (Level lv) = wonLevel (Level lv)

    reverseAction pair
     | snd (fst pair) == North = (((fst (fst (fst pair)) - 1, 
                                  snd (fst (fst pair))), South), snd pair)
     | snd (fst pair) == South = (((fst (fst (fst pair)) + 1, 
                                  snd (fst (fst pair))), North), snd pair)
     | snd (fst pair) == East = (((fst (fst (fst pair)), 
                                  snd (fst (fst pair)) + 1), West), snd pair)
     | snd (fst pair) == West = (((fst (fst (fst pair)), 
                                  snd (fst (fst pair)) - 1), East), snd pair)
     | otherwise = pair
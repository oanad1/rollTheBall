{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    Tipul unei nod utilizat în procesul de căutare. Contine:
    * starea;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = TNode s (Maybe a) (Maybe (Node s a)) Int [Node s a]

instance Eq s => Eq (Node s a) where 
    (TNode s1 _ _ _ _ ) == (TNode s2 _ _ _ _) = (s1 == s2)

instance (Show s) => Show (Node s a) 
    where show (TNode st _ _ dep _)=  (show st) ++ (show dep) ++ "\n"

{-
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (TNode st _ _ _ _) = st

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (TNode _ _ p _ _) = p

nodeDepth :: Node s a -> Int
nodeDepth (TNode _ _ _ d _) = d

nodeAction :: Node s a -> Maybe a
nodeAction (TNode _ ac _ _ _) = ac

nodeChildren :: Node s a -> [Node s a]
nodeChildren (TNode _ _ _ _ ch) = ch


{-
    Creeaza un nod, generand recursiv copiii acestuia prin apelarea functiei
    successors.
-}
buildNode :: (ProblemState s a) => Maybe (Node s a) -> Int -> (a,s) -> Node s a
buildNode parent depth ac_st = let 
    current = TNode (snd ac_st) (Just (fst ac_st)) parent
              depth (map (buildNode (Just current) (depth + 1)) 
              (successors (snd ac_st)))
    in current


{-
    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace lv = let root = TNode lv Nothing Nothing 0 
                                 (map (buildNode (Just root) 1) (successors lv)) 
                      in root


{-   
    Construieste o lista de noduri parcurse. Primul element din tuplu 
    reprezinta frontiera curenta (toti copiii nodului curent). Al 
    doilea element reprezinta intreaga frontiera (aici se adauga copiii
    nodului curent doar daca nodul curent nu a mai fost vizitat).
    Se mentine o lista de noduri vizitate pentru a evita ciclurile.
    La fiecare pas se extrage primul element din frontiera (cea intreaga)
    si acesta devine urmatorul nod ce va fi vizitat.
-}
bfsHelper :: Ord s => Node s a -> [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfsHelper node lastF visited = ((nodeChildren node), (newFront lastF node visited)):
                               (bfsHelper(head (newFront lastF node visited)) 
                               (newFront lastF node visited) (node:visited)) 
                               where 
                               getChildren n v = if(elem n v) 
                                   then [] 
                                   else (nodeChildren n)
                               newFront l n v = (tail l) ++ (getChildren n v)
      

{-   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera
-}
bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = ([node], [node]) : (bfsHelper node [node] [])


{-
    Primește starea inițială și finală și întoarce o pereche de noduri, 
    reprezentând intersecția dintre cele două frontiere.
-}

intersect :: Eq s => [Node s a] -> [Node s a] -> [(Node s a, Node s a)]
intersect lst1 lst2 = foldr (\n1 acc1-> (foldr (\n2 acc2-> if(n1 == n2) 
    then [(n1,n2)] else acc2) acc1 lst2)) [] lst1

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = head (head (filter (\lst -> lst /= [])
                     (zipWith (\l1 l2 -> intersect l1 l2) 
                     (map fst (bfs start)) (map fst (bfs end)))))


{-
    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

extractPath :: (Eq s) => Node s a -> [(Maybe a, s)]
extractPath node = reverse (zip (map (\(Just n) -> nodeAction n) (nodePath node)) 
                                (map (\(Just n) -> nodeState n) (nodePath node)))
                   where nodePath n = takeWhile (\el -> el /= Nothing) 
                                      (iterate (\(Just nd) -> nodeParent nd) (Just n))

{-
    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.
    Pentru calea gasită în a doua parcurgere, actiunile trebuie inversate, decalate cu
    o pozitie si adaugate in ordine inversa in lista. (Pentru ca se porneste de la
    punctul de intersectie-deja adaugat in prima cale, inspre nodul de finish, iar
    parcurgerea obtinuta(si actiunile asociate) vor fi initial in ordine inversa - de
    la finish spre nodul de intersectie)
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

reverseP :: (ProblemState s a, Ord s) => (Maybe a) -> s -> (Maybe a, s)
reverseP Nothing st = (Nothing, st)
reverseP (Just ac) st = let rev_pair = reverseAction (ac, st) 
                        in (Just (fst rev_pair), snd rev_pair)

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve start end = let mid = (bidirBFS (createStateSpace start) (createStateSpace end))
                      align mov = zipWith (\x1 x2 -> (fst x1, snd x2)) mov (tail mov) 
                  in (extractPath (fst mid)) ++ (align (map (\pair -> reverseP (fst pair) 
                     (snd pair)) (reverse ((extractPath (snd mid))))))

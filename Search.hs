{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S
import Debug.Trace

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    stare :: s,
    actiune :: Maybe a,
    parinte :: Maybe (Node s a),
    adancime :: Int,
    cost :: Float,
    copii :: [Node s a]
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    Node s1 _ _ _ _ _ == Node s2 _ _ _ _ _ = s1 == s2

instance Ord s => Ord (Node s a) where
    Node s1 _ _ _ _ _ <= Node s2 _ _ _ _ _ = s1 <= s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState nod = (stare nod)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent nod = (parinte nod)

nodeDepth :: Node s a -> Int
nodeDepth nod = (adancime nod)

nodeChildren :: Node s a -> [Node s a]
nodeChildren nod = (copii nod) 

nodeHeuristic :: Node s a -> Float
nodeHeuristic nod = (cost nod)

nodeAction :: Node s a -> Maybe a
nodeAction nod = (actiune nod)

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = myNode{copii = map (\x -> (createNodeKids x 0 myNode)) (successors initialState)}
    where myNode =  Node {stare = initialState, actiune = Nothing, parinte = Nothing, adancime = 0, cost = h initialState, copii = []}

createNodeKids :: (ProblemState s a, Eq s) => (a,s) -> Int -> Node s a -> Node s a
createNodeKids (action, state) depth_parent parent = myParent {copii = foldl (\acc x ->  (createNodeKids x (depth_parent + 1) myParent) : acc ) [] (successors state)}
    where myParent = Node {actiune = Just action, stare = state, parinte = Just parent, adancime = depth_parent + 1, cost = h state}

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter (\x -> not (S.member (stare x) visited)) (copii node)

--noduri succesdor ale arg
--daca vizitate, nu le pun
--(verific in visited)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith (\new old -> if new < old then new else old) node (fromIntegral (adancime node) + (cost node)) frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl (\acc x -> insertSucc acc x) frontier noduriNevizitate
    where noduriNevizitate = suitableSuccs node visited

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = if isGoal stareMinNode then
                            minNode
                            else astar' newVisited frontierAfterAdding
    where
        minNode = fst (deleteFindMin frontier)
        stareMinNode = stare minNode
        newVisited = S.insert stareMinNode visited
        restOfFrontier = snd (deleteFindMin frontier)
        frontierAfterAdding = insertSuccs minNode restOfFrontier newVisited

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' emptySet initialPQ
    where 
        emptySet = S.empty
        initialPQ = PQ.insert initialNode 0 PQ.empty
{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = extractPathHelper goalNode []

extractPathHelper :: Node s a -> [(a, s)] -> [(a, s)]
extractPathHelper currNode aux = if isNothing (parinte currNode) then aux
                                else extractPathHelper (fromJust (parinte currNode)) ((fromJust(actiune currNode), (stare currNode)) : aux)
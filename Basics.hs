{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe
import Debug.Trace

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    dimension :: (Int, Int),
    hunter :: Position,
    targets :: [Target],
    obstacole :: [Position],
    gateways :: [(Position, Position)]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
-- Pentru o pozitie, returneaza ce char este pe tabla de joc
getCharAt :: Game -> Position -> Char
getCharAt game pos
    | snd pos == snd (dimension game) = '\n'
    | (fst pos == 0) || (snd pos == 0) || (fst pos == fst (dimension game) - 1) || (snd pos == snd (dimension game) -1) = '@'
    | hunter game == pos = '!'
    | length (filter (\x -> position x == pos) (targets game)) == 1 = '*'
    | elem pos (obstacole game) = '@'
    | length (filter (\x -> fst x == pos || snd x == pos) (gateways game)) >= 1 = '#'
    | otherwise = ' '

-- list comprehension ptr poz de mai sus
gameAsString :: Game -> String
gameAsString game = init $ map (getCharAt game) [(x,y) | x <- [0..((fst (dimension game)) - 1)], y <- [0..((snd (dimension game)))]]

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame x y  = Game (x,y) (1,1) [] [] []

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
-- fct ajutatoare care detecteaza daca e ceva deja pe pos PTR TOATE ADD URILE
addHunter :: Position -> Game -> Game
addHunter pos game
    | (fst pos < 0) || (snd pos < 0) || (fst pos >= fst (dimension game)) || (snd pos >= snd (dimension game)) || (getCharAt game pos /= ' ') = game
    | otherwise = game{hunter = pos}

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget beh pos game
    | (fst pos < 0) || (snd pos < 0) || (fst pos >= fst (dimension game)) || (snd pos >= snd (dimension game)) || (getCharAt game pos /= ' ') = game
    | otherwise = game{targets = (Target pos beh : targets game)}

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1, pos2) game = game{gateways = (pos1, pos2) : gateways game}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game
    | (fst pos < 0) || (snd pos < 0) || (fst pos >= fst (dimension game)) || (snd pos >= snd (dimension game)) || (getCharAt game pos /= ' ') = game
    | otherwise = game{obstacole = pos : obstacole game}

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    | getCharAt game pos == ' ' = Just pos
    | getCharAt game pos == '#' && fst myGateways == pos = Just (snd myGateways)
    | getCharAt game pos == '#' && snd myGateways == pos = Just (fst myGateways)
    | otherwise = Nothing
    where
        myGateways = fromJust $ find (\x -> fst x == pos || snd x == pos) (gateways game)
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

goTo :: Position -> Position -> Game -> Target
goTo posfrom posto game = case attemptMove posto game of
    Just pos -> Target pos myTargetBehavior
    Nothing | null myGatewaysList -> Target posfrom myTargetBehavior
            | fst myGateways == posfrom -> Target (snd myGateways) myTargetBehavior
            | otherwise -> Target (fst myGateways) myTargetBehavior
    where
        myTargetBehavior = behavior $ fromJust $ find (\x -> position x == posfrom) (targets game)
        myGatewaysList = filter (\x -> fst x == posfrom || snd x == posfrom) (gateways game)
        myGateways = head myGatewaysList



goEast :: Behavior
goEast pos game = goTo pos (fst pos, snd pos + 1) game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos game = goTo pos (fst pos, snd pos -1) game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos game = goTo pos (fst pos - 1, snd pos) game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos game = goTo pos (fst pos + 1, snd pos) game

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce i pos game = case i of
    1 -> if pos /= posAfterMoveS then
            if isObstacleInS && (getCharAt game posAfterMoveS) == '#' then
                Target posAfterMoveS (bounce (-1))
            else Target posAfterMoveS (bounce 1)
        else
            Target posAfterMoveN (bounce (-1))
    _ -> if pos /= posAfterMoveN then
            if isObstacleInN && (getCharAt game posAfterMoveN) == '#' then
                Target posAfterMoveN (bounce 1)
            else Target posAfterMoveN (bounce (-1))
        else
            Target posAfterMoveS (bounce 1)
    where
        posAfterMoveS = position (goSouth pos game)
        posAfterMoveN = position (goNorth pos game)
        isObstacleInS = getCharAt game (fst posAfterMoveS + 1, snd posAfterMoveS) == '@'
        isObstacleInN = getCharAt game (fst posAfterMoveS - 1, snd posAfterMoveS) == '@'


{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game = game{targets = map (\x -> (behavior x) (position x) game) (targets game)}

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled pos target
    | (fst (position target) == (fst pos) + 1) && (snd (position target) == snd pos) = True
    | (fst (position target) == (fst pos) - 1) && (snd (position target) == snd pos) = True 
    | (snd (position target) == (snd pos) + 1) && (fst (position target) == fst pos) = True
    | (snd (position target) == (snd pos) - 1) && (fst (position target) == fst pos) = True 
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}


advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bool game = case dir of
    North -> advanceGameStateHelper (-1,0) bool game
    South -> advanceGameStateHelper (1,0) bool game
    East -> advanceGameStateHelper (0,1) bool game
    _ -> advanceGameStateHelper (0,-1) bool game

advanceGameStateHelper :: (Int,Int) -> Bool -> Game -> Game
advanceGameStateHelper (i,j) bool game = if bool then gameAfter2ndKill
                                            else modifiedGame
    where
        modifiedGame = game{hunter = getMovedHunter (i,j) game}
        gameAfter1stKill = killTargets (hunter modifiedGame) modifiedGame
        gameAfter2ndKill = killTargets (hunter modifiedGame) (moveTargets gameAfter1stKill)

killTargets :: Position -> Game -> Game
killTargets pos game = game{targets = filter (\x -> not (isTargetKilled pos x)) (targets game)}

getMovedHunter :: (Int,Int) -> Game -> (Int, Int)
getMovedHunter (i,j) game = position (goTo (hunter game) ((fst (hunter game)) + i, (snd (hunter game)) + j) game)
        
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not $ null $ targets game

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors state = filter (\x -> hunter (snd x) /= hunter state) completeList
        where
            completeList = pairNorth:pairSouth:pairWest:pairEast:[]
            pairNorth = (North, advanceGameState North False state)
            pairSouth = (South, advanceGameState South False state)
            pairEast = (East, advanceGameState East False state)
            pairWest = (West, advanceGameState West False state)
    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = any (\x -> isTargetKilled (hunter game) x) (targets game)

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = case find (\x -> isTargetKilled (hunter game) x) (targets game) of
        Just target -> hEuclidean (hunter game) (position target)
        _ -> 0 :: Float

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined

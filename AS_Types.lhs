Übung 2 Typen
--------------

In dieser Übung geht es darum einfache Programmieraufgaben mit verschiedenen Basistypen von Haskell zu lösen. 

1. Basis Typen

a) Implementieren Sie folgende Funktion, die nur dann True zurückgibt, falls alle drei Argumente gleich sind. 

> threeEquals :: Int -> Int -> Int -> Bool
> threeEquals a b c = a == b && a == c


b) Nun geht es um die Funktion

> fourEquals :: Int -> Int -> Int -> Int -> Bool
>-- fourEquals a b c d = a == b && a == c && a == d
> fourEquals a b c d = threeEquals a b c && a == d

welche nur dann nur dann True zurück gibt, falls alle vier Argumente gleich sind. 
Implementieren Sie die Funktion fourEquals auf zwei verschiedenen Arten:
	- Geben Sie eine Implementation welche analog zur Implementation von threeEquals ist.
	- Geben Sie eine Implementation welche threeEquals aufruft um das Resultat zu berechnen.
Vergleichen Sie die beiden Implementationen.


c) Implementieren Sie eine Funktion, welche den Durchschnitt dreier Ints berechnet und als Double zurück gibt.
Hinweis: Sie benötigen die Funktion fromIntegral

> averageThree :: Int -> Int -> Int -> Double
> averageThree a b c = fromIntegral (a + b + c) / 3


d)Implementieren Sie die Funktion

> xor :: Bool -> Bool -> Bool
> xor a b = a /= b

die nur dann True zurück gibt, wenn die beiden Argumente unterschiedlich sind. 



2. Aufzählungstypen und Tuples

a) Gegeben ist der Typ Op der für arithmetische Operationen steht:

> data Op = Add | Sub 

Definieren Sie die Funktion 

> calc :: Op -> Int -> Int -> Int
> calc Add x y = x + y
> calc Sub x y = x - y

Der erste Parameter bestimmt mit welcher Operation der zweite und der dritte Parameter verknüpft werden:
Bsp: calc Add 2 3 ~> 5


b) In dieser Aufgabe implementieren Sie eine einfache McDonalds Kasse. In unserer Filiale werden nur zwei Menus angeboten: BigMac und CheeseRoyal. Die Menus gibt es jeweils in zwei unterschiedlichen Grössen: Small und Large.
Überlegen Sie, wie der Typ einer Bestellung (Order) abgebildet werden könnte unter der Verwendung von Enums und Tupels und implementieren Sie dann die Funktion  price :: Order -> Int, die den Preis einer Bestellung berechnet. Ein BigMac Menu kostet 10 CHF und ein CheeseRoyal Menu kostet 11 CHF. Die gegebenen Preise gelten für die kleinen Menus, die grossen Menus kosten jeweils zwei CHF mehr.

> --data Menu = BigMac | CheeseRoyal
> type Order = (Int, Int, Int, Int) --(small BigMac, large BigMac, small CheeseRoyal, large CheeseRoyal)
> price :: Order -> Int
> price (a, b, c, d) = 10*a + 12*b + 11*c + 13*d
> order :: Order
> order = (2, 3, 0, 0)

3. Typen bestimmen
Bestimmen Sie jeweils den allgemeinsten Typen der folgenden Funktionen:

a)	swap (x, y)  = (y, x)
    (a, b) -> (b, a)
b)	pair x y = (x, y)
    a -> b -> (a, b)
c)	double x = x * 2
    Num -> Num
d)	crazy (a, '&', (b, True)) = not (a && b)
Hinweis: Was muss der Typ von a und b sein, damit && verwendet werden kann?
    (Bool, Char, (Bool, Bool)) -> Bool
e)	twice f x = f (f x)
Hinweis: überlegen Sie sich zuerst was f eigentlich ist. Dann bestimmen Sie den Typ von f!
    (a -> a) -> a -> a


4. Funktionen bestimmen
Geben Sie eine beliebige legale Implementierung für folgende Definitionen:
Hinweis: Verzweifeln Sie nicht, wenn Sie eine Funktion nicht implementieren können!

> f1 :: Int -> Int
> f1 a = 2 * a

> f2 :: (Int, Bool) -> Int
> f2 (a, b) = if b then -a else a

> f3 :: a -> (a,Int)
> f3 a = (a, 1)

> f4 :: a -> b
> f4 = todo

> f5 :: a -> (a ->  b) -> b
> f5 a f = f a


> todo = error "TODO"

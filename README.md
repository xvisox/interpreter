# seeemcrd interpreter

### Tekstowy opis języka

*seeemcrd* to statycznie typowany język imperatywny, którego pliki źródłowe mają rozszerzenie `see`.
Składnia języka bazuje na składni *Latte* - nie posiada specjalnych udziwnień,
jeśli chodzi o standardowe operacje, definicje czy wrażenia. Do języka została dodana
możliwość przekazywania zmiennych przez referencje do funkcji (poza standardowym przekazywaniem przez wartość).
Dodatkowo mamy także możliwość tworzenia funkcji anonimowych tzw. `lambd` które również respektują oba sposoby
przekazywania zmiennych do funkcji. Pozwalamy także na dowolną liczbę zagnieżdżeń funkcji i tworzenie globalnych
zmiennych.

Programem w `seeemcrd` nazywamy kolekcję deklaracji funkcji i zmiennych, w tym wymagamy, aby jedna z nich
miała nazwę `main`, gdzie zacznie się wykonywanie całego kodu.

W języku możemy korzystać z trzech podstawowych typów: `int`, `bool`, `string` oraz dodatkowego typu dla funkcji
`Type(ArgType)` gdzie `ArgType` to może być typ z referencją. Gdy nie przypiszemy wartości do zmiennej podstawowego typu
podczas deklaracji, to zostają one ustawione na domyślną wartość. Nie tyczy się to jednak funkcji, zmienne tego typu
muszą być zainicjalizowane, inaczej program nie przejdzie weryfikacji typów.

W przypadku argumentów przekazywanych przez referencję `&`, możliwe jest przekazanie wyłącznie zmiennych; niemożliwe
jest używanie stałych lub bezpośrednich wartości (na przykład wyniku operacji `2 + 2`,
wywołania funkcji `f(1)` lub wyrażenia `x + 1`).

Weryfikacja typów odbywa się w metodzie statycznej, zgodnie z ustalonymi regułami. To wymaga od programisty wyraźnego
zadeklarowania wszystkich typów; system nie dokonuje samodzielnie dedukcji ani rekonstrukcji typów. Zatem, nie można
przypisać wartości do zmiennej, na przykład `x = 1`, bez wcześniejszego zdefiniowania tej zmiennej. Dotyczy to również
funkcji wyższego rzędu.

Zagnieżdżoną funkcje można stworzyć na dwa sposoby: standardowa deklaracja lub funkcja anonimowa (przykład
w `examples`). Funkcja zagnieżdżona ma dostęp do wszystkich identyfikatorów widocznych w funkcji, w której jest
osadzona. Domknięcia automatycznie ujmują wszystkie zmienne dostępne w zakresie, w którym funkcja została zdefiniowana,
odnosząc się do nich przez referencję. To implikuje, że zarówno zmienne lokalne, jak i globalne zostają wchłonięte przez
funkcję, chyba że są one zastąpione przez inne deklaracje. Funkcje te nie są dostępne poza zakresem otaczającej je
funkcji.

Język będzie posiadał kilka funkcji standardowych m.in. `printStr, printInt, printBool` oraz funkcje
pomocnicze takie jak `toStr(int), toInt(str)`.

### Przykłady

Wszystkie dostępne konstrukcje języka można znaleźć w folderze `examples`. Jest tam kilka folderów z przykładami
min. `good` oraz `bad` gdzie można znaleźć poprawne oraz niepoprawne programy. Dodatkowo stworzyłem folder `typing`
gdzie znajdują się przykłady związane z weryfikacją typów. Dostępny jest także folder `complex` gdzie znajdują się
bardziej zaawansowane przykłady programów. Oba foldery głównie służyły do testowania konkretnych funkcjonalności
type-checkera oraz interpretera, dlatego są osobno. Dodałem także skrypt pomocniczy, który porównuje wyniki działania
interpretera z oczekiwanymi wynikami (w celu testowania regresyjnego).

### Tabelka cech

```txt
  Na 15 punktów
  01 (trzy typy)
  02 (literały, arytmetyka, porównania)
  03 (zmienne, przypisanie)
  04 (print)
  05 (while, if)
  06 (funkcje lub procedury, rekurencja)
  07 (przez zmienną / przez wartość)
  
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie)
  10 (obsługa błędów wykonania)
  11 (funkcje zwracające wartość)
  
  Na 30 punktów
  12 (4) (statyczne typowanie)
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  
  Razem: 30
```

Autor: Hubert Michalski

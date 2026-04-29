# Supermarket Queue Simulator 🛒

Acest proiect este o aplicație dezvoltată în **Racket**, bazată exclusiv pe **programare funcțională** (fără efecte laterale sau variabile mutabile). Aplicația simulează fluxul clienților la casele de marcat dintr-un magazin, gestionând dinamic cozile, timpul de așteptare și optimizarea resurselor.

Proiectul a fost dezvoltat iterativ, în 4 etape, complexitatea crescând treptat de la manipulări de bază ale listelor până la implementarea unor Tipuri de Date Abstracte (TDA) optimizate cu fluxuri (streams) și evaluare leneșă.

## 🛠 Tehnologii și Concepte Utilizate
* **Limbaj:** Racket (Lisp dialect)
* **Paradigmă:** Programare Funcțională pură
* **Concepte cheie:**
  * Funcții de ordin superior (Higher-Order Functions) & Currying
  * Tipuri de Date Abstracte (TDA / ADT)
  * Complexitate amortizată $O(1)$ folosind cozi bazate pe două stive
  * Evaluare leneșă (Lazy Evaluation) și Fluxuri (Streams)
  * Pattern Matching

## 🚀 Evoluția Proiectului (Etape de dezvoltare)

### Etapa 1: Fundamentele
* Implementarea structurilor de bază pentru casele de marcat (`counter`), având un timp total de așteptare (`tt`) și o coadă de clienți (`queue`).
* Separarea caselor în case obișnuite și case rapide (limitate la un număr maxim de produse).
* Funcționalități de bază: așezarea la casa cea mai avantajoasă (cu timpul de așteptare minim) și procesarea întârzierilor neprevăzute.

### Etapa 2: Funcții de Ordin Superior
* Trecerea la un număr dinamic de case de marcat.
* Introducerea funcțiilor abstractizate, a funcțiilor anonime (`λ`) și a funcționalelor predefinite din Racket.
* Implementarea ieșirii clienților din magazin pe baza timpului estimat (`et`).
* Optimizarea mediei timpilor de așteptare prin simularea deschiderii automate de noi case atunci când aglomerația depășește un anumit prag.

### Etapa 3: Tipuri de Date Abstracte (TDA) și Trecerea Timpului
* Implementarea unui **TDA Queue** propriu, complet izolat de logica aplicației printr-o barieră de abstractizare.
* Coada este implementată folosind **două stive** (liste Racket: `left` pentru extrageri, `right` pentru adăugări) pentru a obține un cost amortizat **O(1)** la operațiile de `enqueue` și `dequeue`.
* Refactorizarea simulatorului pentru a modela curgerea reală a timpului (clienții avansează și părăsesc magazinul proporțional cu minutele trecute).

### Etapa 4: Fluxuri (Streams) și Optimizare Avansată
* Reimplementarea TDA-ului `queue` pentru performanță maximă folosind **evaluarea leneșă**.
* Stiva `left` a devenit un **flux (stream)**.
* Menținerea invariantului `size(left) >= size(right)`. Când invariantul este încălcat, elementele din `right` sunt mutate în `left` printr-o operație de rotație leneșă. Aceasta previne blocajele de performanță prin amânarea calculelor până în momentul în care elementele sunt efectiv accesate.
* Suport pentru închiderea și deschiderea dinamică a caselor de marcat, cu redistribuirea inteligentă a clienților existenți la alte case.

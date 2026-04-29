#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul
;; până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client
;; + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat Ana cu 3 produse, apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute =>
;; et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO 1 (5p)
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO 2 (15p)
; Implementați o funcție care aplică o transformare f
; casei cu un anumit index.
; f = funcție unară cu un parametru de tip casă,
; counters = listă de case,
; index = indexul casei care trebuie transformată
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index,
; întoarceți lista nemodificată.
(define (update f counters index)
  (map (λ (counter) (if (= (counter-index counter) index) (f counter) counter)) counters)) ;map ia fiecare casa din lista


; TODO 3 (7.5p)
; Memento: tt+ crește tt-ul unei case cu un număr de minute.
; Obs: tt+ afectează doar câmpul tt, nu și câmpul et.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument
;   funcției update în cel mai facil mod
; Obs: Facil înseamnă că o aplicație parțială a funcției tt+ 
; va produce o funcție unară cu parametru de tip casă, fără
; să fie nevoie de funcții anonime sau funcții auxiliare.
; Scheletul nu menționează parametrii funcției tt+, întrucât
; trebuie să determinați voi înșivă cum este cel mai bine
; ca tt+ să își primească parametrii.
;
; Apoi implementați funcția checker-tt+, care apelează funcția
; tt+ pe o casă și un număr de minute.
; Funcția checker-tt își precizează clar parametrii și
; poate fi testată, acesta este singurul său rol.
; RESTRICȚII (5p)
;  - Implementați tt+ conform cerinței anterioare.
(define (tt+ C)
  (λ (minutes) (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C))))
  

(define (checker-tt+ C minutes)
  ((tt+ C) minutes))


; TODO 4 (7.5p)
; Implementați o funcție care crește et-ul unei case
; cu un număr dat de minute.
; Obs: et+ afectează doar câmpul et, nu și câmpul tt.
; Păstrați formatul folosit pentru tt+.
; Apoi implementați funcția checker-et+ care apelează
; et+, pentru testare.
; RESTRICȚII (5p)
;  - Implementați et+ conform cerinței anterioare.
(define (et+ C)
  (λ (minutes) (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C))))

(define (checker-et+ C minutes)
  ((et+ C) minutes))


; TODO 5 (10p)
; Memento: add-to-counter adaugă o persoană
; (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter din aceleași
; rațiuni pentru care ați actualizat funcția tt+.
; Atenție la cum se modifică tt și et!
; Apoi implementați funcția checker-add-to-counter
; care apelează add-to-counter, pentru testare.
; RESTRICȚII (5p)
;  - Implementați add-to-counter conform cerinței anterioare.
(define (add-to-counter C)
  (λ (name) (λ (n-items)
              (if (null? (counter-queue C))
                  (make-counter (counter-index C) (+ (counter-tt C) n-items) (+ (counter-et C) n-items) (append (counter-queue C) (list (cons name n-items))))
                  (make-counter (counter-index C) (+ (counter-tt C) n-items) (counter-et C) (append (counter-queue C) (list (cons name n-items))))))))

(define (checker-add-to-counter C name n-items)
  (((add-to-counter C) name) n-items))


; TODO 6 (15p)
; Întrucât vom folosi atât min-tt (implementat în etapa 1)
; cât și min-et (funcție nouă), definiți o funcție mai abstractă
; din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă nevidă de case și
; întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (la același et, este preferată casa cu indexul cel mai mic)
; Obs: în etapele 2-4, listele de case sunt sortate după index.
; RESTRICȚII (10p - 2*5p)
;  - min-tt și min-et vor fi aplicații parțiale ale funcției abstracte.

(define (fancy-minim-tt-et placeholder)
  (λ(counters) ;lista de case o dau cand o sa apelez minim-tt si minim-et
    (foldl (λ (counter minim)
             (cond ((< (placeholder counter) (cdr minim))
                    (cons (counter-index counter) (placeholder counter)))
                              
                   ((= (placeholder counter) (cdr minim))
                    (if (< (counter-index counter) (car minim))
                        (cons (counter-index counter) (placeholder counter))
                        minim))
                   (else minim)))
           (cons (counter-index (car counters)) (placeholder (car counters))) ;acumulatorul pentru fold, prima casa din lista
           (cdr counters)))) ;restul listei pe care se aplica foldl

(define min-tt
  (fancy-minim-tt-et counter-tt)) ; folosind funcția de mai sus
(define min-et
  (fancy-minim-tt-et counter-et))  ; folosind funcția de mai sus


; TODO 7 (10p)
; Implementați o funcție care scoate prima persoană
; din coada unei case.
; Funcția presupune, fără să verifice, că există
; minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin
; modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva,
; înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (if (null? (cdr (counter-queue C)))
      (make-counter (counter-index C) 0 0 (cdr (counter-queue C)))
      ;(make-counter (counter-index C) (tt+ (cdr (counter-queue C))) (cdr (cdr (counter-queue C))) (cdr (counter-queue C)))))
      (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr (car (cdr (counter-queue C)))) (cdr (counter-queue C))))); et contine si timpul de procesare al primului client, dar si intarzierea

; TODO 8 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a procesa liste de case)
; - requests conține 4 tipuri de cereri (două în plus față de etapa 1):
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care este
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (ca înainte, dar folosind fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc
;   (chiar dacă nu are clienți)
; - persoana cea mai avansată este prima persoană la casa cu et-ul minim
;   (dintre casele care au clienți)
;   (dacă nicio casă nu are clienți, ignoră cererea)
; - dacă tt-ul mediu pentru toate casele > <average>,
;   adaugă case slow până când media <= <average>
;   (puteți determina matematic de câte case noi este nevoie sau
;   să adăugați recursiv una câte una cât timp este necesar)
; Considerați casele indexate de la 1 și mereu sortate după index.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie adăugați întâi casa 16, apoi casa 17, etc.
; RESTRICȚII (25p - 5*5p)
;  - Folosiți minim două funcționale predefinite în Racket. (2*5p)
;  - Nu apelați checker-tt+, checker-et+, checker-add-to-counter,
;    ci doar tt+, et+, add-to-counter. (3*5p) 
(define (serve requests fast-counters slow-counters)
  ;(define (helper-delay C)
  ;  (make-counter (counter-index C) ((tt+ C) minutes) ((et+ C) minutes) (counter-queue C))) nu merge sa il pun aici, minutes nu a fost inca declarat si da eroare
  (define (number-of-counters)
    (+ (length fast-counters) (length slow-counters)))

  (define (sum-tt)
    ;(foldl + 0 (map number-of-counters (append fast-counters slow-counters)))) avem nevoie de parametrii, iar number-of-counters nu primeste nimic, + am nevoie de tt, nu de cate case sunt
    (foldl + 0 (map counter-tt (append fast-counters slow-counters))))

  (define (do-we-have-clients C)
    (not (null? (counter-queue C))))
  
  (if (null? requests)
      (append fast-counters slow-counters) ;daca nu am/mai am request-uri, returnez casele asa cum sunt
      (match (car requests)
        [(list 'delay index minutes) ;(make-counter index ((tt+ index) minutes) ((et+ index) minutes) (counter-queue index))]
         ;(if (<= index 3) (update ((tt+ index) minutes) fast-counters index) doar in exemplul de pe ocw am sigur 2 case rapide, altfel pot sa am oricate
         ;(update ((tt+ index) minutes) fast-counters index) TRAAAAAASH
         ;(update ((tt+ index) minutes) slow-counters index)] nu merge asa, iar am pierdut apelul recursiv
         (serve (cdr requests) (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) fast-counters index) (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) slow-counters index))]
  
        [(list 'remove-first) (if (and (null? (filter do-we-have-clients fast-counters)) (null? (filter do-we-have-clients slow-counters)))
                                  (serve (cdr requests) fast-counters slow-counters) ;daca nu avem clienti, nu avem ce scoate asa ca trecem la cererea urmatoare
                                  ;(if (< (cdr (min-et fast-counters)) (cdr (min-et slow-counters)))
                                  ;    (serve (cdr requests) (update remove-first-from-counter fast-counters (car (min-et fast-counters))) slow-counters)
                                  ;    (serve (cdr requests) fast-counters (update remove-first-from-counter slow-counters (car (min-et slow-counters))))))]
                                  (if (or (null? (filter do-we-have-clients slow-counters))
                                          (and (not (null? (filter do-we-have-clients fast-counters)))
                                               (<= (cdr (min-et (filter do-we-have-clients fast-counters)))
                                                   (cdr (min-et (filter do-we-have-clients slow-counters)))))) ;verific daca clientul cu et minim se afla in fast counters sau slow counters si modific in lista aferenta
                                      (serve (cdr requests)
                                             (update remove-first-from-counter fast-counters (car (min-et (filter do-we-have-clients fast-counters))))
                                             slow-counters)
                                      (serve (cdr requests)
                                             fast-counters
                                             (update remove-first-from-counter slow-counters (car (min-et (filter do-we-have-clients slow-counters)))))))]
        
        [(list 'ensure average) (if (> (/ (sum-tt) (number-of-counters)) average)
                                    (serve requests fast-counters (append slow-counters (list (empty-counter (+ (number-of-counters) 1))))) ;indexul noului counter va fi nr de countere + 1
                                    (serve (cdr requests) fast-counters slow-counters))]
        
        [(list name n-items) ;(if (< n-items ITEMS) ((add-to-counter (car (min-tt fast-counters)) name) n-items)
         ;    ((add-to-counter (car (min-tt slow-counters)) name) n-items))] nu merge asa, daca casele rapide au un tt mai mare decat cele lente nu respect cerinta si trimit clientul la o casa cu un tt mai mare
         (if (<= n-items ITEMS) (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                                    ;((add-to-counter (car (min-tt fast-counters)) name) n-items) chestia asta doar calculeaza fara sa mai apeleze iar functia pentru apelul recursiv, calculeaza si nu face nimic dupa
                                    (serve (cdr requests) (update (λ (C) (((add-to-counter C) name) n-items)) fast-counters (car (min-tt fast-counters))) slow-counters)
                                    ;((add-to-counter (car (min-tt slow-counters)) name) n-items))
                                    (serve (cdr requests) fast-counters (update (λ (C) (((add-to-counter C) name) n-items)) slow-counters (car (min-tt slow-counters)))))
             ;(serve (cdr requests) fast-counters (update ((add-to-counter name) n-items) slow-counters (car (min-tt slow-counters)))))]
             (serve (cdr requests) fast-counters (update (λ (C) (((add-to-counter C) name) n-items)) slow-counters (car (min-tt slow-counters)))))])))



            
           


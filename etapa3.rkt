#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Este necesar să implementați întâi
;;          TDA-ul queue în fișierul queue.rkt.
;; Reveniți la acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Se modifică însă implementarea câmpului queue:
; - în loc de listă, acesta va fi o structură de tip queue
; - modificarea nu este vizibilă în definiția structurii,
;   ci în implementarea operațiilor tipului counter
(define-struct counter (index tt et queue) #:transparent)


; TODO 6 (20p)
; Actualizați funcțiile de mai jos conform cu 
; noua reprezentare a cozii de persoane.
; Elementele cozii rămân perechi (nume . nr_produse).
; RESTRICȚII (5p per abatere)
;  - Respectați "bariera de abstractizare", adică 
;    operați cu coada folosind exclusiv interfața:
;    - empty-queue
;    - queue-empty?
;    - enqueue
;    - dequeue
;    - top
; Obs: Doar câteva funcții necesită actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (map (λ (counter) (if (= (counter-index counter) index) (f counter) counter)) counters))

(define (tt+ C)
  (λ (minutes) 
    (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C))))

(define (et+ C)
  (λ (minutes)
    (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C))))


(define ((add-to-counter name items) C) ; testată de checker nu modificați signatura!
  (if (queue-empty? (counter-queue C))
      (make-counter (counter-index C) (+ (counter-tt C) items) (+ (counter-et C) items) (enqueue (cons name items) (counter-queue C)))
      (make-counter (counter-index C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C)))))
    

(define (fancy-minim-tt-et placeholder)
  (λ(counters) (foldl (λ (counter acumulator) (cond ((< (placeholder counter) (cdr acumulator)) (cons (counter-index counter) (placeholder counter)))
                                                    ((= (placeholder counter) (cdr acumulator)) (if (< (counter-index counter) (car acumulator)) (cons (counter-index counter) (placeholder counter))
                                                                                                    acumulator))
                                                    (else acumulator)))
                      (cons (counter-index (car counters)) (placeholder (car counters))) (cdr counters))))

(define min-tt
  (fancy-minim-tt-et counter-tt)) ; folosind funcția de mai sus
(define min-et
  (fancy-minim-tt-et counter-et))  ; folosind funcția de mai sus

(define (remove-first-from-counter C)
  (if (queue-empty? (dequeue (counter-queue C)))
      (make-counter (counter-index C) 0 0 (dequeue (counter-queue C)))
      (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C))))); et contine si timpul de procesare al primului client, dar si intarzierea

; TODO 7 (10p)
; Implementați o funcție care calculează starea
; unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp
; nu iese nimeni din coadă, deci se modifică
; doar câmpurile tt și et.
; Este responsabilitatea utilizatorului să nu apeleze
; funcția cu minutes > et și coadă nevidă.
; La casele fără clienți, este responsabilitatea
; voastră să nu produceți timpi negativi.
(define ((pass-time-through-counter minutes) C)
  ;(if (and (zero? (counter-tt C)) (zero? (counter-et C)))
  ;    (make-counter (counter-index C) (counter-tt C) (counter-et C) (counter-queue C))
  ;    (if (or (< (- (counter-tt C) minutes) 0) (< (- (counter-et C) minutes) 0))
  ;(make-counter (counter-index C) 0 0 (counter-queue C)) cand tt>0 si et=0 se seteaza tot la 0 si nu este ok
  ;        (make-counter (counter-index C) (max 0 (- (counter-tt C) minutes)) (max 0 (- (counter-et C) minutes)) (counter-queue C))
  ;        (make-counter (counter-index C) (- (counter-et C) minutes) (- (counter-et C) minutes) (counter-queue C)))))
  (make-counter (counter-index C)
                (max 0 (- (counter-tt C) minutes))
                (max 0 (- (counter-et C) minutes))
                (counter-queue C)))
  

; TODO 8 (60p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri:
;   3 moștenite din etapa 2:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   plus noutatea:
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
; Obs: Cererile (remove-first) din etapa 2 sunt înlocuite de un mecanism  
; mai sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul procesează cererile în ordine, astfel:
; - nicio modificare pentru cererile moștenite din etapa 2
; - când timpul prin sistem avansează cu <x> minute, starea caselor
;   se actualizează pentru a reflecta trecerea timpului;
;   ieșirile clienților din coadă se rețin în ordine cronologică.
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Sugestii:
; - gestionați cronologia folosind în mod repetat funcția min-et 
; - pentru a menține lista clienților plecați, definiți o funcție ajutătoare
; (cu un parametru în plus față de serve), pe care serve doar o apelează.
; RESTRICȚII (5p per abatere)
;  - Folosiți minim un let și un let* (care nu ar putea fi let). (2*5p)
;  - Respectați "bariera de abstractizare" oricând operați cu tipul queue.
(define (serve requests fast-counters slow-counters)
  ;(define (number-of-counters)
  ;  (+ (length fast-counters) (length slow-counters))) ;total number of counters(fast+slow)
  ;nu merge sa copiez direct din partea 2 pentru ca aici apelez recursiv si tot timpul imi raman fast-counters si slow-counters initiale
  
  (define (sum-tt) 
    (foldl + 0 (map counter-tt (append fast-counters slow-counters)))) ;total sum of total times (of all counters)
  ;extragem tt din toate casele, si facem suma totala a timpilor totali

  (define (do-we-have-clients C)
    ;(not (null? (counter-queue C)))) acum avem queue, nu mai merge sa verific doar daca e null
    (not (queue-empty? (counter-queue C)))) ;returnam true daca casa este goala si false daca are clienti

  (define (min-et-fara-case-goale lista-case) ;nu am mai folosit-o, dar calculeaza et minim al tuturor caselor care AU clienti, ignorand casele fara clienti
    ;(if (and (> (counter-et C) 0) (< (counter-et C) ??? <- aici mi-ar fi trebuit minimul caselor pana in momentul asta
    (let ((case-folosite (filter do-we-have-clients lista-case)))
      (min-et case-folosite)))

  (let procesare ((requests requests) (case-rapide fast-counters) (case-lente slow-counters) (clienti-plecati-total '()))
    ;folosit pentru apelul recursiv, aici initializez

    (if (null? requests) ;daca nu avem/ nu *mai* avem clienti (cazul de baza al lui procesare)
        (cons clienti-plecati-total (append case-rapide case-lente)) ;returnam clientii plecati + lista tuturor caselor in starea finala
        (match (car requests) ;ne uitam la primul request si verificam cu ce se potriveste
          [(list 'delay index minutes) ;daca avem delay
           (procesare (cdr requests) (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) case-rapide index) (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) case-lente index) clienti-plecati-total)]
  
          [(list 'remove-first) (let* ((toate (append case-rapide case-lente)) ;aici stocam toate casele din apelul recursiv curent
                                       (active (filter do-we-have-clients toate))) ;in active stocam casele care au clienti
                                  (if (null? active) ;daca nu avem case active (adica fara clienti)
                                      (procesare (cdr requests) case-rapide case-lente clienti-plecati-total) ;apelam recursiv si luam urmatorul request
                                      (let* ((minim (min-et active)) 
                                             (id-case (car minim)))
                                        (procesare (cdr requests)
                                                   (update remove-first-from-counter case-rapide id-case)
                                                   (update remove-first-from-counter case-lente id-case)
                                                   clienti-plecati-total))))] ;actualizam lista de case   
          [(list 'ensure average)
           ; (let ((total (+ (length case-rapide) (length case-lente))))
           ;(if (> (/ (sum-tt) (number-of-counters)) average)
           ;(procesare (cdr requests) case-rapide (append case-lente (list (empty-counter (+ (number-of-counters) 1)))) clienti-plecati-total)
           ;(procesare (cdr requests) case-rapide case-lente clienti-plecati-total))]

           (let* ((number-of-counters (+ (length case-rapide) (length case-lente))) 
                  (sum-tt (foldl + 0 (map counter-tt (append case-rapide case-lente))))) ;la fel ca la number-of-counters, nu merge pe recursiv varianta din etapa 2
             (if (> (/ sum-tt number-of-counters) average)
                 ;(procesare (cdr requests) case-rapide (append case-lente (list (empty-counter (+ number-of-counters 1)))) clienti-plecati-total) nu putem trece la cererea urmatoare pana nu rezolvam recursiv tot din cererea asta
                 ;(procesare (cdr requests) case-rapide case-lente clienti-plecati-total)))]
                 (procesare requests case-rapide (append case-lente (list (empty-counter (+ number-of-counters 1)))) clienti-plecati-total)
                 (procesare (cdr requests) case-rapide case-lente clienti-plecati-total)))]
          [(list name n-items)
           (if (<= n-items ITEMS) (if (<= (cdr (min-tt case-rapide)) (cdr (min-tt case-lente)))
                                      (procesare (cdr requests) (update (add-to-counter name n-items) case-rapide (car (min-tt case-rapide))) case-lente clienti-plecati-total)
                                      (procesare (cdr requests) case-rapide (update (add-to-counter name n-items) case-lente (car (min-tt case-lente))) clienti-plecati-total))
               (procesare (cdr requests) case-rapide (update (add-to-counter name n-items) case-lente (car (min-tt case-lente))) clienti-plecati-total))]
          ;(let-values (( (output 1...output n) (functie input 1...input n) )) ...operatii de unde ies outputurile...)
          ;nu exista un named let-values :(
          ;[(list x)  (let-values actualizare-recurenta ((timp-minim (cdr (min-et (filter ;;;;X NU ESTE LISTAAAAAAAAA                                  
          [x (let ((rezultat-simulare ;asta folosesc pentru a returna ce am nevoie
                    (let actualizare-recurenta ((timp-ramas x) (cr case-rapide) (cl case-lente) (clienti '())) ;"initializez" variabilele pe care trebuie sa le folosesc in functie
                      (let ((case-cu-clienti (filter do-we-have-clients (append cr cl)))) ;imi salvez casele cu clienti
                        (if (null? case-cu-clienti) ;daca nu am clienti in magazin, nu pot sa aflu un et minim al caselor "cu clienti", primesc eroare
                            (list (map (pass-time-through-counter timp-ramas) cr) 
                                  (map (pass-time-through-counter timp-ramas) cl)
                                  clienti) ;cazul de baza
                            (let* ((minim-et-cu-conditie (min-et case-cu-clienti)) ;daca am clienti la case, caut cine pleaca primul
                                   (et-minim (cdr minim-et-cu-conditie))
                                   (id-casa-et-min (car minim-et-cu-conditie)))
                              (if (<= et-minim timp-ramas) ;daca clientul care pleaca primul, termina inainte (sau fix atunci) cand se termina timpul, il trec in lista
                                  (let* ((toate-casele (append cr cl))
                                         (casa-gasita (car (filter (λ(C) (= (counter-index C) id-casa-et-min)) toate-casele)))
                                         (nume-client (car (top (counter-queue casa-gasita)))))
                                    (actualizare-recurenta (- timp-ramas et-minim)
                                                           (update remove-first-from-counter (map (pass-time-through-counter et-minim) cr) id-casa-et-min)
                                                           (update remove-first-from-counter (map (pass-time-through-counter et-minim) cl) id-casa-et-min)
                                                           (append clienti (list (cons id-casa-et-min nume-client))))) ;daca timpul nu a expirat, repetam tot procesul
                                  (list (map (pass-time-through-counter timp-ramas) cr) ;daca nu mai am clienti sau cei care sunt nu termina in intervalul de timp dat, doar scad secundele ramase fara a mai modifica altceva
                                        (map (pass-time-through-counter timp-ramas) cl)
                                        clienti))))))))
               (procesare (cdr requests) (car rezultat-simulare) (cadr rezultat-simulare) (append clienti-plecati-total (caddr rezultat-simulare))))])))) ;iau tot ce am dedus in "bucla" curenta si trec la urmatorul request
#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO (0p)
; Aveți libertatea să vă structurați programul cum doriți
; (dar cu restricțiile de mai jos), astfel încât
; funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - va exista în continuare funcția (empty-counter index)
; - veți reprezenta cozile folosind noul TDA queue
(define (empty-counter index)
  (make-counter index 0 0 empty-queue 'slow))

(define-struct counter (index tt et queue type) #:transparent)
  
; TODO 7 (70p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 6 tipuri de cereri:
;   4 moștenite din etapa 3:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă deschisă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al caselor deschise depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
;   plus 2 noi:
;   - (close <index>) - închide casa cu indexul <index> (casa există deja)
;   - (open <index>) - deschide casa cu indexul <index> (casa există deja)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa DESCHISĂ cu tt minim la care are voie;
;   se garantează că persoana poate fi distribuită la o casă
; - nicio modificare pentru situația când o casă suferă o întârziere
; - dacă tt-ul mediu pentru toate casele DESCHISE > <average>,
;   adaugă case slow până când media <= <average>
; - nicio modificare în modelarea trecerii timpului
; - o casă care se închide nu mai primește clienți noi și:
;   - primul client (dacă există) își continuă treaba la această casă
;   - restul clienților se redistribuie la celelalte case,
;     în ordinea în care erau așezați la coadă
; - o casă care se deschide redevine disponibilă pentru clienți
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista cozilor nevide în starea finală, sortată după indexul casei
;   - elementele listei au forma (index_casă . coadă) (coada este de tip queue)

(define (update f counters index)
  (map (λ (counter) (if (= (counter-index counter) index) (f counter) counter)) counters)) ;aplica functia f fiecarei case in parte

(define (tt+ C) (λ (minutes) ;incrementeaza tt
                  (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C) (counter-type C))))

(define (et+ C) ;incrementeaza et
  (λ (minutes) (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C) (counter-type C))))


(define ((add-to-counter name items) C) 
  (if (queue-empty? (counter-queue C))
      (make-counter (counter-index C) (+ (counter-tt C) items) (+ (counter-et C) items) (enqueue (cons name items) (counter-queue C)) (counter-type C))
      (make-counter (counter-index C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C)) (counter-type C))))
    

(define (fancy-minim-tt-et placeholder) ;un fel de "helper" sa fac pe min-tt si pe min-et
  (λ(counters) (foldl (λ (counter acumulator)
                        (cond ((< (placeholder counter) (cdr acumulator))
                               (cons (counter-index counter) (placeholder counter)))
                              ((= (placeholder counter) (cdr acumulator))
                               (if (< (counter-index counter) (car acumulator))
                                   (cons (counter-index counter) (placeholder counter))
                                   acumulator))
                              (else acumulator)))
                      (cons (counter-index (car counters)) (placeholder (car counters))) (cdr counters))))

(define min-tt ;afla tt minim al tuturor caselor
  (fancy-minim-tt-et counter-tt))
(define min-et ;afla et minim al tuturor caselor
  (fancy-minim-tt-et counter-et))

(define (remove-first-from-counter C)
  (if (queue-empty? (dequeue (counter-queue C))) ;verificam pe dequeue pentru a vedea daca scoatem ultima persoana
      (make-counter (counter-index C) 0 0 (dequeue (counter-queue C)) (counter-type C))
      (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)) (counter-type C)))); et contine si timpul de procesare al primului client, dar si intarzierea

(define ((pass-time-through-counter minutes) C)
  (make-counter (counter-index C)
                (max 0 (- (counter-tt C) minutes)) ;pentru a nu ajunge la timpi negativi
                (max 0 (- (counter-et C) minutes))
                (counter-queue C) (counter-type C)))
  

(define (serve requests fast-counters slow-counters)
  (let* ((fast-counter-with-type (map (λ (C) (make-counter (counter-index C) (counter-tt C) (counter-et C) (counter-queue C) 'fast)) fast-counters))
         (slow-counter-with-type (map (λ (C) (make-counter (counter-index C) (counter-tt C) (counter-et C) (counter-queue C) 'slow)) slow-counters)))
  
    (define (sum-tt) 
      (foldl + 0 (map counter-tt (append fast-counters slow-counters)))) ;total sum of total times (of all counters)
    ;extragem tt din toate casele, si facem suma totala a timpilor totali

    (define (do-we-have-clients C)
      (not (queue-empty? (counter-queue C)))) ;returnam true daca casa este goala si false daca are clienti

    (define (min-et-fara-case-goale lista-case) ;nu am mai folosit-o, dar calculeaza et minim al tuturor caselor care AU clienti, ignorand casele fara clienti
      (let ((case-folosite (filter do-we-have-clients lista-case)))
        (min-et case-folosite)))

    (define (functie-extragere-clienti q)
      (if (queue-empty? q)
          '() ;vreau sa returneze o lista pentru a lipi-o de restul de request-uri
          ;(dequeue q))) vreau sa fie lista sa o lipesc de cereri
          (let extrage ((coada-actualizata (dequeue q)))
            (if (queue-empty? coada-actualizata)
                '() ;cazul de baza, nu exista un al doilea client in coada / am terminat de adaugat in lista toti clientii
                (cons (list (car (top coada-actualizata)) (cdr (top coada-actualizata))) (extrage (dequeue coada-actualizata)))))))
    

    (let procesare ((requests requests) (case-rapide fast-counter-with-type) (case-lente slow-counter-with-type) (clienti-plecati-total '()) (case-inchise '()))
      ;folosit pentru apelul recursiv pentru a tine minte si clientii plecati (de la etapa 3), dar si casele inchise de la etapaa asta, aici initializez
      ;am adaugat si lista de case inchise pentru a nu modifica structura caselor si TOT ce am scris in etapele trecute

      (if (null? requests) ;daca nu avem/ nu *mai* avem request-uri (cazul de baza al lui procesare)
          ;(cons clienti-plecati-total (append case-rapide case-lente)) ;returnam clientii plecati + lista tuturor caselor in starea finala
          (cons clienti-plecati-total
                (map (λ (C) (cons (counter-index C) (counter-queue C)))
                     (sort (filter do-we-have-clients (append case-rapide case-lente case-inchise))
                           (λ (a b) (< (counter-index a) (counter-index b))))));sortez dupa index
          (match (car requests) ;ne uitam la primul request si verificam cu ce se potriveste
            [(list 'delay index minutes) ;daca avem delay
             (procesare (cdr requests)
                        (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) case-rapide index)
                        (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) case-lente index)
                        clienti-plecati-total (update (λ(C) ((tt+ ((et+ C) minutes)) minutes)) case-inchise index))]


            [(list 'ensure average) ;daca unele case devin mai aglomerate decat average, atunci redistribuim clientii si adaugam case pana toate ajung sub average
             (let* ((number-of-open-counters (+ (length case-rapide) (length case-lente)))
                    (total-number-of-counters (+ number-of-open-counters (length case-inchise)))
                    (sum-tt (foldl + 0 (map counter-tt (append case-rapide case-lente))))) ;la fel ca la number-of-counters, nu merge pe recursiv varianta din etapa 2
               (if (and (< 0 number-of-open-counters) (> (/ sum-tt number-of-open-counters) average))
                   (procesare requests case-rapide (append case-lente (list (empty-counter (+ total-number-of-counters 1)))) clienti-plecati-total case-inchise) ; am fost nevoita sa fac un total number of counters
                   ;pentru ca daca am avea 4 case si daca una ar fi inchisa, noua casa adaugata ar avea
                   ;tot indexul 4 si am avea doua case 4
                   (procesare (cdr requests) case-rapide case-lente clienti-plecati-total case-inchise)))]

            [(list 'close index)
             (let* ((toate-casele (append case-rapide case-lente)) ;lista cu toate casele
                    (casa-gasita (car (filter (λ(C) (= (counter-index C) index)) toate-casele))) ;caut si salvez casa cu indexul primit
                    (lista-clienti-ramasi (functie-extragere-clienti (counter-queue casa-gasita))) ;fac o lista in care sa retin toti clientii de la casa fara primul
                    (casa-doar-cu-primul-client (if (queue-empty? (counter-queue casa-gasita)) ;returnez casa doar cu primul client pentru a o pune in lista de case inchise
                                                    casa-gasita ;daca e goala nu avem ce sa ii mai facem
                                                    (make-counter (counter-index casa-gasita) 
                                                                  ;(cdr (top (counter-queue casa-gasita)))
                                                                  ;(cdr (top (counter-queue casa-gasita))) nu le pune in ordinea care trebuie
                                                                  (counter-et casa-gasita)
                                                                  (counter-et casa-gasita)
                                                                  (enqueue (top (counter-queue casa-gasita)) empty-queue) (counter-type casa-gasita)))))
               ;in loc de tt si et punem numarul de produse al primului si singurului cumparator de la casa
               (procesare (append lista-clienti-ramasi (cdr requests)) ;pun lista cu nume si numarul de produse iar ca requesturi
                          (filter (λ (C) (not (= (counter-index C) index))) case-rapide) ;daca e in case-rapide o scot
                          (filter (λ (C) (not (= (counter-index C) index))) case-lente) ;daca e in case-lente o scot
                          clienti-plecati-total (cons casa-doar-cu-primul-client case-inchise)))] ;adaug casa in case-inchise

            [(list 'open index)
             (procesare (cdr requests)
                        (if (eq? (counter-type (car (filter (λ (C) (= (counter-index C) index)) case-inchise))) 'fast) ;cauta casa cu indexul dat si verifica daca este fast
                            (cons (car (filter (λ (C) (= (counter-index C) index)) case-inchise)) case-rapide) ;daca era casa rapida o pune inapoi in lista aferenta
                            case-rapide) ;daca nu este casa rapida lasa lista asa cum e
                        (if (eq? (counter-type (car (filter (λ (C) (= (counter-index C) index)) case-inchise))) 'slow) ;la fel si pentru slow
                            (cons (car (filter (λ (C) (= (counter-index C) index)) case-inchise)) case-lente)
                            case-lente)
                        clienti-plecati-total
                        (filter (λ (C) (not (= (counter-index C) index))) case-inchise))];scot casa din lista de case inchise


            [(list name n-items)
             ;(if (<= n-items ITEMS) (if (<= (cdr (min-tt case-rapide)) (cdr (min-tt case-lente)))
             ;                           (procesare (cdr requests) (update (add-to-counter name n-items) case-rapide (car (min-tt case-rapide))) case-lente clienti-plecati-total case-inchise)
             ;                           (procesare (cdr requests) case-rapide (update (add-to-counter name n-items) case-lente (car (min-tt case-lente))) clienti-plecati-total case-inchise))
             ;exista situatii cand incercam sa facem min-tt pe liste vide si crapa
             (let ((casa-potrivita (if (<= n-items ITEMS)
                                       (car (min-tt (append case-rapide case-lente)))
                                       (car (min-tt case-lente)))))
               (procesare (cdr requests) (update (add-to-counter name n-items) case-rapide casa-potrivita) (update (add-to-counter name n-items) case-lente casa-potrivita) clienti-plecati-total case-inchise))]                                


            [x (let ((rezultat-simulare ;asta folosesc pentru a returna ce am nevoie
                      (let actualizare-recurenta ((timp-ramas x) (cr case-rapide) (cl case-lente) (ci case-inchise) (clienti '())) ;"initializez" variabilele pe care trebuie sa le folosesc in functie
                        (let ((case-cu-clienti (filter do-we-have-clients (append cr cl ci)))) ;imi salvez casele cu clienti
                          (if (null? case-cu-clienti) ;daca nu am clienti in magazin, nu pot sa aflu un et minim al caselor "cu clienti", primesc eroare
                              (list (map (pass-time-through-counter timp-ramas) cr) 
                                    (map (pass-time-through-counter timp-ramas) cl)
                                    (map (pass-time-through-counter timp-ramas) ci)
                                    clienti) ;cazul de baza
                              (let* ((minim-et-cu-conditie (min-et case-cu-clienti)) ;daca am clienti la case, caut cine pleaca primul
                                     (et-minim (cdr minim-et-cu-conditie))
                                     (id-casa-et-min (car minim-et-cu-conditie)))
                                (if (<= et-minim timp-ramas) ;daca clientul care pleaca primul, termina inainte (sau fix atunci) cand se termina timpul, il trec in lista
                                    (let* ((toate-casele (append cr cl ci))
                                           (casa-gasita (car (filter (λ(C) (= (counter-index C) id-casa-et-min)) toate-casele)))
                                           (nume-client (car (top (counter-queue casa-gasita)))))
                                      (actualizare-recurenta (- timp-ramas et-minim)
                                                             (update remove-first-from-counter (map (pass-time-through-counter et-minim) cr) id-casa-et-min)
                                                             (update remove-first-from-counter (map (pass-time-through-counter et-minim) cl) id-casa-et-min)
                                                             (update remove-first-from-counter (map (pass-time-through-counter et-minim) ci) id-casa-et-min)
                                                             (append clienti (list (cons id-casa-et-min nume-client))))) ;daca timpul nu a expirat, repetam tot procesul
                                    (list (map (pass-time-through-counter timp-ramas) cr) ;daca nu mai am clienti sau cei care sunt nu termina in intervalul de timp dat, doar scad secundele ramase fara a mai modifica altceva
                                          (map (pass-time-through-counter timp-ramas) cl)
                                          (map (pass-time-through-counter timp-ramas) ci)
                                          clienti))))))))
                 (procesare (cdr requests) (car rezultat-simulare) (cadr rezultat-simulare) (append clienti-plecati-total (cadddr rezultat-simulare)) (caddr rezultat-simulare)))])))));iau tot ce am dedus in "bucla" curenta si trec la urmatorul request
           

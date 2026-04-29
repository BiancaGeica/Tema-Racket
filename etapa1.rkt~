#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un magazin.
;; C1 acceptă doar clienți care au cumpărat maxim ITEMS produse
;; (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întârzieri (delay).
;; La un moment dat, la fiecare casă există
;; 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă
;; timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei +
;; întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO 1 (10p)
; Implementați o funcție care întoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat
; o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  ;(counter index 0 '()) cumva merge si asa, cu counter in loc de make-counter
  (make-counter index 0 '()))


; TODO 2 (10p)
; Implementați o funcție care crește tt-ul unei case
; cu un număr dat de minute.
(define (tt+ C minutes)
  ;(+ (counter-tt C) minutes)) nu merge asa, structurile sunt imutabile, trebuie creata o structura noua pe care sa o returnez
  (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-queue C)))
  ;(make-counter C (+ (counter-tt C) minutes) (counter-queue C))) cumva merge sa pun si toata structura in loc de index


; TODO 3 (20p)
; Implementați o funcție care primește o listă nevidă 
; de case și întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt,
; este preferată casa cu indexul cel mai mic
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe coadă.
(define (fancy-min-tt counters smallest) ;"returneaza" index + total-time
  (cond ((null? counters) smallest) ;daca s-a treminat lista, returnez ce am gasit
        
        ((< (counter-tt (car counters)) (cdr smallest))
         (fancy-min-tt (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters)))))
        
        ((= (counter-tt (car counters)) (cdr smallest))
         (if (<= (car smallest) (counter-index (car counters))) ;daca sunt egale, verific indexul
             (fancy-min-tt (cdr counters) smallest)
             (fancy-min-tt (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))))))
        
        (else (fancy-min-tt (cdr counters) smallest))))
(define (min-tt counters)
  ;(fancy-min-tt counters '())) nu merge pentru ca nu pot verifica cdr dintr-o lista vidaaaa, scrie si in enunt
  (fancy-min-tt (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))))) ;nu accepta sa ii dau doar car counters pentru ca asteapta o pereche, nu toata structura
 

; TODO 4 (20p)
; Implementați aceeași funcționalitate de mai sus,
; cu recursivitate pe stivă.
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe stivă.
(define (min-tt-stack counters)
  ;(cond ((null? counters) counters) nu poate sa intoarca o lista vida, in enunt se cere sa intoarca o pereche
  (cond ((= (length counters) 1) (cons (counter-index (car counters)) (counter-tt (car counters)))) ;daca am/mai am doar o casa inseamna ca ea este minimul (ca doar e singura)

        ((< (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
         (cons (counter-index (car counters)) (counter-tt (car counters))))
        
        ((= (counter-tt (car counters)) (cdr (min-tt-stack (cdr counters))))
         (if (<= (counter-index (car counters)) (car (min-tt-stack (cdr counters))))
             (cons (counter-index (car counters)) (counter-tt (car counters)))
             (min-tt-stack (cdr counters))))
        
        (else (min-tt-stack (cdr counters)))))

; TODO 5 (10p)
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei,
; n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii
; (name . n-items) la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items) ;la add to counter trebuie sa incrementez si tt-ul pentru ca atunci cand se aseaza cineva la coada, nu se aseaza cu mana goala
  ;(make-counter (counter-index C) (tt+ C n-items) (append (counter-queue C) (list (cons name n-items))))) chestia asta incearca sa inghesuie...iar...toata structura intr-un slot
  (make-counter (counter-index C) (+ (counter-tt C) n-items) (append (counter-queue C) (list (cons name n-items)))))


; TODO 6 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - așază persoana <name> la coadă la o casă
; - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (conform logicii implementate de min-tt)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)
  
  ; Puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele
  ;   requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; Puteți să vă definiți funcții ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți testa fiecare funcție imediat ce ați implementat-o
  ; Nu este obligatoriu să definiți funcții ajutătoare.

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (cond ;((= index 1) (tt+ C1 minutes) (serve (cdr requests) C1 C2 C3 C4)) nu merge asa, C1 este imutabil, rezultatul trebuie adaugat in apelul recursiv
                                           ((= index 1) (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4))
                                           ((= index 2) (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4))
                                           ((= index 3) (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4))
                                           (else (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes))))]
        
        [(list name n-items) (cond ((<= n-items ITEMS) ;daca putem pune la casa rapida, punem
                                    (cond ((= (car (min-tt (list C1 C2 C3 C4))) 1)
                                           (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4))
                                          
                                          ((= (car (min-tt (list C1 C2 C3 C4))) 2)
                                           (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))
                                          
                                          ((= (car (min-tt (list C1 C2 C3 C4))) 3) (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))
                                          
                                          (else (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items)))))
                                   

                                   ;DACA ARE MAI MULTE CUMPARATURI DECAT LA CASA RAPIDA
                                   (else (cond ((= (car (min-tt (list C2 C3 C4))) 2) (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))
                                                          ((= (car (min-tt (list C2 C3 C4))) 3) (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))
                                                          (else (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))))))])))

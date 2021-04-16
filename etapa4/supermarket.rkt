#lang racket
(require racket/match)
(require racket/trace)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.

(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (update2 f counters index null))

(define (update2 f counters index restul)
  (cond
    ; restul va contine bucata deja parcursa din counters
    ((null? counters) (reverse restul))
    ; se ataseaza counter-ul schimbat de f la rest daca este counterul cautat
    ((equal? index (counter-index (car counters))) (update2 f (cdr counters) index (cons (f (car counters)) restul)))
    ; altfel se continua cautarea
    ((update2 f (cdr counters) index (cons (car counters) restul)))))

(define (tt+ minutes)
  (lambda (C)
  (match C
    [(counter index tt et queue)
     (struct-copy counter C [tt (+ tt minutes)])])))

(define (et+ minutes)
  (lambda (C)
  (match C
    [(counter index tt et queue)
     (struct-copy counter C [et (+ et minutes)])])))

(define (add-to-counter name n-items)
 (lambda (C)
   (if (queue-empty? (counter-queue C))
   (struct-copy counter C [tt (+ (counter-tt C) n-items)] [et (+ (counter-et C) n-items)] [queue (enqueue (cons name n-items) (counter-queue C))])
   (struct-copy counter C [tt (+ (counter-tt C) n-items)] [queue (enqueue (cons name n-items) (counter-queue C))]))))

(define (min-tt-min-et counters)
  (min-tt-et-helper counters 10000000 0 10000000 0))

; calculeaza simultan si pentru tt si pentru et
(define (min-tt-et-helper counters mn-tt index-tt mn-et index-et)
  (cond
    ((null? counters)(list (cons index-tt mn-tt) (cons index-et mn-et)))
    ((and (< (counter-tt (car counters)) mn-tt) (< (counter-et (car counters)) mn-et)) (min-tt-et-helper (cdr counters) (counter-tt (car counters)) (counter-index (car counters)) (counter-et (car counters)) (counter-index (car counters))))
    ((and (< (counter-tt (car counters)) mn-tt) (>= (counter-et (car counters)) mn-et)) (min-tt-et-helper (cdr counters) (counter-tt (car counters)) (counter-index (car counters)) mn-et index-et ))
    ((and (>= (counter-tt (car counters)) mn-tt) (< (counter-et (car counters)) mn-et)) (min-tt-et-helper (cdr counters) mn-tt index-tt (counter-et (car counters)) (counter-index (car counters)) ))
    ((and (>= (counter-tt (car counters)) mn-tt) (>= (counter-et (car counters)) mn-et)) (min-tt-et-helper (cdr counters) mn-tt index-tt mn-et index-et ))
))

(define (min-tt counters)
  (car (min-tt-min-et counters))) ; folosind funcția de mai sus
(define (min-et counters)
  (car (cdr (min-tt-min-et counters)))) ; folosind funcția de mai sus


(define (remove-first-from-counter C)
  (if (queue-empty? (dequeue (counter-queue C))) ; daca o coada are 1 persoana, va ramane goala
      (struct-copy counter C [tt 0] [et 0] [queue empty-queue])
      (struct-copy counter C [tt (- (counter-tt C) (counter-et C))] [et (cdr (top (dequeue (counter-queue C))))] [queue (dequeue (counter-queue C))])))

; modificare astfel incat sa poata fi apelata in update
(define (remove-first-from-counter2)
  (lambda (C)
    (remove-first-from-counter C)))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [tt (max 0 (- (counter-tt C) minutes))] [et (max 0 (- (counter-et C) minutes))])))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

; verifica daca elementul C_index e in counters
(define (isInCounters index counters)
  (cond
    ((null? counters) #f)
    ((equal? index (counter-index (car counters))) #t)
    (else (isInCounters index (cdr counters)))))

; modificare cu functie curry
(define (add-delay-to-counter delay)
  (lambda (C)
    (struct-copy counter C [tt (+ (counter-tt C) delay)] [et (+ (counter-et C) delay)])))

(define (isClosed close)  ; verifica daca o casa este inchisa (daca se afla in close)
  (lambda (C)
    (isInCounters (counter-index C) close)))

(define (isOpen close)  ; verifica daca o casa e deschisa
  (lambda (C)
    (not (isInCounters (counter-index C) close))))

(define (remove-empty-counters counters)
  (cond
      ((null? counters) '())
      ((queue-empty? (counter-queue (car counters)))(remove-empty-counters (cdr counters)))
      (else (cons (car counters) (remove-empty-counters (cdr counters))))))

(define (tt-sum counters acc)
  (if (null? counters)
      acc
      (tt-sum (cdr counters) (+ acc (counter-tt (car counters))))))

(define (tt-mediu sum counters)
  (/ sum (length counters)))

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '() '()))

(define (sort-index x y)
  (< (counter-index x) (counter-index y)))

(define (remove-index index counters acc) ; intoarce lista fara C_index
  (cond
    ((null? counters) (reverse acc))
    ((equal? (counter-index (car counters)) index) (remove-index index (cdr counters) acc))
    (else (remove-index index (cdr counters) (cons (car counters) acc)))))

(define (find-index index counters)   ; intoarce C_index
  (cond
    ((null? counters) #f)
    ((equal? (counter-index (car counters)) index) (car counters))
    (else (find-index index (cdr counters)))
   ))
(define (f C)
  (cons (counter-index C) (counter-queue C)))

(define (remove-helper time counters acc out)
  (cond
    ; s-a consumat tot time-ul, vom returna counters + clientii ce parasesc coada
    ((equal? time 0) (cons (sort counters sort-index) out))
    ; s-au parcurs toate casele, se reia cu urmatoarea perioada de timp (urmatoarea iteratie)
    ((null? counters) (remove-helper (- time 1) (sort acc sort-index) '() out))
    ; cazul cand et > 1, nu se intampla nimic cu coada, doar se modifica et
    ((> (cdr (min-et counters)) 1) (remove-helper time (remove-index (car (min-et counters)) counters '()) (cons ((pass-time-through-counter 1)(find-index (car (min-et counters)) counters)) acc) out))
    ; et < time, coada este goala
    ((and (<= (cdr (min-et counters)) 1) (queue-empty? (counter-queue (find-index (car (min-et counters)) counters)))) (remove-helper time (remove-index (car (min-et counters)) counters '()) (cons ((pass-time-through-counter 1)(find-index (car (min-et counters)) counters)) acc) out))
    ; trebuie sa mutam primul element din coada in out
    (else (remove-helper time (remove-index (car (min-et counters)) counters '()) (cons (remove-first-from-counter (find-index (car (min-et counters)) counters)) acc) (append out (list (cons (car (min-et counters)) (car (top (counter-queue (find-index (car (min-et counters)) counters))))) )) ))
    ))
        
(define (serve-helper requests fast-counters slow-counters close out)
  (if (null? requests)
      (cons out (map f (sort (remove-empty-counters (append fast-counters slow-counters close)) sort-index)))
      (match (car requests)
        [(list 'close index)
         (if (isInCounters index fast-counters)
             (serve-helper (cdr requests) (remove-index index fast-counters null) slow-counters (cons (find-index index (append fast-counters slow-counters)) close) out)
             (serve-helper (cdr requests) fast-counters (remove-index index slow-counters null) (cons (find-index index (append fast-counters slow-counters)) close) out))
         ]
        [(list 'ensure average)
         ; daca tt-mediu este mai mare decat average, crestem numarul de slow-counters, dar ramanem la acelasi request
         (if (> (tt-mediu (tt-sum (append fast-counters slow-counters) 0) (append fast-counters slow-counters)) average)
             (serve-helper requests fast-counters (append slow-counters (list (empty-counter (+ 1 (length (append fast-counters slow-counters close)))))) close out)
             ; problema rezolvata, putem trece la urmatorul request
             (serve-helper (cdr requests) fast-counters slow-counters close out))]
        [(list name n-items)
         (if (<= n-items ITEMS)
             ; verificam in ce lista de case se afla minimul
             (if (isInCounters (car (min-tt (append fast-counters slow-counters))) slow-counters)
                 ; apelam recursiv functia modificand slow-counters deoarece acolo se afla minimul
                 (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters)))) close out)
                 ; apelam recursiv functia modificand fast-counters deoarece acolo se afla minimul
                 (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters)))) slow-counters close out))
             (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) close out))]
        [(list 'delay index minutes)
         ; vedem in ce tip de counters se afla si adaugam in mod corespunzator
         (cond 
             ((isInCounters index slow-counters) (serve-helper (cdr requests) fast-counters (update (add-delay-to-counter minutes) slow-counters index) close out))
             ((isInCounters index fast-counters) (serve-helper (cdr requests) (update (add-delay-to-counter minutes) fast-counters index) slow-counters close out))
             (else (serve-helper (cdr requests) fast-counters slow-counters (update (add-delay-to-counter minutes) close index) out)))
         ]
        ; cazul in care trebuie sa treaca x minute
        [_
         (serve-helper (cdr requests)
                       (take (filter (isOpen close) (car (remove-helper (car requests) (sort (append fast-counters slow-counters close) sort-index) '() '()))) (length fast-counters))
                       (take-right (filter (isOpen close) (car (remove-helper (car requests) (sort (append fast-counters slow-counters close) sort-index) '() '()))) (length slow-counters))
                       (filter (isClosed close) (car (remove-helper (car requests) (sort (append fast-counters slow-counters close) sort-index) '() '())))
                       (append out (cdr (remove-helper (car requests) (sort (append fast-counters slow-counters close) sort-index) '() '()))))
         ]
        )))

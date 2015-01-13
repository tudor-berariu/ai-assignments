;; Inteligență Artificială
;; Tudor Berariu, 2014

#lang racket/gui
(require test-engine/racket-tests)

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; PARTEA I : Reprezentarea elementelor din logica cu predicate de ordinul întâi

;; Sentence    ->  Relation |
;;                 Sentence Connective Sentence |
;;                 Quantifier Variable Sentence |
;;                 ¬ Sentence
;;
;; Relation    ->  Predicate Terms
;;
;; Terms       ->  Term Terms |
;;
;; Term        ->  Constant | Variable | Function Terms
;;
;; Constant    ->  [symbol starting with a lower case letter] | number
;; Variable    ->  [symbol starting with an upper case letter]
;; Predicate   ->  [symbol starting with a lower case letter]
;; Function    ->  [any defined procedure]
;; Quantifier  ->  ∃ | ∀
;; Connective  ->  ∧ | ∨ | → | ↔


;; --- variabile
(define variable?
  (λ (x)
    (and (symbol? x) 
         ((compose char-upper-case? first
                   string->list symbol->string)
          x)
         )
    ))

;(check-expect (variable? 'X) #t)
;(check-expect (variable? 'a) #f)
;(check-expect (variable? '(X)) #f)
;(check-expect (variable? '(p X)) #f)
;(check-expect (variable? '_and) #f)
;(check-expect (variable? add1) #f)
;; ---

;; --- constante
(define constant?
  (λ (x)
    (or (and (symbol? x)
             (char-lower-case? (car (string->list (symbol->string x)))))
        (number? x)
        )))

;(check-expect (constant? 'X) #f)
;(check-expect (constant? 'a) #t)
;(check-expect (constant? '(X)) #f)
;(check-expect (constant? '(p X)) #f)
;(check-expect (constant? '_and) #f)
;(check-expect (constant? add1) #f)
;; ---

;; --- predicate
(define predicate?
  (λ (x)
    (and (symbol? x) (char-lower-case? (car (string->list (symbol->string x)))))
    ))
;; ---

;; --- funcții
(define function?
  (λ (x) (or (procedure? x)
             (and (symbol? x)
                  (let ([s (symbol->string x)])
                    (equal? "sk" (substring s 0 (min (string-length s) 2))))))))
;; ---

;; --- cuantificatori și conectori
(define quantifier? (λ (x) (member x '(∃ ∀ _exists _forall))))
(define connective? (λ (x) (member x '(∧ ∨ → ↔ _and _or _implies _iff))))
(define negation? (λ (x) (equal? x '¬)))
;; ---

;; --- termeni
(define term?
  (λ (x) (or (variable? x)
             (constant? x)
             (and (list? x)
                  (function? (car x))
                  (andmap term? (cdr x))))
    ))
;; ---

;; --- relații
(define relation?
  (λ (x)
    (and (list? x) (> (length x) 0)
         (predicate? (car x)) (andmap term? (cdr x)))
    ))

;(check-expect (relation? '(p a b)) #t)
;(check-expect (relation? 'a) #f)
;(check-expect (relation? 'X) #f)
;(check-expect (relation? '(X a b)) #f)
;(check-expect (relation? '(p)) #t)
;(check-expect (relation? '(_or X b)) #f)
;; ---

;; --- propoziții
;; CERINȚA 1: Implementați predicatul sentence? care verifică dacă argumentul
;; primit x este reprezentarea unei propoziții bine formate în logica de ordinul
;; întâi.
(define sentence?
  (λ (x)
    (if (list? x)
        (or
         ;; negated sentence
         (and (= (length x) 2) (negation? (first x)) (sentence? (second x)))
         ;; quantified sentence
         (and (= (length x) 3) (quantifier? (first x)) (variable? (second x))
              (sentence? (third x)))
         ;; connected sentences
         (and (= (length x) 3) (sentence? (first x)) (connective? (second x))
              (sentence? (third x)))
         ;; atomic sentence / relation
         (relation? x))
        #f)
    ))

;; -- pentru testarea predicatului sentence?
;(check-expect (sentence? 'a) #f)
;(check-expect (sentence? 'p) #f)
;(check-expect (sentence? '(p X Y)) #t)
;(check-expect (sentence? '(∃ X (∃ Y (p X Y)))) #t)
;(check-expect (sentence? '(∃ X ((p1 x) → (∃ Y (p X Y))))) #t)
;(check-expect (sentence? '((p1 X) ∨ (p2 X))) #t)
;(check-expect (sentence? '((p1 X) ∨ x)) #f)
;(check-expect (sentence? `(∃ X (∀ Z ((p (,add1 X) a) ↔ (q Z a))))) #t)
;(display "Tests for sentence? : ")
;(test)
;; ---
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; PARTEA a II-a : Unificare: Algoritmul Robinson

;; Funcție ce verifică apariția unei variabile într-un alt termen.
;; Se aplică substituțiile deja formate pentru o verificare completă.

(define occur-check?
  (λ (var t subst)
    (cond
      ((equal? var t) #f)
      ((assoc t subst)
       (occur-check? var (substitute t subst) subst))
      ((list? t)
       (andmap (λ (a) (occur-check? var a subst)) (cdr t)))
      (else #t))))

;(check-expect (occur-check? 'X `(,add1 X) '()) #f)
;(check-expect (occur-check? 'Y `(,add1 X) '((X . Y))) #f)
;(check-expect (occur-check? 'Z `(,add1 X) '((X . Y) (Y . Z))) #f)
;(check-expect (occur-check? 'Z `(p X) '((X . Y) (Y . Z))) #f)
;(check-expect (occur-check? 'Z `(p (,add1 X)) '((X . Y) (Y . Z))) #f)
;(check-expect (occur-check? 'Z `(p W) '((X . Y) (Y . Z))) #t)



;; -- Aplicarea unei substitutii asupra unei expresii
;; CERINȚA 2: Implementați funcția substitute care aplică substituția
;; subst asupra expresiei x.
(define substitute
  (λ (x subst)
    (cond ((and (variable? x) (assoc x subst))
           (cdr (assoc x subst)))
          ((list? x) (map (λ (a) (substitute a subst)) x))
          (else x))
    ))

;(check-expect (substitute 'X '((X . a))) 'a)
;(check-expect (substitute 'X '((Y . b) (X . a))) 'a)
;(check-expect (substitute '(p X) '((X . a))) '(p a))
;(check-expect (substitute '(p X b Y d Z) '((Z . e) (Y . c) (X . a)))
;              '(p a b c d e))
;(check-expect (substitute 'x '((Z . f) (Y . e) (X . d))) 'x)

;(display "Tests for substitute: ")
;(test)
;; --
;; -- Unificarea
;; CERINȚA 3: Implementați funcția unify care unifică doi termeni s și t
;; și întoarce unificatorul dacă sau fals daca aceaștia nu unifică
(define unify
  (λ (s1 s2)
    (let rob ([stack `((,s1 . ,s2))] [subst '()])
      (if (null? stack) subst
          (let* ([s (caar stack)] [t (cdar stack)])
            (cond
              ;; s is a variable in subst
              ((and (variable? s) (assoc s subst))
               (rob `((,(substitute s subst) . ,t) . ,(cdr stack)) subst))
              ;; t is a variable in subst
              ((and (variable? t) (assoc t subst))
               (rob `((,s . ,(substitute t subst)) . ,(cdr stack)) subst))
              ;; s and t are equal
              ((equal? s t) (rob (cdr stack) subst))
              ;; s is a variable
              ((variable? s)
               (if (occur-check? s t subst)
                   (rob (cdr stack) `((,s . ,t) . ,subst))
                   #f))
              ;; t is a variable
              ((variable? t)
               (if (occur-check? s t subst)
                   (rob (cdr stack) `((,t . ,s) . ,subst))
                   #f))
              ;; s and t are both functions or relations
              ((and (list? s) (list? t) (= (length s) (length t))
                    (equal? (car s) (car t))
                    (or (and (function? (car t)) (function? (car s)))
                        (and (predicate? (car s)) (predicate? (car t)))))
               (rob (append (map cons (cdr s) (cdr t)) (cdr stack)) subst))
              ;; otherwise...
              (else #f))
            )
          ))
    ))

(define equal-substs?
  (λ (s1 s2)
    (or (equal? s1 s2)
        (and (list? s1) (list? s2) (= (length s1) (length s2))
             (andmap (λ (b) (equal? (assoc (car b) s2) b)) s1)))))

;(check-expect (equal-substs? (unify '(p a) '(p a)) '()) #t)
;(check-expect (unify '(p b) '(p a)) #f)
;(check-expect (unify '(p X) '(p a)) '((X . a)))
;(check-expect (unify '(p X X) '(p a a)) '((X . a)))
;(check-expect (unify '(p X a) '(p a X)) '((X . a)))
;(check-expect (unify '(p X a X) '(p a X a)) '((X . a)))
;(check-expect (equal-substs? (unify '(p X) `(p (,add1 Z)))
;                             `((X . (,add1 Z)))) #t)
;(check-expect (equal-substs? (unify '(p X a) `(p (,add1 Z) a))
;                             `((X . (,add1 Z)))) #t)
;(check-expect (equal-substs? (unify '(p X Y Z) '(p a b c))
;                             '((Z . c) (Y . b) (X . a))) #t)
;(check-expect (equal-substs? (unify `(q 2 3 (,+ X Y)) `(q X Y (,+ 2 3)))
;                             '((X . 2) (Y . 3))) #t)

;(display "Tests for unification: ")
;(test)
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; Partea a III-a : Transformarea în forma normal conjunctivă

(define definitia-soacrei
  '(∀ X ((femeie X) → ((soacra X) ↔ (∃ Y ((copil Y X) ∧ (∃ Z (sot Z Y))))))))

;; -- Transformarea dublelor implicații
;; CERINȚA 1
(define transform-iffs
  (λ (x)
    (if (sentence? x)
        (if (equal? (second x) '↔)
            (let ([s1 (transform-iffs (first x))]
                  [s2 (transform-iffs (third x))])
              `((,s1 → ,s2) ∧ (,s2 → ,s1)))
            (map transform-iffs x)
            )
        x)))

;(check-expect (transform-iffs '(∃ X ((p X) ↔ (¬ (r X)))))
;              '(∃ X (((p X) → (¬ (r X))) ∧ ((¬ (r X)) → (p X)))))

;; -- Transformarea implicațiilor simple
;; CERINȚA 2
(define transform-implications
  (λ (x)
    (if (sentence? x)
        (if (equal? (second x) '→)
            (let ([s1 (transform-implications (first x))]
                  [s2 (transform-implications (third x))])
              `((¬ ,s1) ∨ ,s2))
            (map transform-implications x)
            )
        x)))

;(check-expect (transform-implications '(∃ X ((p X) → (r X))))
;              '(∃ X ((¬ (p X)) ∨ (r X))))

;; -- Mutarea negațiilor către literali
;; CERINȚA 3
(define push-negations
  (λ (x)
    (cond ((not (sentence? x)) x)
          ((relation? x) x)
          ((negation? (first x))
           (cond ((relation? (second x)) x)
                 ((negation? (first (second x))) 
                  (push-negations (second (second x))))
                 ((quantifier? (first (second x)))
                  `(,(if (equal? (first (second x)) '∃) '∀ '∃)
                    ,(second (second x))
                    ,(push-negations `(¬ ,(third (second x))))))
                 ((connective? (second (second x)))
                  `(,(push-negations `(¬ ,(first (second x))))
                    ,(if (equal? (second (second x)) '∧) '∨ '∧)
                    ,(push-negations `(¬ ,(third (second x))))))
                 (else (error "Strange input"))))
          (else (map push-negations x)))))

;(check-expect (push-negations '(¬ (∃ X (∀ Y ((p X) ∧ (¬ (r Y)))))))
;              '(∀ X (∃ Y ((¬ (p X)) ∨ (r Y)))))

;; -- Eliminarea cuantificatiorilor existențiali (skolemizare)

(define elim-exists
  (λ (x)
    (let skolemize ([e x] [univ '()] [substs '()])
      (cond ((relation? e)
             (map (λ (k) (if (assoc k substs) (cdr (assoc k substs)) k)) e))
            ((negation? (first e)) `(¬ ,(skolemize (second e) univ substs)))
            ((connective? (second e)) `(,(skolemize (first e) univ substs)
                                        ,(second e)
                                        ,(skolemize (third e) univ substs)))
            ((equal? '∃ (first e))
             (skolemize (third e) univ
                        (cons (cons (second e) (if (null? univ) (gensym "sk") 
                                                   (cons (gensym "sk") univ)))
                              substs)))
            ((equal? '∀ (first e))
             `(∀ ,(second e)
                 ,(skolemize (third e) (cons (second e) univ) substs)))
            (else (error "The tasmanian devil bites your ass!")))
      )))

;; -- Standardizarea și eliminearea cuantificatorilor universali

(define elim-foralls
  (λ (x)
    (car
     (let standardize ([e x] [substs '()] [vars '()])
       (cond ((relation? e)
              `(,(map (λ (k) (if (assoc k substs) (cdr (assoc k substs)) k)) e)
                . ,vars))
             ((negation? (first e))
              (let ([a (standardize (second e) substs vars)])
                `((¬ ,(car a)) . ,(cdr a))))
             ((connective? (second e))
              (let* ([a1 (standardize (first e) substs vars)]
                     [a2 (standardize (third e) substs (cdr a1))])
                `((,(car a1) ,(second e) ,(car a2)) . ,(cdr a2))))
             ((equal? '∀ (first e))
              (standardize (third e)
                           (if (member (second e) vars)
                               `((,(second e) . ,(gensym (second e))) . ,substs)
                               substs)
                           (cons (second e) vars)))
             (else (error "The tasmanian devil bites your ass!")))
       ))))

;; -- Împingerea disjuncțiilor către literali
;; CERINȚA 4
(define to-cnf
  (λ (x)
    (cond ((relation? x) (list (list x)))
          ((negation? (first x)) (list (list x)))
          ((equal? '∨ (second x))
           (let ([left (to-cnf (first x))]
                 [right (to-cnf (third x))])
             (apply append (map (λ (l) (map (λ (r) (append l r)) right)) left))
             ))
          ((equal? '∧ (second x))
           (append (to-cnf (first x)) (to-cnf (third x)))))
    ))

;(check-expect
; (not (not
;       (member (to-cnf '((((p) ∧ (q)) ∨ ((r) ∧ (s))) ∧ (t)))
;               (permutations '(((p) (r)) ((p) (s)) ((q) (r)) ((q) (s)) ((t))))))
;      ) #t)
;
;(display "Tests for CNF : ")
;(test)

(define cnf
  (λ (e) ((compose to-cnf
                   elim-foralls
                   elim-exists
                   push-negations
                   transform-implications
                   transform-iffs
                   )
          e)))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; PARTEA a IV-a : Respingerea rezolutivă

;; Redenumirea tuturor variabilelor dintr-o clauză cu nume noi
(define rename-vars
  (λ (d)
    (letrec ([extract-vars
              (λ (e vars)
                (cond 
                  ((variable? e)
                   (if (assoc e vars) vars `((,e . ,(gensym "X")) . ,vars)))
                  ((list? e) (foldl extract-vars vars e))
                  ((pair? e) (extract-vars (cdr e) (extract-vars (car e))))
                  (else vars)))]
             [new-vars (extract-vars d '())])
      (substitute d new-vars))))

(define truism?
  (λ (c) (cond [(null? c) #f]
               [(negation? (first (first c)))
                (or (member (second (first c)) (cdr c)) (truism? (cdr c)))]
               [else (or (member `(¬ ,(first c)) (cdr c)) (truism? (cdr c)))])))

(define reduce-clause
  (λ (c) (cond [(null? c) c]
               [(member (first c) (cdr c)) (reduce-clause (cdr c))]
               [else `(,(first c) . ,(reduce-clause (cdr c)))])))

;; Obținerea tuturor clauzelor noi aplicând rezuluția între c1 și c2
(define resolve
  (λ (d1 d2)
    (letrec ([u (λ (l1 l2)
                  (cond ((and (negation? (first l1)) (relation? l2))
                         (unify (second l1) l2))
                        ((and (negation? (first l2)) (relation? l1))
                         (unify l1 (second l2)))
                        (else #f)))]
             [deep-subst (λ (e subst)
                           (let ([e2 (substitute e subst)])
                             (if (equal? e2 e) e
                                 (deep-subst e2 subst))))])
      (let search ([i 0] [j 0] [acc '()])
        (cond ((equal? i (length d1)) acc)
              ((equal? j (length d2)) (search (add1 i) 0 acc))
              (else
               (let ([result (u (list-ref d1 i) (list-ref d2 j))])
                 (search i (add1 j)
                         (if result
                             (cons
                              (append (deep-subst (take d1 i) result)
                                      (deep-subst (drop d1 (add1 i)) result)
                                      (deep-subst (take d2 j) result)
                                      (deep-subst (drop d2 (add1 j)) result))
                              acc)
                             acc)))))
      ))))


;; Respingerea rezolutivă
(define resolution
  (λ (clauses theorem (verbose? #f))
    (let infer
      ([old (apply hash
                   (apply append
                          (map (λ (c) `(,(gensym "cl") (,(rename-vars c) "null" "null")))
                               (cnf `(¬ ,theorem)))))]
       [unused (map (λ (c) `(,(rename-vars c) "null" "null"))
                    (apply append (append (map cnf clauses) `(,(cnf `(¬ ,theorem))))))])
      (if (null? unused) #f
          (let*
              ([e (car unused)]
               [l (gensym "cl")]
               [new (apply append
                           (map (λ (k)
                                  (map (λ (c) `(,(reduce-clause (rename-vars c)) ,k ,l))
                                       (resolve (first (hash-ref old k)) (first e))))
                                (hash-keys old)))]
               [unique-new (remove-duplicates new (λ (s1 s2) (equal-clauses? (first s1) (first s2))))]
               [f-new (filter (λ (new-c) (and (not (truism? (car new-c)))
                                              (not (member (car new-c) (map car unused) equal-clauses?))
                                              (not (member (car new-c) (map car (hash-values old)) equal-clauses?))))
                              unique-new)])
            (when verbose?
              (displayln "------------------------------------------")
              ;(displayln "\tClauze nefolosite:")
              ;(for-each (λ (fn) (displayln (~a "\t\t" (nice-text fn)))) (map car unused))
              (displayln "\tClauze vechi:")
              (for-each (λ (fn) (displayln (~a "\t\t" (nice-text fn)))) (map car (hash-values old)))
              (displayln (~a (hash-count old) " done & " (length unused) " to go."))
              (displayln (~a "Clauză nouă: [" l "] : " (nice-text (first e)) "    [din " (cdr e) "]"))
              (displayln "\tRezultate din rezoluție:")
              (for-each
               (λ (fn) (displayln (~a "\t\t" (car fn))))
               f-new)
              (displayln "------------------------------------------")
              ;(read))
              )
            (if (member null (map car f-new))
                (hash-set (hash-set old l e) "■" (car (filter (λ (c) (equal? (car c) null)) f-new)))
                (infer (hash-set old l e)
                       (sort (append (cdr unused) f-new) (λ (s1 s2) (< (length (car s1)) (length (car s2))))))))))))

;; Două clauze sunt egale
(define same-clauses?
  (λ (c1 c2)
    (let perfect-match ([p1 c1] [p2 c2] [bindings '()])
      (cond [(equal? bindings #f) #f]
            [(and (variable? p1) (variable? p2))
             (if (member `(,p1 . ,p2) bindings) bindings
                 (if (and (not (member p1 (map car bindings)))
                          (not (member p2 (map cdr bindings))))
                     `((,p1 . ,p2) . ,bindings)
                     #f))]
            [(and (list? p1) (list? p2) (= (length p1) (length p2)))
             (foldl perfect-match bindings p1 p2)]
            [else (if (equal? p1 p2) bindings #f)]
            ))))
(define equal-clauses?
  (λ (c1 c2)
    (ormap (λ (x) (same-clauses? c1 x)) (permutations c2))))

;; Transformarea unei clauze într-un string
(define nice-text
  (λ (c)
    (if (null? c)
        "■"
        (string-join
         (map (λ (l)
                (if (negation? (first l))
                    (~a "¬" (first (second l)) "(" (string-join (map ~a (cdr (second l))) ",") ")")
                    (~a (first l) "(" (string-join (map ~a (cdr l)) ",")  ")")))
              c)  " ∨ ")
        )))

;; Filtrează clauzele importante pentru concluzia finală
(define filter-useful
  (λ (full-proof)
    (let add-to-proof ([key-to-add "■"] [proof (hash)])
      (if (equal? key-to-add "null")
          proof
          (let* ([v (hash-ref full-proof key-to-add)]
                 [p1 (second v)]
                 [p2 (third v)])
            (add-to-proof p1 (add-to-proof p2 (hash-set proof key-to-add v))))
          ))))

(define my-canvas%
  (class canvas%
    (define/override (refresh)
      (super refresh))
    (define/override (on-paint)
      (flush-output)
      (super on-paint))
    (super-new)))


(define display-proof
  (λ (proof)
    (begin
      (define full-width 3000)
      (define full-height 3000)
      (let* ([horiz-space 50]
             [vert-space 150]
             [top-margin 50]
             [left-margin 50]
             [inside-margin 10]
             [frame (new frame% [label "Proof"] [width 1200] [height 700])]
             [todo (filter-useful proof)]
             [canvas (new my-canvas% [parent frame] [style '(no-autoclear hscroll vscroll)]
                          [paint-callback
                           (λ (canvas dc)
                             (let paint ([free (list (cons "■" (hash-ref todo "■")))]
                                         [other (hash-remove todo "■")])
                               (let* ([parents (apply append (map (λ (pair) `(,(second (cdr pair)) ,(third (cdr pair)))) free))]
                                      [other-parents (apply append (hash-map other (λ (k v) `(,(second v) ,(third v)))))]
                                      [free-parents (filter (λ (p) (and (not (member p other-parents))
                                                                        (not (equal? p "null")))) parents)]
                                      [next-free (foldr (λ (k acc) `((,k . ,(hash-ref other k)) . ,acc)) null free-parents)]
                                      [next-other (foldl (λ (k acc) (hash-remove acc k)) other free-parents)])
                                 (let* ([above-info (if (not (zero? (hash-count other)))
                                                        (paint next-free next-other)
                                                        `(0 500 ,(hash)))]
                                        [line-width (foldl (λ (c acc) (+ 
                                                                       (let-values
                                                                           ([(a b v1 v2) (send dc get-text-extent (nice-text (cadr c)))])
                                                                         a)
                                                                         horiz-space acc)) (* horiz-space -1) free)]
                                        [left-indent (+ left-margin (/ (- (max line-width (second above-info))
                                                                          (min line-width (second above-info))) 2))])
                                   ;(set! full-width (exact-ceiling (+ (* 2 left-margin) (max line-width (second above-info)))))
                                   (set! full-height (exact-ceiling (+ (* 2 top-margin) (* vert-space (add1 (first above-info))))))
                                   `(,(add1 (first above-info))
                                     ,(max line-width (second above-info))
                                     ,(car (foldl (λ (l info)
                                                    (let-values
                                                        ([(w h c d) (send dc get-text-extent (nice-text (second l)))])
                                                      (send dc set-smoothing 'smoothed)
                                                      (send dc set-brush "white" 'transparent)
                                                      (send dc draw-rectangle
                                                            (cdr info)
                                                            (+ top-margin (* vert-space (first above-info)))
                                                            (+ (* 2 inside-margin) w)
                                                            (+ (* 2 inside-margin) h))
                                                      (send dc draw-text (nice-text (second l))
                                                            (+ (cdr info) inside-margin)
                                                            (+ inside-margin top-margin (* vert-space (first above-info))))
                                                      (when (not (equal? "null" (third l)))
                                                        (let ([p1 (hash-ref (car info) (third l))])
                                                          (send dc draw-line 
                                                                (+ (cdr info) (/ w 2) inside-margin)
                                                                (+ top-margin (* vert-space (first above-info)))
                                                                (+ (fourth p1) (/ (sixth p1) 2))
                                                                (+ (fifth p1) (seventh p1) ))))
                                                      (when (not (equal? "null" (fourth l)))
                                                        (let ([p2 (hash-ref (car info) (fourth l))])
                                                          (send dc draw-line 
                                                                (+ (cdr info) (/ w 2) inside-margin)
                                                                (+ top-margin (* vert-space (first above-info)))
                                                                (+ (fourth p2) (/ (sixth p2) 2))
                                                                (+ (fifth p2) (seventh p2)))))
                                                      (cons (hash-set (car info) (car l)
                                                                      (append (cdr l) (list (cdr info)
                                                                                            (+ top-margin (* vert-space (first above-info)))
                                                                                            (+ (* 2 inside-margin) w)
                                                                                            (+ (* 2 inside-margin) h) )))
                                                            (+ (cdr info) w (* 2 inside-margin) horiz-space))
                                                      )
                                                    )
                                                  `(,(third above-info) . ,(exact-ceiling left-indent))
                                                  free)))))))])])
        (begin
          (send frame show #t)
          (sleep/yield 0.5)
          (send canvas init-auto-scrollbars full-width full-height 0.5 0)
          ))
      )))


;;; TESTAREA REZOLUȚIEI
;; Cerința 2
;;; Exemplul 1. Gina Pistol
(define gina-pistol-facts
  `((∀ X ((iubeste gina X) → (googler X)))
    (student mirel)
    (∀ X ((student X) → ((¬ (invata X)) → (¬ (stie X teorema-bayes)))))
    (∀ X ((¬ (stie X teorema-bayes)) → (¬ (promoveaza X ia))))
    (∀ X ((¬ (promoveaza X ia)) → (¬ (googler X))))))

(define gina-pistol-theorem
  `((¬ (invata mirel)) → (¬ (iubeste gina mirel))))
 
;(display-proof (resolution gina-pistol-facts gina-pistol-theorem))


;;; Exemplul 2. Moș Crăciun

(define kids-facts
  `((∀ X ((copil X) ↔ ((fata X) ∨ (baiat X))))
    (∀ X ((baiat X) → (¬ (fata X))))
    (∀ X ((fata X) → (¬ (baiat X))))
    (∀ X ((copil X) → (((nuielusa X) ∨ (minge X)) ∨ (papusa X))))
    (∀ X ((baiat X) → (¬ (papusa X))))
    (∀ X ((copil X) → ((cuminte X) → (¬ (nuielusa X))))))) 

(define kids-theorem
  `((∀ X ((copil X) → (¬ (minge X)))) → (∀ X ((baiat X) → (¬ (cuminte X))))))

;(resolution kids-facts kids-theorem)


;;; Interpretor interactiv
(define resolution-prover
  (λ ()
    (let loop ([clauses '()])
      (let ([command (begin (displayln "Your command, master:") (read))])
        (cond [(equal? command 'add-one) (loop (cons (read) clauses))]
              [(equal? command 'add-many) (loop (append (read) clauses))]
              [(equal? command 'print-all)
               (begin (for-each (λ (c) (displayln c)) clauses) (loop clauses))]
              [(equal? command 'print-all-cnf)
               (begin (for-each (λ (c) (displayln c)) (map cnf clauses)) (loop clauses))]
              [(equal? command 'prove)
               (let ([result (resolution clauses (read))])
                 (if result (thread-wait (thread (λ () (display-proof (filter-useful result))))) (displayln "Cannot prove that"))
                 (loop clauses))]
              [(equal? command 'clear) (loop '())]
              [(equal? command 'quit) #t]
              [else (displayln "Unknown command! (Use add-one, add-many, print-all prove, clear, quit)")]
              )))
    ))
    
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; PARTEA a V-a : Sisteme bazate pe reguli, înlănțuire înapoi

(define deep-substitute
  (λ (e s) 
    (let ([e2 (substitute e s)]) (if (equal? e e2) e (deep-substitute e2 s)))))

(define backward-chaining
  (λ (stack rules)
    (displayln stack)
    (read)
    
    (if (null? stack)
        `(())
        (let* ([goal (first stack)])
          (foldl (λ (r acc)
                   (let* ([rule (rename-vars r)]
                          [conclusion (cdr rule)]
                          [subst (unify conclusion goal)])
                     (if subst
                         (let ([subst2 (backward-chaining (append (deep-substitute (car rule) subst)
                                                                  (deep-substitute (cdr stack) subst))
                                                          rules)])
                           (if subst2 (append (map (λ (s) (append subst s)) subst2) acc) acc))
                         acc)))
                 '()
                 rules)))))

(define prove
  (λ (goal facts rules)
    (let ([all-substs (backward-chaining (list goal) (append (map (λ (f) (cons '() f)) facts) rules))])
      (if (null? all-substs)
          #f
          (map (λ (s) (deep-substitute goal s)) all-substs)
          ))))

(define graph-facts
  '((node a) (node b) (node c) (node d) (node e)
             (arc a b) (arc b c) (arc a c) (arc c e) (arc d c) (arc d e)))

(define graph-rules
  '((((node X) (node Y) (node Z) (edge X Y) (edge X Z) (edge Y Z)) . (d-clique X Y Z))
    (() . (permutation X Y Z X Y Z))
    (() . (permutation X Y Z X Z Y))
    (() . (permutation X Y Z Y X Z))
    (() . (permutation X Y Z Y Z X))
    (() . (permutation X Y Z Z X Y))
    (() . (permutation X Y Z Z Y X))
    (((d-clique X Y Z) (permutation X Y Z A B C)) . (clique A B C))
    (((arc X Y)) . (edge X Y)) (((arc X Y)) (edge Y X))
    (((arc X Y)) . (path X Y))
    (((arc X Y) (path Y Z)) . (path X Z))))

;; (prove '(clique X Y Z) graph-facts graph-rules)

(define prolog
  (λ (facts rules)
    (let loop ([goal (read)])
      (prove goal facts rules))))
               
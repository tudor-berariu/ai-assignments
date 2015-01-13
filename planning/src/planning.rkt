#lang racket/gui
;;; Inteligență Artificială 2014
;;; Tema 2: Planificare
;;; Tudor Berariu, Decembrie 2014

;;; SECȚIUNILE ACESTUI FIȘIER:
;;;  1. Definițiile agenților **COMPLETAȚI AICI PENTRU REZOLVAREA TEMEI**
;;;  2. Definițiile scenariilor de test
;;;  3. Configurarea afișării pe ecran și a interfeței grafice **UTILĂ PENTRU TESTARE**
;;;  4. Funcții pentru accesarea informațiilor din starea unui agent sau din starea lumii
;;;  5. Funcții utile în viață
;;;  6. Statistici
;;;  7. Mesaje informative
;;;  8. Verificarea operatorilor înainte de aplicare
;;;  9. Aplicarea unui operator
;;; 10. Verificarea testelor
;;; 11. Poziționare pe hartă
;;; 12. GUI
;;; 13. Rulare

(require test-engine/racket-tests)

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 1 : AGENȚII
;;;
;;; Mai jos sunt definițiile unui planificator aleator și scheletul de cod pentru memoryless-agent și advanced-agent.

;; Agent care planifică aleator

(define dummy-agent
  (λ (goal world-state rest-actions memory)
    `(,(car (foldl (λ (i acc)
                     (cond [(> (random) 0.33) ;; Adaug un operator Move
                            (let* ([loc (cdr acc)]
                                   [neighbours (map third (filter (λ (p) (and (equal? (first p) 'door)
                                                                              (equal? (second p) loc)))
                                                                  world-state))]
                                   [next (first (shuffle neighbours))])
                              `(,(append (car acc) `((Move ,loc ,next))) . ,next))]
                           [(> (random) 0.33) ;; Adaug un operator Load, eventual precedat de un Test
                            (let* ([sc (car (shuffle '(red blue gray)))]
                                   [load `(Load ,sc)]
                                   [test `(Test (positive N3) (succ N3 N2) (succ N1 N2) (spheres ,sc ,(cdr acc) N1))])
                              (if (> (random) 0.5)
                                  `(,(append (car acc) `(,load)) . ,(cdr acc))
                                  `(,(append (car acc) `(,test ,load)) . ,(cdr acc))))]
                           [else ;; Adaug un operator Unload
                            `(,(append (car acc) `((Unload ,(car (shuffle '(red blue gray)))))) . ,(cdr acc))]))
                   `(() . ,(second (findf (λ (p) (equal? (car p) 'location)) world-state)))
                   (range (add1 (random 20))))) . null)))

;; CERINȚA 2: Agent fără memorie care construiește un plan pentru aducerea următoarei bile

(define memoryless-agent dummy-agent)

;; BONUS: Agent cu memorie care construiește un plan pentru aducerea următoarei bile

(define advanced-agent dummy-agent)

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 2 : SCENARIILE
;;;
;;; Scenariile sunt reprezentate ca perechi
;;;     (m . initial-world-state)
;;; unde m este numărul de bile roșii și numărul de bile albastre, iar initial-world-state este o lista de predicate
;;; (color Room Color), (door Room1 Room2) sau (spheres Color Room N).

;; Scenariul 0
(define scenario0
  '(4 . (;; Culorile camerelor
         (color blueWarehouse blue)
         (color redWarehouse red) 
         (color room1 white) (color room2 white)
         ;; Ușile
         (door blueWarehouse room1)
         (door room1 blueWarehouse) (door room1 room2)
         (door room2 room1) (door room2 redWarehouse)
         (door redWarehouse room2)
         ;; Sferele
         (spheres gray blueWarehouse 1) (spheres red blueWarehouse 0) (spheres blue blueWarehouse 0)
         (spheres gray room1 1) (spheres red room1 2) (spheres blue room1 2)
         (spheres gray room2 1) (spheres red room2 2) (spheres blue room2 2)
         (spheres gray redWarehouse 1) (spheres red redWarehouse 0) (spheres blue redWarehouse 0)
         )))

;; Scenariul 1
(define scenario1
  '(4 . (;; Culorile camerelor
         (color blueWarehouse blue)
         (color redWarehouse red)
         (color room1 white) (color room2 white) (color room3 white)
         ;; Ușile
         (door blueWarehouse room1) 
         (door room1 blueWarehouse) (door room1 room2) (door room1 room3)
         (door room2 room1) (door room2 room3) (door room2 redWarehouse)
         (door room3 room1) (door room3 room2)
         (door redWarehouse room2)
         ;; Sferele
         (spheres gray blueWarehouse 1) (spheres red blueWarehouse 0) (spheres blue blueWarehouse 0)
         (spheres gray room1 1) (spheres red room1 1) (spheres blue room1 1)
         (spheres gray room2 1) (spheres red room2 1) (spheres blue room2 1)
         (spheres gray room3 1) (spheres red room3 2) (spheres blue room3 2)
         (spheres gray redWarehouse 1) (spheres red redWarehouse 0) (spheres blue redWarehouse 0)
         )))

;; Scenariul 2
(define scenario2
  '(5 . (;; Culorile camerelor
         (color blueWarehouse blue)
         (color redWarehouse red)
         (color room1 white) (color room2 white) (color room3 white) (color room4 white) (color room5 white)
         ;; Ușile
         (door blueWarehouse room1)
         (door room1 blueWarehouse) (door room1 room3)
         (door room2 room3)
         (door room3 room1) (door room3 room2) (door room3 room4) (door room3 room5)
         (door room4 room3)
         (door room5 room3) (door room5 redWarehouse)
         (door redWarehouse room5)
         ;; Sferele
         (spheres gray blueWarehouse 1) (spheres red blueWarehouse 0) (spheres blue blueWarehouse 0)
         (spheres gray room1 1) (spheres red room1 2) (spheres blue room1 0)
         (spheres gray room2 1) (spheres red room2 1) (spheres blue room2 1)
         (spheres gray room3 1) (spheres red room3 1) (spheres blue room3 1)
         (spheres gray room4 1) (spheres red room4 1) (spheres blue room4 1)
         (spheres gray room5 1) (spheres red room5 0) (spheres blue room5 2)
         (spheres gray redWarehouse 1) (spheres red redWarehouse 0) (spheres blue redWarehouse 0)
         )))

;; Scenariul 3
(define scenario3
  '(11 . (;; Culorile camerelor
          (color blueWarehouse blue)
          (color redWarehouse red)
          (color room1 white) (color room2 white) (color room3 white) (color room4 white) (color room5 white)
          (color room6 white) (color room7 white) (color room8 white)
          ;; Ușile
          (door room1 room4) (door room2 room4) (door room3 room4)
          (door room4 room1) (door room4 room2) (door room4 room3) (door room4 redWarehouse)
          (door redWarehouse room4) (door redWarehouse blueWarehouse)
          (door blueWarehouse redWarehouse) (door blueWarehouse room5)
          (door room5 blueWarehouse) (door room5 room6) (door room5 room7) (door room5 room8)
          (door room6 room5) (door room7 room5) (door room8 room5)
          ;; Sferele
          (spheres gray blueWarehouse 1) (spheres red blueWarehouse 0) (spheres blue blueWarehouse 0)
          (spheres gray room1 1) (spheres red room1 1) (spheres blue room1 2)
          (spheres gray room2 1) (spheres red room2 0) (spheres blue room2 3)
          (spheres gray room3 1) (spheres red room3 1) (spheres blue room3 2)
          (spheres gray room4 1) (spheres red room4 2) (spheres blue room4 0)
          (spheres gray room5 1) (spheres red room5 0) (spheres blue room5 2)
          (spheres gray room6 1) (spheres red room6 2) (spheres blue room6 1)
          (spheres gray room7 1) (spheres red room7 2) (spheres blue room7 1)
          (spheres gray room8 1) (spheres red room8 3) (spheres blue room8 0)
          (spheres gray redWarehouse 1) (spheres red redWarehouse 0) (spheres blue redWarehouse 0)
          )))

;; Scenariul 4
(define scenario4
  '(5 . (;; Culorile camerelor
         (color blueWarehouse blue)
         (color redWarehouse red)
         (color room1 white) (color room2 white) (color room3 white) (color room4 white) (color room5 white)
         (color room6 white)
         ;; Ușile
         (door blueWarehouse room1)
         (door room1 blueWarehouse)  (door room1 room2)
         (door room2 room3) (door room3 room4)
         (door room4 redWarehouse) (door room4 room5) 
         (door room5 room6) (door room6 room1)
         (door redWarehouse room4)
         ;; Sferele
         (spheres gray blueWarehouse 1) (spheres red blueWarehouse 0) (spheres blue blueWarehouse 0)
         (spheres gray room1 1) (spheres red room1 2) (spheres blue room1 0)
         (spheres gray room2 1) (spheres red room2 1) (spheres blue room2 0)
         (spheres gray room3 1) (spheres red room3 1) (spheres blue room3 1)
         (spheres gray room4 1) (spheres red room4 0) (spheres blue room4 2)
         (spheres gray room5 1) (spheres red room5 0) (spheres blue room5 1)
         (spheres gray room6 1) (spheres red room6 1) (spheres blue room6 1)
         (spheres gray redWarehouse 1) (spheres red redWarehouse 0) (spheres blue redWarehouse 0)
         )))

;; Scenariul 4
(define scenario5
  '(7 . (;; Culorile camerelor
         (color blueWarehouse blue)
         (color redWarehouse red)
         (color room1 white) (color room2 white) (color room3 white) (color room4 white) (color room5 white)
         (color room6 white) (color room7 white)
         ;; Ușile
         (door blueWarehouse room1) (door blueWarehouse room3)
         (door room1 blueWarehouse) (door room1 room2)
         (door room2 room1) (door room2 room4)
         (door room3 blueWarehouse) (door room3 room4)
         (door room4 room2) (door room4 room3) (door room4 room5) (door room4 room6)
         (door room5 room4) (door room5 redWarehouse)
         (door room6 room4) (door room6 room7)
         (door room7 room6) (door room7 redWarehouse)
         (door redWarehouse room5) (door redWarehouse room7)
         ;; Sferele
         (spheres gray blueWarehouse 1) (spheres red blueWarehouse 0) (spheres blue blueWarehouse 0)
         (spheres gray room1 1) (spheres red room1 1) (spheres blue room1 1)
         (spheres gray room2 1) (spheres red room2 2) (spheres blue room2 1)
         (spheres gray room3 1) (spheres red room3 1) (spheres blue room3 0)
         (spheres gray room4 1) (spheres red room4 2) (spheres blue room4 1)
         (spheres gray room5 1) (spheres red room5 1) (spheres blue room5 1)
         (spheres gray room6 1) (spheres red room6 0) (spheres blue room6 1)
         (spheres gray room7 1) (spheres red room7 0) (spheres blue room7 2)
         (spheres gray redWarehouse 1) (spheres red redWarehouse 0) (spheres blue redWarehouse 0)
         )))
;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 3 : CONFIGURARE
;;;
;;; În această secțiune se configurează aspecte legate de informațiile afișate la rularea unui scenariu.

;; Se afișează markere pentru delimitarea rundelor?
(define _print-round-separator? #f)
;; Se așteaptă apăsarea unei taste după fiecare rundă?
(define _pause-before-next-round? #f)
;; Se afișează scorul curent?
(define _print-score? #f)
;; Se afișează starea lumii la finalul fiecărui tur?
(define _print-world-state? #f)
;; Se afișează starea agenților la finalul fiecărui tur?
(define _print-agent-state? #f)
;; Se afișează planul complet reîntors de planificator?
(define _print-new-plan? #f)
;; Se afișează acțiunile care nu s-au putut aplica?
(define _print-action-error? #f)
;; Se afișează acțiunile aplicate?
(define _print-action? #f)
;; Se afișează condițiile (testele)?
(define _print-condition? #f)
;; Se afișează GUI?
(define _display-gui? #t)
;; Adorame execuția pentru un timp înainte de runda următoare
(define _SLEEP-TIME-BEFORE-NEXT-ROUND 0.01)

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 5 : INFORMAȚII DESPRE STAREA LUMII Și A AGENȚILOR
;;;
;;; Funcții necesare pentru accesarea informațiilor din starea lumii sau a agenților


;;; Cealaltă culoare față de @color dintre Red și Blue
(define _next-agent (λ (color) (if (equal? color 'blue) 'red 'blue)))

;;; lista camerelor din scenariu
(define _all-rooms
  (λ (world-state)
    (map second (filter (λ (arg) (and (equal? (first arg) 'color) (equal? (length arg) 3))) world-state))))

(check-expect (_all-rooms (cdr scenario0))
              '(blueWarehouse redWarehouse room1 room2))
(check-expect (_all-rooms (cdr scenario1))
              '(blueWarehouse redWarehouse room1 room2 room3))
(check-expect (_all-rooms (cdr scenario2))
              '(blueWarehouse redWarehouse room1 room2 room3 room4 room5))
(check-expect (_all-rooms (cdr scenario3))
              '(blueWarehouse redWarehouse room1 room2 room3 room4 room5 room6 room7 room8))
(check-expect (_all-rooms (cdr scenario4))
              '(blueWarehouse redWarehouse room1 room2 room3 room4 room5 room6))
(check-expect (_all-rooms (cdr scenario5))
              '(blueWarehouse redWarehouse room1 room2 room3 room4 room5 room6 room7))

;;; depozitul de culoare @color conform @world-state
(define _warehouse
  (λ (color world-state)
    (second (findf (λ (arg) (and (equal? (first arg) 'color) (equal? (third arg) color))) world-state))))
(check-expect (_warehouse 'red (cdr scenario0)) 'redWarehouse)
(check-expect (_warehouse 'blue (cdr scenario0)) 'blueWarehouse)
(check-expect (_warehouse 'red (cdr scenario1)) 'redWarehouse)
(check-expect (_warehouse 'blue (cdr scenario1)) 'blueWarehouse)
(check-expect (_warehouse 'red (cdr scenario2)) 'redWarehouse)
(check-expect (_warehouse 'blue (cdr scenario2)) 'blueWarehouse)
(check-expect (_warehouse 'red (cdr scenario3)) 'redWarehouse)
(check-expect (_warehouse 'blue (cdr scenario3)) 'blueWarehouse)
(check-expect (_warehouse 'red (cdr scenario4)) 'redWarehouse)
(check-expect (_warehouse 'blue (cdr scenario4)) 'blueWarehouse)
(check-expect (_warehouse 'red (cdr scenario5)) 'redWarehouse)
(check-expect (_warehouse 'blue (cdr scenario5)) 'blueWarehouse)

;;; culoarea camerei @room
(define _color-of-room
  (λ (room world-state) 
    (third (findf (λ (arg) (and (equal? (first arg) 'color) (equal? (second arg) room))) world-state))))

(check-expect (_color-of-room 'room1 (cdr scenario0)) 'white)
(check-expect (_color-of-room 'redWarehouse (cdr scenario0)) 'red)
(check-expect (_color-of-room 'blueWarehouse (cdr scenario0)) 'blue)
(check-expect (_color-of-room 'room1 (cdr scenario1)) 'white)
(check-expect (_color-of-room 'redWarehouse (cdr scenario1)) 'red)
(check-expect (_color-of-room 'blueWarehouse (cdr scenario1)) 'blue)
(check-expect (_color-of-room 'room1 (cdr scenario2)) 'white)
(check-expect (_color-of-room 'redWarehouse (cdr scenario2)) 'red)
(check-expect (_color-of-room 'blueWarehouse (cdr scenario2)) 'blue)
(check-expect (_color-of-room 'room1 (cdr scenario3)) 'white)
(check-expect (_color-of-room 'redWarehouse (cdr scenario3)) 'red)
(check-expect (_color-of-room 'blueWarehouse (cdr scenario3)) 'blue)
(check-expect (_color-of-room 'room1 (cdr scenario4)) 'white)
(check-expect (_color-of-room 'redWarehouse (cdr scenario4)) 'red)
(check-expect (_color-of-room 'blueWarehouse (cdr scenario4)) 'blue)
(check-expect (_color-of-room 'room1 (cdr scenario5)) 'white)
(check-expect (_color-of-room 'redWarehouse (cdr scenario5)) 'red)
(check-expect (_color-of-room 'blueWarehouse (cdr scenario5)) 'blue)

;;; numărul de sfere de culoarea @color încărcate de robot
(define _loaded-spheres-color
  (λ (agent-state color)
    (third (findf (λ (arg) (and (equal? (first arg) 'carries) (equal? (second arg) color))) agent-state))))

;;; numărul total de sfere încărcate de robot
(define _loaded-spheres-total
  (λ (agent-state)
    (apply + (map (λ (c) (_loaded-spheres-color agent-state c)) '(red blue gray)))))

;;; sferele încărcate de robot
(define _loaded-spheres
  (λ (agent-state)
    (apply append (map (λ (c) (let ((n (_loaded-spheres-color agent-state c)))
                                (cond ((= n 1) `(,c)) ((= n 2) `(,c ,c)) (#t '()))))
                       '(red blue gray)))))

;;; numărul de sfere de culoarea @color din camera @room
(define _in-room-spheres-color
  (λ (color room world-state)
    (fourth (findf (λ (arg) (and (equal? (first arg) 'spheres) (equal? (second arg) color) (equal? (third arg) room)))
                   world-state))))

(check-expect (_in-room-spheres-color 'gray 'room1 (cdr scenario0)) 1)

;;; *_delivered* întoarce '(Spheres @color warehouse n)
;;; pentru culoarea @color, unde warehouse este depozitul de culoare @color
;;; iar n numărul de sfere livrate până acum
(define _delivered
  (λ (color world-state)
    (findf (λ (arg) (and (equal? (first arg) 'spheres) (equal? (second arg) color)
                         (equal? (third arg) (_warehouse color world-state))))
           world-state)))

(check-expect (_delivered 'red (cdr scenario0)) '(spheres red redWarehouse 0))
(check-expect (_delivered 'blue (cdr scenario0)) '(spheres blue blueWarehouse 0))
(check-expect (_delivered 'red (cdr scenario1)) '(spheres red redWarehouse 0))
(check-expect (_delivered 'blue (cdr scenario1)) '(spheres blue blueWarehouse 0))

;;; *_one-sphere-goal* întoarce '(Spheres @color warehouse (+ n 1))
;;; pentru culoarea @color, unde warehouse este depozitul de culoare @color
;;; iar n numărul de sfere livrate până acum
(define _one-sphere-goal
  (λ (color world-state)
    (let* ((done (_delivered color world-state))) `(,(first done) ,(second done) ,(third done) ,(+ 1 (fourth done))))))

(check-expect (_one-sphere-goal 'red (cdr scenario0)) '(spheres red redWarehouse 1))
(check-expect (_one-sphere-goal 'blue (cdr scenario0)) '(spheres blue blueWarehouse 1))
(check-expect (_one-sphere-goal 'red (cdr scenario1)) '(spheres red redWarehouse 1))
(check-expect (_one-sphere-goal 'blue (cdr scenario1)) '(spheres blue blueWarehouse 1))


;;; *_all-spheres-goal* întoarce '(Spheres @color warehouse M)
;;; pentru culoarea @color, unde warehouse este depozitul de culoare @color
(define _all-spheres-goal
  (λ (color scenario)
    (let* ((done (_delivered color (cdr scenario)))) `(,(first done) ,(second done) ,(third done) ,(car scenario)))))

(check-expect (_all-spheres-goal 'red scenario0) '(spheres red redWarehouse 4))
(check-expect (_all-spheres-goal 'blue scenario0) '(spheres blue blueWarehouse 4))
(check-expect (_all-spheres-goal 'red scenario1) '(spheres red redWarehouse 4))
(check-expect (_all-spheres-goal 'blue scenario1) '(spheres blue blueWarehouse 4))
(check-expect (_all-spheres-goal 'red scenario2) '(spheres red redWarehouse 5))
(check-expect (_all-spheres-goal 'blue scenario2) '(spheres blue blueWarehouse 5))
(check-expect (_all-spheres-goal 'red scenario3) '(spheres red redWarehouse 11))
(check-expect (_all-spheres-goal 'blue scenario3) '(spheres blue blueWarehouse 11))
(check-expect (_all-spheres-goal 'red scenario4) '(spheres red redWarehouse 5))
(check-expect (_all-spheres-goal 'blue scenario4) '(spheres blue blueWarehouse 5))
(check-expect (_all-spheres-goal 'red scenario5) '(spheres red redWarehouse 7))
(check-expect (_all-spheres-goal 'blue scenario5) '(spheres blue blueWarehouse 7))

;;; *_initial-agent-state* întoarce starea inițială a agentului de culoare
;;; @color conform stării inițiale a lumii *world-state*
(define _initial-agent-state
  (λ (color world-state)
    (append (map (λ (c) (list 'carries c 0)) '(red blue gray))
            `((location ,(_warehouse color world-state)) (color ,color)))))

;;; *_winner?* întoarce #t dacă în starea @world-state unul dintre agenți
;;; a reușit să ducă toate sferele la depozit și #f altfel
(define _winner?
  (λ (world-state m)
    (or (= (fourth (_delivered 'red world-state)) m)
        (= (fourth (_delivered 'blue world-state)) m))))

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 5 : FUNCȚII AUXILIARE
;;;
;;; Funcții Utile

;;; extrage valoarea după cheie dintr-o listă de perechi
(define _value-of (λ (val pairs) (let ((val (assoc val pairs))) (if val (cdr val) val))))

;;; *_modify* reîntoarce lista obținută prin aplicarea funcției @proc asupra
;;; tuturor elementelor din @list asupra cărora aplicarea funcției @test
;;; reîntoarce #t (restul elementelor rămân neschimbate).
(define _modify (λ (lst test proc) (map (λ (arg) (if (test arg) (proc arg) arg)) lst)))
(check-expect (_modify '(1 2 3 4 5 6) (λ (x) (eq? (modulo x 2) 1)) (λ (x) (* -1 x))) '(-1 2 -3 4 -5 6))

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 6 : STATISTICI
;;;
;;; Statistici

;;; *_end-game* afișează rezultatele finale
(define _end-game
  (λ (world-state statistics)
    (let ([red-score (fourth (_delivered 'red world-state))]
          [blue-score (fourth (_delivered 'blue world-state))])
      (cond [(> red-score blue-score) (displayln "Agentul ROȘU a câștigat!\n")]
            [(< red-score blue-score) (displayln "Agentul ALBASTRU a câștigat!\n")]
            [#t (displayln "Egalitate!\n")])
      (displayln (~a (~a "ROȘU vs. ALBASTRU" #:min-width 25 #:align 'left) " : "
                     (~a red-score #:min-width 8 #:max-width 8 #:align 'right)
                     " ~ "
                     (~a blue-score #:min-width 8 #:max-width 8 #:align 'left)))
      (for-each (λ (arg) (displayln (~a (~a (first arg) #:min-width 25 #:align 'left) " : "
                                        (~a (second (second arg)) #:min-width 8 #:max-width 8 #:align 'right)
                                        " ~ "
                                        (~a (second (third arg)) #:min-width 8 #:max-width 8 #:align 'left))))
                (_compute-average-planning-time statistics)))))

;;; *_initial-statistics* construiește o listă de perechi cu contoarele pe zero
(define _initial-statistics
  '(("Acțiuni" . ((red 0) (blue 0)))
    ("Replanificări" . ((red 0) (blue 0)))
    ("Erori de aplicare" . ((red 0) (blue 0)))
    ("Teste trecute" . ((red 0) (blue 0)))
    ("Teste eșuate" . ((red 0) (blue 0)))
    ("Timp total planificare" . ((red .0) (blue .0)))
    ("Timp mediu planificare" . ((red .0) (blue .0)))))

;;; *_increment* adaugă în @statistics 1 la categoria @str-name pentru robotul
;;; de culoare @color
(define _increment
  (λ (str-name color statistics)
    (_modify statistics
             (λ (x) (string=? str-name (car x)))
             (λ (x) `(,(car x)
                      .
                      ,(_modify (cdr x) (λ (c) (equal? (car c) color)) (λ (c) `(,(car c) ,(+ 1 (second c)))))))
             )))

;;; *_add-to-statistics* adaugă în @statistics @val la categoria @str-name pentru robotul
;;; de culoare @color
(define _add-to-statistics
  (λ (str-name color val statistics)
    (_modify statistics
             (λ (x) (string=? str-name (car x)))
             (λ (x) (cons (car x) 
                          (_modify (cdr x)
                                   (λ (c) (equal? (car c) color))
                                   (λ (c) (list (car c) (+ val (second c)))))
                          )
               )
             )
    )
  )

;;; *_compute-average-planning-time* calculează timpul mediu de planificare
(define _compute-average-planning-time
  (λ (statistics)
    (_modify statistics
             (λ (x) (string=? "Timp mediu planificare" (car x)))
             (λ (x) (cons (car x)
                          (map
                           (λ (z) 
                             (list (car z)
                                   (/ 
                                    (car (_value-of (car z) (_value-of "Timp total planificare" statistics)))
                                    (car (_value-of (car z) (_value-of "Replanificări" statistics)))))
                             )
                           (cdr x))
                          ))
             )
    )
  )
;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 7 : AFIȘARE
;;;
;;; Funcții pentru afișarea informațiilor în timpul rulării scenariului

(define _print-round-separator
  (λ (color)
    (displayln "--------------------------------------")
    (displayln "--------------------------------------")
    (displayln (~a "Robotul : " color "\n"))))

(define _print-score
  (λ (world-score)
    (displayln (~a "ROȘU : " (fourth (_delivered 'red world-score)) " ~~~ "
                   (fourth (_delivered 'blue world-score)) " : ALBASTRU\n"))))

(define _print-world-state
  (λ (world-state)
    (displayln (~a "|" (~a "Cameră" #:min-width 17 #:align 'center)
                   "|" (~a "Roșii" #:min-width 10 #:align 'center)
                   "|" (~a "Albastre"  #:min-width 10 #:align 'center)
                   "|" (~a "Gri" #:min-width 10 #:align 'center)
                   "|"))
    (for-each (λ (room)
                (displayln (~a "|" (~a room #:min-width 17 #:align 'left)
                               "|" (~a (_in-room-spheres-color 'red room world-state) #:min-width 10 #:align 'right)
                               "|" (~a (_in-room-spheres-color 'blue room world-state)  #:min-width 10 #:align 'right)
                               "|" (~a (_in-room-spheres-color 'gray room world-state) #:min-width 10 #:align 'right)
                               "|")))
              (_all-rooms world-state))
    (displayln "\n")))

(define _print-agent-state
  (λ (agent-state)
    (displayln (~a (_value-of 'color agent-state) " robot # Sfere: "
                   (~a (_loaded-spheres-color agent-state 'red)) " red, "
                   (~a (_loaded-spheres-color agent-state 'blue)) " blue, "
                   (~a (_loaded-spheres-color agent-state 'gray)) " gray, "
                   "Localizare: " (_value-of 'location agent-state) "\n"))))

(define _print-new-plan
  (λ (plan) (displayln (~a "Plan nou: " plan "\n"))))

(define _print-action-error 
  (λ (action message) (displayln (~a "EROARE:\t" action "\n" message "\n"))))

(define _print-action
  (λ (action) (display (~a "EXECUTĂ:\t" action "\n"))))

(define _print-condition
  (λ (condition result) (displayln (~a "TEST:\t" condition " ===> " (if result "ADEVĂRAT" "FALS") "\n"))))

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 8 : VERIFICAREA OPERATORILOR
;;;
;;; Verificarea acțiunilor

;;; *_check-move-action* verifică dacă o acțiune Move este validă
;;; @args reprezintă parametrii acțiunii
;;; @agent-state este starea agentului
;;; @world-state este starea lumii
(define _check-move-action
  (λ (args agent-state world-state)
    (cond [(not (= (length args) 2))
           '(#f . "Număr incorect de parametri")]
          [(or (not (member (first args) (_all-rooms world-state)))
               (not (member (second args) (_all-rooms world-state))))
           '(#f . "Argumentele nu sunt camere din scenariul curent")]
          [(not (member `(location ,(first args)) agent-state))
           '(#f . "Primul argument din Move nu coincide cu camera curentă")]
          [(not (member `(door ,(first args) ,(second args)) world-state))
           '(#f . "Nu exista o ușă între camerele date ca parametri")]
          [(= (_loaded-spheres-total agent-state) 1)
           '(#f . "Robotul nu poate călători cu o singură sferă")]
          [else '(#t . "ok")])))

;;; *_check-load-action* verifică dacă o acțiune Load este validă
(define _check-load-action
  (λ (args agent-state world-state)
    (let* ([room (second (assoc 'location agent-state))]
           [color (first args)])
      (cond [(= (_loaded-spheres-total agent-state) 2)
             '(#f . "Robotul are compartimentele pline")]
            [(equal? (_color-of-room room world-state) color)
             '(#f . "Nu se pot ridica sferele duse deja la depozitul lor")]
            [(zero? (_in-room-spheres-color color room world-state))
             '(#f . "Nu există sfere de acea culoare în camera curentă")]
            [else '(#t . "ok")]))))

;;; *_check-unload-action*
(define _check-unload-action
  (λ (args agent-state world-state)
    (if (> (_loaded-spheres-color agent-state (first args)) 0)
        '(#t . "ok")
        '(#f . "Robotul nu are sfere pe care sa le descarce"))))

;;; *_sort-conditions* sorteaza predicatele astfel încât să se poată
;;; lega variabilele și face verificările... dacă se poate
(define _sort-conditions
  (λ (conditions)
    (let my-sort ([predicates conditions]
                  [known-vars null]
                  [sorted null])
      (if (empty? predicates)
          sorted
          (let* ([p1
                  (findf (λ (arg)
                           (or 
                            ;; Fie este un predicat 'Spheres
                            (equal? (first arg) 'spheres)
                            ;; Fie este un Succ cu măcar o variabilă legată
                            (and (equal? (first arg) 'succ)
                                 (or (member (second arg) known-vars)
                                     (number? (second arg))
                                     (member (third arg) known-vars)
                                     (number? (third arg))))
                            ;; Fie este un Greater cu ambele variable legate
                            (and (equal? (first arg) 'greater)
                                 (or (member (second arg) known-vars)
                                     (number? (second arg)))
                                 (or (member (third arg) known-vars)
                                     (number? (third arg))))
                            ;; Fie este un Positive cu variabila legată
                            (and (equal? (first arg) 'positive)
                                 (or (number? (second arg))
                                     (member (second arg) known-vars)))))
                           predicates)])
            (if p1
                (let* ([new-vars (cond [(equal? (first p1) 'spheres) 
                                        (if (not (or (number? (fourth p1)) (member (fourth p1) known-vars)))
                                            `(,(fourth p1))
                                            null)]
                                       [(equal? (first p1) 'succ)
                                        (append (if (not (or (number? (second p1)) (member (second p1) known-vars)))
                                                    `(,(second p1))
                                                    null)
                                                (if (not (or (number? (third p1)) (member (third p1) known-vars)))
                                                    `(,(third p1))
                                                    null))]
                                       [else null])])
                  (my-sort (remove p1 predicates) (append new-vars known-vars) (append sorted `(,p1))))
                #f))))))

;;; *_check-test-action*
(define _check-test-action
  (λ (arguments agent-state world-state)
    (if (_sort-conditions arguments) '(#t . "ok") '(#f . "Nu s-a găsit o ordonare bună a condițiilor."))))

;;; *_operator-check* verifică dacă se poate aplica acțiunea @action
;;; din statea agentului @agent-state și starea lumii @world-state
(define _operator-check
  (λ (action agent-state world-state)
    (cond [(equal? (first action) 'Move)   (_check-move-action (cdr action) agent-state world-state)]
          [(equal? (first action) 'Load)   (_check-load-action (cdr action) agent-state world-state)]
          [(equal? (first action) 'Unload) (_check-unload-action (cdr action) agent-state world-state)]
          [(equal? (first action) 'Test)   (_check-test-action (cdr action) agent-state world-state)]
          [else '(#f . "Operator necunoscut")])))

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 10 : EXECUTARE
;;;
;;; Aplicarea operatorilor

;;; *_apply-action* reîntoarce noua starea a lumii și noua stare
;;; a agentului după aplicarea acțiunii @action în starea mediului
;;; @world-state și în starea internă a agentului @ag-state .
;;; (car @action) poate fi doar Move, Unload sau Load .
(define _apply-action
  (λ (action world-state ag-state)
    (if (equal? (first action) 'Move)
        ;; acțiunea Move: schimbăm Location în starea agentului
        `(,world-state
          .
          ,(_modify ag-state (λ (arg) (equal? (first arg) 'location)) (λ (arg) (list 'location (third action)))))
        ;; acțiune Load sau Unload
        (let* ([n (_value-of (first action) '((Load . 1) (Unload . -1)))]
               [color (second action)]
               [room (first (_value-of 'location ag-state))])
          (cons
           (_modify world-state
                    (λ (arg) (and (equal? (first arg) 'spheres) (equal? (second arg) color) (equal? (third arg) room)))
                    (λ (arg) `(spheres ,color ,room ,(- (fourth arg) n))))
           (_modify ag-state
                    (λ (arg) (and (equal? (first arg) 'carries) (equal? (second arg) color)))
                    (λ (arg) `(carries ,color ,(+ (third arg) n)))))
          )))
  )

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 11 : TEST
;;;
;;; Verificarea testelor


;;; *_check-conditions* evaluează conjuncția de condiții @conditions
;;; conform stării curente a lumii @world-state
(define _check-conditions
  (λ (conditions world-state)
    (_verify-conditions (_sort-conditions conditions) world-state '())))

;;; *_verify-conditions* evaluează condițiile și le verifică cu starea
;;; curentă a lumii @world-state
;;; funcția este recursivă, iar @bindings reprezintă legările anterioare
;;; ale variabilelor la numere
(define _verify-conditions
  (λ (conditions world-state bindings)
    (if (empty? conditions) #t
        (let ([cond1 (first conditions)])
          (cond [;; Dacă este un termen spheres
                 (equal? (first cond1) 'spheres)
                 (let* ([s (findf
                            (λ (arg) 
                              (and (equal? (first arg) (first cond1))
                                   (equal? (second arg) (second cond1))
                                   (equal? (third arg) (third cond1))))
                            world-state)]
                        [x (fourth cond1)]
                        [val (fourth s)])
                   (cond 
                     [(number? x)
                      (and (equal? x val) (_verify-conditions (cdr conditions) world-state bindings))]
                     [(assoc x bindings)
                      (and (equal? val (_value-of x bindings))
                           (_verify-conditions (cdr conditions) world-state bindings))]
                     [#t (_verify-conditions (cdr conditions) world-state `((,x . ,val) . ,bindings))]))]
                [;; Dacă este un succc
                 (equal? (first cond1) 'succ)
                 (let ([n1 (second cond1)] [n2 (third cond1)])
                   (cond [(number? n1)
                          (cond [(number? n2)
                                 (and (= (add1 n1) n2)
                                      (_verify-conditions (cdr conditions) world-state bindings))]
                                [(assoc n2 bindings)
                                 (and (= (add1 n1) (_value-of n2 bindings))
                                      (_verify-conditions (cdr conditions) world-state bindings))]
                                [else (_verify-conditions (cdr conditions)
                                                          world-state `((,n2 . ,(add1 n1)) . ,bindings))])]
                         [(assoc n1 bindings)
                          (cond [(number? n2)
                                 (and (= (+ 1 (_value-of n1 bindings)) n2)
                                      (_verify-conditions (cdr conditions) world-state bindings))]
                                [(assoc n2 bindings)
                                 (and (= (+ 1 (_value-of n1 bindings)) (_value-of n2 bindings))
                                      (_verify-conditions (cdr conditions) world-state bindings))]
                                [else (_verify-conditions (cdr conditions) world-state
                                                          `((,n2 . ,(add1 (_value-of n1 bindings))) . ,bindings))])]
                         [else (cond [(number? n2)
                                      (_verify-conditions (cdr conditions) world-state
                                                          `((,n1 . ,(sub1 n2)) . ,bindings))]
                                   [(assoc n2 bindings)
                                    (_verify-conditions (cdr conditions) world-state
                                                        `((,n1 . ,(sub1 (_value-of n2 bindings))) . ,bindings))]
                                   [else #f])]))]
                [;;; este un Positive
                 (equal? (first cond1) 'positive)
                 (and (> (if (number? (second cond1)) (second cond1) (_value-of (second cond1) bindings)) 0)
                      (_verify-conditions (cdr conditions) world-state bindings))]
                [;;; este un Greater
                 (equal? (first cond1) 'greater)
                 (let ([x (if (number? (second cond1)) (second cond1) (_value-of (second cond1) bindings))]
                       [y (if (number? (third cond1)) (third cond1) (_value-of (third cond1) bindings))])
                   (and (> x y) (_verify-conditions (cdr conditions) world-state bindings)))]
                [else #f])
          ))))

(check-expect (_check-conditions '((positive 2) (succ 2 3))
                                 '((spheres red room1 2) (spheres red room2 3)))
              #t)
(check-expect (_check-conditions '((spheres red room1 N1) (spheres red room2 N2) (greater N1 N2))
                                 '((spheres red room1 2) (spheres red room2 3)))
              #f)
(check-expect (_check-conditions '((spheres red room1 N1) (spheres red room2 N2) (greater N2 N1))
                                 '((spheres red room1 2) (spheres red room2 3)))
              #t)
(check-expect (_check-conditions '((spheres red room1 N1) (spheres red room2 N2) (positive N2))
                                 '((spheres red room1 2) (spheres red room2 3)))
              #t)
(check-expect (_check-conditions '((spheres red room1 N1) (spheres red room2 N2) (positive N2))
                                 '((spheres red room1 2) (spheres red room2 0)))
              #f)
(check-expect (_check-conditions '((spheres red room1 N1) (spheres red room2 N2) (positive N2) (greater 3 N1))
                                 '((spheres red room1 2) (spheres red room2 3)))
              #t)

(check-expect (_check-conditions '((spheres red room1 N1) (spheres red room2 N2) (succ Na N1) (succ N2 Na) (succ Nb N2)
                                                          (succ Nc Nb) (positive Nc))
                                 '((spheres red room1 5) (spheres red room2 3)))
              #t)
(check-expect (_check-conditions '((positive N4) (greater N1 1) (succ N1 N2) (succ N2 N3) (succ N4 N3) (succ N5 N4)
                                                 (spheres red room1 N5) (positive N4))
                                 '((spheres red room1 2)))
              #t)
(check-expect (_check-conditions '((greater N1 1) (succ N1 N2) (succ N2 N3) (succ N4 N3) (succ N5 N4)
                                                  (spheres red room1 N5))
                                 '((spheres red room1 1)))
              #f)
(check-expect (_check-conditions '((greater N1 1) (succ N1 N2) (succ N2 N3) (succ N4 N3) (succ N5 N4)
                                                  (spheres red room1 N5))
                                 '((spheres red room1 1)))
              #f)
(test)

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 11 : POZIȚII PE CANVAS
;;;
;;; Funcții pentru poziționarea camerelor pe canvas

(define _get-matrix
  (λ (initial-world-state)
    (let* ([rooms (map (λ (x) `(,(second x) . 0))
                       (filter (λ (x) (and (equal? (car x) 'color) (= (length x) 3))) initial-world-state))]
           [edges (map (λ (x) `(,(second x) . ,(third x)))
                       (filter (λ (x) (equal? (car x) 'door)) initial-world-state))])
      `((,(caar rooms) . (0 . 0))
        . ,(_search-pos `((,(caar rooms) . (0 . 0)))
                        (map (λ (arg)
                               (if (or (member `(,(car arg) . ,(car (car rooms))) edges)
                                       (member `(,(car (car rooms)) . ,(car arg)) edges))
                                   (cons (car arg) (+ 1 (cdr arg)))
                                   arg))
                             (cdr rooms))
                        edges)))))

(define _get-all-around
  (λ (p)
    (map (λ (arg) `(,(+ (car p) (car arg)) . ,(+ (cdr p) (cdr arg))))
         '((1 . 0) (1 . 1) (1 . -1) (0 . 1) (0 . -1) (-1 . 0) (-1 . 1) (-1 . -1)))))

(define _next-to 
  (λ (p1 p2)
    (member `(,(- (car p1) (car p2)) . ,(- (cdr p1) (cdr p2)))
            '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1)))))

(define _search-pos
  (λ (fixed left edges)
    (if 
     ;; s-a găsit o soluție.. întoarce o listă
     (empty? left) '()
     ;; mai avem camere
     (let* ([sorted (sort left (λ (r1 r2) (>= (cdr r1) (cdr r2))))]
            ;; next-room e camera cu cei mai mulți vecini
            [next-room (first sorted)]
            ;; positions este lista cu pozițiile valide pentru next-room
            [positions
             (filter
              (λ (position)
                (not (member position (map cdr fixed))))
              (foldl (λ (r p)
                       (if (or (member `(,(car next-room) . ,(car r)) edges)
                               (member `(,(car r) . ,(car next-room)) edges))
                           (if (equal? p 'start)
                               (_get-all-around (cdr r))
                               (filter (λ (pos) (_next-to pos (cdr r))) (remove (cdr r) p)))
                           p)
                       )
                     'start
                     fixed))])
       (foldl (λ (p r)
                (if (list? r) r
                    (let ([result
                           (_search-pos
                            `((,(car next-room) . ,p) . ,fixed)
                            (map (λ (arg)
                                   (if (or (member `(,(car next-room) . ,(car arg)) edges)
                                           (member `(,(car arg) . ,(car next-room)) edges))
                                       `(,(car arg) . ,(+ (cdr arg) 1))
                                       arg))
                                 (cdr sorted))
                            edges)])
                      (if (list? result) `((,(car next-room) . ,p) . ,result) #f))))
              #f
              positions)))))

;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 12 : GUI
;;;
;;; Funcții pentru interfața grafică


(define _in-room-positions '((-1 . 0) (0 . -1) (-2 . 0) (-1 . -1) (0 . -2) (-3 . 0) (-2 . -1) (-1 . -2) (0 . -3)
                                      (-3 . -1) (-2 . -2) (-1 . -3)))
(define _RED-AGENT-COLOR (send the-color-database find-color "Red"))
(define _BLUE-AGENT-COLOR (send the-color-database find-color "Blue"))
(define _RED-ROOM-COLOR (send the-color-database find-color "Orange Red"))
(define _BLUE-ROOM-COLOR (send the-color-database find-color "RoyalBlue"))
(define _WHITE-ROOM-COLOR (send the-color-database find-color "White"))
(define _RED-SPHERE-COLOR (send the-color-database find-color "Red"))
(define _BLUE-SPHERE-COLOR (send the-color-database find-color "Blue"))
(define _GRAY-SPHERE-COLOR (send the-color-database find-color "Gray"))
(define _AGENT-CONTOUR-COLOR (send the-color-database find-color "Black"))
(define _ROOM-CONTOUR-COLOR (send the-color-database find-color "Black"))
(define _SPHERE-CONTOUR-COLOR (send the-color-database find-color "Olive"))

(define _COLOR-DICT `((red . ,_RED-SPHERE-COLOR) (blue . ,_BLUE-SPHERE-COLOR) (gray . ,_GRAY-SPHERE-COLOR)))

(define _ROOM-RADIUS 80)
(define _EXT-ROOM-RADIUS (* .25 _ROOM-RADIUS))
(define _Q-RADIUS (- (/ _ROOM-RADIUS 4) 6))

(define _redraw
  (λ (canvas positions world-state red-state blue-state)
    (begin
      ;(send _canvas suspend-flush)
      (let ([redraw-proc 
             (λ (dc)
               (send dc erase)
               (for-each
                (λ (room)
                  (let* ([x (* (cadr room) 2.5 _ROOM-RADIUS)]
                         [y (* (cddr room) 2.5 _ROOM-RADIUS)]
                         [to (map third (filter (λ (arg)
                                                  (and (equal? (first arg) 'door) (equal? (second arg) (car room))))
                                                world-state))]
                         [ncoord (map (λ (arg)
                                        `(,(- (car (_value-of arg positions)) (cadr room))
                                          . ,(- (cdr (_value-of arg positions)) (cddr room)))) to)])
                    (send dc set-smoothing 'aligned)
                    (send dc set-pen _ROOM-CONTOUR-COLOR 2 'solid)
                    (send dc set-brush
                          (_value-of
                           (_color-of-room (car room) world-state)
                           `((red . ,_RED-ROOM-COLOR) (blue . ,_BLUE-ROOM-COLOR) (white . ,_WHITE-ROOM-COLOR)))
                          'crossdiag-hatch)
                    (send dc draw-ellipse 
                          (+ x _EXT-ROOM-RADIUS) (+ y _EXT-ROOM-RADIUS) (* 2 _ROOM-RADIUS) (* 2 _ROOM-RADIUS))
                    ;; desenează arcele
                    (for-each
                     (λ (arg)
                       (let* ([startx (if (= (cdr arg) 0) 
                                          (+ x _EXT-ROOM-RADIUS _ROOM-RADIUS (* _ROOM-RADIUS (sgn (car arg))))
                                          (+ x _EXT-ROOM-RADIUS _ROOM-RADIUS 
                                             (* (/ _ROOM-RADIUS (sqrt 2)) (sgn (car arg)))))]
                              [starty (if (= (car arg) 0) 
                                          (+ y _EXT-ROOM-RADIUS _ROOM-RADIUS (* _ROOM-RADIUS (sgn (cdr arg))))
                                          (+ y _EXT-ROOM-RADIUS _ROOM-RADIUS
                                             (* (/ _ROOM-RADIUS (sqrt 2)) (sgn (cdr arg)))))]
                              [endx (if (= (cdr arg) 0) 
                                        (+ startx (* (sgn (car arg)) (* .5 _ROOM-RADIUS)))
                                        (+ startx (* (sgn (car arg)) (* 2 _ROOM-RADIUS (- 1.25 (/ 1(sqrt 2)))))))]
                              [endy (if (= (car arg) 0) 
                                        (+ starty (* (sgn (cdr arg)) (* .5 _ROOM-RADIUS)))
                                        (+ starty (* (sgn (cdr arg)) (* 2 _ROOM-RADIUS (- 1.25 (/ 1(sqrt 2)))))))]
                              [arrowx1 (cond [(= (cdr arg) 0) (- endx (* 10 (sgn (car arg))))]
                                             [(= (car arg) 0) (- endx (* 10 (sgn (cdr arg))))]
                                             [#t (- endx (* 14.1 (sgn (car arg))))])]
                              [arrowx2 (cond [(= (cdr arg) 0) (- endx (* 10 (sgn (car arg))))]
                                             [(= (car arg) 0) (+ endx (* 10 (sgn (cdr arg))))]
                                             [#t endx])]
                              [arrowy1 (cond [(= (car arg) 0) (- endy (* 10 (sgn (cdr arg))))]
                                             [(= (cdr arg) 0) (+ endy (* 10 (sgn (car arg))))]
                                             [#t endy])]
                              [arrowy2 (cond [(= (car arg) 0) (- endy (* 10 (sgn (cdr arg))))]
                                             [(= (cdr arg) 0) (- endy (* 10 (sgn (car arg))))]
                                             [#t (- endy (* 14.1 (sgn (cdr arg))))])])
                         (send dc set-pen _ROOM-CONTOUR-COLOR 2 'solid)
                         (send dc draw-line startx starty endx endy)
                         (send dc draw-line endx endy arrowx1 arrowy1)
                         (send dc draw-line endx endy arrowx2 arrowy2)
                         ;; agenții
                         (when (equal? (car (_value-of 'location red-state)) (car room))
                           (send dc set-pen _AGENT-CONTOUR-COLOR 1 'solid)
                           (send dc set-brush _RED-AGENT-COLOR 'solid)
                           (send dc draw-rectangle (+ x _ROOM-RADIUS 1) (+ y _ROOM-RADIUS 1) 
                                 (- (/ _ROOM-RADIUS 4) 2) (- (/ _ROOM-RADIUS 2) 2))
                           (let ([s (_loaded-spheres red-state)])
                             (when (> (length s) 0)
                               (send dc set-brush (_value-of (first s) _COLOR-DICT) 'solid)
                               (send dc set-pen _ROOM-CONTOUR-COLOR 1 'solid)
                               (send dc draw-ellipse (+ x _ROOM-RADIUS 3) (+ y _ROOM-RADIUS 3)
                                     _Q-RADIUS _Q-RADIUS)
                               )
                             (when (> (length s) 1)
                               (send dc set-brush (_value-of (second s) _COLOR-DICT) 'solid)
                               (send dc set-pen _ROOM-CONTOUR-COLOR 1 'solid)
                               (send dc draw-ellipse
                                     (+ x _ROOM-RADIUS 3) (+ y (* _ROOM-RADIUS 1.25) 3) _Q-RADIUS _Q-RADIUS)
                               )
                             )
                           )
                         (when (equal? (car (_value-of 'location blue-state)) (car room))
                           (send dc set-pen _AGENT-CONTOUR-COLOR 1 'solid)
                           (send dc set-brush _BLUE-AGENT-COLOR 'solid)
                           (send dc draw-rectangle (+ x (* 1.25 _ROOM-RADIUS) 1) (+ y _ROOM-RADIUS 1) 
                                 (- (/ _ROOM-RADIUS 4) 2) (- (/ _ROOM-RADIUS 2) 2))
                           (let ([s (_loaded-spheres blue-state)])
                             (when (> (length s) 0)
                               (send dc set-brush (_value-of (first s) _COLOR-DICT) 'solid)
                               (send dc set-pen _ROOM-CONTOUR-COLOR 1 'solid)
                               (send dc draw-ellipse 
                                     (+ x (* _ROOM-RADIUS 1.25) 3) (+ y _ROOM-RADIUS 3) _Q-RADIUS _Q-RADIUS))
                             (when (> (length s) 1)
                               (send dc set-brush (_value-of (second s) _COLOR-DICT) 'solid)
                               (send dc set-pen _ROOM-CONTOUR-COLOR 1 'solid)
                               (send dc draw-ellipse 
                                     (+ x (* _ROOM-RADIUS 1.25) 3) (+ y (* _ROOM-RADIUS 1.25) 3) _Q-RADIUS _Q-RADIUS))))
                         ;; bilele
                         (for-each
                          (λ (color)
                            (let* ((n (_in-room-spheres-color (car color) (car room) world-state))
                                   (spheres-pos (take _in-room-positions n)))
                              (for-each
                               (λ (s)
                                 (send dc set-brush (_value-of (car color) _COLOR-DICT) 'solid)
                                 (send dc set-pen "black" 0.5 'solid)
                                 (send dc draw-ellipse
                                       (+ x (* _ROOM-RADIUS 1.25) 3
                                          (* (cadr color) (car s) (/ _ROOM-RADIUS 4))
                                          (if (> (cadr color) 0) (/ _ROOM-RADIUS -4) 0))
                                       (+ y (* _ROOM-RADIUS 1.25) 3
                                          (* (cddr color) (cdr s) (/ _ROOM-RADIUS 4))
                                          (if (> (cddr color) 0) (/ _ROOM-RADIUS -4) 0))
                                       _Q-RADIUS _Q-RADIUS))
                               spheres-pos)))
                          '((red . (-1 . -1)) (blue . (1 . -1)) (gray . (-1 . 1)))
                          )
                         )
                       )
                     ncoord)))
                positions))])
        (send canvas refresh-now redraw-proc)))))
        


;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------
;;; SECȚIUNEA 13 : RULARE
;;;
;;; Funcții pentru lansarea și rularea scenariilor

;;; Rularea unui scenariu

(define _play
  (λ (color world-state m statistics positions canvas
            ag1 ag1-state ag1-plan ag1-goal ag1-info ag1-level
            ag2 ag2-state ag2-plan ag2-goal ag2-info ag2-level)
    (when _pause-before-next-round? (read-char) #t)
    (when (and (not _display-gui?) (> _SLEEP-TIME-BEFORE-NEXT-ROUND 0)) (sleep _SLEEP-TIME-BEFORE-NEXT-ROUND))
    (when _print-round-separator? (_print-round-separator color))
    (when _print-score? (_print-score world-state))
    (when _print-world-state? (_print-world-state world-state))
    (when _print-agent-state? 
      (if (equal? (first (_value-of 'color ag1-state)) 'red)
          (begin (_print-agent-state ag1-state) (_print-agent-state ag2-state))
          (begin (_print-agent-state ag2-state) (_print-agent-state ag1-state))))
    (cond 
      ;; A câștigat vreun agent? Atenție: poate câștiga și agentul roșu ca urmare a unei acțiuni a
      ;; agentului albastru și invers (serendipity).
      [(_winner? world-state m) (_end-game world-state statistics)]
      ;; Trebuie replanificare (atins obiectiv parțial, buleală la tura trecută sau plan vid)
      [(or (> (fourth (_delivered color world-state)) (fourth ag1-goal))
           (empty? ag1-plan)
           (equal? (car ag1-plan) 'PAUSE))
       (let*-values 
           ([(ag1-new-goal) (if (= ag1-level 1) (_one-sphere-goal color world-state) ag1-goal)]
            [(planning-result cpu-time a b) 
             (time-apply ag1 `(,ag1-new-goal 
                               ,(append world-state ag1-state) 
                               ,(if (= ag1-level 1) null (if (empty? ag1-plan) null (cdr ag1-plan)))
                               ,(if (= ag1-level 1) #f ag1-info)))]
            [(ag1-new-plan) (car (car planning-result))]
            [(ag1-new-info) (cdr (car planning-result))])
         (when _print-new-plan? (_print-new-plan ag1-new-plan))
         (_play (_next-agent color) world-state m
                (_add-to-statistics "Timp total planificare" color cpu-time
                                    (_increment "Replanificări" color statistics))
                positions canvas
                ag2 ag2-state ag2-plan ag2-goal ag2-info ag2-level
                ag1 ag1-state ag1-new-plan ag1-new-goal ag1-new-info ag1-level))]
      ;; Se aplică un operator greșit
      [(let* ([check-result (_operator-check (first ag1-plan) ag1-state world-state)])
         (if (car check-result) #f
             (begin (when _print-action-error? (_print-action-error (first ag1-plan) (cdr check-result))) #t)))
       (_play (_next-agent color) world-state m
              (_increment "Erori de aplicare" color statistics)
              positions canvas
              ag2 ag2-state ag2-plan ag2-goal ag2-info ag2-level
              ag1 ag1-state (cons 'PAUSE ag1-plan) ag1-goal ag1-info ag1-level)]
      ;; Operator Test
      [(equal? (caar ag1-plan) 'Test)
       (let ([result (_check-conditions (cdar ag1-plan) world-state)])
         (when _print-condition? (_print-condition (cdar ag1-plan) result))
         (if result
             (_play (_next-agent color) world-state m
                    (_increment "Teste trecute" color statistics) positions canvas
                    ag2 ag2-state ag2-plan ag2-goal ag2-info ag2-level
                    ag1 ag1-state (cdr ag1-plan) ag1-goal ag1-info ag1-level)
             (_play (_next-agent color) world-state m
                    (_increment "Teste eșuate" color statistics) positions canvas
                    ag2 ag2-state ag2-plan ag2-goal ag2-info ag2-level
                    ag1 ag1-state (cons 'PAUSE ag1-plan) ag1-goal ag1-info ag1-level)))]
      ;; Aplică acțiunea următoare
      [#t 
       (let ([new-state (_apply-action (car ag1-plan) world-state ag1-state)])
         (when _print-action? (_print-action (car ag1-plan)))
         (when _display-gui?
           (_redraw canvas positions (car new-state)
                    (if (equal? color 'red) (cdr new-state) ag2-state)
                    (if (equal? color 'red) ag2-state (cdr new-state)))
           (sleep/yield (max _SLEEP-TIME-BEFORE-NEXT-ROUND 0.02)))
         (_play (_next-agent color) (car new-state) m
                (_increment "Acțiuni" color statistics) positions canvas
                ag2 ag2-state ag2-plan ag2-goal ag2-info ag2-level
                ag1 (cdr new-state) (cdr ag1-plan) ag1-goal ag1-info ag1-level))]
      )))


;;; Lansarea unui scenariu
(define run
  (λ (scenario red-agent blue-agent (advanced? #f))
    (let* ([initial-world-state (cdr scenario)]
           [red-state (_initial-agent-state 'red initial-world-state)]
           [blue-state (_initial-agent-state 'blue initial-world-state)]
           [red-goal (if advanced? (_all-spheres-goal 'red scenario)
                         (_one-sphere-goal 'red initial-world-state))]
           [red-level (if advanced? 2 1)]
           [blue-goal (_one-sphere-goal 'blue initial-world-state)]
           ;; ----
           [statistics _initial-statistics]
           ;; ----
           [frame (new frame% [label "Planificare"])]
           [canvas (new canvas% [parent frame])]
           [positions (_get-matrix initial-world-state)]
           [minx (apply min (map cadr positions))]
           [miny (apply min (map cddr positions))]
           [tr-pos (map (λ (p) `(,(car p) . (,(- (cadr p) minx) . ,(- (cddr p) miny)))) positions)]
           [f-width (+ 1 (apply max (map cadr tr-pos)))]
           [f-height (+ 1 (apply max (map cddr tr-pos)))]
           [m (car scenario)])
      (when _display-gui? 
        (begin
          (send frame resize
                (inexact->exact (* 2.5 _ROOM-RADIUS f-width)) (inexact->exact (* 2.5 _ROOM-RADIUS f-height)))
          (_redraw canvas tr-pos initial-world-state red-state blue-state)
          (send frame show #t)))
      (_play 'red initial-world-state m statistics tr-pos canvas
             red-agent red-state null red-goal null red-level
             blue-agent blue-state null blue-goal null 1)
      )))
;;; --------------------------------------------------------------------------------------------------------------------
;;; --------------------------------------------------------------------------------------------------------------------

(run scenario3 dummy-agent dummy-agent)
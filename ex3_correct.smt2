; COUNTER
(declare-var COUNTER.init Int)
(declare-var COUNTER.incr Int)
(declare-var COUNTER.X Bool)
(declare-var COUNTER.reset Bool)
(declare-var COUNTER.C Int)
(declare-var COUNTER.COUNTER.__COUNTER_1_c Int)
(declare-var COUNTER.COUNTER.__COUNTER_1_x Int)
(declare-var COUNTER.PC Int)
(declare-rel COUNTER_init (Int Int Bool Bool Int Int))
(declare-rel COUNTER_step (Int Int Bool Bool Int Int Int))

(rule (=> 
  (and (= COUNTER.PC COUNTER.init)
       (= COUNTER.C (ite COUNTER.reset COUNTER.init
                       (ite COUNTER.X (+ COUNTER.PC COUNTER.incr) COUNTER.PC)))
       (= COUNTER.COUNTER.__COUNTER_1_x COUNTER.C)
  )
  (COUNTER_init COUNTER.init COUNTER.incr COUNTER.X COUNTER.reset COUNTER.C COUNTER.COUNTER.__COUNTER_1_x)
))

(rule (=> 
  (and (= COUNTER.PC COUNTER.COUNTER.__COUNTER_1_c)
       (= COUNTER.C (ite COUNTER.reset COUNTER.init
                       (ite COUNTER.X (+ COUNTER.PC COUNTER.incr) COUNTER.PC)))
       (= COUNTER.COUNTER.__COUNTER_1_x COUNTER.C)
  )
  (COUNTER_step COUNTER.init COUNTER.incr COUNTER.X COUNTER.reset COUNTER.C COUNTER.COUNTER.__COUNTER_1_c COUNTER.COUNTER.__COUNTER_1_x)
))

; speed
(declare-var speed.beacon Bool)
(declare-var speed.second Bool)
(declare-var speed.late Bool)
(declare-var speed.early Bool)
(declare-var speed.speed.__speed_3_c Bool)
(declare-var speed.speed.__speed_6_c Bool)
(declare-var speed.ni_4.COUNTER.__COUNTER_1_c Int)
(declare-var speed.speed.__speed_3_x Bool)
(declare-var speed.speed.__speed_6_x Bool)
(declare-var speed.ni_4.COUNTER.__COUNTER_1_x Int)
(declare-var speed.__speed_1 Bool)
(declare-var speed.__speed_2 Bool)
(declare-var speed.__speed_4 Bool)
(declare-var speed.__speed_5 Bool)
(declare-var speed.__speed_7 Bool)
(declare-var speed.__speed_8 Bool)
(declare-var speed.diff Int)
(declare-var speed.incr Int)
(declare-rel speed_init (Bool Bool Bool Bool Bool Bool Int))
(declare-rel speed_step (Bool Bool Bool Bool Bool Bool Int Bool Bool Int))

(rule (=> 
  (and (= speed.__speed_2 (and speed.second (not speed.beacon)))
       (= speed.__speed_1 (and speed.beacon (not speed.second)))
       (= speed.incr (ite speed.__speed_1 1 (ite speed.__speed_2 2 0)))
       (COUNTER_init 0 speed.incr (or speed.beacon speed.second) false speed.diff speed.ni_4.COUNTER.__COUNTER_1_x)
       (= speed.__speed_7 speed.speed.__speed_6_c)
       (= speed.__speed_8 (ite speed.__speed_7 (< speed.diff 0)
                             (<= speed.diff (- 10))))
       (= speed.late false)
       (= speed.__speed_4 speed.speed.__speed_3_c)
       (= speed.__speed_5 (ite speed.__speed_4 (> speed.diff 0)
                             (>= speed.diff 10)))
       (= speed.early false)
       (= speed.speed.__speed_6_x speed.late)
       (= speed.speed.__speed_3_x speed.early)
  )
  (speed_init speed.beacon speed.second speed.late speed.early speed.speed.__speed_3_x speed.speed.__speed_6_x speed.ni_4.COUNTER.__COUNTER_1_x)
))

(rule (=> 
  (and (= speed.__speed_2 (and speed.second (not speed.beacon)))
       (= speed.__speed_1 (and speed.beacon (not speed.second)))
       (= speed.incr (ite speed.__speed_1 1 (ite speed.__speed_2 2 0)))
       (COUNTER_step 0 speed.incr (or speed.beacon speed.second) false speed.diff speed.ni_4.COUNTER.__COUNTER_1_c speed.ni_4.COUNTER.__COUNTER_1_x)
       (= speed.__speed_7 speed.speed.__speed_6_c)
       (= speed.__speed_8 (ite speed.__speed_7 (< speed.diff 0)
                             (<= speed.diff (- 10))))
       (= speed.late speed.__speed_8)
       (= speed.__speed_4 speed.speed.__speed_3_c)
       (= speed.__speed_5 (ite speed.__speed_4 (> speed.diff 0)
                             (>= speed.diff 10)))
       (= speed.early speed.__speed_5)
       (= speed.speed.__speed_6_x speed.late)
       (= speed.speed.__speed_3_x speed.early)
  )
  (speed_step speed.beacon speed.second speed.late speed.early speed.speed.__speed_3_c speed.speed.__speed_6_c speed.ni_4.COUNTER.__COUNTER_1_c speed.speed.__speed_3_x speed.speed.__speed_6_x speed.ni_4.COUNTER.__COUNTER_1_x)
))

; top
(declare-var top.beacon Bool)
(declare-var top.second Bool)
(declare-var top.OK Bool)
(declare-var top.top.__top_1_c Bool)
(declare-var top.ni_1.speed.__speed_3_c Bool)
(declare-var top.ni_1.speed.__speed_6_c Bool)
(declare-var top.ni_1.ni_4.COUNTER.__COUNTER_1_c Int)
(declare-var top.top.__top_1_x Bool)
(declare-var top.ni_1.speed.__speed_3_x Bool)
(declare-var top.ni_1.speed.__speed_6_x Bool)
(declare-var top.ni_1.ni_4.COUNTER.__COUNTER_1_x Int)
(declare-var top.early Bool)
(declare-var top.late Bool)
(declare-rel top_init (Bool Bool Bool Bool Bool Bool Int))
(declare-rel top_step (Bool Bool Bool Bool Bool Bool Int Bool Bool Bool Int))

(rule (=> 
  (and (speed_init top.beacon top.second top.late top.early top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
       (= top.OK true)
       (= top.top.__top_1_x top.early)
  )
  (top_init top.beacon top.second top.OK top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
))

(rule (=> 
  (and (speed_step top.beacon top.second top.late top.early top.ni_1.speed.__speed_3_c top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
       (= top.OK (or (not top.top.__top_1_c) (not top.late)))
       (= top.top.__top_1_x top.early)
  )
  (top_step top.beacon top.second top.OK top.top.__top_1_c top.ni_1.speed.__speed_3_c top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
))

; Collecting semantics with main node top

(declare-rel MAIN (Bool Bool Bool Int Bool))
; Initial set
(declare-rel INIT_STATE ())
(rule INIT_STATE)
(rule (=> 
  (and INIT_STATE
       (top_init top.beacon top.second top.OK top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
  )
  (MAIN top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x top.OK)
))

; Inductive def
(declare-var dummy Bool)
(rule (=> 
  (and (MAIN top.top.__top_1_c top.ni_1.speed.__speed_3_c top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c dummy)
       (top_step top.beacon top.second top.OK top.top.__top_1_c top.ni_1.speed.__speed_3_c top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
  )
  (MAIN top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x top.OK)
))

; Property def
(declare-rel ERR ())
(rule (=> 
  (and (not (= top.OK true))
       (MAIN top.top.__top_1_c top.ni_1.speed.__speed_3_c
       top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c top.OK))
  ERR))
(query ERR)

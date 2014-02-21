; COUNTER
(declare_var COUNTER.init Int)
(declare_var COUNTER.incr Int)
(declare_var COUNTER.X Bool)
(declare_var COUNTER.reset Bool)
(declare_var COUNTER.C Int)
(declare_var COUNTER.COUNTER.__COUNTER_1_c Int)
(declare_var COUNTER.COUNTER.__COUNTER_1_x Int)
(declare_var COUNTER.PC Int)
(declare-rel COUNTER_reset (Int Int Bool Bool Int Int))
(declare-rel COUNTER_step (Int Int Bool Bool Int Int Int))

(rule (=> 
  (and (= COUNTER.PC COUNTER.init)
       (= COUNTER.C (ite COUNTER.reset COUNTER.init
                       (ite COUNTER.X (+ COUNTER.PC COUNTER.incr) COUNTER.PC)))
       (= COUNTER.__COUNTER_1_x COUNTER.C)
  )
  (COUNTER_init COUNTER.init COUNTER.incr COUNTER.X COUNTER.reset COUNTER.C COUNTER.COUNTER.__COUNTER_1_x)
))

(rule (=> 
  (and (= COUNTER.PC COUNTER.__COUNTER_1_c)
       (= COUNTER.C (ite COUNTER.reset COUNTER.init
                       (ite COUNTER.X (+ COUNTER.PC COUNTER.incr) COUNTER.PC)))
       (= COUNTER.__COUNTER_1_x COUNTER.C)
  )
  (COUNTER_step COUNTER.init COUNTER.incr COUNTER.X COUNTER.reset COUNTER.C COUNTER.COUNTER.__COUNTER_1_c COUNTER.COUNTER.__COUNTER_1_x)
))

; speed
(declare_var speed.beacon Bool)
(declare_var speed.second Bool)
(declare_var speed.late Bool)
(declare_var speed.early Bool)
(declare_var speed.speed.__speed_3_c Bool)
(declare_var speed.speed.__speed_6_c Bool)
(declare_var speed.ni_4.COUNTER.__COUNTER_1_c Int)
(declare_var speed.speed.__speed_3_x Bool)
(declare_var speed.speed.__speed_6_x Bool)
(declare_var speed.ni_4.COUNTER.__COUNTER_1_x Int)
(declare_var speed.__speed_1 Bool)
(declare_var speed.__speed_2 Bool)
(declare_var speed.__speed_4 Bool)
(declare_var speed.__speed_5 Bool)
(declare_var speed.__speed_7 Bool)
(declare_var speed.__speed_8 Bool)
(declare_var speed.diff Int)
(declare_var speed.incr Int)
(declare-rel speed_reset (Bool Bool Bool Bool Bool Bool Int))
(declare-rel speed_step (Bool Bool Bool Bool Bool Bool Int Bool Bool Int))

(rule (=> 
  (and (= speed.__speed_2 (and speed.second (not speed.beacon)))
       (= speed.__speed_1 (and speed.beacon (not speed.second)))
       (= speed.incr (ite speed.__speed_1 1 (ite speed.__speed_2 2 0)))
       (COUNTER_init 0 speed.incr ((or speed.beacon speed.second)) 0 speed.diff speed.ni_4.COUNTER.__COUNTER_1_x)
       (= speed.__speed_7 speed.__speed_6_c)
       (= speed.__speed_8 (ite speed.__speed_7 (< speed.diff 0)
                             (<= speed.diff (- 10))))
       (= speed.late 0)
       (= speed.__speed_4 speed.__speed_3_c)
       (= speed.__speed_5 (ite speed.__speed_4 (> speed.diff 0)
                             (>= speed.diff 10)))
       (= speed.early 0)
       (= speed.__speed_6_x speed.late)
       (= speed.__speed_3_x speed.early)
  )
  (speed_init speed.beacon speed.second speed.late speed.early speed.speed.__speed_3_x speed.speed.__speed_6_x speed.ni_4.COUNTER.__COUNTER_1_x)
))

(rule (=> 
  (and (= speed.__speed_2 (and speed.second (not speed.beacon)))
       (= speed.__speed_1 (and speed.beacon (not speed.second)))
       (= speed.incr (ite speed.__speed_1 1 (ite speed.__speed_2 2 0)))
       (COUNTER_step 0 speed.incr ((or speed.beacon speed.second)) 0 speed.diff speed.ni_4.COUNTER.__COUNTER_1_c speed.ni_4.COUNTER.__COUNTER_1_x)
       (= speed.__speed_7 speed.__speed_6_c)
       (= speed.__speed_8 (ite speed.__speed_7 (< speed.diff 0)
                             (<= speed.diff (- 10))))
       (= speed.late speed.__speed_8)
       (= speed.__speed_4 speed.__speed_3_c)
       (= speed.__speed_5 (ite speed.__speed_4 (> speed.diff 0)
                             (>= speed.diff 10)))
       (= speed.early speed.__speed_5)
       (= speed.__speed_6_x speed.late)
       (= speed.__speed_3_x speed.early)
  )
  (speed_step speed.beacon speed.second speed.late speed.early speed.speed.__speed_3_c speed.speed.__speed_6_c speed.ni_4.COUNTER.__COUNTER_1_c speed.speed.__speed_3_x speed.speed.__speed_6_x speed.ni_4.COUNTER.__COUNTER_1_x)
))

; top
(declare_var top.beacon Bool)
(declare_var top.second Bool)
(declare_var top.OK Bool)
(declare_var top.top.__top_1_c Bool)
(declare_var top.ni_1.speed.__speed_3_c Bool)
(declare_var top.ni_1.speed.__speed_6_c Bool)
(declare_var top.ni_1.ni_4.COUNTER.__COUNTER_1_c Int)
(declare_var top.top.__top_1_x Bool)
(declare_var top.ni_1.speed.__speed_3_x Bool)
(declare_var top.ni_1.speed.__speed_6_x Bool)
(declare_var top.ni_1.ni_4.COUNTER.__COUNTER_1_x Int)
(declare_var top.early Bool)
(declare_var top.late Bool)
(declare-rel top_reset (Bool Bool Bool Bool Bool Bool Int))
(declare-rel top_step (Bool Bool Bool Bool Bool Bool Int Bool Bool Bool Int))

(rule (=> 
  (and (speed_init top.beacon top.second top.late top.early top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
       (= top.OK 1)
       (= top.__top_1_x top.early)
  )
  (top_init top.beacon top.second top.OK top.top.__top_1_x top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
))

(rule (=> 
  (and (speed_step top.beacon top.second top.late top.early top.ni_1.speed.__speed_3_c top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c top.ni_1.speed.__speed_3_x top.ni_1.speed.__speed_6_x top.ni_1.ni_4.COUNTER.__COUNTER_1_x)
       (= top.OK (or (not top.__top_1_c) (not top.late)))
       (= top.__top_1_x top.early)
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
       (MAIN top.top.__top_1_c top.ni_1.speed.__speed_3_c top.ni_1.speed.__speed_6_c top.ni_1.ni_4.COUNTER.__COUNTER_1_c))
  ERR))
(query ERR)

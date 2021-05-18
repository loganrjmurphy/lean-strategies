import justification common_meta property_catalogue.LTL
import pump1 
open interactive S A

variable {α : Type}

-- def foobar : Strategy (path pump1) := property.strategy $
-- property.input.mk
-- (Claim.mk
-- {x : path pump1 | true}
-- (λ p, p ⊨ (absent.after_until (coe BolusRequest) (formula.act_predicate (λ a : pump1.Act, a ∈ [Cond_6_3_, x1____MAX_WAIT_INPUT_T])) (coe Infusion_NormalOperation)))
-- )
-- ([
-- (λ p, p ⊨  (exist.after (formula.state_predicate (λ s : pump1.S, s ∈ [Alrm_EmptyReservoir, Alrm_LongWait_ChangeDoseRate])) (formula.act_predicate (λ a : pump1.Act, a ∈ [Cond_6_3_, x1____MAX_WAIT_INPUT_T])))),
-- (λ  p, p ⊨ (absent.between (coe BolusRequest) (coe Cond_6_3_) (coe Alrm_EmptyReservoir))),
-- (λ  p , p ⊨ (absent.between (coe BolusRequest) (coe x1____MAX_WAIT_INPUT_T) (coe Alrm_LongWait_ChangeDoseRate))),
-- (λ  p, p ⊨ (absent.after_until (coe BolusRequest) (formula.state_predicate (fun s : pump1.S, s ∈ [Alrm_EmptyReservoir, Alrm_LongWait_ChangeDoseRate])) (coe Infusion_NormalOperation)) )
-- ])

--- theorem foobarll {M : LTS} (B E A N : formula M) (x : path M ) :
-- (x ⊨ ◾(E ⇒ formula.next A))∧ 
-- (x ⊨ ◾(A ⇒ ((!B) U N))) → 
-- (x ⊨ ◾(E ⇒ ((!B) U N))) := 
-- begin 
--   rintros ⟨H1, H2⟩,
--   intro i, 
--   replace H1 := H1 i,
--   have EM : (x.drop i⊨(E ⇒  (!B U N))) ∨ ¬ (x.drop i⊨(E ⇒  (!B U N))), from or_not,
--   cases EM,
--   assumption,
--   rw sat at EM,
--   simp at EM,
--   cases EM with left right,
--   rw sat at right,
--   simp at right,
--   replace H1 := H1 left,
--   replace H2 := H2 (i+1),
--   rw sat at H1,
--   rw path.drop_drop at H1,
--   replace H2 := H2 H1,
--   cases H2 with w Hw,
--   rw path.drop_drop at Hw,
--   cases w,
--   simp at Hw,
--   replace right := right (1),
--   rw path.drop_drop at right,
--   replace right := right Hw,
--   cases right with w Hww,
--   have : w = 0, from sorry, -- positive
--   rw this at Hww,
--   simp at *,
--   rw sat at Hww,simp at Hww, sorry, --positive,
--   cases Hw with L R,
--   replace right := right (w),
--   have cheat : ((x.drop i).drop (w)⊨N), by sorry, --positive, 
--   replace right := right cheat,
--   cases right with z Hw,
--   cases Hw with one two,
--   cases z,
--   rw sat at two,
--   simp at two,
--   sorry, -- contradiction,
--   rw sat at two,
--   simp at two,
--   replace R := R z _,
--   sorry,
--   sorry,
-- end 



-- theorem fooBar (p : path pump1) : 
-- (sat (responds.globally  (coe Cond_6_3_) (coe Alrm_EmptyReservoir) ) p) → 
-- (sat (absent.between (coe BolusRequest) (coe Cond_6_3_) (coe Alrm_EmptyReservoir)) p) → 
-- (sat (absent.after_until (coe BolusRequest) (coe Alrm_EmptyReservoir) (coe Infusion_NormalOperation)) p)→ (sat (absent.after_until (coe BolusRequest) (coe Cond_6_3_) (coe Infusion_NormalOperation)) p) := 
-- begin intros H1 H2 H3,
-- intro i,
-- replace H1 := H1 i,
-- intro Hcond, cases Hcond with L R,
-- replace H1 := H1 L,
-- rw absent.between at H2,
-- have : ((p.drop i) ⊨ (↑Cond_6_3_ &  ◆↑Alrm_EmptyReservoir)), by {rw sat, split,assumption,assumption},
-- replace H2 := H2 (i) this,
-- cases H1 with w Hw,
-- cases R with k Hk,
-- clear this,
-- cases H2 with z Hz,
-- cases Hz with z1 z2,
-- have : k < z ∨ ¬ (k < z), from or_not,
-- cases this, 
-- use k,
-- split, assumption,
-- intros j Hj,
-- have fact : j < z, by omega,
-- replace z2 := z2 j fact, assumption,
-- simp at this,
-- have EM : z = k ∨ z < k, by omega,
-- clear this,
-- cases EM, use k,
-- split, assumption, rw ← EM, assumption,
-- replace H3 := H3 (i+z),
-- rw ← path.drop_drop at H3,
-- have help : (((p.drop i).drop z) ⊨ ◆(↑Infusion_NormalOperation)), by {use (k-z),
-- rw path.drop_drop, rw path.drop_drop,have : i + (z + (k - z)) = i+k, by omega, rw this, rw ← path.drop_drop, assumption,},
-- have : ( ((p.drop i).drop z) ⊨  (↑Alrm_EmptyReservoir &  ◆↑Infusion_NormalOperation)), by {rw sat, split, assumption, assumption,},
-- clear help, replace H3 := H3 this,
-- cases H3 with t Ht,
-- clear this,
-- cases Ht with Ht Ht',
-- rw path.drop_drop at *,
-- use (z+t),split,
-- assumption,
-- intros j Hj,
-- have : j < z ∨ ¬ (j < z), from or_not,
-- cases this, replace z2 := z2 j this,
-- assumption,
-- simp at this,
-- have EM' : z = j ∨ z < j, by omega,
-- cases EM', rw EM' at Hj,
-- replace Ht' := Ht' 0 _,
-- rw path.drop_drop at Ht',
-- rw← EM',
-- simp at Ht', rw path.drop_drop,assumption,
-- omega,
-- clear this,
-- replace Ht' := Ht' (j-z),
-- rw path.drop_drop at Ht',
-- have : (i + z + (j - z)) = (i + j), by omega,
-- rw this at Ht',
-- rw path.drop_drop,
--  apply Ht', omega,
-- end 


-- theorem foobar (p : path pump1) : 
-- ( p ⊨ ◾ ((coe BolusRequest) ⇒ 
-- (!(formula.next (coe Alrm_EmptyReservoir))))) → 
-- ( p ⊨ ◾ ((coe Alrm_EmptyReservoir) ⇒ (!(coe BolusRequest) U  (coe Infusion_NormalOperation))))→ 
-- ( p⊨ (◾ ((coe Cond_6_3_) ⇒ (!(coe BolusRequest) U  (coe Alrm_EmptyReservoir))))) → 
-- ( p ⊨ ◾((coe Cond_6_3_) ⇒ formula.next (coe Alrm_EmptyReservoir))) → 
-- (p ⊨ ◾ ((coe Cond_6_3_) ⇒ (!(coe BolusRequest) U  (coe Infusion_NormalOperation)))) := 
-- begin 
-- intros goobar H1 H2 H3,
-- have : (p ⊨ ◾ ((coe Cond_6_3_) ⇒ (!(coe BolusRequest) U  (coe Infusion_NormalOperation)))) ∨ ¬ (p ⊨ ◾ ((coe Cond_6_3_) ⇒ (!(coe BolusRequest) U  (coe Infusion_NormalOperation)))), from or_not,
-- cases this,
-- assumption,

-- repeat {rw sat at this},
-- simp at this,
-- -- ◆ ((Cond_6_3) ∧ (∀ i, i ⊨ NormalOp → ∃ j < i, j ⊨ Bolus)

-- intros i Hi,

-- -- time k : error condition 
-- -- time k+1 : alarm
-- -- time k+z : Bolus request
-- -- 

-- cases this with k Hk, replace H3 := H3 k Hk.1,
-- rw sat at H3,
-- replace H1 := H1 (k+1),
-- rw sat at H2,
-- rw ← path.drop_drop at H1,
-- replace H1 := H1 H3,
-- replace Hk := Hk.2,
-- cases H1 with q Hq,
-- cases Hq with L R,
-- replace Hk := Hk (q+1),
-- rw path.drop_drop at Hk,
-- rw path.drop_drop at L,
-- rw path.drop_drop at L,
-- have eqq: k + (q + 1) = k + (1 + q), by omega,
-- rw ← eqq at L,
-- replace Hk := Hk L,
-- cases Hk with z Hz,
-- cases Hz with LL RR,
-- cases z,
-- replace goobar := goobar k RR, rw sat at goobar, rw sat at goobar,simp at goobar, 
-- have foooo: k.succ = k+1, by {exact rfl},
-- rw path.drop_drop at H3,
-- rw ← foooo at H3, contradiction,
-- have : z.succ ≤ q, by omega,
-- rw ← path.drop_drop at L,
-- have EM: z.succ = q ∨ z.succ < q, by {exact eq_or_lt_of_le this,},
-- cases EM,
-- sorry,
-- have : z ≤ q, by omega,
-- have newEM : z = q ∨ z < q, by {exact eq_or_lt_of_le this,},
-- cases newEM, sorry,
-- replace R := R z,
-- replace R := R newEM,
-- rw path.drop_drop at RR,
-- rw path.drop_drop at R,
-- rw path.drop_drop at R,
-- have obvi : z+1 = z.succ, from rfl,
-- rw ← obvi at RR,
-- rw sat at R, rw @add_comm _ _ z 1 at RR, contradiction, 
-- -- intros i Hi,
-- -- replace H2 := H2 i Hi,



--  -- have : 


-- end 



-- ([
-- (fun p : path pump1, sat (exist.after (coe Alrm_EmptyReservoir) (coe Cond_6_3_)) p),
-- (fun p : path pump1, sat (absent.between (coe BolusRequest) (coe Cond_6_3_) (coe Alrm_EmptyReservoir)) p),
-- (fun p : path pump1, sat (absent.after_until (coe BolusRequest) (coe Alrm_EmptyReservoir) (coe Infusion_NormalOperation)) p)
-- ])


-- -- theorem dwarf (x : path M) : (x ⊨ (formula.state E₁ ⅋ formula.state E₂ &  ◆N)) → (x ⊨ (exist.after (formula.state A₁) (formula.state E₁)).conj (exist.after (formula.state A₂) (formula.state E₂))) → (x ⊨ ◆(formula.state A₁ ⅋ formula.state A₂ &  ◆N)) := begin 
-- --   intros H1 H2,
-- --   cases H1 with H1 H3,
-- --   cases H1 with H1 H4, cases H1,
-- --   cases H2 with H2 H5,
-- --   rw exist.after at H2,
-- --   cases H2,
-- --   replace H2 := H2 0,
-- --   unfold path.drop at H2, rw sat at H2, contradiction,
-- --   cases H2 with w Hw, cases Hw with Hw Hz,
-- --   cases Hz with p Hp, use w+p,
-- --   rw ← path.drop_drop, split,left, assumption, 

-- -- end 



-- theorem secondCases (x : path M) :  
-- (x⊨ ◆(formula.state E₁ &  ◆(B &  ◆N))) →  
-- (x ⊨(exist.after (formula.state A₁) (formula.state E₁))) → 
-- (x⊨ ◆((formula.state E₁) &  ◆(B &  ◆(formula.state A₁)))) ∨  (x⊨ ◆(formula.state A₁ &  ◆(B &  ◆N))) := 
-- begin 
-- intros H1 H2,
-- rw exist.after at H2,
-- cases H1 with w Hw,
-- cases Hw with H1 H3,
-- cases H2,
-- replace H2 := H2 w, contradiction,
-- cases H2 with k Hk,
-- cases Hk with Hk Hz,
-- cases Hz with z Hz,
-- cases H3 with b Hb,
-- cases Hb with Hb1 Hb2,
-- have : (k + z) < (w+b) ∨ ¬((k + z) < (w+b)), from or_not,
-- cases this,right,
-- use (k+z),
-- rw ← path.drop_drop,split,
-- assumption,
-- use (w+b) - ((k+z)),
-- have equal:  k + (z + (w + b - (k + z))) = w+b, by omega,
-- rw path.drop_drop, rw path.drop_drop,
-- rw equal, split, rw ← path.drop_drop, assumption, 
-- rw ← path.drop_drop, assumption,

-- left,
-- simp at this,
-- have equ : w < k ∨ ¬ (w < k), by omega,
-- cases equ,
-- use w, split, assumption,
-- use b, split, assumption,
-- use (k+z)-(w+b),
-- have E : (w + (b + (k + z - (w + b)))) = k+z, by omega,
-- rw path.drop_drop, rw path.drop_drop, rw E, rwa ← path.drop_drop,
-- simp at equ,
-- use k,split, assumption,
-- use (w-k+b), rw path.drop_drop,
-- have E :k + (w - k + b) = w+b, by omega, 
-- rw E, rw ← path.drop_drop,  split, assumption,
-- use (k+z - (w+b)),
-- have E': w + (b + (k + z - (w + b))) = k+z, by omega,
-- rw path.drop_drop,
-- rw path.drop_drop, rw E', rw ← path.drop_drop,
-- assumption,
-- end 


-- theorem foobar2 {HE : E₁ ≠ A₁} (x : path M) : 
-- (x ⊨ (absent.between B ((formula.state A₁).disj (formula.state A₂)) N)) → 
-- (x ⊨ (absent.between (B) (formula.state E₂) (formula.state A₂)))→
-- (x ⊨ (absent.between (B) (formula.state E₁) (formula.state A₁)))→ 
-- (x ⊨ (exist.after (formula.state A₁) (formula.state E₁)).conj (exist.after (formula.state A₂) (formula.state E₂))) → 
-- (x ⊨ ◾((formula.state E₁).disj (formula.state E₂) ⇒ ◾(B.impl ◾ !N))) := 
-- begin
--    intros H1 H2 H3 H4,
--    have LEM : (x ⊨ ◾((formula.state E₁).disj (formula.state E₂) ⇒ ◾(B.impl ◾ !N))) ∨ ¬(x ⊨ ◾((formula.state E₁).disj (formula.state E₂) ⇒ ◾(B.impl ◾ !N))), from or_not,
--    cases LEM, assumption,
--    repeat {rw sat at LEM},
--    simp at LEM, 
--    have := @helper M _ B E₁ E₂ N x,
--    have new : (∃ (x_1 : ℕ), ((x.drop x_1⊨(formula.state E₁)) ∨ (x.drop x_1⊨(formula.state E₂))) ∧ ∃ (a : ℕ), ((x.drop x_1).drop a⊨B) ∧ ∃ (x_2 : ℕ), (((x.drop x_1).drop a).drop x_2⊨N)) ↔ ((x ⊨ ◆(((formula.state E₁).disj (formula.state E₂)).conj ◆(B.conj (◆N))))), by 
--    {split, intro H,
--    cases H with w Hw, cases Hw with Hw Hk, cases Hk with k Hk, cases Hk with Hk Hz, cases Hz with z Hz,
--    use w, split, assumption, use k, split, assumption, use z,assumption,
--    intro H,cases H with w Hw, cases Hw with Hw Hk, cases Hk with k Hk, cases Hk with Hk Hz, cases Hz with z Hz,
--    use w, split, assumption, use k, split, assumption, use z,assumption,
--    },
--    replace new := new.mp LEM, clear LEM,
--    replace this :=  this new,
--    cases this, 
--      replace this := secondCases x this H4.1,
--      cases this,
--         clear H2,
--         rw absent.between at H3,
--         cases this with w Hw,
--         cases Hw with Hw Hw',
--         have THIS : ((x.drop w) ⊨ formula.state E₁) → ¬ ((x.drop w) ⊨ formula.state A₁), by { intros H H', rw sat at H, rw sat at H',rw H at H', exact HE H',},
--         replace THIS := THIS Hw,
--         replace H3 := H3 w,
--         have nnn : ((x.drop w) ⊨(formula.state E₁ &  !formula.state A₁ &  ◆formula.state A₁)), by {rw sat, split, split, assumption, rw sat, assumption,cases Hw' with z Hz, cases Hz with Hz1 Hz2, cases Hz2 with pp Hpp, use (z+pp),
--         rw ← path.drop_drop, assumption},
--         replace H3 := H3 nnn, clear nnn,
--         rw sat at H3,
--         cases H3 with j Hj,
--         cases Hj with Hj1 Hj2,
--         cases Hw' with k Hk, -- 
--         have thirdCases : k < j ∨ ¬(k < j),
--         by omega,
--         cases thirdCases,
--         replace Hj2 := Hj2 k thirdCases,
--         rw sat at Hj2,
--         cases Hk with Hk1 Hk2,
--         contradiction,
--         sorry, 
--         sorry,
--         sorry,

-- end 


    --  clear H2,
    --  cases this with w Hw,
    --  cases Hw with Hw Hk,
    --  cases Hk with k Hk,
    --  cases Hk with Hk Hz, 
    --  cases Hz with z Hz,
    --  cases H4 with H4 H5,
    --  clear H5,



    --  rw exist.after at H4,
    --  cases H4,
    --  replace H4 := H4 w, rw sat at H4, contradiction,
    --  cases H4 with m Hm,
    --  rw absent.between at H3,
    --  replace H3 := H3 m,
    --  cases Hm with Hm Hm2,
    --  have : ((x.drop m) ⊨ formula.state E₁) → ¬ ((x.drop m) ⊨ formula.state A₁), by { intros H H', rw sat at H, rw sat at H',rw H at H', exact HE H',},
    --  replace this := this Hm,
    --  have This : ((x.drop m) ⊨ !(formula.state A₁)), by {rwa sat,},
    --  clear this,
    --  have ThisS : ((x.drop m) ⊨ ((formula.state E₁) & !(formula.state A₁) & ◆ (formula.state A₁))), by {rw sat, rw sat, split, split,assumption,assumption, assumption,},
    --  rw sat at H3,replace H3 := H3 ThisS,
    --  rw sat at H3,
    --  cases H3 with p Hp,







-- theorem may13 ( p : path pump1) : 
-- (p ⊨  (exist.after (formula.state_predicate (λ s : pump1.S, s ∈ [Alrm_EmptyReservoir, Alrm_LongWait_ChangeDoseRate])) (formula.act_predicate (λ a : pump1.Act, a ∈ [Cond_6_3_, x1____MAX_WAIT_INPUT_T])))) ∧   
-- (p ⊨ (absent.between (coe BolusRequest) (coe Cond_6_3_) (coe Alrm_EmptyReservoir))) ∧ 
-- (p ⊨ (absent.between (coe BolusRequest) (coe x1____MAX_WAIT_INPUT_T) (coe Alrm_LongWait_ChangeDoseRate))) ∧  
-- (p ⊨ (absent.after_until (coe BolusRequest) (formula.state_predicate (fun s : pump1.S, s ∈ [Alrm_EmptyReservoir, Alrm_LongWait_ChangeDoseRate])) (coe Infusion_NormalOperation))) → 
-- (p ⊨ (absent.after_until (coe BolusRequest) (formula.act_predicate (λ a : pump1.Act, a ∈ [Cond_6_3_, x1____MAX_WAIT_INPUT_T])) (coe Infusion_NormalOperation))) :=
-- begin 
--   rintros ⟨H1, H2, H3, H4⟩, simp at *,
--   rw absent.after_until,
--   rw sat, 
--   rw absent.after_until at H4, rw absent.between at H2,
--   rw exist.after at H1,
--   intro i, rw sat,
--   rw sat,
--   intro H5,cases H5 with H5 H6, rw sat.weak_until,
--   rw absent.between at H3,
--   cases H1, 
--   have : false, by {replace H1 := H1 i, contradiction,},
--   contradiction,
--   have H7 : (p ⊨ (◆formula.state_predicate (λ (s : pump1.S), s = Alrm_EmptyReservoir ∨ s = Alrm_LongWait_ChangeDoseRate))), by 
--   { cases H1 with w Hw, cases Hw with Hw1 Hw2, cases Hw2 with k Hw,
--   use w+k, rw path.drop_drop at Hw, assumption,},
--   cases H7 with w Hw,
--   cases H1 with k Hk,
--   cases Hk with Hk1 Hk2,
--   cases Hk2 with z Hz,

--   replace H4 := H4 (k+z),
--   rw sat at H4, rw sat at H4,
--   simp at H4, rw path.drop_drop at Hz,
--   replace H4 := H4 Hz, 
--   rw sat at Hz,
--   rw sat at H4, unfold_coes at H4,
--   rw sat at H4, simp at H4,
--   cases Hz,
--   have : ¬(p.drop (k + z)).init = Infusion_NormalOperation, by {finish,},
--   replace H4 := H4 this, 
--   rw sat.weak_until at H4,
 
-- end  






-- -- Parent:
-- -- no Bolus after Errors until NormalOp
-- -- Children:
-- -- 1) Alarms after Errors
-- -- 2a) no Bolus between Error1 and Alarm1
-- -- 2b) no Bolus between Error2 and Alarm2
-- -- 2c) (there will be a third in the future, not yet)
-- -- 3) no Bolus after Alarms until NormalOp


-- theorem may13 (Alarms Bolus Errors Alarms Error1 Alarm1 Error2 Alarm2 NormalOp : formula pump1) (x : path pump1) : 
-- (x ⊨ absent.after_until Bolus Alarms NormalOp) →  
-- (x ⊨ absent.between Bolus Error2 Alarm2) →  
-- (x ⊨ (absent.between Bolus Error1 Alarm1)) → 
-- (x ⊨ (exist.after Alarms Errors)) → 
-- (x ⊨(absent.after_until Bolus Errors NormalOp)) := 

-- theorem helper (Bolus Reset Alarm : formula pump1) (x : path pump1) : 
-- (x ⊨ absent.after_until Bolus Alarm Reset) := 
-- begin
--   rw absent.after_until,
--   rw sat,
--   intro i, rw sat,
--   revert i,
  

-- end  

-- theorem helper (Bolus Error Alarm : formula pump1) (x : path pump1) : 
-- (x ⊨ ◾ (Error ⇒ Alarm)) → 
-- (x ⊨ absent.after Bolus Alarm) → 
-- (x ⊨ (absent.after Bolus Error)) := 
-- begin 
--   intros H1 H2,
--   rw absent.after at H2,
--   rw absent.after,
--   intro i,
--   replace H1 := H1 i,
--   intro H,
--   rw sat at H1,
--   replace H1 := H1 H,
--   replace H2 := H2 i,
--   rw sat at H2,
--   apply H2, assumption,
-- end 


-- theorem helper (P Q R : formula pump1) (x : path pump1) : (x ⊨ responds.globally Q R) → (x ⊨ absent.after P R) → (x ⊨ (absent.after P Q)) := 
-- begin 
--   intros H1 H2,
--   rw responds.globally at H1,
--   rw absent.after at H2,
--   rw absent.after,
--   rw sat,
--   intro i, replace H1 := H1 i,
--   rw sat at H1,
--   rw sat, 
--   intro Hq, replace H1 := H1 Hq,
--   cases H1 with w Hw,
--   replace H2 := H2 (i+w),
--   rw sat at H2, 

-- end 


-- theorem helper (P Q R : formula pump1) (x : path pump1) : (x ⊨ exist.after R Q) → (x ⊨ absent.after P R) → (x ⊨ (absent.after P Q)) := 
-- begin 
-- intros H1 H2,
-- rw absent.after,
-- rw exist.after at H1,
-- rw absent.after at H2,
-- rw sat at H2,
-- rw sat at H1,
-- cases H1,
-- rw sat, intro i,
-- rw sat, rw imp_iff_not_or,
-- left, replace H1 := H1 i,
-- rwa sat at H1,
-- rw sat,
-- intro i,
-- rw sat,
-- intro H3,
-- cases H1 with w Hw,
-- intro j,
-- rw sat at Hw,
-- cases Hw with Hw1 Hw2,
-- cases Hw2 with k Hk,
-- have LEM: (w + k < i + j) ∨ ¬ (w + k < i + j), from or_not,
-- cases LEM,
-- have H3 := H2 (w + k),
-- rw sat at H3,
-- rw ← path.drop_drop at H3,
-- replace H3 := H3 Hk,
-- rw sat at H3,
-- rw path.drop_drop at H3,
-- replace H3 := H3 (i + j - (w + k)),
-- rw path.drop_drop at H3,
-- have : w + k + (i + j - (w + k)) = i + j, by omega,
-- rw this at H3, rwa path.drop_drop,
-- simp at LEM, admit
-- end

-- theorem fff : deductive _ foobar := 
-- begin 
--   apply property.deductive_of_justfd _,
--   intro x, simp, intro H,
--   have H1 := H ⟨0, by dec_trivial⟩,
--   have H2 := H ⟨1, by dec_trivial⟩,
--   clear H,
--   dsimp at *,
-- end 



set_option pp.structure_instances_qualifier true 

def local_input_name : string := "pump1_input_1"
def local_strat_name : string := "pump1_strat_1"
def local_prf_name : string := "pump1_prf_1"

def preamble : string := "import justification pump1 common_meta property_catalogue.LTL \n open S A"

meta def proof_template (p₁ p₂ : string) : string := 
"\n\n
theorem " ++ local_prf_name ++ " : " ++ p₁ ++ " := \nbegin \n" ++ p₂ ++ "\nend" ++ "\n\n\n" 

meta def evidence_file_template (ps : PROOF_STATE α) : string := 
preamble 
++ "\n\n @[reducible] def " ++ local_input_name 
++ " : property.input (path pump1) := "++ ps.input_string
++ "\n\n @[reducible] def "++ local_strat_name 
++ " : Strategy (path pump1) := property.strategy " ++ local_input_name  
++ proof_template ("deductive (path pump1) " ++ local_strat_name) (tscript_string ps.tscript)
++ ps.unused ++ hints_string ps.hints 

meta def output (s : string) : io unit := do 
  of ← io.mk_file_handle "src/evidence.lean" io.mode.write, 
  io.fs.write of s.to_char_buffer

meta def driver (input : pexpr) : tactic unit := 
let α := path pump1 in 
let ps : PROOF_STATE α := {} in 
do 
  STRAT ← tactic.to_expr input,
  match STRAT with 
  | `(property.input.mk %%CLAIM %%PROPS) := 
  do 
  inpt ← tactic.eval_expr (property.input α) STRAT,
  input_fmt ← tactic_format_expr STRAT,
  let input_s := input_fmt.to_string, 
  let ps := { input := inpt, 
              input_string := input_s,
              strat_expr := STRAT , 
              PROPS := PROPS,
              -- TODO : Clean this
              init_goal := `(deductive (path pump1) (property.strategy %%STRAT)), 
              ..ps},
              -- And this
  let ps := get_originals ps,
  let goal_str := "deductive (path pump1) " ++ local_strat_name,
  set_goal ps.init_goal, 
  ps ← Switch (ps),
  str ← get_unused ps,
    match ps.solved with 
    | tt := tactic.trace "True"
    | ff := tactic.trace "False"
    end,
    tactic.unsafe_run_io $ output $ evidence_file_template {unused := str ..ps}
| _ := return ()
end


@[user_command]
meta def main
(meta_info : decl_meta_info)
(_ : parse (lean.parser.tk "main")) : lean.parser unit :=
do 
   F ← read "src/input/pumpExample1.txt" types.texpr,
   lean.parser.of_tactic $ driver F
. 

main
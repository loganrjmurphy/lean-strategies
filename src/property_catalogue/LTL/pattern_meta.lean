import property_catalogue.LTL.sat.absent 
import property_catalogue.LTL.sat.precedes 
import common_meta 

<<<<<<< HEAD
variable {α : Type}

meta def switch (s : string) : tactic unit := 
=======
meta def switch (s : string) : tactic string := 
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff
do 
  tgt ← tactic.target, ctx ← tactic.local_context,
  match tgt with
  | `(sat (precedes.globally %%e₁ %%e₂) _) :=  
    precedes.globally.solve e₁ e₂ s ctx
  | `(sat (absent.globally %%e₁) _) :=  
    absent.globally.solve e₁ s ctx 
<<<<<<< HEAD
  | _ := return ()
  end 


meta def debug_inductive : tactic (string) := 
do 
   tgt ← tactic.target,

   match tgt with 
   | `(¬(%%path ⊨ %%e1)) := 
      do  
      let e3 : expr := expr.const `transitions_safe [],
      let e4 : expr := expr.mk_app e3 [e1],
      e2 ← tactic_format_expr e1,
      e4 ← tactic_format_expr e4,
      return $ to_string e4 ++ " " ++ to_string e2
   |  _ :=    return string.empty
   end 


meta def solve_inductive (ps : PROOF_STATE α) : tactic (PROOF_STATE α) := 
do 
   let s₁ := string.empty,
   p ← by_induction ps,
   b ← is_solved, 
   if !b then do 
   h ← debug_inductive,
   return {hints := p.hints ++ [h] ..p} else 
   return {solved := b, ..p} 
=======
  | _ := return string.empty
  end 


meta def solve_patterns' (n : ℕ) : tactic (string × string) := 
do 
   let s₁ := string.empty,
   p ← by_induction s₁,
 --  tactic.trace s₂,
   return p 
 --  ((by_induction s₁ >> return s₁) <|>
 --    (switch s₁ >> return s₁) <|> return s₁)
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff

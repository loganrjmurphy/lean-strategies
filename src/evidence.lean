import justification fcs common_meta property_catalogue.LTL 
 open S A

 @[reducible] def fcs_input_1 : property.input (path fcs) := {property.input .
 Clm := {Claim . X := {x : path fcs | true}, P := λ (π : path fcs), π⊨absent.globally ↑Damaged},
 Props := [λ (π : path fcs), π⊨not_init ↑Damaged, λ (π : path fcs), π⊨holds_over_transition ↑Damaged]}

 @[reducible] def fcs_strat_1 : Strategy (path fcs) := property.strategy fcs_input_1


theorem fcs_prf_1 : deductive (path fcs) fcs_strat_1 := 
begin 
by_induction, 
 base_case P1,
end


--holds_over_transition ↑Damaged

--HINT transitions_safe ↑Damaged
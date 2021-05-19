import justification pump1 common_meta property_catalogue.LTL 
 open S A

 @[reducible] def pump1_input_1 : property.input (path pump1) := {property.input .
 Clm := {Claim .
         X := {x : path pump1 | true},
         P := λ (p : path pump1), p⊨absent.after_until ↑BolusRequest ↑Cond_6_3_ ↑Infusion_NormalOperation},
 Props := [λ (p : path pump1), p⊨responds.globally ↑Cond_6_3_ ↑Alrm_EmptyReservoir, λ (p : path pump1),
              p⊨absent.between ↑BolusRequest ↑Cond_6_3_ ↑Alrm_EmptyReservoir, λ (p : path pump1),
              p⊨absent.after_until ↑BolusRequest ↑Alrm_EmptyReservoir ↑Infusion_NormalOperation]}

 @[reducible] def pump1_strat_1 : Strategy (path pump1) := property.strategy pump1_input_1


theorem pump1_prf_1 : deductive (path pump1) pump1_strat_1 := 
begin 
analyze 3, 
 apply absent.after_until.from_absent_between_response, 
 match_premises,
end


--responds.globally ↑Cond_6_3_ ↑Alrm_EmptyReservoir

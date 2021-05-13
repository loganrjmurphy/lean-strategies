import LTS

inductive S
| Clean_Non_hazardous_No_object
| Clean_Non_hazardous_Has_object
| Grimy_Non_hazardous_No_object
| Grimy_Non_hazardous_Has_object
| Damaged
open S

inductive A
| Add_object_2
| Dirty_tile
| Vacuum_object_2
| Clean_tile_1
| Add_object_1
| Vacuum_object_1
| Clean_tile_4
| Clean_tile_2
| Clean_tile_3
| Vacuum_tile_3
open A

def TR : set (S × A × S) :=
{
  (Clean_Non_hazardous_No_object, Add_object_2, Clean_Non_hazardous_Has_object),
  (Clean_Non_hazardous_No_object, Dirty_tile, Grimy_Non_hazardous_No_object),
  (Clean_Non_hazardous_Has_object, Vacuum_object_2, Clean_Non_hazardous_No_object),
  (Grimy_Non_hazardous_No_object, Clean_tile_1, Clean_Non_hazardous_No_object),
  (Grimy_Non_hazardous_No_object, Add_object_1, Grimy_Non_hazardous_Has_object),
  (Grimy_Non_hazardous_Has_object, Vacuum_object_1, Grimy_Non_hazardous_No_object),
  (Clean_Non_hazardous_Has_object, Clean_tile_4, Damaged),
  (Grimy_Non_hazardous_Has_object, Clean_tile_2, Damaged),
}

def fcs : LTS := LTS.mk S A TR

-- def fcs : LTS {S := fcs_states, A :=  fcs_actions, Δ := fcs_transitions}

@[instance] def S_to_fcsS : has_coe S (fcs.S) :=⟨id⟩

@[instance] def S_to_form : has_coe fcs.S (formula fcs) :=
⟨λ s, formula.state s⟩

@[instance] def S_to_form' : has_coe S (formula fcs) :=
⟨λ s, formula.state s⟩

@[instance] def Act_to_form : has_coe A (formula fcs) :=
⟨λ a, formula.act a⟩
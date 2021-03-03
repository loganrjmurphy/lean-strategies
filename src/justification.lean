import data.set

variables {α β: Type}

def Property (α : Type) : Type := α → Prop 

structure Claim (α : Type) :=
(X : set α)
(P : Property α)

<<<<<<< HEAD
@[instance] def claim_default {α : Type} : inhabited (Claim α) := 
⟨Claim.mk ∅ (λ x, true)⟩ 


=======
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff
@[reducible]
def meaning {α : Type} (C : Claim α) : Prop := 
∀ x ∈ C.X, C.P x 

notation ⟦C⟧ := meaning C 

structure Strategy (α : Type) :=
(parent : Claim α) (decomp : Claim α → list (Claim α))

<<<<<<< HEAD
@[instance] def strat_default {α : Type} : inhabited (Strategy α) := 
⟨Strategy.mk (default (Claim α)) (λ c, [])⟩



=======
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff
def deductive (α : Type) (S : Strategy α) : Prop := 
let subclaims := (S.decomp) S.parent in 
(∀ clm ∈ subclaims, ⟦clm⟧) →  ⟦S.parent⟧ 

namespace property 

@[reducible]
def decomposition 
(Ps : list (Property α)) 
(Clm : Claim α) : list (Claim α) := 
list.map (Claim.mk Clm.X) Ps

structure input (α : Type) :=
(Clm : Claim α)
(Props : list (Property α))

<<<<<<< HEAD
@[instance] def input_default {α : Type} : inhabited (input α) := 
⟨input.mk (default (Claim α)) []⟩

=======
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff
namespace input 

@[reducible]
def length (Γ : input α) : ℕ := Γ.Props.length

lemma len_decomp (Γ : input α) : 
Γ.length = (decomposition Γ.Props Γ.Clm).length := 
by {rw decomposition, simp}

@[reducible]
def subsets (Γ : input α) : fin (Γ.length) → set α := 
λ i, set_of (Γ.Props.nth_le i.1 i.2)

end input

def justified
(Γ : input α) : Prop :=
(⋂ i, Γ.subsets i) ⊆ {x | Γ.Clm.P x}

@[reducible]
def strategy (Γ : input α) : Strategy α := 
{ parent := Γ.Clm, 
  decomp := property.decomposition Γ.Props }  



theorem deductive_of_justfd
: Π {Γ : property.input α},
justified Γ → deductive α (property.strategy Γ) :=  
begin 
  intro Γ,
  rw [justified,deductive, meaning,strategy], 
  simp only [fin.val_eq_coe],
  intros H1 H2 x xMem, apply H1,
  simp only [set.mem_Inter, set.mem_set_of_eq],
  intro i, 
  replace H2 := H2 ((decomposition Γ.Props Γ.Clm).nth_le i.1 (fin.cast (input.len_decomp Γ) i).2),
  simp at H2, apply H2, 
  apply list.nth_le_mem, assumption,
end 

end property 




/-
namespace domain 

@[reducible]
def to_claim {α : Type}
(P : Property α) (X : set α) : Claim α := 
Claim.mk X P

@[reducible]
def preimages {α β: Type} 
(f : α → β)
(Clm : Claim α)
(bs : list (set β)) : list (set α) := 
list.map (set.preimage f) bs

@[reducible]
def decomposition {α β: Type} 
(f : α → β)
(sets_range : list (set β))
(Clm : Claim α) : list (Claim α) :=
(preimages f Clm sets_range).map (to_claim Clm.P)

structure auxiliary (α β : Type) :=
(Clm : Claim α)
(f : α → β)
(range_sets : list (set β))

def set_of_list : list α → set α 
| [] := ∅ 
| (h::t) := {h} ∪ set_of_list t

def complete (Γ : auxiliary α β) : Prop := 
 ∀ b : β, ∃ s ∈ Γ.range_sets, b ∈ s

def to_strategy (Γ : auxiliary α β) : strategy α := 
strategy.mk (Γ.Clm) (domain.decomposition Γ.f Γ.range_sets)

theorem deductive_of_justfd_comp (Γ : auxiliary α β) : complete Γ →  deductive α (to_strategy Γ) :=
begin 
  rw [complete, deductive, meaning, to_strategy], 
  simp only [fin.val_eq_coe],
  intros H1 H2 x xMem,
  unfold decomposition at H2,simp at H2,
  unfold to_claim at H2,
  have H3 : ∃ s ∈ Γ.range_sets, Γ.f x ∈ s, from H1 (Γ.f x),
  rcases H3 with ⟨s,H3,H4⟩,
  replace H2 := H2 s H3,
  apply H2, assumption,
end 

end domain
-/
import LTS

inductive S 
| pay | select | beer 
| soda| serve 
open S 

inductive A 
| insert_coin 
| choose_b | choose_s 
| get_soda | get_beer | restart open A 

def TR : set (S × A × S) := 
{   (pay,insert_coin, select), (select, choose_b, beer),
    (select, choose_s, soda),  (soda, get_soda, serve),
    (beer, get_beer, serve),   (serve, restart, pay) }

def myLTS : LTS := LTS.mk S A TR 

@[instance] def S_to_myLTSS : has_coe S (myLTS.S) :=⟨id⟩ 

@[instance] def S_to_form : has_coe myLTS.S (formula myLTS) := 
⟨λ s, formula.state s⟩ 


@[instance] def S_to_form' : has_coe S (formula myLTS) := 
⟨λ s, formula.state s⟩ 

@[instance] def Act_to_form : has_coe A (formula myLTS) := 
⟨λ a, formula.act a⟩ 


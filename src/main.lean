import justification common_meta property_catalogue.LTL
import fcs 
open interactive S A

<<<<<<< HEAD
variable {α : Type}
=======
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff

set_option pp.structure_instances_qualifier true 

def local_input_name : string := "fcs_input_1"
def local_strat_name : string := "fcs_strat_1"
def local_prf_name : string := "fcs_prf_1"

<<<<<<< HEAD
def preamble : string := "import justification fcs common_meta property_catalogue.LTL \n open S A"

meta def proof_template (p₁ p₂ : string) : string := 
"\n\n
theorem " ++ local_prf_name ++ " : " ++ p₁ ++ " := \nbegin \n" ++ p₂ ++ "\nend" ++ "\n\n\n" 

meta def evidence_file_template (ps : PROOF_STATE α) : string := 
preamble 
++ "\n\n @[reducible] def " ++ local_input_name 
++ " : property.input (path fcs) := "++ ps.input_string
++ "\n\n @[reducible] def "++ local_strat_name 
++ " : Strategy (path fcs) := property.strategy " ++ local_input_name  
++ proof_template ("deductive (path fcs) " ++ local_strat_name) (tscript_string ps.tscript)
++ ps.unused ++ hints_string ps.hints 
=======
/- Some hacky stuff for formatting evidence.lean -/

def preamble : string := "import justification fcs common_meta property_catalogue.LTL \n open S A"

meta def proof_template (g : format) (p : string × string) : string := 
"\n\n
theorem " ++ local_prf_name ++ " : " ++ (g.to_string) ++  " := \nbegin \n" ++ p.1 ++ "\nend" ++ "\n \n \n --" ++ p.2

meta def evidence_file_template (e : format) (prf : string) : string := 
preamble 
++ "\n\n @[reducible] def " 
++ local_input_name 
++ " : property.input (path fcs) := "
++ e.to_string 
++ "\n\n @[reducible] def "
++ local_strat_name 
++ " : Strategy (path fcs) := property.strategy " 
++ local_input_name  
++ prf
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff

meta def output (s : string) : io unit := do 
  of ← io.mk_file_handle "src/evidence.lean" io.mode.write, 
  io.fs.write of s.to_char_buffer

<<<<<<< HEAD
meta def driver (input : pexpr) : tactic unit := 
let α := path fcs in 
let ps : PROOF_STATE α := {} in 
=======

meta def driver (input : pexpr) : tactic unit := 
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff
do 
  STRAT ← tactic.to_expr input,
  match STRAT with 
  | `(property.input.mk %%CLAIM %%PROPS) := 
  do 
<<<<<<< HEAD
  inpt ← tactic.eval_expr (property.input α) STRAT,
  input_fmt ← tactic_format_expr STRAT,
  let input_s := input_fmt.to_string, 
  let ps := { input := inpt, 
              input_string := input_s,
              strat_expr := STRAT , 
              PROPS := PROPS,
              -- TODO : Clean this
              init_goal := `(deductive (path fcs) (property.strategy %%STRAT)), 
              ..ps},
              -- And this
  let ps := get_originals ps,
  let goal_str := "deductive (path fcs) " ++ local_strat_name,
  set_goal ps.init_goal, 
  ps ← solve_inductive (ps),
  str ← get_unused ps,
    tactic.unsafe_run_io $ output $ evidence_file_template {unused := str ..ps}
=======
  aux ← tactic.eval_expr (property.input (path fcs)) STRAT,
  let g := `(deductive (path fcs) (property.strategy %%STRAT)),
  set_goal g,
  let goal_str := 
  "deductive (path fcs) " ++ local_strat_name, 
  p ← solve_patterns' (aux.Props.length),
  STRAT ← (tactic_format_expr STRAT),
  tactic.unsafe_run_io $ output $ evidence_file_template STRAT $ proof_template goal_str p,
  res ← is_solved,
  match res with 
  | tt := tactic.trace "True"
  | _ := tactic.trace "False"
  end   
>>>>>>> 63e27cbf4711c89d309d9ecd2af369c127146fff
| _ := return ()
end


@[user_command]
meta def main
(meta_info : decl_meta_info)
(_ : parse (lean.parser.tk "main")) : lean.parser unit :=
do 
   F ← read "src/input/Inductive_Example.txt" types.texpr,
   lean.parser.of_tactic $ driver F
. 
main

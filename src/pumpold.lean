import justification common_meta property_catalogue.LTL
import pump1 
open interactive S A

variable {α : Type}

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
  ps ← SOLVE (ps),
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


-- OLD version, will integrate 
-- def foobar : property.input (path fcs) := {}




-- variable {α : Type}

-- set_option pp.structure_instances_qualifier true 

-- def local_input_name : string := "fcs_input_1"
-- def local_strat_name : string := "fcs_strat_1"
-- def local_prf_name : string := "fcs_prf_1"

-- def preamble : string := "import justification fcs common_meta property_catalogue.LTL \n open S A"

-- meta def proof_template (p₁ p₂ : string) : string := 
-- "\n\n
-- theorem " ++ local_prf_name ++ " : " ++ p₁ ++ " := \nbegin \n" ++ p₂ ++ "\nend" ++ "\n\n\n" 

-- meta def evidence_file_template (ps : PROOF_STATE α) : string := 
-- preamble 
-- ++ "\n\n @[reducible] def " ++ local_input_name 
-- ++ " : property.input (path fcs) := "++ ps.input_string
-- ++ "\n\n @[reducible] def "++ local_strat_name 
-- ++ " : Strategy (path fcs) := property.strategy " ++ local_input_name  
-- ++ proof_template ("deductive (path fcs) " ++ local_strat_name) (tscript_string ps.tscript)
-- ++ ps.unused ++ hints_string ps.hints 

-- meta def output (s : string) : io unit := do 
--   of ← io.mk_file_handle "src/evidence.lean" io.mode.write, 
--   io.fs.write of s.to_char_buffer

-- meta def driver (input : pexpr) : tactic unit := 
-- let α := path fcs in 
-- let ps : PROOF_STATE α := {} in 
-- do 
--   STRAT ← tactic.to_expr input,
--   match STRAT with 
--   | `(property.input.mk %%CLAIM %%PROPS) := 
--   do 
--   inpt ← tactic.eval_expr (property.input α) STRAT,
--   input_fmt ← tactic_format_expr STRAT,
--   let input_s := input_fmt.to_string, 
--   let ps := { input := inpt, 
--               input_string := input_s,
--               strat_expr := STRAT , 
--               PROPS := PROPS,
--               -- TODO : Clean this
--               init_goal := `(deductive (path fcs) (property.strategy %%STRAT)), 
--               ..ps},
--               -- And this
--   let ps := get_originals ps,
--   let goal_str := "deductive (path fcs) " ++ local_strat_name,
--   set_goal ps.init_goal, 
--   ps ← solve_inductive (ps),
--   str ← get_unused ps,
--     match ps.solved with 
--     | tt := tactic.trace "True"
--     | ff := tactic.trace "False"
--     end,
--     tactic.unsafe_run_io $ output $ evidence_file_template {unused := str ..ps}
-- | _ := return ()
-- end


-- @[user_command]
-- meta def main
-- (meta_info : decl_meta_info)
-- (_ : parse (lean.parser.tk "main")) : lean.parser unit :=
-- do 
--    F ← read "src/input/Inductive_Example.txt" types.texpr,
--    lean.parser.of_tactic $ driver F
-- . 
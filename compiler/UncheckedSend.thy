theory UncheckedSend
  imports Main List "./Ast" "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl" "./eth-isabelle/lem/Keccak"
begin

definition this_address :: address
where "this_address = undefined"

abbreviation "the_code  == [Stack (PUSH_N [5])]"

value "(program_content (program_of_lst the_code program_content_of_lst)) 1"

abbreviation always_fail_account_state :: "w256 \<Rightarrow> account_state"
where
"always_fail_account_state balance \<equiv>
   \<lparr> account_address = this_address
   , account_storage = (\<lambda> (a::w256). 0)
   , account_code = program_of_lst the_code program_content_of_lst
   , account_balance = balance
   , account_ongoing_calls = []
   , account_killed = False
  \<rparr>"


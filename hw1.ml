open List;;
open Pervasives;;

let rec subset a b = match a with 
  | [] -> true
  | h::t -> if mem h b then subset t b else false
;;


let equal_sets a b =
(subset a b) && (subset b a)
;;    


let set_union a b = 
sort_uniq compare (append a b)
;;


let rec set_intersection a b = match a with 
  | [] -> []
  | h::t -> if mem h b then h::set_intersection t b else set_intersection t b
;;


let rec set_diff a b = match a with 
  | [] -> []
  | h::t -> if not (mem h b) then h::set_diff t b else set_diff t b
;;


let rec computed_fixed_point eq f x = 
if eq (f x) x then x    
else computed_fixed_point eq f (f x)
;;


let rec computed_periodic_point eq f p x =
if p = 0 then x
else if eq x (f (computed_periodic_point eq f (p-1) (f x))) then x
else computed_periodic_point eq f p (f x)
;;


let rec while_away s p x = 
if p x then x::while_away s p (s x)
else []
;;

let rec rle_decode lp =
	let rec repeat char num = match num with
      | 0 -> []
      | _ -> char::repeat char (num-1) in
match lp with
 | [] -> []
 | h::t -> repeat (snd h) (fst h) @ rle_decode t 
;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

let is_nonterm sym r = match sym with
| T sym -> true
| N sym -> if exists (fun x -> (fst x) = sym) r then true else false
;;

let rec check_terms r terms = match r with
 | [] -> true
 | h::t -> if (is_nonterm h terms) then (check_terms t terms) else false
;;

let rec rm_bad rules terms = match rules with
 | [] -> terms
 | h::t -> if (check_terms (snd h) terms) && (not (subset [h] terms)) then rm_bad t (h::terms) else rm_bad t terms
;;

let filter_blind_alleys g =
(fst g, filter (fun x -> exists (fun e -> e = x) (computed_fixed_point (=) (rm_bad (snd g)) [])) (snd g))
;;


let my_subset_test0 = subset [1;5;3;5] [1;2;3;3;4;5]
let my_subset_test1 = subset [] []
let my_subset_test2 = not (subset [1;5] [1;2;3])

let my_equal_sets_test0 = equal_sets [1;2;3] [3;2;1;1;2;3]
let my_equal_sets_test1 = not (equal_sets [1;2;3] [2;3;4])

let my_set_union_test0 = equal_sets (set_union [1;2;3] [2;3;4]) [1;2;3;4]
let my_set_union_test1 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]

let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3] [2;3;4]) [2;3]
let my_set_intersection_test1 = equal_sets (set_intersection [1;2] [3;4]) []

let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [2;3;4]) [1]
let my_set_diff_test1 = equal_sets (set_diff [1;2;3;3;2;1] [1;1;2;4;5]) [3]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 5) 1000 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x) 10 = 10

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.

let my_while_away_test0 = while_away ((+) 1) ((>) 10) 1 = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let my_while_away_test1 = while_away ((+) 10) ((>) 100) 15 = [15; 25; 35; 45; 55; 65; 75; 85; 95]

let my_rle_decode_test0 = rle_decode [2,"a";5,"s";0,"b";1,"o"] = ["a"; "a"; "s"; "s"; "s"; "s"; "s"; "o"]
let my_rle_decode_test1 = rle_decode [1,4;0,5;1,3;2,1;1,0] = [4; 3; 1; 1; 0]

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Sneeze | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Sneeze, [T"achoo!"];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Sneeze];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Sneeze];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let my_filter_blind_alleys_test0 =
    filter_blind_alleys (Sentence, tl (tl (snd giant_grammar))) = (Sentence, [Sneeze, [T "achoo!"];
    Grunt, [T "khrgh"]; Shout, [T "aooogah!"]; Sentence, [N Sneeze]; Sentence, [N Grunt]; Sentence,
    [N Shout]; Conversation, [N Sneeze]; Conversation, [N Sentence; T ","; N Conversation]])

let my_filter_blind_alleys_test1 =
  filter_blind_alleys (Sentence, tl (tl (tl (snd giant_grammar)))) =
    (Sentence, [Grunt, [T "khrgh"]; Shout, [T "aooogah!"]; Sentence, [N Grunt]; Sentence, [N Shout]])
;;



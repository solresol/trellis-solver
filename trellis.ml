(* $Id$ *)

type cost_comparison = Better | Worse | Same

module type TRELLIS =
  sig
    type coordinate
    type data_array
    type data_point
    type cost
    type history
    val viable_neighbours_backwards : coordinate -> coordinate list
    val fetch : data_array -> coordinate -> data_point
    val calculate_cost : history -> coordinate -> coordinate -> data_point -> data_point -> cost
    val combine_costs : cost -> cost -> cost
    val compare_costs : cost -> cost -> cost_comparison
    val zero_cost : cost
    val starting_edge : coordinate list
    val next : coordinate list -> coordinate list
    val finished : coordinate list -> bool
    val add_to_history : history -> coordinate -> history
    val initial_history : history
  end;;

module Trellis_solver =
  functor (Puzzle : TRELLIS) ->
    struct
      let find_best (costlist : (Puzzle.history * Puzzle.cost) list) =
	let rec find_best_after_this ((extra,start) : ('a * Puzzle.cost)) (others : ('a * Puzzle.cost) list) =
	  match others with [] -> (extra,start)
	    | ((w,o)::os) -> (
		match (Puzzle.compare_costs start o) with
		    Better -> find_best_after_this (extra,start) os
		  | Worse -> find_best_after_this (w,o) os
		  | Same -> find_best_after_this (extra,start) os
		      (* interesting situation that one -- two alternate
			 paths are both equally good. Which should we choose?
		      *)
	      )
	in
	  find_best_after_this (List.hd costlist) (List.tl costlist)
	    
      let solve (puzzle : Puzzle.data_array) =
	let rec solve_one_column (previous_column  (*: (Puzzle.coordinate * (Puzzle.history * Puzzle.cost)) Hashtbl.t *) ) =
	  let c_old = Hashtbl.fold (fun coord cst d -> coord :: d) 
	    previous_column  [] in
	  let this_column_coordinates = Puzzle.next c_old in
	  let solve_one_coordinate (c : Puzzle.coordinate) =
	    let neighbours = Puzzle.viable_neighbours_backwards c in
	    let choice_fun next_coord =
	      let (associated_history,associated_cost) = (Hashtbl.find previous_column next_coord)
	      and new_data_point = Puzzle.fetch puzzle next_coord
	      and old_data_point = Puzzle.fetch puzzle c in
	      let cost_of_this_coord =
		Puzzle.calculate_cost associated_history next_coord c new_data_point old_data_point in 
		(Puzzle.add_to_history associated_history next_coord,
		 Puzzle.combine_costs associated_cost cost_of_this_coord
		) in
	    let choices = List.map choice_fun neighbours in
	      (c,find_best choices)
	  and next_column = Hashtbl.create (List.length this_column_coordinates) in
	  let insert_next x =
	    let (coord,(hist,cst)) = solve_one_coordinate x in
	      Hashtbl.add next_column coord (hist,cst) 
	  in
	    ( List.iter insert_next this_column_coordinates ; next_column ) 
	in
	let initial_column =
	  let first_column = Hashtbl.create (List.length Puzzle.starting_edge) in 
	  let put_nothing_in coord = Hashtbl.add first_column coord (Puzzle.initial_history,Puzzle.zero_cost) in
	    ( List.iter put_nothing_in Puzzle.starting_edge ; first_column )
	in let rec until_finished a_column =
	    let c_old = Hashtbl.fold (fun coord cst d -> coord :: d) 
		a_column  [] in
	    if Puzzle.finished c_old then a_column
	    else until_finished (solve_one_column a_column)
	in
	  until_finished initial_column
    end;;




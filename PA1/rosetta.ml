(* OCAML: Reverse-sort the lines from standard input *)
module Sset = Set.Make(String);;
let lines = ref [] in (* store the lines; a 'ref' is a mutable pointer *)
try
  while true do (* we'll read all the lines until something stops us *)
    (* read one line, add it to the list *)
    let a = read_line () in
	let b = read_line () in
	lines := (b,a):: !lines
	(* X :: Y makes a new list with X as the head element and Y as the rest *)
    (* !reference loads the current value of 'reference' *)
    (* reference := value assigns value into reference *)
  done (* read_line will raise an exception at the end of the file *)
with _ -> begin (* until we reach the end of the file *)
(*let sorted = List.sort (* sort the linst *)
    (fun line_a line_b -> (* how do we compare two lines? *)
      compare line_b line_a) (* in reverse order! *)
      (* (fun (argument) -> body) is an anonymous function *)
    !lines (* the list we are sorting *)
  in (* let ... in introduces a new local variable *)
  List.iter print_endline sorted (* print them to standard output *)
  (* List.iter applies the function 'print_endline' to every element
   * in the list 'sorted' *) *)
  

	let rec getResult a a_set = match a with
		| [] -> List.sort compare (Sset.elements a_set)
		| _ -> begin
				let b_set = List.fold_left (fun acc (x,y) -> Sset.add y acc) Sset.empty a in
				let possibles = Sset.diff a_set b_set in
				if Sset.is_empty possibles then [] else begin
					let next_elt = Sset.min_elt possibles in
					let a' = List.filter(fun (x,y) -> x <> next_elt) a in
					let a_set' = Sset.diff a_set (Sset.singleton next_elt) in
					let result = getResult a' a_set' in
						if List.length result == 0 then [] else
						next_elt :: result
				end
			end
	in
	let linesA = List.fold_left (fun acc (x,y) -> Sset.add x acc) Sset.empty !lines in
	let linesB = List.fold_left (fun acc (x,y) -> Sset.add y acc) Sset.empty !lines in
	let finalSet = Sset.union linesA linesB in
		let finalLines = getResult !lines finalSet in
			match finalLines with
				| [] -> Printf.printf("cycle")
				| _ -> List.iter print_endline finalLines

end

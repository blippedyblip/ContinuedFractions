let rec cftodec xs acc = match xs with
| [] -> 0.
| [x] -> x +. acc 
| x::xs -> cftodec xs (1. /. (x +. acc));;

let float_rev xs = List.rev_map float xs;; 

let cf2dec xs = cftodec (float_rev xs) 0.;;

let rec dectocf decimal acc originaldec accuracy = 
	if((abs_float ((cf2dec (List.rev acc)) -. (originaldec))) <= accuracy) then List.rev acc
	else
		let intpart = truncate decimal in
		let decpart = decimal -. (float intpart) in
		dectocf (1./.decpart) (intpart::acc) originaldec accuracy;; 

let rec dec2cf decimal n = dectocf decimal [] decimal (1./.(10. ** (float n)));;
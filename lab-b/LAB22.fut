
let abs (x: i32) = if (0 > x) then -x else x

let process (n: i32) (xs: [n]i32, ys: [n]i32) = 
	let max (x: i32) (y: i32) = if (y > x) then y else x
	let min (x: i32) (y: i32) = if (y > x) then x else y	
	let maxXS = reduce max xs[0] xs
	let maxYS = reduce max ys[0] ys
	let minXS = reduce min xs[0] xs		
	let minYS = reduce min ys[0] ys	
	in if ( n == 0 ) then 0 
		else if ( abs(maxXS - minYS) < abs(maxYS - minXS) ) then abs(maxYS - minXS) else abs(maxXS - minYS)	

let argmax [ n ] ( xs : [ n ]i32 ) =
	reduce_comm ( \(x, i) (y, j) ->
		if x < y then (y , j ) else (x , i ) )
	(i32.smallest, -1)
	( zip xs (iota n ) )

let argmin [ n ] ( xs : [ n ]i32 ) =
	reduce_comm ( \(x, i) (y, j) ->
		if x > y then (y , j ) else (x , i ) )
	(i32.largest, -1)
	( zip xs (iota n ) )

let process_idx (n: i32) (xs: [n]i32, ys: [n]i32) : (i32, (i32, i32)) =
	let (maxXS, imaxXS) = argmax xs
	let (maxYS, imaxYS) = argmax ys
	let (minXS, iminXS) = argmin xs		
	let (minYS, iminYS) = argmin ys	
	in if ( n == 0 ) then (0, (0, 0)) 
		else if ( abs(maxXS - minYS) < abs(maxYS - minXS) ) then (abs(maxYS - minXS), (imaxYS, iminXS)) else (abs(maxXS - minYS), (iminYS, imaxXS))


let s1 : [14]i32 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
let s2 : [14]i32 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]

let main () = 
	let (x, (y, z)) = process_idx 14 (s1, s2)
	in (x, y, z) 
	
	
-- (v1, f1) OP' (v2, f2) = (if f2 then v2 else v1 OP v2, f1 OR f2)
-- 
-- (0,false) is left neutral because of the following reduction: 
-- 
-- (0, false) OP' (v2, f2) = (if f2 then v2 else 0 OP v2, false OR f2) =
-- 
-- = (0, false) OP' (v2, f2) = (if f2 then v2 else v2, f2) =
-- 
-- = (0, false) OP' (v2, f2) = (v2, f2)
-- 
-- Since 0 is neutral for OP, 0 OP v2 evaluates to v2
-- Since false is neutral to OR, false OR f2 evaluates to f2
-- Since both clauses in the conditional evaluated to v2 it can be reduced to v2

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


let segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): [n]t =
	let f (b2':bool) (t1' :t) (t2' :t) = if b2' then t2' else (op t1' t2')	
	let op' (t1 :t, b1:bool) (t2:t, b2:bool) = (f b2 t1 t2, (b1 || b2) ) 	
	let (vs, _) = unzip (scan op' (ne, false) arr)
	in vs

let estimate_pi [n] (size: f32) (xs: [n]f32) (ys: [n]f32) = 
	let succ_hits:f32  = reduce (+) 0.0f32 (map ( \((x:f32), (y:f32)) -> if (((x - 1.0f32)*(x - 1.0f32)) + 
		    ((y - 1.0f32)*(y - 1.0f32)) <= 1.0f32) then 1.0f32 else 0.0f32) (zip xs ys)) 
	in (4.0f32 * succ_hits) / size



let s1 : [14]i32 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
let s2 : [14]i32 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]
let s3 = [(1,true), (2,false), (3,false), (4,true), (5,false), (6,true)]

let main [n] (xs:[n]f32, ys:[n]f32) = 
	estimate_pi 1000000.0f32 xs ys 
	
	
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

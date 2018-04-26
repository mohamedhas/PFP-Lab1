import "/futlib/math"
import "/futlib/sobol"
import "/futlib/sobol-dir-50"
module array = import "/futlib/array"


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


let integrate [n] (size: f32) (xs:[n]f32) (ys:[n]f32) =
	let f (x:f32, y:f32): f32 =
		2.0f32*x*x*x*x*x*x*y*y - x*x*x*x*x*x*y
		+ 3.0f32*x*x*x*y*y*y - x*x*y*y*y +
		x*x*x*y - 3.0f32*x*y*y + x*y -
		5.0f32*y + 2.0f32*x*x*x*x*x*y*y*y*y -
		2.0f32*x*x*x*x*x*y*y*y*y*y + 250.0f32
	let sum = reduce (+) 0.0f32 (map f (zip xs ys))
	in (4.0f32/size)*sum

module S2 = Sobol sobol_dir { let D = 2 }
module R = S2.Reduce { type t = f32
                       let ne = 0f32
                       let op (x:f32) (y:f32) = x f32.+ y
                       let f (v : [2]f64) : f32 =
                         let x:f64 = v[0]
                         let y:f64 = v[1]
                         in f32.bool(x*x+y*y < 1.0f64) }



let s1 : [14]i32 = [23,45,-23,44,23,54,23,12,34,54,7,2, 4,67]
let s2 : [14]i32 = [-2, 3, 4,57,34, 2, 5,56,56, 3,3,5,77,89]
let s3 = [(1,true), (2,false), (3,false), (4,true), (5,false), (6,true)]

let main () = 
	let sobol = array.transpose (S2.chunk 0 1000000)
	let xs' = map f32.f64 sobol[0]
	let ys' = map f32.f64 sobol[1]
	in integrate 1000000.0f32 xs' ys' 


	
-- Answer to 1.3
--	
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

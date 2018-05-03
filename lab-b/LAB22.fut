import "/futlib/math"
import "/futlib/sobol"
import "/futlib/sobol-dir-50"
import "/futlib/random"
import "/futlib/math"
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


let segscan' [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): [n](t,bool) =
	let f (b2':bool) (t1' :t) (t2' :t) = if b2' then t2' else (op t1' t2')	
	let op' (t1 :t, b1:bool) (t2:t, b2:bool) = (f b2 t1 t2, (b1 || b2) ) 	
	in scan op' (ne, false) arr


let segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): [n]t =
	let (vs, _) = unzip (segscan' op ne arr)
	in vs

let fst (x, _) = x 
let snd (_, y) = y

let segreduce [n] 't (op : t -> t -> t) (ne:t)
	(arr : [n](t, bool)) : []t =
		let bs = snd (unzip arr)
		let f (_, flag) fpo = if (
		let (values, _) = unzip (filter  (zip (zip (segscan op ne arr)) (bs) (rotate bs 1) ))
		in values

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

let sbool_gen () = 
	let sobol = array.transpose (S2.chunk 0 1000000)
	let xs' = map f32.f64 sobol[0]
	let ys' = map f32.f64 sobol[1]
	in integrate 1000000.0f32 xs' ys' 

type spin = i8
module dist = uniform_int_distribution i8 minstd_rand

module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine


let random_grid (seed: i32) (w: i32) (h: i32)
                : ([w][h]rng_engine.rng, [w][h]spin) =
			let rng = minstd_rand.rng_from_seed [seed]
			let randomGen : (rng_engine.rng -> (rng_engine.rng, i8)) = (rand_i8.rand (0i8,1i8))
			let (rng1:rng_engine.rng, x:i8) = randomGen rng	
			let (a, b) = unzip ((scan (\ (fstEltRng:rng_engine.rng, _) _ -> randomGen fstEltRng) (rng1,x) (replicate (w * h) (rng1,x))))
			in ((reshape (h, w) a), (reshape (h, w) b) )

let deltas [w][h] (spins:[w][h]spin) : [w][h]i8 =
	let ds = (reshape (h * w, 1) (rotate@1 (-1) spins))[0]
	let us = (reshape (h * w, 1) (rotate@1 (1) spins))[0]
	let ls = (reshape (h * w, 1) (rotate@0 (1) spins))[0]
	let rs = (reshape (h * w, 1) (rotate@0 (-1) spins))[0]
	let cs = (reshape (h * w, 1) spins)[0]
	let delta (c:i8) (d:i8) (u:i8) (l:i8) (r:i8) = 2i8 * c * ( u + d + l + r )
	in reshape (h, w) (map5 delta cs ds us ls rs)

-- (x: [][]rng_engine.rng, y: [][]spin) 

let step [w][h] (abs_temp: f32) (samplerate: f32)
                  (rngs: [w][h]rng_engine.rng) (spins: [w][h]spin)
                : ([w][h]rng_engine.rng, [w][h]spin) =
   let rshp 't (x: [][]t) = (reshape (h * w, 1) x)[0]
   let p = samplerate 
   let t = abs_temp
   let deltasF32 = map (\x -> f32.i8 x) (rshp (deltas spins))
   let randomGen : (rng_engine.rng -> (rng_engine.rng, i8)) = (rand_i8.rand (0i8,1i8))
   let bs :[](rng_engine.rng, i8) = map (\x -> randomGen x ) (rshp rngs) 
   let getC' c (rng2: rng_engine.rng, b:i8) Delta_e = if ((f32.i8 b) > p && (Delta_e < (- Delta_e) || (f32.i8 b) < f32.exp( (- Delta_e) / t ) )) then (rng2, -1i8 * c) else (rng2, c)
   in unzip (reshape (h, w) (map3 getC' (rshp spins) (bs) deltasF32))
		


let main () = 
	--random_grid 123i32 10i32 20i32	
	--deltas [[1i8,-1i8], [1i8,1i8]]
	--segreduce (+) 0 s3
	segscan (+) 0 s3
-- 
	
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

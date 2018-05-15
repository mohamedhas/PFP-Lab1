-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8

import "/futlib/random"

-- Pick an RNG engine and define random distributions for specific types.
--module rng_engine = minstd_rand
--module rand_f32 = uniform_real_distribution f32 rng_engine
--module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For an RNG state 'r', we can generate random floats in the range
-- (0,1) by calling 'rand_f32.rand (0f32, 1f32) r'.
--
-- Remember to consult https://futhark-lang.org/docs/futlib/random.html

--let rand = rand_f32.rand (0f32, 1f32)

-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.

module dist = uniform_int_distribution i8 minstd_rand

module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

entry random_grid (seed: i32) (w: i32) (h: i32)
                : ([w][h]rng_engine.rng, [w][h]spin) =
			let rng = minstd_rand.rng_from_seed [seed]
			let randomGen : (rng_engine.rng -> (rng_engine.rng, i8)) = (rand_i8.rand (0i8,1i8))
			let (rng1:rng_engine.rng, x:i8) = randomGen rng	
			let (a, b) = unzip ((scan (\ (fstEltRng:rng_engine.rng, _) _ -> randomGen fstEltRng) (rng1,x) (replicate (w * h) (rng1,x))))
			in ((reshape (h, w) a), (reshape (h, w) b) )
  

-- Compute $\Delta_e$ for each spin in the grid, using wraparound at
-- the edges.
entry deltas [w][h] (spins: [w][h]spin): [w][h]i8 =
	let ds = (reshape (h * w, 1) (rotate@1 (-1) spins))[0]
	let us = (reshape (h * w, 1) (rotate@1 (1) spins))[0]
	let ls = (reshape (h * w, 1) (rotate@0 (1) spins))[0]
	let rs = (reshape (h * w, 1) (rotate@0 (-1) spins))[0]
	let cs = (reshape (h * w, 1) spins)[0]
	let delta (c:i8) (d:i8) (u:i8) (l:i8) (r:i8) = 2i8 * c * ( u + d + l + r )
	in reshape (h, w) (map5 delta cs ds us ls rs)

-- The sum of all deltas of a grid.  The result is a measure of how
-- ordered the grid is.
--entry delta_sum [w][h] (spins: [w][h]spin): i32 =
--   deltas spins |> flatten |> map1 i32.i8 |> reduce (+) 0

-- Take one step in the Ising 2D simulation.
let step' [w][h] (abs_temp: f32) (samplerate: f32)
                  (rngs: [w][h]rng_engine.rng) (spins: [w][h]spin)
                : ([w][h]rng_engine.rng, [w][h]spin) =
   	let rshp 't (x: [][]t) = (reshape (h * w, 1) x)[0]
   	let p = samplerate 
   	let t = abs_temp
   	let deltasF32 = map (\x -> f32.i8 x) (rshp (deltas spins))
   	let randomGen : (rng_engine.rng -> (rng_engine.rng, i8)) = (rand_i8.rand (0i8,1i8))
   	let bs :[](rng_engine.rng, i8) = map (\x -> randomGen x ) (rshp rngs) 
   	let getC' c (rng2: rng_engine.rng, b:i8) Delta_e = if ((f32.i8 b) > p && 
		(Delta_e < (- Delta_e) || (f32.i8 b) < f32.exp( (- Delta_e) / t ) )) 
			then (rng2, -1i8 * c) else (rng2, c)
  	in unzip (reshape (h, w) (map3 getC' (rshp spins) (bs) deltasF32))

entry step [w][h] (abs_temp: f32) (samplerate: f32)
                  (rngs: [w][h]rng_engine.rng) (spins: [w][h]spin)
                : ([w][h]rng_engine.rng, [w][h]spin) =
	step abs_temp samplerate rngs spins

import "/futlib/colour"

-- | Turn a grid of spins into an array of pixel values, ready to be
-- blitted to the screen.
entry render [w][h] (spins: [w][h]spin): [w][h]argb.colour =
  let pixel spin = if spin == -1i8
                   then argb.(bright <| light red)
                   else argb.(bright <| light blue)
  in map1 (map1 pixel) spins

--let main() = 
-- | Just for benchmarking.
entry main (abs_temp: f32) (samplerate: f32)
         (w: i32) (h: i32) (n: i32): [w][h]spin =
  (loop (rngs, spins) = random_grid 1337 w h for _i < n do
     step abs_temp samplerate rngs spins).2

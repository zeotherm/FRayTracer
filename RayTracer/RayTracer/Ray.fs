module Ray

open Tuples
open Matrix

type Ray = Tuple * Tuple

type Intersection = double * int
type Intersections = Intersection list

let make_ray (o: Tuple) (d: Tuple): Ray = (make_point o.x o.y o.z, make_vector d.x d.y d.z)
let origin (r: Ray) = fst r
let direction (r: Ray) = snd r

let position (r: Ray) (t: double) =
    (origin r) + t * (direction r)

let t_val (i: Intersection): double = fst i
let object (i: Intersection): int = snd i
let make_intersection t o_id: Intersection = (t, o_id)

let transform (r: Ray) m: Ray =
    let p = origin r
    let v = direction r
    let p' = mat_tuple_mul m p
    let v' = mat_tuple_mul m v
    make_ray p' v'

let hit (is: Intersections) : Intersection option =
    is |> List.sortBy (fun i -> t_val i) 
       |> List.filter (fun i -> (t_val i) > 0.0) 
       |> List.tryHead

        


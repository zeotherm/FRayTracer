module Ray

open System
open Tuples

type Ray = Tuple * Tuple

let make_ray (o: Tuple) (d: Tuple): Ray = (make_point o.x o.y o.z, make_vector d.x d.y d.z)
let origin (r: Ray) = fst r
let direction (r: Ray) = snd r

let position (r: Ray) (t: double) =
    (origin r) + t * (direction r)

let sphere =
    let counter = ref 0
    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

let intersect s r =
    let sphere_to_ray = (origin r) - (make_point 0 0 0)
    let a = dot (direction r) (direction r)
    let b = 2. * (dot (direction r) sphere_to_ray)
    let c = (dot sphere_to_ray sphere_to_ray) - 1.
    let d = b*b - 4.*a*c
    if d < 0 then
        List.empty<double>
    else
        [(-b - sqrt(d))/(2.*a);
         (-b + sqrt(d))/(2.*a)]



module Sphere
open Matrix
open Ray
open Tuples

type Sphere = int * double[,]

let id (s: Sphere): int = 
    let (i, _) = s
    i
let extract_transform (s: Sphere): double[,] = 
    let ( _, t) = s
    t
let set_transform (s: Sphere) t: Sphere = 
    (id s, t)

let get_unique  =
    let counter = ref 0
    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

let make_sphere: Sphere = (get_unique(), make_ident_mat 4)

let intersect s r =
    let r' = s |> extract_transform |> inverse |> transform r
    let sphere_to_ray = (origin r') - (make_point 0 0 0)
    let a = dot (direction r') (direction r')
    let b = 2. * (dot (direction r') sphere_to_ray)
    let c = (dot sphere_to_ray sphere_to_ray) - 1.
    let d = b*b - 4.*a*c
    if d < 0 then
        List.empty<Intersection>
    else
        let t1 = (-b - sqrt(d))/(2.*a)
        let t2 = (-b + sqrt(d))/(2.*a)
        [(make_intersection t1 (id s)); (make_intersection t2 (id s))]


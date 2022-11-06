module Ray

open System
open Tuples
open Transforms
open Matrix

type Ray = Tuple * Tuple

type Sphere = Tuple * int * double[,]

type Intersection = double * int
type Intersections = Intersection list

let make_ray (o: Tuple) (d: Tuple): Ray = (make_point o.x o.y o.z, make_vector d.x d.y d.z)
let origin (r: Ray) = fst r
let direction (r: Ray) = snd r

let position (r: Ray) (t: double) =
    (origin r) + t * (direction r)

let get_unique  =
    let counter = ref 0
    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value


let center (s:Sphere): Tuple = 
    let (c, _, _) = s
    c
let id (s: Sphere): int = 
    let (_, i, _) = s
    i
let extract_transform (s: Sphere): double[,] = 
    let (_, _, t) = s
    t
let set_transform (s: Sphere) t: Sphere = 
    (center s, id s, t)

let make_sphere: Sphere = (make_point 0 0 0, get_unique(), make_ident_mat 4)

let t_val (i: Intersection): double = fst i
let object (i: Intersection): int = snd i
let make_intersection t o: Intersection = (t, id o)

let transform (r: Ray) m: Ray =
    let p = origin r
    let v = direction r
    let p' = mat_tuple_mul m p
    let v' = mat_tuple_mul m v
    make_ray p' v'

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
        [(make_intersection t1 s); (make_intersection t2 s)]

let hit (is: Intersections) : Intersection option =
    is |> List.sortBy (fun i -> t_val i) 
       |> List.filter (fun i -> (t_val i) > 0.0) 
       |> List.tryHead

        


module Intersection

open Tuples
open Matrix
open Shape
open Ray

let EPSILON = 0.00001

type Intersection = double * Shape
type Intersections = Intersection list

let t_val (i: Intersection): double = fst i
let object (i: Intersection): Shape = snd i
let make_intersection t o: Intersection = (t, o)

let hit (is: Intersections) : Intersection option =
    is |> List.sortBy (fun i -> t_val i) 
       |> List.filter (fun i -> (t_val i) > 0.0) 
       |> List.tryHead

let intersect s r =
    let r' = s |> extract_transform |> inverse |> transform r
    match extract_shape_type s with 
    | Sphere -> let sphere_to_ray = (origin r') - (make_point 0 0 0)
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
    | Plane ->  let org, dir = r'
                if abs (dir.y) < EPSILON then
                    List.empty<Intersection>
                else
                    let t = -org.y/dir.y
                    [make_intersection t s]
    | _ -> failwith "Unimplemented intersection shape function"
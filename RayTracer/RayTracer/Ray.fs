module Ray

open System
open Tuples
open Matrix
open Color
open Pattern
type Ray = Tuple * Tuple

type PointLight = Tuple * Color

type Material = Pattern option * Color * double * double * double * double

let make_ray (o: Tuple) (d: Tuple): Ray = (make_point o.x o.y o.z, make_vector d.x d.y d.z)
let origin (r: Ray) = fst r
let direction (r: Ray) = snd r

let position (r: Ray) (t: double) =
    (origin r) + t * (direction r)

let make_pointlight (p: Tuple) (c: Color): PointLight = (p, c)
let intensity (pl: PointLight) = snd pl
let location (pl: PointLight) = fst pl

let make_def_material: Material = (None, Color(1, 1, 1), 0.1, 0.9, 0.9, 200.0)
let make_material (p: Pattern option) (c: Color) (ambient: double) (diffuse: double) (specular: double) (shininess: double): Material =
    (p, c, ambient, diffuse, specular, shininess)
let make_material_with_pattern p: Material = make_material (Some(p)) (Color (1, 1, 1)) 0.1 0.7 0.3 200.0

let mat_pattern (m: Material): Pattern option = 
    let (p, _, _, _, _, _) = m
    p
let mat_color (m: Material): Color =
    let (_, c, _, _, _, _) = m
    c
let ambient (m: Material): double = 
    let (_, _, a, _, _, _) = m
    a
let diffuse (m: Material): double =
    let (_, _, _, d, _, _) = m
    d
let specular (m: Material): double =
    let (_, _, _, _, s, _) = m
    s
let shininess (m: Material): double = 
    let (_, _, _, _, _, s) = m
    s
let override_ambient (m: Material) (a: double): Material = 
    (mat_pattern m, mat_color m, a, diffuse m, specular m, shininess m)
let override_color (m: Material) (c: Color): Material = 
    (mat_pattern m, c, ambient m, diffuse m, specular m, shininess m)

let transform (r: Ray) m: Ray =
    let p = origin r
    let v = direction r
    let p' = mat_tuple_mul m p
    let v' = mat_tuple_mul m v
    make_ray p' v'

let reflect (v:Tuple) (n: Tuple): Tuple = 
    v - 2. * (dot v n) * n




        


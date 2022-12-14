module Ray

open System
open Tuples
open Matrix
open Color
open Pattern
type Ray = Tuple * Tuple

type PointLight = Tuple * Color

type Material = Pattern list * double * double * double * double * double * double * double

let make_ray (o: Tuple) (d: Tuple): Ray = (make_point o.x o.y o.z, make_vector d.x d.y d.z)
let origin (r: Ray) = fst r
let direction (r: Ray) = snd r

let position (r: Ray) (t: double) =
    (origin r) + t * (direction r)

let make_pointlight (p: Tuple) (c: Color): PointLight = (p, c)
let intensity (pl: PointLight) = snd pl
let location (pl: PointLight) = fst pl

let make_def_material: Material = ([make_pattern (Solid(Color(1, 1, 1)))], 0.1, 0.9, 0.9, 200.0, 0.0, 0.0, 1.0)
let make_material (p: Pattern list) (ambient: double) (diffuse: double) 
                  (specular: double) (shininess: double) (reflective: double)
                  (transparency: double) (refractive_indx: double): Material =
    (p, ambient, diffuse, specular, shininess, reflective, transparency, refractive_indx)
let make_material_with_pattern p: Material = make_material p 0.1 0.7 0.3 200.0 0.0 0.0 1.0

let mat_pattern (m: Material): Pattern list = 
    let (p, _, _, _, _, _, _, _) = m
    p
let mat_color (m: Material): Color =
    let p = (mat_pattern m).Head
    match extract_patt_type p with 
    | Solid(c) -> c
    | _ -> failwith "Can't get a single color from a non-Solid pattern without a point"
let ambient (m: Material): double = 
    let (_, a, _, _, _, _, _, _) = m
    a
let diffuse (m: Material): double =
    let (_, _, d, _, _, _, _, _) = m
    d
let specular (m: Material): double =
    let (_, _, _, s, _, _, _, _) = m
    s
let shininess (m: Material): double = 
    let (_, _, _, _, s, _, _, _) = m
    s
let reflective (m: Material): double = 
    let (_, _, _, _, _, r, _, _) = m
    r
let transparency (m: Material): double = 
    let (_, _, _, _, _, _, t, _) = m
    t
let refractive_index (m: Material): double = 
    let (_, _, _, _, _, _, _, n) = m
    n

let override_ambient (a: double) (m: Material): Material = 
    (mat_pattern m, a, diffuse m, specular m, shininess m, reflective m, transparency m, refractive_index m)
let override_color (c: Color) (m: Material): Material = 
    ([make_pattern (Solid(c))], ambient m, diffuse m, specular m, shininess m, reflective m, transparency m, refractive_index m)
let override_reflective (r: double) (m: Material) = 
    (mat_pattern m, ambient m, diffuse m, specular m, shininess m, r, transparency m, refractive_index m)
let override_refractive_idx (n: double) (m: Material) = 
    (mat_pattern m, ambient m, diffuse m, specular m, shininess m, reflective m, transparency m, n)
let override_transparancy (t: double) (m: Material) = 
    (mat_pattern m, ambient m, diffuse m, specular m, shininess m, reflective m, t, refractive_index m)
let override_pattern (ps: Pattern list) (m: Material) = 
    (ps, ambient m, diffuse m, specular m, shininess m, reflective m, transparency m, refractive_index m)
let make_glass_material: Material = make_def_material 
                                    |> override_refractive_idx 1.5 
                                    |> override_transparancy 1.0 

let transform (r: Ray) m: Ray =
    let p = origin r
    let v = direction r
    let p' = mat_tuple_mul m p
    let v' = mat_tuple_mul m v
    make_ray p' v'

let reflect (v:Tuple) (n: Tuple): Tuple = 
    v - 2. * (dot v n) * n




        


module Ray

open System
open Tuples
open Matrix
open Color

type Ray = Tuple * Tuple

type PointLight = Tuple * Color

type Material = Color * double * double * double * double

let make_ray (o: Tuple) (d: Tuple): Ray = (make_point o.x o.y o.z, make_vector d.x d.y d.z)
let origin (r: Ray) = fst r
let direction (r: Ray) = snd r

let position (r: Ray) (t: double) =
    (origin r) + t * (direction r)

let make_pointlight (p: Tuple) (c: Color): PointLight = (p, c)
let intensity (pl: PointLight) = snd pl
let location (pl: PointLight) = fst pl

let make_def_material: Material = (Color(1, 1, 1), 0.1, 0.9, 0.9, 200.0)
let make_material (c: Color) (ambient: double) (diffuse: double) (specular: double) (shininess: double): Material =
    (c, ambient, diffuse, specular, shininess)

let mat_color (m: Material): Color =
    let (c, _, _, _, _) = m
    c
let ambient (m: Material): double = 
    let (_, a, _, _, _) = m
    a
let diffuse (m: Material): double =
    let (_, _, d, _, _) = m
    d
let specular (m: Material): double =
    let (_, _, _, s, _) = m
    s
let shininess (m: Material): double = 
    let (_, _, _, _, s) = m
    s
let override_ambient (m: Material) (a: double): Material = 
    (mat_color m, a, diffuse m, specular m, shininess m)
let override_color (m: Material) (c: Color): Material = 
    (c, ambient m, diffuse m, specular m, shininess m)

let transform (r: Ray) m: Ray =
    let p = origin r
    let v = direction r
    let p' = mat_tuple_mul m p
    let v' = mat_tuple_mul m v
    make_ray p' v'


let reflect (v:Tuple) (n: Tuple): Tuple = 
    v - 2. * (dot v n) * n

let lighting (m: Material) (l: PointLight) (p: Tuple) (ev: Tuple) (nv: Tuple) (in_shadow: bool): Color = 
    let effective_color = (mat_color m) * (intensity l)
    let lightv = normalize ((location l) - p)
    let ambient_val = effective_color * (ambient m)
    let light_dot_normal = dot lightv nv
    let black = Color(0,0,0)
    let (diffuse_val, specular_val) = if light_dot_normal < 0 || in_shadow then
                                        (black, black)
                                      else
                                        let diff = effective_color * (diffuse m) * light_dot_normal
                                        let reflectv = reflect (-lightv) nv
                                        let reflect_dot_eye = dot reflectv ev
                                        if reflect_dot_eye <= 0. then
                                            (diff, black)
                                        else
                                            let f = Math.Pow(reflect_dot_eye, (shininess m))
                                            let spec = (intensity l) * (specular m) * f
                                            (diff, spec)
    ambient_val + diffuse_val + specular_val




        


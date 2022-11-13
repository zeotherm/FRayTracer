module Sphere
open Matrix
open Ray
open Tuples

type Sphere = int * double[,] * Material

let id (s: Sphere): int = 
    let (i, _, _) = s
    i
let extract_transform (s: Sphere): double[,] = 
    let ( _, t, _) = s
    t
let extract_material (s: Sphere): Material =
    let (_, _, m) = s
    m
let set_sphere_transform t (s: Sphere): Sphere = 
    (id s, t, extract_material s)
let set_sphere_material (m: Material) (s: Sphere): Sphere = 
    (id s, extract_transform s, m)

let get_unique  =
    let counter = ref 0
    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

let make_sphere: Sphere = (get_unique(), make_ident_mat 4, make_def_material)

let normal_at (s:Sphere) (world_point: Tuple) = 
    let t_inv = s|> extract_transform |> inverse
    let t_inv_trans = transpose t_inv
    let obj_point = mat_tuple_mul t_inv world_point
    let obj_normal = obj_point - origin000
    let world_norm = mat_tuple_mul t_inv_trans obj_normal
    normalize (make_vector world_norm.x world_norm.y world_norm.z) // includes a hack to ensure that the w value didn't get monkeyed with during the transformations



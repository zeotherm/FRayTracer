module Shape
open Matrix
open Ray
open Tuples

type ShapeType = 
    | Sphere
    | Plane
    | Cube
    | Cylinder
    | Default

type Shape = int * double[,] * Material * ShapeType

let id (s: Shape): int = 
    let (i, _, _, _) = s
    i
let extract_transform (s: Shape): double[,] = 
    let ( _, t, _, _) = s
    t
let extract_material (s: Shape): Material =
    let (_, _, m, _) = s
    m
let extract_shape_type (s: Shape): ShapeType =
    let (_, _, _, st) = s
    st
let set_sphere_transform t (s: Shape): Shape = 
    (id s, t, extract_material s, extract_shape_type s)
let set_shape_material (m: Material) (s: Shape): Shape = 
    (id s, extract_transform s, m, extract_shape_type s)

let get_unique  =
    let counter = ref 0
    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

let make_shape t: Shape = (get_unique(), make_ident_mat 4, make_def_material, t)

let local_normal_at (s: Shape) (p: Tuple) = 
    match extract_shape_type s with
    | Sphere -> p - origin000
    | Plane -> make_vector 0 1 0
    | Default -> make_vector p.x p.y p.z
    | _ -> failwith "Unknown shape type"

let normal_at (s:Shape) (world_point: Tuple) = 
    let t_inv = s|> extract_transform |> inverse
    let t_inv_trans = transpose t_inv
    let obj_point = mat_tuple_mul t_inv world_point
    let obj_normal = match extract_shape_type s with
                     | Sphere -> obj_point - origin000
                     | Plane -> make_vector 0 1 0
                     | Default -> make_vector obj_point.x obj_point.y obj_point.z
                     | _ -> failwith "Unknown shape type"
    let world_norm = mat_tuple_mul t_inv_trans obj_normal
    normalize (make_vector world_norm.x world_norm.y world_norm.z) // includes a hack to ensure that the w value didn't get monkeyed with during the transformations




module Shape
open System
open Matrix
open Ray
open Tuples
open Pattern
open Color

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
let set_shape_transform t (s: Shape): Shape = 
    (id s, t, extract_material s, extract_shape_type s)
let set_shape_material (m: Material) (s: Shape): Shape = 
    (id s, extract_transform s, m, extract_shape_type s)

let get_unique  =
    let counter = ref 0
    fun () ->
        printfn "Someone asked for an id %A" counter
        counter.Value <- counter.Value + 1
        counter.Value

let make_shape t: Shape = let uid = get_unique()
                          printfn "Requested a new shape its type is %A, it's id is %A" t uid
                          (uid, make_ident_mat 4, make_def_material, t)

let make_glass_sphere: Shape = (get_unique(), 
                                make_ident_mat 4, 
                                make_glass_material,
                                Sphere)

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

let pattern_at_object (patts: Pattern list) (object: Shape) (world_point: Tuple) = 
    let obj_inv = extract_transform object |> inverse 
    let object_point = mat_tuple_mul obj_inv world_point
    let total_color = patts |> List.map (fun patt -> let patt_inv = extract_patt_transform patt |> inverse
                                                     let pattern_point = mat_tuple_mul patt_inv object_point
                                                     pattern_at patt pattern_point)
                            |> List.fold (fun patt_color tot_color -> patt_color + tot_color) black
    // Return the average color of all the patterns
    total_color / double(patts.Length) 
            

let lighting (m: Material) (object: Shape) (l: PointLight) (p: Tuple) (ev: Tuple) (nv: Tuple) (in_shadow: bool): Color = 
    let raw_color = pattern_at_object (mat_pattern m) object p
    let effective_color = raw_color * (intensity l)
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



module World
open Ray
open Shape
open Transforms
open Color
open Tuples
open Intersection
open Matrix
open Canvas

let EPSILON = 0.00001
(* World type and accompanying functions *)

type World = PointLight list * Shape list

let make_empty_world : World = (List.Empty, List.Empty)
let make_default_world : World = 
    let pl = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let s1 = make_shape Sphere |> set_shape_material (make_material None (Color(0.8, 1.0, 0.6)) 0.1 0.7 0.2 200.0)
    let s2 = make_shape Sphere |> set_shape_transform (scaling 0.5 0.5 0.5)
    ([pl], [s1;s2])

let make_world ls ss: World = (ls, ss)

let has_light (w: World): bool =
    not (fst w).IsEmpty

let light (w: World): PointLight = 
    (fst w).Item(0)

let lights (w: World): PointLight list = 
    (fst w)

let world_objects (w: World): Shape list = snd w

let assign_light (p: PointLight) (w: World): World = 
    ([p], world_objects w)

let add_light (p: PointLight) (w: World): World =
    (p::(lights w), world_objects w)

let world_contains (s: Shape) (w: World): bool =
    List.contains s (snd w)

let intersect_world (r: Ray) (w: World): Intersections =
    let ts = List.collect (fun o -> intersect o r) (world_objects w)
    List.sortBy (fun i -> t_val i) ts

let is_shadowed w p: bool =
    let single_light_shadow l: bool =
        let v = (location l) - p
        let dist = magnitude v
        let dir = normalize v

        let r = make_ray p dir
        let is = intersect_world r w
        let h = hit is
        match h with 
        | Some(i) -> t_val i < dist
        | None -> false

    List.map (fun l -> single_light_shadow l) (lights w) |> List.contains true

(* Pre computation type and associated functions *)
type PreCompute = double * Shape * Tuple * Tuple * Tuple * Tuple * bool
let make_precompute t s point over_point eyev normalv inside: PreCompute = (t, s, point, over_point, eyev, normalv, inside)
let extract_t (p: PreCompute) = 
    let (t, _, _, _, _, _, _) = p
    t
let extract_obj (p: PreCompute) = 
    let (_, o, _, _, _, _, _) = p
    o
let extract_point (p: PreCompute) = 
    let (_, _, point, _, _, _, _) = p
    point
let extract_over_point (p: PreCompute) = 
    let (_, _, _, overp, _, _, _) = p
    overp
let extract_eyev (p: PreCompute) = 
    let (_, _, _, _, e, _, _) = p
    e
let extract_normalv (p: PreCompute) =
    let (_, _, _, _, _, n, _) = p
    n
let extract_inside (p: PreCompute) =
    let (_, _, _, _, _, _, i) = p
    i

let prepare_computations (i: Intersection) (r: Ray) =
    let t = t_val i
    let o = object i
    let point = position r t
    let eyev = -(direction r)
    let normalv = normal_at o point
    let inside, adj_normal = if (dot normalv eyev) < 0 then (true, -normalv) else (false, normalv)
    let over_point = point + adj_normal * EPSILON
    
    make_precompute t o point over_point eyev adj_normal inside

let shade_hit (p: PreCompute) (w: World): Color = 
    let mat = extract_obj p |> extract_material
    let in_shadow = is_shadowed w (extract_over_point p)
    lights w 
    |> List.map (fun l -> lighting mat (extract_obj p) l (extract_over_point p) (extract_eyev p) (extract_normalv p) in_shadow)
    |> List.fold (fun acc c -> acc + c) (Color(0,0,0))

let color_at (w: World) (r: Ray): Color = 
    match hit (intersect_world r w) with
    | Some i -> shade_hit (prepare_computations i r) w
    | None -> Color(0,0,0)

(* Camera section *)
type Camera = int * int * double * double[,]

let make_camera w h fov: Camera = (w, h, fov, (make_ident_mat 4)) 
let extract_height (c: Camera): int =
    let (_, h, _, _) = c
    h
let extract_width (c: Camera): int = 
    let (w, _, _, _) = c
    w
let extract_field_of_view (c: Camera): double = 
    let (_, _, fov, _) = c
    fov
let extract_cam_transform (c: Camera): double[,] =
    let (_, _, _, t) = c
    t
let set_camera_transform t (c: Camera): Camera = 
    (extract_width c, extract_height c, extract_field_of_view c, t)

let halves (c: Camera): (double * double) = 
    let half_view = System.Math.Tan((extract_field_of_view c)/2.0)
    let hsize = extract_width c
    let vsize = extract_height c
    let aspect = float(hsize)/float(vsize)
    if aspect >= 1.0 then
        (half_view, half_view/aspect)
    else
        (half_view*aspect, half_view)

let half_width (c: Camera): double = fst (halves c)
let half_height (c: Camera): double = snd (halves c)

let pixel_size (c: Camera): double = 
    let half_width, _ = halves c
    half_width * 2.0 / float(extract_width c)

let ray_for_pixel c x y =
    let ps = pixel_size c
    let xoffset = (float(x) + 0.5) * ps
    let yoffset = (float(y) + 0.5) * ps
    // Untransformed coordinates of the pixel in WORLD space
    // camera looks towards -z, so +x is to the left
    let world_x = half_width c - xoffset
    let world_y = half_height c - yoffset
    let cti = extract_cam_transform c |> inverse
    let pixel =  mat_tuple_mul cti (make_point world_x world_y -1) //canvas is at z=-1
    let origin = mat_tuple_mul cti origin000
    let dir = normalize (pixel - origin)
    make_ray origin dir

let render c w : Canvas = 
    let blank = make_canvas (extract_width c) (extract_height c)
    blank 
    |> get_all_pixels
    |> List.map (fun (p: Pixel) ->
                             let r = ray_for_pixel c (fst p) (snd p)
                             let color = color_at w r
                             (p, color))
    |> List.fold (fun canv pix_w_color -> write_pixel (fst pix_w_color) (snd pix_w_color) canv) blank




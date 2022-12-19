module World
open Ray
open Shape
open Transforms
open Color
open Tuples
open Intersection
open Matrix
open Canvas
open Pattern
let EPSILON = 0.00001
let REC_LIMIT = 6
(* World type and accompanying functions *)

type World = PointLight list * Shape list

let make_empty_world : World = (List.Empty, List.Empty)
let make_default_world : World = 
    let pl = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let s1 = make_shape Sphere |> set_shape_material (make_material [make_pattern (Solid(Color(0.8, 1.0, 0.6)))] 0.1 0.7 0.2 200.0 0.0 0.0 1.0)
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

let add_object (s: Shape) (w: World): World = 
    (lights w, s::(world_objects w))

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
type PreCompute = double * Shape * Tuple * Tuple * Tuple * Tuple * Tuple * Tuple * double * double * bool
let make_precompute t s point over_point under_point eyev normalv reflectv n1 n2 inside: PreCompute = (t, s, point, over_point, under_point, eyev, normalv, reflectv, n1, n2, inside)
let extract_t (p: PreCompute) = 
    let (t, _, _, _, _, _, _, _, _, _, _) = p
    t
let extract_obj (p: PreCompute) = 
    let (_, o, _, _, _, _, _, _, _, _, _) = p
    o
let extract_point (p: PreCompute) = 
    let (_, _, point, _, _, _, _, _, _, _, _) = p
    point
let extract_over_point (p: PreCompute) = 
    let (_, _, _, overp, _, _, _, _, _, _, _) = p
    overp
let extract_under_point(p: PreCompute) =
    let (_, _, _, _, underp, _, _, _, _, _, _) = p
    underp
let extract_eyev (p: PreCompute) = 
    let (_, _, _, _, _, e, _, _, _, _, _) = p
    e
let extract_normalv (p: PreCompute) =
    let (_, _, _, _, _, _, n, _, _, _, _) = p
    n
let extract_reflectv (p: PreCompute) = 
    let (_, _, _, _, _, _, _, r, _, _, _) = p
    r
let extract_n1 (p: PreCompute) = 
    let (_, _, _, _, _, _, _, _, n1, _, _) = p
    n1
let extract_n2 (p: PreCompute) = 
    let (_, _, _, _, _, _, _, _, _, n2, _) = p
    n2
let extract_inside (p: PreCompute) =
    let (_, _, _, _, _, _, _, _, _, _, i) = p
    i

let prepare_computations (i: Intersection) (r: Ray) (xs: Intersection list) (logit: bool) =
    let compute_n1n2 (hit: Intersection): double * double = 
        let index_from_containers (cs: Shape list) : double option = 
            match cs with
            | h::_ -> Some(extract_material h |> refractive_index)
            | [] -> Some(1.0)
        let update_containers (i: Intersection) (cs: Shape list): Shape list =
            if List.contains (object i) cs then
                let indexAt = List.findIndex (fun elem -> elem = (object i)) cs
                List.removeAt indexAt cs
            else
                (object i) :: cs
        let rec compute_aux (containers: Shape list) (intersections: Intersection list): double option * double option = 
            match intersections with
            | i::is -> let opt_n1 = if i = hit then
                                        index_from_containers containers
                                    else
                                        None
                       let mod_conts = update_containers i containers
                       if i = hit then
                           let opt_n2 = index_from_containers mod_conts
                           (opt_n1, opt_n2)
                       else
                           compute_aux mod_conts is 
            | _ -> failwith "Can we reach here???"
        
        match (compute_aux List.empty<Shape> xs) with 
        | (Some(n1), Some(n2)) -> (n1, n2)
        | _ -> failwith "Something went wrong"
    
    let t = t_val i
    let o = object i
    let point = position r t
    let eyev = -(direction r)
    let normalv = normal_at o point
    let inside, adj_normal = if (dot normalv eyev) < 0 then (true, -normalv) else (false, normalv)
    let over_point = point + adj_normal * EPSILON
    let under_point = point - adj_normal * EPSILON
    let reflectv = reflect (direction r) normalv
    let junk = if logit then
                    printfn "In precompute"
                    printfn "Main intersection is at t = %A, object id = %A" t (id o)
                    List.map (fun x -> printfn "\tt_val %A" (t_val x)
                                       printfn "\tobj id %A" (id (object x))
                                       printfn "\tn idx %A" (refractive_index (extract_material (object x)))
                    ) xs |> ignore
                    1
               else
                1

    let (n1, n2) = compute_n1n2 i
    make_precompute t o point over_point under_point eyev adj_normal reflectv n1 n2 inside

let rec shade_hit (p: PreCompute) (w: World) (d: int) (logIt: bool): Color = 
    let mat = extract_obj p |> extract_material
    let in_shadow = is_shadowed w (extract_over_point p)
    let surface = lights w 
                  |> List.map (fun l -> lighting mat (extract_obj p) l (extract_over_point p) (extract_eyev p) (extract_normalv p) in_shadow)
                  |> List.fold (fun acc c -> acc + c) (Color(0,0,0))
    let reflect = reflected_color w p d false
    let refract = refracted_color w p d logIt
    surface + reflect + refract
and color_at (w: World) (r: Ray) (d: int) (logIt: bool): Color = 
    let its = intersect_world r w
    match hit its with
    | Some i -> shade_hit (prepare_computations i r its logIt) w d logIt
    | None -> Color(0,0,0)
and reflected_color w p d logIt =
    if d < 1 then
        black
    else
        let reflect_val = extract_obj p |> extract_material |> reflective
        if reflect_val = 0.0 then
            black
        else
            let reflect_ray = make_ray (extract_over_point p) (extract_reflectv p)
            let c = color_at w reflect_ray (d-1) logIt
            c * reflect_val 
and refracted_color (w: World) (comps: PreCompute) (remaining: int) (logIt: bool): Color = 
    let t = comps |> extract_obj |> extract_material |> transparency
    let n1 = extract_n1 comps
    let n2 = extract_n2 comps
    let o = extract_obj comps |> id
    let n_ratio = n1/n2
    let cos_i = dot (extract_eyev comps) (extract_normalv comps)
    let sin2_t = n_ratio * n_ratio * (1. - cos_i*cos_i)

    if t = 0.0 || sin2_t > 1.0 || remaining < 1 then
        black
    else
        let cos_t = sqrt (1. - sin2_t)
        // Direction of the refracted ray
        let normal_v = extract_normalv comps
        let eye_v = extract_eyev comps
        let direction = normal_v * (n_ratio * cos_i - cos_t) - eye_v * n_ratio
        let under_point = extract_under_point comps
        let refract_ray = make_ray under_point direction
        let color_raw = color_at w refract_ray (remaining - 1) logIt
        let color_ans = color_raw * t
        let junk = if logIt then
                     printfn "== Remaining %A ==" remaining
                     printfn "Object id # %A" o
                     printfn "n1 %A" n1
                     printfn "n2 %A" n2
                     printfn "eyev %A" eye_v
                     printfn "normalv %A" normal_v
                     printfn "under point %A" under_point
                     printfn "n ratio %A" n_ratio
                     printfn "cos_i %A" cos_i
                     printfn "sin2_t %A" sin2_t
                     printfn "cos_t %A" cos_t
                     printfn "refract direction %A" direction
                     printfn "refracted color %A" color_ans
                     1
                   else
                     1
        color_ans
        //white


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
                             let logit = if p = (125, 125) then true else false
                             let color = color_at w r REC_LIMIT logit
                             (p, color))
    |> List.fold (fun canv pix_w_color -> write_pixel (fst pix_w_color) (snd pix_w_color) canv) blank




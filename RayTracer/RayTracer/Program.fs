
module RayTracer
open Canvas
open Color
open System
open System.IO
open Tuples
open Cannon
open Clock
open Transforms
open Intersection
open Ray
open Sphere

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

let determine_hit ray_origin wall_z pixel_size half light shape pixel: (Pixel * Color) option = 
    let (x,y) = (get_x pixel, get_y pixel)
    let (world_x, world_y) = (-half + pixel_size * float(x), half - pixel_size * float(y))
    let pos = make_point world_x world_y wall_z
    let r = make_ray ray_origin (normalize (pos - ray_origin))
    let xs = intersect shape r
    match hit xs with
    | Some i -> 
        let point = position r (t_val i)
        let normal = normal_at (object i) point
        let eye = -(direction r)
        let c = lighting (extract_material (object i)) light point eye normal
        Some((x, y), c)
    | None -> None

[<EntryPoint>]
let main argv = 
    let filepath = Path.Combine(Path.Combine(__SOURCE_DIRECTORY__, "canvas_" + DateTime.Now.ToString("yyyyMMdd_HH_mm_ss") + ".ppm"))
    
    //make_canvas 10 2 |> canvas_fill (Color(1, 0.8, 0.6)) |> canvas_to_ppm path
   
    // Chapter 2 End
    //let wind = make_vector -0.0385 0 0
    //let gravity = make_vector 0 -0.1 0
    //let start_pos = make_point 0 1 0
    //let start_v = normalize (make_vector 1 1.8 0) * 11.25
    //let e: Environment = (wind, gravity)
    //let p_init: Projectile = (start_pos, start_v)
    ////let path = evolve e p_init

    //display_path (evolve e p_init) |> canvas_to_ppm filepath

    // Chapter 4 End
    //display_clock 256 |> canvas_to_ppm filepath

    // Chapter 5 End
    let ray_origin = make_point 0 0 -5
    let wall_z = 10
    let wall_size = 7
    let canvas_pixels = 640
    let pixel_size = float(wall_size)/float(canvas_pixels)
    let half = float(wall_size)/2.0

    let canvas = make_canvas canvas_pixels canvas_pixels
    let s = make_sphere 
    let m = override_color (make_material) (Color(1, 0.2, 1))
    let s' = set_material s m

    let light_position = make_point -10 10 -10
    let light_color = Color(1, 1, 1)
    let light = make_pointlight light_position light_color
    let shape = set_transform s' (chain [scaling 1 0.25 1]) //; shearing -0.75 0 0 0 0 0]) 
    let hit_pixels = canvas 
                     |> get_all_pixels
                     |> List.map (determine_hit ray_origin wall_z pixel_size half light shape)
                     |> List.choose (fun x -> x)
    let final_canvas = List.fold (fun canv pix_w_color -> write_pixel (fst pix_w_color) (snd pix_w_color) canv) canvas hit_pixels
    canvas_to_ppm filepath final_canvas
    0
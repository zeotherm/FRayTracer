
module RayTracer
open Canvas
open Color
open System
open System.IO
open Tuples
open Cannon
open Clock
open Transforms
open Ray

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

let determine_hit canvas_pixels ray_origin wall_z pixel_size half shape pixel: Pixel option = 
    let x = get_x pixel
    let y = get_y pixel
    let world_y = half - pixel_size * float(y)
    let world_x = half - pixel_size * float(x)
    let position = make_point world_x world_y wall_z
    let r = make_ray ray_origin (normalize (position - ray_origin))
    let xs = intersect shape r
    match hit xs with
    | Some i -> Some((x, canvas_pixels - y))
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
    let canvas_pixels = 256
    let pixel_size = float(wall_size)/float(canvas_pixels)
    let half = float(wall_size)/2.0

    let canvas = make_canvas canvas_pixels canvas_pixels
    let color = Color(1, 0, 0)
    let s = make_sphere 
    let shape = set_transform s (chain [scaling 0.5 1 1; shearing -0.75 0 0 0 0 0]) 
    let hit_pixels = canvas 
                     |> get_all_pixels
                     |> List.map (determine_hit canvas_pixels ray_origin wall_z pixel_size half shape)
                     |> List.choose (fun x -> x)
    let final_canvas = List.fold (fun canv pix -> write_pixel pix color canv) canvas hit_pixels
    canvas_to_ppm filepath final_canvas
    0
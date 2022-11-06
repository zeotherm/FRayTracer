
module RayTracer
open Canvas
open Color
open System
open System.IO
open Tuples
open Cannon
open Clock
open Ray

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

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
    let canvas_pixels = 480
    let pixel_size = float(wall_size)/float(canvas_pixels)
    let half = float(wall_size)/2.0

    let canvas = make_canvas canvas_pixels canvas_pixels
    let color = Color(1, 0, 0)
    let shape = make_sphere
    let x_pixels = [0 .. canvas_pixels - 1]
    let y_pixels = [0 .. canvas_pixels - 1]
    let all_pixels = x_pixels |> List.collect (fun x -> y_pixels |> List.map (fun y -> x, y))
    let opt_hit_pixels = List.map (fun (x, y) ->
                                        let world_y = half - pixel_size * float(y)
                                        let world_x = half - pixel_size * float(x)
                                        let position = make_point world_x world_y wall_z
                                        let r = make_ray ray_origin (normalize (position - ray_origin))
                                        let xs = intersect shape r
                                        match hit xs with
                                        | Some i -> Some((x, canvas_pixels - y))
                                        | None -> None) all_pixels
    let hit_pixels = List.choose (fun x -> x) opt_hit_pixels
    let final_canvas = List.fold (fun canv pix -> write_pixel pix color canv) canvas hit_pixels
    canvas_to_ppm filepath final_canvas
    0

module RayTracer
open Canvas
open Color
open System
open System.IO
open Tuples
open Cannon
open Clock

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
    display_clock 256 |> canvas_to_ppm filepath
    0
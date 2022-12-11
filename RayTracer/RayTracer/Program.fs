
module RayTracer
open Canvas
open Color
open System
open System.IO
open Tuples
open Transforms
open Ray
open Pattern
open Shape
open World

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

[<EntryPoint>]
let main argv = 
    let filepath = Path.Combine(Path.Combine(__SOURCE_DIRECTORY__, "canvas_" + DateTime.Now.ToString("yyyyMMdd_HH_mm_ss") + ".ppm"))
    let halfPi    = Math.PI/2.
    let quarterPi = halfPi/2.
    // Chapter 8 End
    let stripe_patt= make_pattern (Stripes(lawngreen, darkgreen)) 
                     |> set_patt_transform (chain [scaling 0.175 0.125 0.125;
                                                   rotation_y (-Math.PI/3.0);
                                                   rotation_z (-3.*Math.PI/8.0)])
    let grad_patt  = make_pattern (Gradient(yellow, red)) 
                     |> set_patt_transform (chain [translation 0.5 0 0; 
                                                  scaling 2 2 2;
                                                  rotation_z (quarterPi/1.25)])
    let checkers   = make_pattern (Checkers(white, black))
    let stripes_1   = make_pattern (Stripes(black, white)) 
    //let floor_mat  = make_material_with_pattern [checkers] //[stripes_1; set_patt_transform (rotation_y halfPi) stripes_1]
    let floor_mat  = make_material [stripes_1; set_patt_transform (rotation_y halfPi) stripes_1] 0.3 0.7 0.3 200.0
    let floor      = make_shape Plane 
                     |> set_shape_material floor_mat
    let wall_patt  = make_pattern (Stripes(grey, dark_grey)) 
                     |> set_patt_transform (rotation_y (quarterPi/1.5))
    let wall_mat   = make_material_with_pattern [wall_patt]
    let wall       = make_shape Plane
                     |> set_shape_transform (chain [rotation_x halfPi; translation 0 0 5 ])
                     |> set_shape_material wall_mat

    let mid_mat    = make_material_with_pattern [stripe_patt]
    let middle     = make_shape Sphere
                     |> set_shape_transform (translation -0.5 1.0 0.5)
                     |> set_shape_material mid_mat
    let right_mat  = make_material_with_pattern [grad_patt]
    let right      = make_shape Sphere
                     |> set_shape_transform (chain [scaling 0.5 0.5 0.5; translation 1.65 0.5 -0.5])
                     |> set_shape_material right_mat
    
    let ls = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let world = make_world [ls] [floor;wall; middle; right]
    let camera = make_camera 1024 768 (Math.PI/2.) |> set_camera_transform (view_transform (make_point 0 1.5 -5)
                                                                                           (make_point 0 1 0)
                                                                                           (make_vector 0 1 0))

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let final_canvas = render camera world
    canvas_to_ppm filepath final_canvas
    stopWatch.Stop()
    let time = Convert.ToInt32 (stopWatch.Elapsed.TotalMilliseconds / 1000.0)
    let min, sec = (time/60, time%60)
    printfn "Total time to render = %i:%02i m:s" min sec                                                                                     
    0

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

let EPSILON = 0.0001

let approx a b = if abs(a - b) < EPSILON then true else false

[<EntryPoint>]
let main argv = 
    let filepath = Path.Combine(Path.Combine(__SOURCE_DIRECTORY__, "canvas_" + DateTime.Now.ToString("yyyyMMdd_HH_mm_ss") + ".ppm"))
    let halfPi    = Math.PI/2.
    let quarterPi = halfPi/2.
    // Chapter 8 End
    let checkers   = make_pattern (Checkers(Color(0.15,0.15,0.15), Color(0.85,0.85,0.85)))
    let floor_mat  = make_material_with_pattern [checkers] |> override_reflective 0.25 //[stripes_1; set_patt_transform (rotation_y halfPi) stripes_1]
    let floor_material = make_material [checkers] 0.8 0.2 0.0 0.0 0.0 0.0 0.0
    let floor      = make_shape Plane 
                     |> set_shape_transform (chain [rotation_x halfPi; translation 0 0 10])
                     |> set_shape_material floor_material

    let wall_patt  = make_pattern (Stripes(grey, dark_grey)) 
                     |> set_patt_transform (chain [rotation_y (halfPi); scaling 0.75 0.75 0.75])
    let wall_mat   = make_material_with_pattern [wall_patt] |> override_ambient 0.05
    let back_wall  = make_shape Plane
                     |> set_shape_transform (chain [rotation_x halfPi; translation 0 0 5 ])
                     |> set_shape_material wall_mat
    let side_wall  = make_shape Plane
                     |> set_shape_transform (chain [rotation_x halfPi; rotation_y halfPi; translation -6 0 0 ])
                     |> set_shape_material wall_mat
    let mid_mat    = make_material (solid_pattern darkred) 0.2 0.9 0.45 10.0 0.0 0.0 1.0
        
    let middle     = make_shape Sphere
                     |> set_shape_transform (translation -0.5 1.0 0.5)
                     |> set_shape_material mid_mat
    
    let glass_ball = make_shape Sphere
                     |> set_shape_material (make_material (solid_pattern white) 0 0 0.9 300 0.9 0.9 1.5)

    let air_bubble = make_shape Sphere
                     |> set_shape_material (make_material (solid_pattern white) 0 0 0.9 300 0.9 0.9 1.0000034)
                     |> set_shape_transform (scaling 0.5 0.5 0.5)
    let ls = make_pointlight (make_point 2 10 -5) (Color(0.9, 0.9, 0.9))
    let world = make_world [ls] [floor; glass_ball]//;back_wall; side_wall; middle]
    let camera = make_camera 300 300 0.45 |> set_camera_transform (view_transform (make_point 0 0 -5)
                                                                                  (make_point 0 0 0)
                                                                                  (make_vector 0 1 0))

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let final_canvas = render camera world
    canvas_to_ppm filepath final_canvas
    stopWatch.Stop()
    let time = Convert.ToInt32 (stopWatch.Elapsed.TotalMilliseconds / 1000.0)
    let min, sec = (time/60, time%60)
    printfn "Total time to render = %i:%02i m:s" min sec                                                                                     
    0
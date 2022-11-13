
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
open World

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

//let determine_hit ray_origin wall_z pixel_size half light shape pixel: (Pixel * Color) option = 
//    let (x,y) = (get_x pixel, get_y pixel)
//    let (world_x, world_y) = (-half + pixel_size * float(x), half - pixel_size * float(y))
//    let pos = make_point world_x world_y wall_z
//    let r = make_ray ray_origin (normalize (pos - ray_origin))
//    let xs = intersect shape r
//    match hit xs with
//    | Some i -> 
//        let point = position r (t_val i)
//        let normal = normal_at (object i) point
//        let eye = -(direction r)
//        let c = lighting (extract_material (object i)) light point eye normal
//        Some((x, y), c)
//    | None -> None

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
    //let ray_origin = make_point 0 0 -5
    //let wall_z = 10
    //let wall_size = 7
    //let canvas_pixels = 640
    //let pixel_size = float(wall_size)/float(canvas_pixels)
    //let half = float(wall_size)/2.0

    //let canvas = make_canvas canvas_pixels canvas_pixels
    //let s = make_sphere 
    //let m = override_color (make_def_material) (Color(1, 0.2, 1))
    //let s' = s|> set_sphere_material m

    //let light_position = make_point -10 10 -10
    //let light_color = Color(1, 1, 1)
    //let light = make_pointlight light_position light_color
    //let shape = s' |> set_sphere_transform (chain [scaling 1 0.25 1]) //; shearing -0.75 0 0 0 0 0]) 
    //let hit_pixels = canvas 
    //                 |> get_all_pixels
    //                 |> List.map (determine_hit ray_origin wall_z pixel_size half light shape)
    //                 |> List.choose (fun x -> x)
    //let final_canvas = List.fold (fun canv pix_w_color -> write_pixel (fst pix_w_color) (snd pix_w_color) canv) canvas hit_pixels
    
    // Chapter 7 End
    //def material: (Color(1, 1, 1), 0.1, 0.9, 0.9, 200.0)
    let halfPi = Math.PI/2.
    let quarterPi = halfPi/2.
    let floor_material = make_material (Color(1,0.9,0.9)) 0.1 0.9 0.0 200.0
    let floor = make_sphere
                |> set_sphere_transform (scaling 10 0.01 10)
                |> set_sphere_material floor_material
    let left_wall = make_sphere
                    |> set_sphere_transform (chain [scaling 10 0.01 10; 
                                                    rotation_x halfPi;
                                                    rotation_y -quarterPi;
                                                    translation 0 0 5])
                    |> set_sphere_material floor_material
    let right_wall = make_sphere
                    |> set_sphere_transform (chain [scaling 10 0.01 10; 
                                                    rotation_x halfPi;
                                                    rotation_y quarterPi;
                                                    translation 0 0 5])
                    |> set_sphere_material floor_material
    let middle = make_sphere
                 |> set_sphere_transform (translation -0.5 1 0.5)
                 |> set_sphere_material (make_material (Color(0.1, 1, 0.5)) 0.1 0.7 0.3 200.0)
    let right = make_sphere
                |> set_sphere_transform (chain [scaling 0.5 0.5 0.5; translation 1.5 0.5 -0.5])
                |> set_sphere_material (make_material (Color(0.5, 1, 0.1)) 0.1 0.7 0.3 200.0)
    let left = make_sphere
               |> set_sphere_transform (chain [scaling 0.33 0.33 0.33; translation -1.5 0.33 -0.75])
               |> set_sphere_material (make_material (Color(1, 0.8, 0.1)) 0.1 0.7 0.3 200.0)
    
    let ls = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let world = make_world [ls] [floor; left_wall; right_wall; middle; left; right]
    let camera = make_camera 1024 768 (Math.PI/3.) |> set_camera_transform (view_transform (make_point 0 1.5 -5)
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

module RayTracer
open Canvas
open Color
open System
open System.IO
open Tuples

let EPSILON = 0.00001

let approx a b = if abs(a - b) < EPSILON then true else false

type Projectile = Tuple * Tuple
type Environment = Tuple * Tuple

let get_position (p: Projectile) = fst p
let get_velocity (p: Projectile) = snd p

let get_wind (e: Environment) = fst e
let get_gravity (e: Environment) = snd e

let tick (e: Environment) (p: Projectile): Projectile =
    let new_pos = (get_position p) + (get_velocity p)
    let new_velocity = get_velocity p + get_gravity e + get_wind e
    (new_pos, new_velocity)

let evolve (e: Environment) (p:Projectile) = 
    let rec evolve_aux (history: Tuple list) (p_i: Projectile): Tuple list = 
        if (get_position p_i).y <= 0 then
            printfn "Crashed at %A %A!" (get_position p_i).x (get_position p_i).y
            (get_position p_i)::history
        else 
            //printfn "Currently at %A %A moving at %A %A" (get_position p_i).x (get_position p_i).y (get_velocity p_i).x (get_velocity p_i).y
            evolve_aux ((get_position p_i) :: history) (tick e p_i)
    evolve_aux [] p |> List.rev

let display_path (path: Tuple list): Canvas = 
    let scale_x (window_min:int) (window_max:int) (x_min:double) (x_max:double) (x: double): int = 
        x/(x_max - x_min) * (double(window_max) - double(window_min)) |> Convert.ToInt32
    let scale_y (window_min: int) (window_max: int) (y_min: double) (y_max: double) (y: double): int =
        y/(y_max - y_min) * (double(window_max) - double(window_min)) |> Convert.ToInt32

    let max_x = List.maxBy (fun (e: Tuple) -> e.x) path |> fun t -> t.x * 1.01
    let min_x = List.minBy (fun (e: Tuple) -> e.x) path |> fun t -> t.x 
    let max_y = List.maxBy (fun (e: Tuple) -> e.y) path |> fun t -> t.y * 1.01
    let min_y = List.minBy (fun (e: Tuple) -> e.y) path |> fun t -> t.y

    let c = make_canvas 900 550
    let path_xy = List.map (fun (p:Tuple) -> (p.x, p.y)) path
    let path_pixels = List.map (fun (x, y) -> ((scale_x 0 900 min_x max_x x), 550 - (scale_y 0 550 min_y max_y y))) path_xy |> List.filter (fun p -> snd p < 550)// && fst p < 900)
    let red = Color(1,0,0)
    List.fold (fun canv pix -> write_pixel pix red canv) c path_pixels 

[<EntryPoint>]
let main argv = 

    let c = make_canvas 10 2 |> canvas_fill (Color(1, 0.8, 0.6))

    let filepath = Path.Combine(Path.Combine(__SOURCE_DIRECTORY__, "canvas_" + DateTime.Now.ToString("yyyyMMdd_HH_mm_ss") + ".ppm"))
    
    //make_canvas 10 2 |> canvas_fill (Color(1, 0.8, 0.6)) |> canvas_to_ppm path
   
    let wind = make_vector -0.035 0 0
    let gravity = make_vector 0 -0.1 0
    let start_pos = make_point 0 1 0
    let start_v = normalize (make_vector 1 1.8 0) * 11.25
    let e: Environment = (wind, gravity)
    let p_init: Projectile = (start_pos, start_v)
    let path = evolve e p_init 

    //printfn "%A" path
    display_path path |> canvas_to_ppm filepath
    0
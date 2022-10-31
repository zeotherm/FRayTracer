module Cannon
open System
open Tuples
open Canvas
open Color

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

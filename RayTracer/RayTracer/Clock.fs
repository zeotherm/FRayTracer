module Clock
open System
open Tuples
open Transforms
open Matrix
open Canvas
open Color

let display_clock (size: int): Canvas = 
    let scale_x (window_min:int) (window_max:int) (x_min:double) (x_max:double) (x: double): int = 
        x/(x_max - x_min) * (double(window_max) - double(window_min)) |> Convert.ToInt32
    let scale_y (window_min: int) (window_max: int) (y_min: double) (y_max: double) (y: double): int =
        y/(y_max - y_min) * (double(window_max) - double(window_min)) |> Convert.ToInt32

    let hours = [1. .. 12.]
    let up = make_point 0 1 0
    let f_size = float size
    let path = hours |> List.map (fun h -> chain [(rotation_z (-h*Math.PI/6.)); 
                                                   scaling (0.375*f_size) (0.375*f_size) 0;
                                                   translation (f_size/2.) (f_size/2.) 0 ])
                     |> List.map (fun x -> mat_tuple_mul x up) 
    let c = make_canvas size size
    let pixels  = path |> List.map (fun (p:Tuple) -> (p.x, p.y))
                       |> List.map (fun (x, y) -> ((scale_x 0 size 0 size x), size - (scale_y 0 size 0 size y)))
                       |> List.filter (fun p -> snd p < size)
    let red = Color(1,0,0)
    List.fold (fun canv pix -> write_pixel pix red canv) c pixels 


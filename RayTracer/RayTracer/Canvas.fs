module Canvas
open Color
open System
open System.IO
open System.Text

type Pixel = int * int
type Dimension = int * int
type Canvas = Dimension * Map<Pixel, Color> * int

let get_x (p: Pixel) = fst p
let get_y (p: Pixel) = snd p

let dim_height (d: Dimension) = snd d
let dim_width (d: Dimension) = fst d

let size (c: Canvas) = 
    let (s, _, _) = c
    s
let pixel_map (c: Canvas) = 
    let (_, pm, _) = c
    pm
let max_val (c: Canvas) = 
    let (_, _, mv) = c
    mv

let make_canvas w h : Canvas = 
    let d: Dimension = (w, h)
    let p_map = Map.empty<Pixel, Color>
    (d, p_map, 255)

let height (c: Canvas) = dim_height (size c)
let width (c: Canvas) = dim_width (size c)    

let write_pixel pixel color canvas: Canvas = 
    let pm = pixel_map canvas
    let dims = size canvas
    let max_col = max_val canvas
    if get_x pixel > dim_width dims - 1 || get_y pixel > dim_height dims - 1 || get_x pixel < 0 || get_y pixel < 0 then 
        raise (IndexOutOfRangeException("The requested pixel is out of bounds for the given canvas"))
    else
        if Map.containsKey pixel pm then
            (dims, pm |> Map.remove pixel |> Map.add pixel color, max_col)
        else
            (dims, Map.add pixel color pm, max_col)

let pixel_at p c =
    let pm = c |> pixel_map
    if Map.containsKey p pm then
        pm.[p]
    else
        Color(0,0,0)

let canvas_fill color canvas: Canvas =
    let xs = [0 .. (width canvas) - 1]
    let ys = [0 .. (height canvas) - 1]
    let all_pixels = xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))
    List.fold (fun can pix -> write_pixel pix color can) canvas all_pixels
    
let canvas_to_ppm_header (c: Canvas) : string list =
    ["P3"; (width c).ToString() + " " + (height c).ToString(); (max_val c).ToString()]

let canvas_to_ppm_data (can: Canvas) : string list = 
    let convert_color_to_scaled_int (color: Color): int list =
        let c_max = max_val can
        let clamp_to_bounds c =
            if c < 0 then 
                0
            else
                if c > c_max then
                    c_max
                else
                    c
        let red = color.red * double(c_max) |> System.Convert.ToInt32 
        let green = color.green * double(c_max) |> System.Convert.ToInt32
        let blue = color.blue * double(c_max) |> System.Convert.ToInt32
        [clamp_to_bounds red; clamp_to_bounds green; clamp_to_bounds blue]

    let width = width can
    // TODO: This can use some love to remove the mutable variable
    let write_row r: string = 
        let sb_row = StringBuilder()
        let mutable length = 0
        for c in 0 .. width - 1 do
            let colors = can |> pixel_at (c, r)
                             |> convert_color_to_scaled_int
                             |> List.map (fun i -> i.ToString())
            for color in colors do
                length <- length + color.Length + 1
                if length > 70 then
                    sb_row.AppendLine() |> ignore
                    length <- color.Length + 1
                    sb_row.Append(color + " ") |> ignore
                else
                    sb_row.Append(color + " ") |> ignore
        sb_row.ToString()

    let rec line_builder row results can : string list = 
        if row = height can then
            results
        else
            line_builder (row + 1) (write_row row :: results) can
    line_builder 0 [] can |> List.rev

let canvas_to_ppm_string (c: Canvas) : string = 
    let header = canvas_to_ppm_header c |> String.concat Environment.NewLine
    let body = canvas_to_ppm_data c |> String.concat Environment.NewLine
    header + "\n" + body

let canvas_to_ppm (filename: string) (c: Canvas) = 
    use file = File.CreateText(filename)
    file.WriteLine("{0}", (canvas_to_ppm_string c))
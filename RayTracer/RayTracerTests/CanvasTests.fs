module CanvasTests

open NUnit.Framework
open Canvas
open Color
open System
open System.IO

[<Test>]
let CreateCanvasTest () = 
    let c = make_canvas 10 20
    let height = height c
    let width = width c
    Assert.That(height, Is.EqualTo(20))
    Assert.That(width, Is.EqualTo(10))
    for h in 0 .. height - 1 do
        for w in 0 .. width - 1 do
            Assert.That(pixel_at (h, w) c, Is.EqualTo(Color(0,0,0)))

[<Test>]
let WritePixelTest () =
    let c = make_canvas 10 20
    let red = Color(1,0,0)
    let p: Pixel = (2, 3)
    let mod_c = write_pixel p red c
    Assert.That(pixel_at p mod_c, Is.EqualTo(red))

[<Test>]
let PPMHeaderTest () =
    let c = make_canvas 5 3
    let path = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".txt")
    canvas_to_ppm path c
    let expected = ["P3"; "5 3"; "255"]
    let res = System.IO.File.ReadLines(path) 
              |> Seq.take 3 
              |> Seq.map (fun l -> l.TrimEnd()) 
              |> List.ofSeq
    Assert.That(res, Is.EqualTo(expected))
    File.Delete(path)


[<Test>]
let PPMBodyTest () =
    let c = make_canvas 5 3
    let c1 = Color(1.5,0,0)
    let c2 = Color(0, 0.5, 0)
    let c3 = Color(-0.5,0,1)
    let mod_c = c |> write_pixel (0,0) c1 |> write_pixel (2,1) c2 |> write_pixel (4, 2) c3
    
    let path = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".txt")
    
    canvas_to_ppm path mod_c
    
    let res = System.IO.File.ReadLines(path) 
              |> Seq.skip 3
              |> Seq.take 3
              |> Seq.map (fun l -> l.TrimEnd())
              |> List.ofSeq

    let expected = ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0";
                    "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0";
                    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
    Assert.That(res, Is.EqualTo(expected))
    File.Delete(path)

[<Test>]
let PPMBodyLongLineTest () = 
    let c = make_canvas 10 2
    let c_mod = canvas_fill (Color(1, 0.8, 0.6)) c
    let path = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".txt")
    
    canvas_to_ppm path c_mod

    let res = System.IO.File.ReadLines(path)
              |> Seq.skip 3
              |> Seq.take 4
              |> Seq.map (fun l -> l.TrimEnd())
              |> List.ofSeq

    let expected = ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204";
                    "153 255 204 153 255 204 153 255 204 153 255 204 153";
                    "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204";
                    "153 255 204 153 255 204 153 255 204 153 255 204 153"]
    
    Assert.That(res, Is.EqualTo(expected))
    File.Delete(path)

                

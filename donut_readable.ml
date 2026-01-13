(* based on the linux donut *)
let ascii_width  : int = 80
let ascii_height : int = 24
let center_x     : float = float_of_int (ascii_width / 2)
let center_y     : float = float_of_int (ascii_height / 2)

let theta_spacing : float = 0.07
let phi_spacing   : float = 0.02
let r1 : float = 0.25
let r2 : float = 0.5
let k2 : float = 35.0

let k1 : float = 500.0
let change_a = 0.0114592
let change_b = 0.0084567
let luminance_to_char (l: float) : char =
  (* gradient: darkest to brightest *)
  let ramp = ".,-~:;=!*#$@" in
  let n = String.length ramp in
  let l = if l < 0.0 then 0.0 else if l > 1.0 then 1.0 else l in
  let idx = int_of_float (l *. float_of_int (n - 1)) in
  ramp.[idx]

let render_frame (a: float) (b: float) : string =
  (* screen and zbuffer *)
  let output  = Array.make (ascii_width * ascii_height) ' ' in
  let zbuffer = Array.make (ascii_width * ascii_height) 0.0 in

  let cos_a = cos a in
  let sin_a = sin a in
  let cos_b = cos b in
  let sin_b = sin b in

  let rec theta_loop theta =
    if theta < 6.28 then begin
      let cos_theta = cos theta in
      let sin_theta = sin theta in

      let rec phi_loop phi =
        if phi < 6.28 then begin
          let cos_phi = cos phi in
          let sin_phi = sin phi in

          let obj_x = r2 +. r1 *. cos_theta in
          let obj_y = r1 *. sin_theta in

          (* final 3D coords *)
          let x =
            obj_x *. (cos_b *. cos_phi +. sin_a *. sin_b *. sin_phi)
            -. obj_y *. cos_a *. sin_b
          in
          let y =
            obj_x *. (sin_b *. cos_phi -. sin_a *. cos_b *. sin_phi)
            +. obj_y *. cos_a *. cos_b
          in

          let ooz = 1.0 /. (k2 +. cos_a *. obj_x *. sin_phi +. sin_a *. obj_y) in

          (* project to screen *)
          let xp = center_x +. k1 *. ooz *. x in
          let yp = center_y -. k1 *. ooz *. y in

          (* lighting *)
          let luminance =
            0.7
            *. (cos_phi *. cos_theta *. sin_b
                -. cos_a *. cos_theta *. sin_phi
                -. sin_a *. sin_theta
                +. cos_b *. (cos_a *. sin_theta -. cos_theta *. sin_a *. sin_phi))
          in

          (* plot if on screen and facing us *)
          if luminance > 0.0 then begin
            let xi = int_of_float xp in
            let yi = int_of_float yp in
            if xi >= 0 && xi < ascii_width && yi >= 0 && yi < ascii_height then begin
              let idx = xi + yi * ascii_width in
              (* zbuffer: keep nearest point (bigger ooz = closer) *)
              if ooz > zbuffer.(idx) then begin
                zbuffer.(idx) <- ooz;
                output.(idx) <- luminance_to_char luminance;
              end
            end
          end;

          phi_loop (phi +. phi_spacing)
        end
      in

      phi_loop 0.0;
      theta_loop (theta +. theta_spacing)
    end
  in

  theta_loop 0.0;

  (* convert output buffer into lines *)
  let buf = Bytes.create (ascii_height * (ascii_width + 1)) in
  for y = 0 to ascii_height - 1 do
    for x = 0 to ascii_width - 1 do
      Bytes.set buf (y * (ascii_width + 1) + x) output.(x + y * ascii_width)
    done;
    Bytes.set buf (y * (ascii_width + 1) + ascii_width) '\n'
  done;
  Bytes.to_string buf;;

for i = 0 to 10000 do
  print_string "\027[H";
  flush stdout;
  print_string (render_frame(change_a *. float_of_int(i)) (change_b *. float_of_int(i)));
done

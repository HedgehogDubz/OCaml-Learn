

(*u is cos b v is sin b*)
                            let aw = 80 in
              let ah,cx,cy,ts,ps=24,40.0,12.0,0.07,0.02 in
        let r1,r2,k1,k2,ca,cb=0.25,0.5,500.0,35.0,0.011,0.0084 in
      let rf a b = let o=Array.make(80 * 24) ' 'in let ca =cos a in
    let sa,u,v=sin a,cos b,sin b in let z=Array.make(80 * 24) 0.0 in
  let rec tl t = if t < 6.28 then begin let ct , st = cos t , sin t in
  let rec pl p = if p < 6.28 then begin let cp , sp = cos p , sin p in
  let j l = let l = if l<0.0 then 0.0 else if l>1.0 then 1.0 else l in
let dx = int_of_float (l *. 11.0) in let r = ".,-~:;=!*#$@" in r.[dx] in
let ox = r2 +. r1 *.ct in                  let oypos = r1 *. st *.1.0 in
let x=ox*.(u *.cp +.sa                        *.v *.sp)-.oypos*.ca*.v in
let y=ox*.(v*.cp -.sa                          *.u*.sp)+.oypos*.ca*.u in
let oz=1.0/.(k2 +. ca                          *.ox *. sp +.sa*.oypos)in
let xp, yp=cx+.k1*.oz                          *.x,cy-. k1 *. oz *. y in
let l=0.7*. (cp *. ct                          *. v-. ca*.ct*.sp-.sa*.st
+.u*.(ca*.st-.ct*.  sa                        *. sp)) in (if l > 0.0 then
let xi = int_of_float (xp)                  in let yi=int_of_float yp in
if xi>=0 && xi < aw&&yi >= 0 && yi < ah then begin let ix = xi + yi * aw
  in if oz> z.(ix) then begin z.(ix) <- oz; o.(ix) <- j l; end end);
  pl (p+.ps) end in pl 0.0; tl (t+.ts) end in tl 0.0; let b =Bytes.create
  (ah * (aw + 1)) in for y = 0 to ah-1 do for x = 0 to aw - 1 do Bytes
    .set b (y * (aw + 1) + x) o.(x + y * aw)done; Bytes.set b (y * (
      aw + 1) + aw) '\n' done;Bytes.to_string b in for i=0 to 10000
        do print_string "\027[H" ; flush stdout ; print_string(rf
              (ca *. float_of_int i) (cb *. float_of_int i))
                                ;done


        
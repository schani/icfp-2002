
type pos = int

let shift = 10


let get_x i = i land ((1 lsl shift) - 1)
let get_y i = i lsr shift

let inc_x i = i + 1
let inc_y i = i + (1 lsl shift)

let dec_x i = i - 1
let dec_y i = i - (1 lsl shift)

let validP p = 
  let x = get_x p
  and y = get_y p in
    x >= 0 && x <= 1023 && y >= 0 && y <= 1023

let knownP p = 
  p <> max_int

let make_pos x y =
  (y lsl shift) lor x

let make_unknown () =
  max_int

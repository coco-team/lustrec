let debug = false

let log ?debug:(dbg=false) f =
  if debug || dbg then
    Format.printf "%t" f
  else
    ()

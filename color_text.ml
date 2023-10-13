let color_text color_code text =
  Format.sprintf "\027[%dm%s\027[0m" color_code text


let danger = color_text 31

let warning = color_text 33

let info = color_text 34

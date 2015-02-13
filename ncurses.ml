open Ctypes
open Foreign

type window = unit ptr
let window : window typ = ptr void

let initscr =
    foreign "initscr" (void @-> (returning window))

let endwin =
    foreign "endwin" (void @-> (returning void))

let refresh =
    foreign "refresh" (void @-> (returning void))

let wrefresh =
    foreign "wrefresh" (window @-> (returning void))

let newwin =
    foreign "newwin" (int @-> int @-> int @-> int @-> (returning window))

let addch =
    foreign "addch" (char @-> (returning void))

let mvwaddch =
    foreign "mvwaddch" (window @-> int @-> int @-> char @-> (returning void))

let addstr =
    foreign "addstr" (string @-> (returning void))

let mvwaddstr =
    foreign "mvwaddstr" (window @-> int @-> int @-> string @-> (returning void))

let box =
    foreign "box" (window @-> int @-> int @-> (returning void))

let cbreak =
    foreign "cbreak" (void @-> (returning void))

let wgetch = foreign "wgetch" (window @-> returning int)

let keypad = foreign "keypad" (window @-> int @-> returning void)

let wattron = foreign "wattron" (window @-> int @-> returning void)
let wattroff = foreign "wattroff" (window @-> int @-> returning void)

let clear = foreign "clear" (void @-> returning void)
let noecho = foreign "noecho" (void @-> returning void)

let curs_set = foreign "curs_set" (int @-> returning void)

let start_color = foreign "start_color" (void @-> returning void)
let init_pair = foreign "init_pair" (int @-> int @-> int @-> returning void)

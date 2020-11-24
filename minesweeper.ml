open Core
open Ncurses

(* constants copy-pasted from C++ output *)
let color_black = 0
let color_red = 1
let color_green = 2
let color_yellow = 3
let color_white = 7

let a_reverse = 262144

type color = WHITE | RED | YELLOW
let color_of_int = function
  | 1 -> WHITE
  | 2 -> RED
  | 3 -> YELLOW
  | _ -> WHITE
let int_of_color = function
  | WHITE -> 1
  | RED -> 2
  | YELLOW -> 3
let color_pair_of_color = function
  (* constants copy/pasted from C++ output *)
  | WHITE -> 256
  | RED -> 512
  | YELLOW -> 768

type square =
  | HIDDEN
  | HIDDEN_MINE
  | FLAG
  | FLAG_MINE
  | VISIBLE_MINE
  | VISIBLE of int

let ok board r c =
  0 <= r && r < Array.length board && 0 <= c && c < Array.length board.(0)

let count_with board r c fn =
  let ans = ref 0 in
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      let r2 = r + dr in
      let c2 = c + dc in
      if ok board r2 c2 then
        ans := !ans + (fn board.(r2).(c2))
    done
  done;
  !ans

let count_mines board r c =
  count_with board r c (fun x -> match x with
  | HIDDEN_MINE | VISIBLE_MINE | FLAG_MINE -> 1
  | HIDDEN | VISIBLE _ | FLAG -> 0)

let count_flags board r c =
  count_with board r c (fun cell -> match cell with
  | FLAG_MINE | FLAG -> 1
  | HIDDEN_MINE | VISIBLE_MINE | HIDDEN | VISIBLE _ -> 0)

let rec place_mines board mines =
  if mines=0 then ()
  else
    let rows = Array.length board in
    let cols = Array.length board.(0) in
    let r = Random.int rows in
    let c = Random.int cols in
    match board.(r).(c) with
    | HIDDEN ->
        board.(r).(c) <- HIDDEN_MINE;
        place_mines board (mines-1)
    | HIDDEN_MINE -> place_mines board mines
    | VISIBLE _ | VISIBLE_MINE | FLAG | FLAG_MINE -> assert false

let rec reveal_helper board r c =
  match board.(r).(c) with
  | HIDDEN_MINE -> board.(r).(c) <- VISIBLE_MINE
  | HIDDEN ->
    let n = count_mines board r c in
    board.(r).(c) <- VISIBLE n;
    if n = count_flags board r c then
      reveal_around board r c
  | FLAG | FLAG_MINE | VISIBLE _ | VISIBLE_MINE -> ()
and reveal_around board r c =
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      let r2 = r + dr in
      let c2 = c + dc in
      if ok board r2 c2 then
        reveal_helper board r2 c2
    done
  done

let flag_square board r c =
  let () =
    match board.(r).(c) with
    | HIDDEN -> board.(r).(c) <- FLAG
    | HIDDEN_MINE -> board.(r).(c) <- FLAG_MINE
    | FLAG -> board.(r).(c) <- HIDDEN
    | FLAG_MINE -> board.(r).(c) <- HIDDEN_MINE
    | VISIBLE_MINE | VISIBLE _ -> ()
  in
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      let rr = r+dr in
      let cc = c+dc in
      if ok board rr cc then
        match board.(rr).(cc) with
        | VISIBLE n ->
          if n = count_flags board rr cc then
            reveal_around board rr cc
        | HIDDEN_MINE | HIDDEN | FLAG | FLAG_MINE | VISIBLE_MINE -> ()
    done
  done

let reveal_square board r c =
  match board.(r).(c) with
  | VISIBLE _ | VISIBLE_MINE ->
    if count_mines board r c = count_flags board r c then
      reveal_around board r c
  | _ -> reveal_helper board r c

let show_board board_win board rr cc =
  let rows = Array.length board in
  let cols = Array.length board.(0) in
  let string_of_cell r c =
    match board.(r).(c) with
    | HIDDEN | HIDDEN_MINE -> "."
    | FLAG | FLAG_MINE -> "?"
    | VISIBLE_MINE -> "!"
    | VISIBLE n -> string_of_int (n - count_flags board r c)
  in
  let color_of_cell r c =
    match board.(r).(c) with
    | VISIBLE_MINE -> RED
    | FLAG | FLAG_MINE -> YELLOW
    | HIDDEN | HIDDEN_MINE | VISIBLE _ -> WHITE
  in
  let show_cell r c ch color highlight =
    if highlight then wattron board_win a_reverse;
    wattron board_win (color_pair_of_color color);
    mvwaddstr board_win (r+1) (c+2) ch;
    wattroff board_win (color_pair_of_color color);
    if highlight then wattroff board_win a_reverse;
  in
  box board_win 0 0;
  for r = 0 to rows-1 do
    for c = 0 to cols-1 do
      show_cell r c (string_of_cell r c) (color_of_cell r c) (r=rr && c=cc)
    done
  done;
  wrefresh board_win

let main rows cols mines () =
  Random.self_init ();
  let board = Array.make_matrix ~dimx:rows ~dimy:cols HIDDEN in
  let _ = place_mines board mines in
  let r = ref 0 in
  let c = ref 0 in
  let main_window = initscr () in
  clear ();
  noecho ();
  cbreak ();
  curs_set 0;
  start_color ();
  init_pair (int_of_color WHITE) color_white color_black;
  init_pair (int_of_color RED) color_red color_black;
  init_pair (int_of_color YELLOW) color_yellow color_black;
  let board_win = newwin (rows+2) (cols+4) 2 0 in
  keypad board_win 0 (* true *);
  mvwaddstr main_window 0 0 "Arrow keys to move. Space to reveal a square. Enter to flag.";
  refresh ();
  while true do
    show_board board_win board !r !c;
    let mines_left =
      let ans = ref 0 in
      let rows = Array.length board in
      let cols = Array.length board.(0) in
      for r = 0 to rows-1 do
        for c = 0 to cols-1 do
          match board.(r).(c) with
          | HIDDEN_MINE -> ans := (!ans + 1)
          | HIDDEN | FLAG | FLAG_MINE | VISIBLE_MINE | VISIBLE _ -> ()
        done
      done;
      !ans
    in
    let mines_str =
      if mines_left = 0 then
        "You win!"
      else
        (string_of_int mines_left);
    in
    mvwaddstr main_window 1 0 mines_str;
    let ch = wgetch board_win in
    refresh ();
    (match ch with
    | 65 -> r := (!r + rows - 1) % rows (* up *)
    | 66 -> r := (!r + 1) % rows (* down *)
    | 68 -> c := (!c + cols - 1) % cols (* left *)
    | 67 -> c := (!c + 1) % cols (* right *)
    | 32 -> reveal_square board !r !c (* space *)
    | 10 -> flag_square board !r !c (* enter *)
    | _ -> ()
    );
  done

let command =
  Command.basic ~summary:"Play Minesweeper"
    ~readme:(fun () -> "More detailed information")
		Command.Let_syntax.(
			let%map_open r = flag "-r" (optional_with_default 16 int) ~doc:"Rows"
			and c = flag "-c" (optional_with_default 16 int) ~doc:"Columns"
		  and m = flag "-m" (optional_with_default 40 int) ~doc:"Mines"
		  in fun () -> main r c m ())

let () = Command.run command

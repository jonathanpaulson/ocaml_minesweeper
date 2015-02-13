open Core.Std
open Ncurses

let color_black = 0
let color_red = 1
let color_green = 2
let color_yellow = 3
let a_reverse = 0x00200000


type square =
  | HIDDEN
  | HIDDEN_MINE
  | FLAG
  | FLAG_MINE
  | VISIBLE_MINE
  | VISIBLE of int

let ok board r c =
  0 <= r && r < Array.length board && 0 <= c && c < Array.length board.(0)

let count_mines board r c =
  let ans = ref 0 in
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      let r2 = r + dr in
      let c2 = c + dc in
      if ok board r2 c2 then
        match board.(r2).(c2) with
        | HIDDEN_MINE | VISIBLE_MINE | FLAG_MINE -> ans := !ans + 1
        | HIDDEN | VISIBLE _ | FLAG -> ()
    done
  done;
  !ans

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

let flag_square board r c =
  match board.(r).(c) with
  | HIDDEN -> board.(r).(c) <- FLAG
  | HIDDEN_MINE -> board.(r).(c) <- FLAG_MINE
  | FLAG -> board.(r).(c) <- HIDDEN
  | FLAG_MINE -> board.(r).(c) <- HIDDEN_MINE
  | VISIBLE_MINE | VISIBLE _ -> ()

let rec reveal_square board r c =
  match board.(r).(c) with
  | HIDDEN_MINE -> board.(r).(c) <- VISIBLE_MINE
  | FLAG | FLAG_MINE -> ()
  | VISIBLE _ | VISIBLE_MINE -> ()
  | HIDDEN ->
      let n = count_mines board r c in
      board.(r).(c) <- VISIBLE n;
      if n=0 then
        for dr = -1 to 1 do
          for dc = -1 to 1 do
            let r2 = r + dr in
            let c2 = c + dc in
            if ok board r2 c2 then
              reveal_square board r2 c2
          done
        done

let show_board board_win board rr cc =
  let rows = Array.length board in
  let cols = Array.length board.(0) in
  let string_of_cell r c =
    match board.(r).(c) with
    | HIDDEN | HIDDEN_MINE -> "."
    | FLAG | FLAG_MINE -> "?"
    | VISIBLE_MINE -> "!"
    | VISIBLE n -> string_of_int n
  in
  box board_win 0 0;
  for r = 0 to rows-1 do
    for c = 0 to cols-1 do
      if r=rr && c=cc then
        (wattron board_win a_reverse;
        mvwaddstr board_win (r+1) (c+2) (string_of_cell r c);
        wattroff board_win a_reverse)
      else
        mvwaddstr board_win (r+1) (c+2) (string_of_cell r c);
    done
  done;
  wrefresh board_win

let main rows cols mines () =
  let board = Array.make_matrix ~dimx:rows ~dimy:cols HIDDEN in
  let _ = place_mines board mines in
  let r = ref 0 in
  let c = ref 0 in
  let main_window = initscr () in
  clear ();
  noecho ();
  cbreak ();
  curs_set 0;
  let board_win = newwin (rows+2) (cols+4) 2 0 in
  keypad board_win 0 (* true *);
  mvwaddstr main_window 0 0 "Use arrow keys to go up and down. Press enter to select";
  refresh ();
  while true do
    show_board board_win board !r !c;
    let ch = wgetch board_win in
    mvwaddstr main_window 1 0 (string_of_int ch);
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

let () =
  Command.basic ~summary:"Play Minesweeper"
    Command.Spec.(
      empty
      +> flag "-r" (optional_with_default 16 int) ~doc:"Rows"
      +> flag "-c" (optional_with_default 16 int) ~doc:"Columns"
      +> flag "-m" (optional_with_default 40 int) ~doc:"Mines"
    )
    main
  |> Command.run

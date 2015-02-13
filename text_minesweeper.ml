open Core.Std

type square =
  | HIDDEN
  | HIDDEN_MINE
  | VISIBLE_MINE
  | VISIBLE of int

let ok board r c =
  0 <= r && r < Array.length board && 0 <= c && c < Array.length board.(0)

let show_board (board : square array array) : unit =
  let rows = Array.length board in
  let cols = Array.length board.(0) in
  for r = 0 to rows-1 do
    for c = 0 to cols-1 do
      match board.(r).(c) with
      | HIDDEN | HIDDEN_MINE -> printf "."
      | VISIBLE_MINE -> printf "!"
      | VISIBLE n -> printf "%d" n
    done;
    printf "\n";
  done;
  printf "\n"

let count_mines board r c =
  let ans = ref 0 in
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      let r2 = r + dr in
      let c2 = c + dc in
      if ok board r2 c2 then
        match board.(r2).(c2) with
        | HIDDEN_MINE | VISIBLE_MINE -> ans := !ans + 1
        | HIDDEN | VISIBLE _ -> ()
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
    | VISIBLE _ | VISIBLE_MINE -> assert false

let rec reveal_square board r c =
  match board.(r).(c) with
  | HIDDEN_MINE -> board.(r).(c) <- VISIBLE_MINE
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

let main rows cols mines () =
  let board = Array.make_matrix ~dimx:rows ~dimy:cols HIDDEN in
  let _ = place_mines board mines in

  printf "r=%d c=%d m=%d\n" rows cols mines;
  while true do
    show_board board;
    printf ">>> %!";
    let line = read_line () in
    Scanf.sscanf line "%d %d" (fun r c ->
      reveal_square board r c)
  done

let () =
  Command.basic ~summary:"Play Minesweeper"
    Command.Spec.(
      empty
      +> flag "-r" (optional_with_default 8 int) ~doc:"Rows"
      +> flag "-c" (optional_with_default 8 int) ~doc:"Columns"
      +> flag "-m" (optional_with_default 10 int) ~doc:"Mines"
    )
    main
  |> Command.run

(*
  Out_channel.output_string stdout "Pick a timezone: ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> failwith "No timezone provided"
  | Some zone_string ->
      let zone = Core.Zone.find_exn zone_string in
      let time_string = Time.to_string_abs (Time.now ()) ~zone in
      printf "The time in %s is %s.\n%!" (Core.Zone.to_string zone) time_string
*)

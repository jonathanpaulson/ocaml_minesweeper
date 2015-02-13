# Minesweeper
A terminal version of Minesweeper.

Written as an exercise to learn OCaml! Uses the ncurses library for the UI.

Should work in OS X after following [these instructions](https://github.com/realworldocaml/book/wiki/Installation-Instructions) to install OCaml.

Build:
<pre>
corebuild -pkg ctypes.foreign -lflags -cclib,-lncurses minesweeper.native
</pre>

Play:
<pre>
./minesweeper.native # 16x16 board with 40 mines
./minesweeper.native -r 9 -c 9 -m 10
./minesweeper.native -r 30 -c 16 -m 99
</pre>

let rec run = function
  | "" | "run" -> cmd "dune exec -- aoc_2025"
  | "build" -> cmd "dune build --release"
  | "time" -> 
      run "build";
      cmd "time _build/install/default/bin/aoc_2025"
  | _ -> echo "unknown command"; exit 404

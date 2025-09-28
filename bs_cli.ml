open Bs

let usage () =
  Printf.eprintf "Usage:\n  %s price|greeks call|put S K r t sigma [--theta-per-day]\n"
    Sys.argv.(0);
  exit 1

let parse_kind = function
  | "call" -> Call
  | "put"  -> Put
  | x ->
      Printf.eprintf "kind must be call|put, got: %s\n" x;
      usage ()

let parse_float name s =
  try float_of_string s with _ ->
    Printf.eprintf "Bad %s: %s\n" name s; usage ()

let () =
  let argv = Array.to_list Sys.argv in
  let per_day = List.exists ((=) "--theta-per-day") argv in
  (* Remove the optional flag, keep the 7 positional args *)
  let args = List.filter (fun x -> x <> "--theta-per-day") (List.tl argv) in
  match args with
  | [mode; kind_s; s_s; k_s; r_s; t_s; sigma_s] ->
      let mode  = String.lowercase_ascii mode in
      let kind  = parse_kind (String.lowercase_ascii kind_s) in
      let s     = parse_float "S" s_s in
      let k     = parse_float "K" k_s in
      let r     = parse_float "r" r_s in
      let t     = parse_float "t" t_s in
      let sigma = parse_float "sigma" sigma_s in
      begin match mode with
      | "price" ->
          let p = Bs.price ~kind ~s ~k ~r ~sigma ~t in
          Printf.printf "Price = %.10f\n" p
      | "greeks" ->
          let g = Bs.greeks ~kind ~s ~k ~r ~sigma ~t in
          let theta = if per_day then g.theta /. 365.0 else g.theta in
          Printf.printf
            "Delta = %.10f\nGamma = %.10f\nVega(σ=1.00) = %.10f\nTheta%s = %.10f\nRho(Δr=1.00) = %.10f\n"
            g.delta g.gamma g.vega (if per_day then "/day" else "") theta g.rho
      | x ->
          Printf.eprintf "Unknown mode: %s\n" x; usage ()
      end
  | _ -> usage ()

(*
  Black–Scholes pricer in OCaml (no dividends)
  - Prices European calls/puts
  - Greeks: Delta, Gamma, Vega, Theta, Rho
*)

type opt = Call | Put

let pi = 4.0 *. atan 1.0

let pdf x =
  let z = x *. x in
  exp (-. 0.5 *. z) /. sqrt (2.0 *. pi)

(* Abramowitz & Stegun 7.1.26 approximation of erf -> Normal CDF *)
let cdf z =
  let sign = if z < 0.0 then -1.0 else 1.0 in
  let x = abs_float z /. sqrt 2.0 in
  let t = 1.0 /. (1.0 +. 0.3275911 *. x) in
  let a1, a2, a3, a4, a5 =
    0.254829592, (-0.284496736), 1.421413741, (-1.453152027), 1.061405429
  in
  let y = (((((a5 *. t +. a4) *. t +. a3) *. t +. a2) *. t +. a1) *. t) in
  let erf_approx = 1.0 -. y *. exp (-. x *. x) in
  0.5 *. (1.0 +. sign *. erf_approx)

let d1 ~s ~k ~r ~sigma ~t =
  (log (s /. k) +. (r +. 0.5 *. sigma *. sigma) *. t) /. (sigma *. sqrt t)

let d2 ~s ~k ~r ~sigma ~t =
  d1 ~s ~k ~r ~sigma ~t -. sigma *. sqrt t

let price ~kind ~s ~k ~r ~sigma ~t =
  if sigma <= 0.0 || t <= 0.0 then
    (* expiry/zero-vol limit: intrinsic discounted *)
    match kind with
    | Call -> max 0.0 (s -. k *. exp (-. r *. t))
    | Put  -> max 0.0 (k *. exp (-. r *. t) -. s)
  else
    let d1v = d1 ~s ~k ~r ~sigma ~t in
    let d2v = d2 ~s ~k ~r ~sigma ~t in
    match kind with
    | Call -> s *. cdf d1v -. k *. exp (-. r *. t) *. cdf d2v
    | Put  -> k *. exp (-. r *. t) *. cdf (-. d2v) -. s *. cdf (-. d1v)

(* Delta : change in contract price per 1.00 change in underlying price *)
(* Gamma : change in delta per 1.00 change in underlying price *)
(* Vega  : change in contract price per 1.00 change in volatility (i.e., per 1.00 = 100% vol) *)
(* Theta : change in contract price per 1 year passage of time *)
(* Rho   : change in contract price per 1.00 change in risk-free rate *)
type greeks = { delta: float; gamma: float; vega: float; theta: float; rho: float }

let greeks ~kind ~s ~k ~r ~sigma ~t =
  let d1v = d1 ~s ~k ~r ~sigma ~t in
  let d2v = d2 ~s ~k ~r ~sigma ~t in
  let pdf_d1 = pdf d1v in
  let cdf_d1 = cdf d1v in
  let cdf_d2 = cdf d2v in
  let discount = exp (-. r *. t) in

  let gamma = pdf_d1 /. (s *. sigma *. sqrt t) in
  let vega  = s *. pdf_d1 *. sqrt t in
  let theta_core = (-. (s *. pdf_d1 *. sigma) /. (2.0 *. sqrt t)) in

  match kind with
  | Call ->
      {
        delta = cdf_d1;
        gamma;
        vega;
        theta = theta_core -. r *. k *. discount *. cdf_d2;
        rho   = k *. t *. discount *. cdf_d2;
      }
  | Put ->
      {
        delta = cdf_d1 -. 1.0;
        gamma;
        vega;
        theta = theta_core +. r *. k *. discount *. cdf (-. d2v);
        rho   = -. k *. t *. discount *. cdf (-. d2v);
      }

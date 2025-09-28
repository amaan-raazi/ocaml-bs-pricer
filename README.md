# Black–Scholes Pricer (OCaml)

Simple OCaml library + CLI for European option pricing with the Black–Scholes model ( assuming no dividends).  

## Build
```bash
dune build
```

## Usage
bs_cli price|greeks call|put S K r t sigma [--theta-per-day]  

price|greeks: Mode  
call|put: Option kind  
S: Spot price  
K: Strike price  
r: Risk-free rate (annual and continuously compounded, eg 0.01 = 1%)  
t: Time to expiry (in years)  
sigma: annual volatility (e.g. 0.2 = 20%)  

--theta-per-day: optional flag for daily theta, default is yearly  

# Greeks  
Delta: Change in contract price per 1.00 change in underlying price  
Gamma Change in delta per 1.00 change in underlying price  
Vega: Change in contract price per 1.00 change in volatility (i.e., per 1.00 = 100% vol)  
Theta: Change in contract price per 1 year passage of time  
Rho: Change in contract price per 1.00 change in risk-free rate  
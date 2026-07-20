# Cachio

Cachio is an OCaml simulation project for running games, generating balance
reports, and comparing rule changes.

## Requirements

- OCaml
- Dune `3.20` or newer
- opam dependencies from `cachio.opam`

Before running commands in a shell, load the opam environment:

```sh
eval $(opam env)
```

## Run A Simulation Report

Run one simulation:

```sh
dune exec cachio -- run
```

Run several simulations:

```sh
dune exec cachio -- run 1000
```

For backward compatibility, a numeric argument without a subcommand also runs
the standard report:

```sh
dune exec cachio -- 1000
```

The report is printed to stdout.

## Create A Balance Audit

An audit runs simulations with a fixed seed and writes both a text report and a
JSON report.

```sh
dune exec cachio -- audit \
  --name baseline \
  --simulations 1000 \
  --seed 0 \
  --scenario deterministic
```

By default, reports are written to:

```text
reports/balance/
```

Use `--out` to choose another directory:

```sh
dune exec cachio -- audit \
  --name candidate \
  --simulations 1000 \
  --seed 0 \
  --scenario deterministic \
  --out /tmp/cachio-audits
```

Available scenarios:

- `deterministic`
- `scripted`

Generated audit files are named with the timestamp, audit name, seed, and number
of simulations:

```text
2026-07-20T14-01-18Z_baseline_seed-0_n-1000.txt
2026-07-20T14-01-18Z_baseline_seed-0_n-1000.json
```

## Compare Two Audits

Compare a baseline JSON report with a candidate JSON report:

```sh
dune exec cachio -- compare \
  reports/balance/baseline.json \
  reports/balance/candidate.json
```

The comparison prints the most important balance deltas, including:

- initial draw dependency
- worst initial draw win ratio
- best initial draw win ratio
- initial draw win-rate spread
- goals per game
- home win ratio
- strategy win-rate confidence intervals
- strategy goals per action

## Suggested Balance Workflow

1. Create a baseline before changing rules:

   ```sh
   dune exec cachio -- audit --name baseline --simulations 1000 --seed 0
   ```

2. Change one rule or balancing parameter.

3. Create a candidate audit with the same simulation count and seed:

   ```sh
   dune exec cachio -- audit --name candidate --simulations 1000 --seed 0
   ```

4. Compare the two JSON reports:

   ```sh
   dune exec cachio -- compare BASELINE.json CANDIDATE.json
   ```

5. If the result looks promising, confirm with additional seeds.

## Tests

Run the test suite:

```sh
eval $(opam env)
dune test
```


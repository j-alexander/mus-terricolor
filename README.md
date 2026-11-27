# mus-terricolor

`mus-terricolor` is a conflict-driven clause learning (CDCL) solver for Boolean
satisfiability (SAT). It reads problems in DIMACS CNF format and searches for a
truth assignment that satisfies every clause.

## Design

Each clause is represented as an ordered array of signed integer literals. The
solver keeps those clause arrays in their original order and does not rewrite
them as assignments are made. Instead, it represents progress separately:

- An assignment records each variable's value, a reason clause for implied
  values, and a trail of decisions and implications.
- Clauses are associated with the variables they currently watch. When a
  watched variable is assigned, the solver checks the clause against the new
  assignment and looks for another unassigned literal to watch.
- If a clause is already satisfied, it needs no further work. If it has one
  remaining unassigned literal, that literal is queued as an implication. If
  it has no satisfying or unassigned literal, the solver has found a conflict.

This keeps the input clauses intact while assignments, watch lists, and learned
clauses carry the changing state of the search.

## Search and learning

The solver first loads the clauses and propagates any initial unit clauses. It
then alternates between choosing an unassigned literal and propagating its
consequences. Implied values retain their reason clauses; decisions are marked
separately on the trail.

When propagation or a decision discovers a contradiction, conflict analysis
resolves the clauses on the trail to learn a clause at the first unique
implication point (1-UIP). The learned clause can force further implications
or expose a conflict at an earlier search level. Conflict clauses also
influence later choices: the heuristic tracks literal occurrences in conflicts
and prefers literals from unsatisfied learned clauses. If a search cycle is
restarted, the solver rebuilds its propagation state while retaining active
conflict clauses.

Search continues until every variable has a value, in which case the problem
is satisfiable, or every available search path conflicts, in which case it is
unsatisfiable. Parallel mode runs one search worker per available processor;
without it, the solver uses one worker.

## Running

Pass a DIMACS CNF file as the first argument. The optional `-Parallel` switch
enables multiple search workers, and `-Benchmark` enables benchmark counters:

```text
Terricolor problem.cnf [-Parallel] [-Benchmark]
```

On finding a solution, the program prints a DIMACS CNF containing the input
clauses and the chosen assignments as unit clauses. For an unsatisfiable
problem, it prints an unsatisfiable result. With no file argument, it prints a
usage example.

## Code map

- `Reader.fs` parses DIMACS CNF input.
- `Clause.fs`, `Assignment.fs`, and `Propagation.fs` define clauses, assignment
  state, watches, and unit propagation.
- `Learning.fs` and `Conflict.fs` handle conflict analysis and conflict signals.
- `Search.fs`, `Heuristics.fs`, and `CycleSearch.fs` implement decisions,
  clause learning integration, and search cycles/workers.
- `Test/` contains tests for propagation, conflict learning, and heuristics.

Raw notes on how the compilation of a rule works.

    rule N 
      infers c : ct under Env0
      if p1 : pt1 under Env1
      and ...
      and pn : ptn under Envn

## Data Flow

For each rule as presented above the data flows roughly like this:

- `c` is provided as input
  - can impose some constraints (e.g. `1` for `LNum(n)` imposes `n == 1`)

- `Env0` is provided as input
  - can impose some constraints (e.g. `... under x : t` imposes `env(x) == t` and `#env == 1`)

- `ct` is constructed from provided
  - may be constrainted right-away (e.g. `e : int -> b` constraints it to be a function from `int` to some `b`)
  - may use types from `c`, `Env0` or the `pt`<sub>i</sub>

- `p`<sub>i</sub> is constructed from provided
  - may use types and names from `c`, `Env0` or previous `pt`<sub>j</sub> with `j < i` (assuming a topological order)

- `pt`<sub>i</sub> is provided as output of induction
  - may be constrained right-away (e.g. `if e : int` means `pt`<sub>0</sub>` == int`)
  - depends on `p`<sub>i</sub> and `Env`<sub>i</sub> being provided

- `Env`<sub>i</sub> is constructed from provided
  - may use types and names from `c`, `Env0` or previous `pt`<sub>j</sub> where `j < i` (assuming a topological order)

In all these cases using the same meta-variable in two output contexts implies we need an equality constraint. E.g. `if e1 : t and e2 : t` means `typeof(e1) == typeof(e2)`.

From less dependencies to more dependencies:

1. `c`

2. `Env0`

3. `p`<sub>1</sub> and `Env`<sub>1</sub>

4. `pt`<sub>1</sub>

5. ...

6. `p`<sub>n</sub> and `Env`<sub>n</sub>

7. `pt`<sub>n</sub>

8. `ct`

## Compilation

Target IR structure, broadly speaking:

```scala
Switch(
  branches = Seq(
    ...,
    (/* c matches rule-template */ -> And(
      conds = Seq(
        /* env matches Env0 */,
        _pt1 <- /* induction call on p1 with Env1 */,
        /* _pt1 matches pt1 */,
        ...
        _ptn <- /* induction call on pn with Envn */,
        /* _ptn matches ptn */,
      ),
      next = Result(/* ct */)
    )
    ...
  ),
  otherwise = ...
)
```

Where all the matches line can be ommitted when there's nothing to match (e.g. `rule Any infers e : one under Env`)
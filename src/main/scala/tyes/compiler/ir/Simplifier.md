Notes on the simplification process, breaking down some transformations.

# Or(Ands) -> And(..., Switch)

## Original

```scala
(
  for
    _ <- typecheck(e1, env).expecting(Type.Zero)
    _ <- typecheck(e2, env).expecting(Type.Zero)
  yield
    Type.Zero
).orElse(
  for
    t <- typecheck(e1, env)
    _ <- typecheck(e2, env).expecting(Type.Pos)
  yield
    Type.Pos
)
```

## Step 1: extract induction call

```scala
for
  t1 <- typecheck(e1, env)
  resT <- (
    for
      _ <- t1.expecting(Type.Zero)
      _ <- typecheck(e2, env).expecting(Type.Zero)
    yield
      Type.Zero
  ).orElse(
    for
      _ <- typecheck(e2, env).expecting(Type.Pos)
    yield
      Type.Pos
  )
yield
  resT
```

## Step 2: reorder induction first

```scala
for
  t1 <- typecheck(e1, env)
  resT <- (
    for
      _ <- typecheck(e2, env).expecting(Type.Zero)
      _ <- t1.expecting(Type.Zero)
    yield
      Type.Zero
  ).orElse(
    for
      _ <- typecheck(e2, env).expecting(Type.Pos)
    yield
      Type.Pos
  )
yield
  resT
```

## Step 3: extract induction call

```scala
for
  t1 <- typecheck(e1, env)
  resT <- for
    t2 <- typecheck(e2, env)
    resT2 <- (
      for
        _ <- t2.expecting(Type.Zero)
        _ <- t1.expecting(Type.Zero)
      yield
        Type.Zero
    ).orElse(
      for
        _ <- t2.expecting(Type.Pos)
      yield
        Type.Pos
    )
  yield
    resT2
yield
  resT
```

## Step 4: flatten nested And

```scala
for
  t1 <- typecheck(e1, env)
  t2 <- typecheck(e2, env)
  resT <- (
    for
      _ <- t2.expecting(Type.Zero)
      _ <- t1.expecting(Type.Zero)
    yield
      Type.Zero
  ).orElse(
    for
      _ <- t2.expecting(Type.Pos)
    yield
      Type.Pos
  )
yield
  resT
```

## Step 5: And with no decls to Switch

```scala
for
  t1 <- typecheck(e1, env)
  t2 <- typecheck(e2, env)
  resT <- (
    if t2 == Type.Zero && t1 == Type.Zero then
      Right(Type.Zero)
    else
      Left(s"Expected $t1 to be Type.Zero and $t2 to be Type.Zero")
  ).orElse(
    for
      _ <- t2.expecting(Type.Pos)
    yield
      Type.Pos
  )
yield
  resT
```

## Step 6: And with no decls to Switch

```scala
for
  t1 <- typecheck(e1, env)
  t2 <- typecheck(e2, env)
  resT <- (
    if t2 == Type.Zero && t1 == Type.Zero then
      Right(Type.Zero)
    else
      Left(s"TypeError: $t1 should be Type.Zero and $t2 should be Type.Zero")
  ).orElse(
    if t2 == Type.Pos then
      Right(Type.Pos)
    else
      Left(s"TypeError: $t2 should be Type.Pos")
  )
yield
  resT
```

## Step 7: (X)Or-of-Switches with non-overlapping cond to Switch ??

```scala
for
  t1 <- typecheck(e1, env)
  t2 <- typecheck(e2, env)
  resT <- (
    if t2 == Type.Zero && t1 == Type.Zero then
      Right(Type.Zero)
    else if t2 == Type.Pos then
      Right(Type.Pos)
    else
      Left(s"TypeError: either $t1 should be Type.Zero and $t2 should be Type.Zero; or $t2 should be Type.Pos")
  )
yield
  resT
```
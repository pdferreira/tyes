# TyES Project

TyES (Type Experiment System) is a personal project in the area of type systems, aiming to provide a playground to test different type system ideas in a simple way. It is being developed mostly for my own amusement and learning.

Currently it consists on a DSL that allows the declarative specification of type systems by means of inference rules, very similarly to how they are specified in formal works on the topic, already covering enough to type the [STLC](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus).

[<img alt="Simply Typed Lambda Calculus" src="STLCSample.png" width="50%" height="50%" />](samples/in/LambdaCalculus.tye)

It is being developed in a way that allows it to be applied to different language syntaxes and constructs, but at the moment these rules operate on expressions from a simple language, hardcoded into the system. Several examples can be found in [samples/in](samples/in).

Specifications in this DSL can then be either 1) interpreted using `tyer` (Type Experiment Runner), entering an interactive REPL where the type of an expression is evaluated; or 2) compiled using `tyec` (Type Experiment Compiler) to (tentatively) readable code.

```
> 1
const
> (fun x : const => x)
const -> const
> 1 1
TypeError: expected type Const to be a $FunType instead
```

## Implementation

Currently all the code is written using Scala 3. The compiler also targets Scala 3 for its generated code, although the current architecture should be close to allowing other targets.

This is my first big project using Scala, first one using Scala 3 in specific, so the code style and organization is bound to evolve as I get more comfortable.

## How to Use

Type Systems in TyES are specified in files with extension .tye.

To start the "REPL" over a specific type system, run the `tyer` command line. For example:

```scala
sbt "runMain tyer samples/in/LambdaCalculus.tye"
```

To compile a type system to Scala, run the `tyec` command line, specifying the output directory and then the target file(s). For example:

```scala
sbt "runMain tyec -out samples/out/new samples/in/LambdaCalculus.tye"
```

To easily test the generated code, run the `tyer` command line directly over the generated .scala file. For example:

```scala
sbt "runMain tyer samples/out/new/src/LambdaCalculusTypeSystem.scala"
```

## Evolution

This project really is a personal exploration, so it doesn't have a clear roadmap, but it does have several guiding ideas that have and will continue to influence its evolution:

- Type Systems should be specified similarly to their standard formal specifications, declaratively.

  -  This covers both terminology, syntax and the informal naming conventions.
  
  -  *Note:* at the moment I purposedly decided to go *against this* on the syntax to try and improve readability, and experiment with using keywords like `infers` and `under` instead of the usual `----` and `|-`, as well as  a different ordering of the inference rule components.

- The DSL used to specify a type system should be agnostic of specific programming language being typed.

  - The goal is to have a meta DSL, not something hardcoded for specific programming language constructs.

- The generated code should be optimized for readability, following a "if I would write this by hand, I would..." approach.

  - This also implies that the ideal generated code is as simple as the specified type system.

Topics under the radar to explore in the future, in no particular order:

- Variadic rules like the standard means of typing a record literal (which has 1 ... n fields), in some papers specified using a ... or overbar convention 
- Multiple typing enviroments
- Different typing environments to allow specifications with linear types and other substructural type systems
- Generation of LaTeX documentation
- Declarative specification of error messages
- Incorporate definition of syntax of the typed language into the DSL itself
- Automatically verify (fully or partially) type system properties like confluence and decidability
- Composition and refinement of type systems
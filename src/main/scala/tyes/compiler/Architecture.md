Raw notes on the intended compiler architecture.

# Old architecture


| Step                | Output          | Description |
|---------------------|-----------------|-------------|
| (read source file)  | String          ||
| TyesParser          | TypeSystemDecl  | Parses Tyes source code to AST |
| TyesValidator       |                 | Checks if AST is valid for compilation |
| TyesEnvDesugarer    | TypeSystemDecl  | Standardizes the environment ASTs via desugaring to facilitate code generation |
| TyesCodeGenerator   | String          | Generates target language (Scala) source code from the AST |


# New architecture


| Step                  | Output          | Description |
|-----------------------|-----------------|-------------|
| (read source file)    | String          ||
| TyesParser            | TypeSystemDecl  | Parses Tyes source code to AST |
| TyesValidator         |                 | Checks if AST is valid for compilation |
| TyesDesugarer         | TypeSystemDecl  | Standardizes parts of the AST via desugaring to facilitate code generation |
| TyesIRGenerator       | IRNode          | Compiles the AST down to a more general IR (for now, mixed with the next level) |
| IRNodeSimplifier      | IRNode          | Standardizes the IR nodes based on several rewrite rules |
| TargetCodeIRGenerator | TargetCodeNode  | Compiles the general IR down to a low-level target language AST |
| TargetCodeGenerator   | String          | Generates the actual target language source code from the low-level target language AST |

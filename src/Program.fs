// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2023 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Entry point of the Hygge compiler program, including the main function.
module Main

/// Tokenize the given file with the given options, and print the result on the
/// terminal. Return 0 in case of success, non-zero otherwise.
let internal tokenize (opt: CmdLine.TokenizerOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug (lazy $"Parsed command line options:%s{Util.nl}%O{opt}")
    match (Lexer.tokenize opt.File) with
    | Error(msg) ->
        Log.error (lazy $"%s{msg}")
        1 // Non-zero exit code
    | Ok(tokens) ->
        Log.info (lazy "Lexing succeeded.")
        Seq.iter (fun t -> printfn $"%O{t}") tokens
        0 // Success!


/// Parse the given file with the given options, and print the result on the
/// terminal. Return 0 in case of success, non-zero otherwise.
let internal parse (opt: CmdLine.ParserOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug (lazy $"Parsed command line options:%s{Util.nl}%O{opt}")
    match (Util.parseFile opt.File) with
    | Error(msg) ->
        Log.error (lazy $"%s{msg}"); 1 // Non-zero exit code
    | Ok(ast) ->
        printf $"%s{PrettyPrinter.prettyPrint ast}"
        0 // Success!


/// Parse and type-check the given file with the given options, and print the
/// result on the terminal. Return 0 in case of success, non-zero otherwise.
let internal typecheck (opt: CmdLine.TypecheckerOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug (lazy $"Parsed command line options:%s{Util.nl}%O{opt}")
    match (Util.parseFile opt.File) with
    | Error(msg) ->
        Log.error (lazy $"%s{msg}"); 1 // Non-zero exit code
    | Ok(ast) ->
        match (Typechecker.typecheck ast) with
        | Error(typErrs) ->
            for posErr in typErrs do
                Log.error (lazy (Util.formatMsg posErr))
            1 // Non-zero exit code
        | Ok(tast) ->
            Log.info (lazy "Type checking succeeded.")
            printf $"%s{PrettyPrinter.prettyPrint tast}"
            0 // Success!


/// Utility function that runs the Hygge interpreter.
let internal doInterpret (ast: AST.Node<'E,'T>) (verbose: bool): int =
    Log.info (lazy "Starting the interpreter.")
    let expr = Interpreter.interpret ast verbose
    if (Interpreter.isStuck expr) then
        Log.error (lazy $"Reached stuck expression:%s{Util.nl}%s{PrettyPrinter.prettyPrint expr}")
        1 // Non-zero exit code
    else
        Log.info (lazy $"Program reduced to value:%s{Util.nl}%s{PrettyPrinter.prettyPrint expr}")
        0 // Success!


/// Run the Hygge interpreter with the given options, and return the exit code
/// (zero in case of success, non-zero in case of error).
let rec internal interpret (opt: CmdLine.InterpreterOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug (lazy $"Parsed command line options:%s{Util.nl}%O{opt}")
    match (Util.parseFile opt.File) with
    | Error(msg) ->
        Log.error (lazy $"%s{msg}"); 1 // Non-zero exit code
    | Ok(ast) ->
        if (not opt.Typecheck) then
            Log.info (lazy "Skipping type checking.")
            doInterpret ast (opt.LogLevel = Log.LogLevel.debug || opt.Verbose)
        else
            Log.info (lazy "Running type checker (as requested).")
            match (Typechecker.typecheck ast) with
            | Error(typErrs) ->
                for posErr in typErrs do
                    Log.error (lazy (Util.formatMsg posErr))
                1 // Non-zero exit code
            | Ok(tast) ->
                Log.info (lazy "Type checking succeeded.")
                doInterpret tast (opt.LogLevel = Log.LogLevel.debug || opt.Verbose)


/// Auxiliary function that attempts to compile the assembly code in the fiven
/// filename, and returns Ok (with the compiled assembly code) or Error.
let internal generateAsm (filename: string): Result<RISCV.Asm, unit> =
    match (Util.parseFile filename) with
    | Error(msg) ->
        Log.error (lazy $"%s{msg}")
        Error()
    | Ok(ast) ->
        match (Typechecker.typecheck ast) with
        | Error(typErrs) ->
            for posErr in typErrs do
                Log.error (lazy (Util.formatMsg posErr))
            Error()
        | Ok(tast) ->
            Log.info (lazy "Type checking succeeded.")
            let asm = RISCVCodegen.codegen tast
            Ok(asm)


/// Run the Hygge compiler with the given options, and return the exit code
/// (zero in case of success, non-zero in case of error).
let internal compile (opt: CmdLine.CompilerOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug (lazy $"Parsed command line options:%s{Util.nl}%O{opt}")

    match (generateAsm opt.File) with
    | Ok(asm) ->
        match opt.OutFile with
        | Some(f) ->
            try
                System.IO.File.WriteAllText(f, asm.ToString())
                0 // Success!
            with e ->
                Log.error (lazy $"Error writing file %s{f}: %s{e.Message}")
                1 // Non-zero exit code
        | None ->
            printf $"%O{asm}"
            0 // Success!
    | Error() ->
        1 // Non-zero exit code


/// Compile and launch RARS with the compilation result, using the given
/// options.  Return 0 in case of success, and non-zero in case of error.
let internal launchRARS (opt: CmdLine.RARSLaunchOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug (lazy $"Parsed command line options:%s{Util.nl}%O{opt}")

    match (generateAsm opt.File) with
    | Ok(asm) ->
        let exitCode = RARS.launch (asm.ToString()) true
        exitCode
    | Error() ->
        1 // Non-zero exit code


/// The compiler entry point.  Must return zero on success, non-zero on error.
[<EntryPoint>]
let main (args: string[]): int =
    match (CmdLine.parse args) with
    | CmdLine.ParseResult.Error(exitCode) -> exitCode // Non-zero exit code
    | CmdLine.ParseResult.Tokenize(opts) -> tokenize opts
    | CmdLine.ParseResult.Parse(opts) -> parse opts
    | CmdLine.ParseResult.Typecheck(opts) -> typecheck opts
    | CmdLine.ParseResult.Interpret(opts) -> interpret opts
    | CmdLine.ParseResult.Compile(opts) -> compile opts
    | CmdLine.ParseResult.RARSLaunch(opts) -> launchRARS opts
    | CmdLine.ParseResult.Test(opts) -> Test.run opts

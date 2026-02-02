// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2026 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Utility functions for parsing the Hygge programming language.  This module
/// defines FParsec-style parser combinators.
module ParserUtils

open Lexer


/// Stream of tokens with positions.
type TokenStream = {
    /// Tokens with positions being parsed.
    Tokens: array<TokenWithPos>

    /// The current position in the array of tokens.
    mutable Index: int

    /// Expected tokens during parsing: each entry is a stream index and a
    /// description of what was expected at that position.
    Expected: ResizeArray<struct (int * string)>
} with
    /// Did this stream reach past the final token?
    member inline this.Ended() = this.Index = this.Tokens.Length

    /// Retrieve the current token from this stream.
    member inline this.Current() = this.Tokens[this.Index]

    /// Advance this stream to the next token.
    member inline this.Advance() = this.Index <- this.Index + 1


/// Create an AST node for the given pretype, using the given main, beginning,
/// and end positions.
let inline mkPretypeNode (pt: AST.Pretype) (mainPos: Lexer.Position)
                         (beginPos: Lexer.Position) (endPos: Lexer.Position): AST.PretypeNode =
    { AST.PretypeNode.Pretype = pt
      AST.PretypeNode.Pos = { AST.Position.Main = mainPos
                              AST.Position.Begin = beginPos
                              AST.Position.End = endPos } }


/// Create an AST node for the given Hygge expression, using the given main,
/// beginning, and ending positions.
let inline mkNode (expr: AST.UntypedExpr) (mainPos: Lexer.Position)
                  (beginPos: Lexer.Position) (endPos: Lexer.Position): AST.UntypedAST =
    { AST.Node.Expr = expr
      AST.Node.Pos = { AST.Position.Main = mainPos
                       AST.Position.Begin = beginPos
                       AST.Position.End = endPos }
      AST.Node.Env = ()
      AST.Node.Type = () }


/// Parse a token if it has the given 'tokenType'. Return either Ok and the
/// token with position, or Error with a string describing the expected token.
let inline pToken (tokenType: Token) (stream: TokenStream): Result<TokenWithPos, string> =
    let tok = stream.Current()
    match tok.Token with
    | t when t = tokenType ->
        stream.Advance()
        Ok tok
    | _ ->
        Error (tokenType.ToString())


/// Create a parser that parses both 'p1' and then 'p2', and returns both their
/// results as a tuple. If either 'p1' or 'p2' fails, return the corresponding
/// error.
let inline (->>-) (p1: TokenStream -> Result<'A, 'E>)
                  (p2: TokenStream -> Result<'B, 'E>)
                  (stream: TokenStream): Result<'A * 'B, 'E> =
    match p1 stream with
    | Ok res1 -> match p2 stream with
                 | Ok res2 -> Ok (res1, res2)
                 | Error e -> Error e
    | Error e -> Error e


/// Create a parser that parses both 'p1' and 'p2', and returns only the result
/// of 'p1'. If either 'p1' or 'p2' fails, return the corresponding error.
let inline (->>) (p1: TokenStream -> Result<'A, 'E>)
                 (p2: TokenStream -> Result<'B, 'E>)
                 (stream: TokenStream): Result<'A, 'E> =
    match p1 stream with
    | Ok res -> match p2 stream with
                | Ok _ -> Ok res
                | Error e -> Error e
    | Error e -> Error e


/// Create a parser that parses both 'p1' and 'p2', and returns only the result
/// of 'p2'. If either 'p1' or 'p2' fails, return the corresponding error.
let inline (>>-) (p1: TokenStream -> Result<'A, 'E>)
                 (p2: TokenStream -> Result<'B, 'E>)
                 (stream: TokenStream): Result<'B, 'E> =
    match p1 stream with
    | Ok _ -> p2 stream
    | Error e -> Error e


/// Create a parser that parses 'p', then applies 'f' to its result. If 'p'
/// fails, return the corresponding error.
let inline (|>>) (p: TokenStream -> Result<'A, 'E>)
                 (f: 'A -> 'B)
                 (stream: TokenStream): Result<'B, 'E> =
    match p stream with
    | Ok res -> Ok (f res)
    | Error e -> Error e


/// Create a parser that parses 'p', then applies 'f' to its result; then, uses
/// the parser returned by 'f' to continue parsing the stream. If either 'p' or
/// the parser returned by 'f' fail, return the corresponding error.
let inline (>>=) (p: TokenStream -> Result<'A, 'E>)
                 (f: 'A -> TokenStream -> Result<'B, 'E>)
                 (stream: TokenStream): Result<'B, 'E> =
    match p stream with
    | Ok res -> f res stream
    | Error e -> Error e


/// Create a parser that returns the given 'value' without consuming any token
/// from the stream.
let inline preturn (value: 'A) (_stream: TokenStream): Result<'A, 'E> =
    Ok value


/// Create a parser that forwards the parsing to a reference (that must be
/// initialized before use). This is necessary to define parsers with
/// mutually-recursive rules.
let pForwardRef(): (TokenStream -> Result<'A,'E>) * ((TokenStream -> Result<'A,'E>) ref) =
    // Inspired by FParsec's createParserForwardedToRef
    let dummy = fun stream -> failwith "BUG: a parser created with 'pForwardRef' was not initialized"
    let r = ref dummy
    // The lambda below is necessary, but FSharpLint suggests removing it
    // fsharplint:disable reimplementsFunction
    ((fun stream -> r.Value stream), r)


/// Internal helper function for 'chainl' and 'chaninl1'.
[<TailCall>]
let rec chainlRestHelper (p: TokenStream -> Result<'A, string>)
                         (op: TokenStream -> Result<('A -> 'A -> 'A), string>)
                         (stream: TokenStream) (acc: 'A): 'A =
    let savedIndex = stream.Index
    match op stream with
    | Error _ ->
        stream.Index <- savedIndex // Backtrack
        acc
    | Ok opFunc ->
        match p stream with
        | Error _ ->
            stream.Index <- savedIndex // Backtrack
            acc
        | Ok rhs ->
            chainlRestHelper p op stream (opFunc acc rhs)


/// Create a parser that parses zero or more occurrences of 'p' separated by
/// 'op', with left associativity. The parser returns the default value 'v' if
/// the first parsing of 'p' fails. Otherwise, the first parsing of 'p' provides
/// the initial value for an accumulator. The result of parsing 'op' is a
/// function that takes the accumulator and the result of the next parsing of
/// 'p', and returns a new accumulator. Iterate by parsing 'op' and then 'p' and
/// accumulating their result; when either 'op' or 'p' fails, backtrack the
/// 'stream' after the last successful 'p' and return the current value of the
/// accumulator.
let inline chainl (p: TokenStream -> Result<'A, string>)
                  (op: TokenStream -> Result<('A -> 'A -> 'A), string>)
                  (v: 'A)
                  (stream: TokenStream): Result<'A, 'E> =
    let savedIndex = stream.Index
    match p stream with
    | Error _ ->
        stream.Index <- savedIndex // Backtrack
        Ok v
    | Ok first ->
        Ok (chainlRestHelper p op stream first)


/// Create a parser that parses one or more occurrences of 'p' separated by
/// 'op', with left associativity. If the first parsing of 'p' fails, return the
/// corresponding error. Otherwise, the first parsing of 'p' provides the
/// initial value for an accumulator. The result of parsing 'op' is a function
/// that takes the accumulator and the result of the next parsing of 'p', and
/// returns a new accumulator. Iterate by parsing 'op' and then 'p' and
/// accumulating their result; when either 'op' or 'p' fails, backtrack the
/// 'stream' after the last successful 'p' and return the current value of the
/// accumulator.
let inline chainl1 (p: TokenStream -> Result<'A, string>)
                   (op: TokenStream -> Result<('A -> 'A -> 'A), string>)
                   (stream: TokenStream): Result<'A, string> =
    match p stream with
    | Error e -> Error e
    | Ok first ->
        Ok (chainlRestHelper p op stream first)


/// Internal helper function for 'choice'.
[<TailCall>]
let rec choiceHelper savedIndex maxIndex stream parsers =
    stream.Index <- savedIndex
    match parsers with
    | [] -> failwith "BUG: should never reach here"
    | [p] -> p stream
    | p :: rest ->
        match p stream with
        | Ok x -> Ok x
        | Error expected ->
            if stream.Index >= maxIndex then
                // We reached a position in the stream that is not below the
                // previous maximum: the information about what was expected
                // there is informative
                stream.Expected.Add struct (stream.Index, expected)
                choiceHelper savedIndex stream.Index stream rest
            else
                choiceHelper savedIndex maxIndex stream rest


/// Create a parser that tries each parser in the list 'parsers' in order until
/// one succeeds. When a parser fails, backtrack the 'stream' before trying the
/// next parser. If all parsers fail, return the error produced by the last
/// parser.
let inline choice (parsers: list<TokenStream -> Result<'A, string>>)
                  (stream: TokenStream): Result<'A, string> =
    choiceHelper stream.Index stream.Index stream parsers


/// Create a parser that parses 'stream' with 'p'; then, if 'p' succeeds,
/// returns Ok with the result. Otherwise, the parser finds the maximum position
/// reached while attempting 'p' on 'stream', groups all error messages
/// describing what was expected at that position, and returns an Error with
/// that position and a summary of what 'p' expected to find there.
let summarizeErrors (p: TokenStream -> Result<'A, string>)
                    (stream: TokenStream): Result<'A, Lexer.Position * string> =
    match p stream with
    | Ok value -> Ok value
    | Error expected ->
        // Find max index and collect unique expected values at that position
        let mutable maxIdx = stream.Index
        let expectedSet = System.Collections.Generic.HashSet<string>()
        expectedSet.Add(expected) |> ignore

        for (idx, exp) in stream.Expected do
            if idx > maxIdx then
                maxIdx <- idx
                expectedSet.Clear()
                expectedSet.Add(exp) |> ignore
            elif idx = maxIdx then
                expectedSet.Add(exp) |> ignore

        let expectedStr = String.concat ", " expectedSet
        let msg = if expectedSet.Count > 1 then "expected one of:" else "expected"
        let lastTok = stream.Tokens[maxIdx]
        Error (lastTok.Begin, $"found {lastTok.Token} but {msg} {expectedStr}")

// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2026 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Parser for the Hygge programming language, implemented using parser combinators.
module Parser

open Lexer
open ParserUtils


/// Parse pretype. This is a forward reference to pPretype'.
let pPretype, pPretypeRef = pForwardRef()
/// Parse a Hygge expression. This is a forward reference to pExpr'.
let pExpr, pExprRef = pForwardRef()
/// Parse a sequence of expressions. This is a forward reference to pSequenceExpr'.
let pSequenceExpr, pSequenceExprRef = pForwardRef()
/// Parse a simple expression. This is a forward reference to pSimpleExpr'.
let pSimpleExpr, pSimpleExprRef = pForwardRef()
/// Parse an unary expression. This is a forward reference to pUnaryExpr'.
let pUnaryExpr, pUnaryExprRef = pForwardRef()


/// Parse an integer value, producing an AST node with an IntVal expression.
let inline pIntVal (stream: TokenStream) =
    let tok = stream.Current()
    match tok.Token with
        | LIT_INT v ->
            stream.Advance()
            Ok (mkNode (AST.Expr.IntVal v) tok.Begin tok.Begin tok.End)
        | _ -> Error "LIT_INT"


/// Parse a float value, producing an AST node with a FloatVal expression.
let inline pFloatVal (stream: TokenStream) =
    let tok = stream.Current()
    match tok.Token with
        | LIT_FLOAT v ->
            stream.Advance()
            Ok (mkNode (AST.Expr.FloatVal v) tok.Begin tok.Begin tok.End)
        | _ -> Error "LIT_FLOAT"


/// Parse a boolean value, producing an AST node with a BoolVal expression.
let inline pBoolVal (stream: TokenStream) =
    let tok = stream.Current()
    match tok.Token with
        | LIT_BOOL v ->
            stream.Advance()
            Ok (mkNode (AST.Expr.BoolVal v) tok.Begin tok.Begin tok.End)
        | _ -> Error "LIT_BOOL"


/// Parse a string value, producing an AST node with a StringVal expression.
let inline pStringVal (stream: TokenStream) =
    let tok = stream.Current()
    match tok.Token with
        | LIT_STRING v ->
            stream.Advance()
            Ok (mkNode (AST.Expr.StringVal v) tok.Begin tok.Begin tok.End)
        | _ -> Error "LIT_STRING"


/// Parse a unit value, producing an AST node with a UnitVal expression.
let inline pUnitVal (stream: TokenStream) =
    let tok = stream.Current()
    match tok.Token with
        | LIT_UNIT ->
            stream.Advance()
            Ok (mkNode AST.Expr.UnitVal tok.Begin tok.Begin tok.End)
        | _ -> Error "LIT_STRING"


/// Parse an identifier, returning its token with position and name.
let inline pIdent (stream: TokenStream) =
    let tok = stream.Current()
    match tok.Token with
        | IDENT name ->
            stream.Advance()
            Ok (tok, name)
        | _ -> Error "IDENT"


/// Parse a literal value.
let pValue = choice [
    pIntVal
    pStringVal
    pFloatVal
    pUnitVal
    pBoolVal
]


/// Parse a pretype, producing a PretypeNode.
let pPretype' = choice [
    pIdent
        |>> fun (tok, name) ->
            mkPretypeNode (AST.Pretype.TId name) tok.Begin tok.Begin tok.End
]

/// Parse empty parentheses, like (), ( ), (  )... They could be tokenized as
/// either a LIT_UNIT or as an LPAREN followed by an RPAREN. Return the
/// rightmost token.
let pEmptyParen = choice [
    pToken LIT_UNIT
    pToken LPAREN >>- pToken RPAREN
]


/// Parse a readInt() expression.
let pReadInt =
    pToken READ_INT ->>- pEmptyParen
        |>> fun (tok1, tok2) ->
            mkNode AST.Expr.ReadInt tok1.Begin tok1.Begin tok2.End


/// Parse a readFloat() expression.
let pReadFloat =
    pToken READ_FLOAT ->>- pEmptyParen
        |>> fun (tok1, tok2) ->
            mkNode AST.Expr.ReadFloat tok1.Begin tok1.Begin tok2.End


/// Parse a print(...) expression.
let pPrint =
    pToken PRINT ->>- (pToken LPAREN >>- pSimpleExpr) ->>- pToken RPAREN
        |>> fun ((tok1, expr), tok2) ->
            mkNode (AST.Expr.Print expr) tok1.Begin tok1.Begin tok2.End


/// Parse a printLn(...) expression.
let pPrintLn =
    pToken PRINTLN ->>- (pToken LPAREN >>- pSimpleExpr) ->>- pToken RPAREN
        |>> fun ((tok1, expr), tok2) ->
            mkNode (AST.Expr.PrintLn expr) tok1.Begin tok1.Begin tok2.End


/// Parse an assert(...) expression.
let pAssert =
    pToken ASSERT ->>- (pToken LPAREN >>- pSimpleExpr) ->>- pToken RPAREN
        |>> fun ((tok1, expr), tok2) ->
            mkNode (AST.Expr.Assertion expr) tok1.Begin tok1.Begin tok2.End


/// Parse an expression between curly brackets.
let pCurlyExpr =
    pToken LCURLY >>- pExpr ->> pToken RCURLY


/// Parse a type declaration.
let pType =
    pToken TYPE ->>- pIdent ->>-
        (pToken EQ >>- pPretype) ->>-
        (pToken SEMI >>- pExpr)
            |>> fun (((tok, (_, name)), tpe), scope) ->
                mkNode (AST.Expr.Type (name, tpe, scope))
                       tok.Begin tok.Begin scope.Pos.End


/// Parse a variable, producing an AST node with a Var expression.
let pVariable =
    pIdent
        |>> fun (tok, name) ->
            mkNode (AST.Expr.Var name) tok.Begin tok.Begin tok.End


/// Parse a primary expression.
let pPrimary = choice [
                    pToken LPAREN >>- pSimpleExpr ->> pToken RPAREN
                    pValue
                    pVariable
               ]


/// Parse an ascription: a primary expression with (optional) type annotation.
let pAscriptionExpr =
    pPrimary >>= fun node ->
        // Check whether there is a colon and a pretype after the primary
        // expression: if so, create an AST node for a type ascription.
        // Otherwise, return the previously-parsed AST node as it is.
        choice [
            pToken COLON ->>- pPretype
                |>> fun (tok, tpe) ->
                    mkNode (AST.Expr.Ascription (tpe, node))
                           tok.Begin node.Pos.Begin tpe.Pos.End
            preturn node
        ]


/// Parse a unary expression.
let pUnaryExpr' = choice [
    pToken NOT ->>- pUnaryExpr
        |>> fun (tok, arg) ->
            mkNode (AST.Expr.Not arg) tok.Begin tok.Begin arg.Pos.End
    pReadInt
    pReadFloat
    pPrint
    pPrintLn
    pAssert
    pAscriptionExpr
]


/// Parse a multiplicative expression.
let pMultExpr =
    chainl1 pUnaryExpr
        (choice [
            pToken TIMES
                |>> fun tok ->
                    fun acc rhs ->
                        mkNode (AST.Expr.BinNumOp (AST.NumericalOp.Mult, acc, rhs))
                               tok.Begin acc.Pos.Begin rhs.Pos.End
        ])


/// Parse an additive expression.
let pAddExpr =
    chainl1 pMultExpr
        (choice [
            pToken PLUS
                |>> fun tok ->
                    fun acc rhs ->
                        mkNode (AST.Expr.BinNumOp (AST.NumericalOp.Add, acc, rhs))
                               tok.Begin acc.Pos.Begin rhs.Pos.End
        ])


/// Parse a relational expression.
let pRelExpr =
    pAddExpr >>= fun lhs ->
        choice [
            pToken EQ ->>- pAddExpr
                |>> fun (tok, rhs) ->
                    mkNode (AST.Expr.BinRelOp (AST.RelationalOp.Eq, lhs, rhs))
                           tok.Begin lhs.Pos.Begin rhs.Pos.End
            pToken LT ->>- pAddExpr
                |>> fun (tok, rhs) ->
                    mkNode (AST.Expr.BinRelOp (AST.RelationalOp.Less, lhs, rhs))
                           tok.Begin lhs.Pos.Begin rhs.Pos.End
            preturn lhs // Default case if no operator above matches
        ]


/// Parse a logical 'and' expression.
let pAndExpr =
    chainl1 pRelExpr
        (pToken AND
            |>> fun tok ->
                fun acc rhs ->
                    mkNode (AST.Expr.BinLogicOp (AST.LogicOp.And, acc, rhs))
                           tok.Begin acc.Pos.Begin rhs.Pos.End)


/// Parse a logical 'or' expression.
let pOrExpr =
    chainl1 pAndExpr
        (pToken OR
            |>> fun tok ->
                fun acc rhs ->
                    mkNode (AST.Expr.BinLogicOp (AST.LogicOp.Or, acc, rhs))
                           tok.Begin acc.Pos.Begin rhs.Pos.End)


/// Parse an 'if-then-else' expression.
let pIfExpr = choice [
    pToken IF ->>- pSimpleExpr ->>-
        (pToken THEN >>- pSimpleExpr) ->>-
        (pToken ELSE >>- pSimpleExpr)
            |>> fun (((tok, condN), thenN), elseN) ->
                    mkNode (AST.Expr.If (condN, thenN, elseN))
                           tok.Begin tok.Begin elseN.Pos.End
]


/// Parse a "simple" expression, which (unlike the more general 'pExpr') cannot
/// result in a 'Seq'uence of sub-expressions, unless they are explicitly
/// enclosed in curly brackets.
let pSimpleExpr' = choice [
    pIfExpr
    pOrExpr
    pCurlyExpr
]


/// Parse a sequence of expressions.
let pSequenceExpr' = choice [
    pCurlyExpr >>= fun node ->
        // We do not require a semicolon between an expression in curly brackets
        // and the next expression. Check whether there is an optional semicolon
        // and another expression after the closing bracket: if so, 'node' is
        // part of a sequence. Otherwise, return 'node' as it is.
        choice [
            choice [
              pToken SEMI >>- pExpr
              pExpr
            ] |>> fun node2 ->
                    match node2.Expr with
                    | AST.Expr.Seq nodes ->
                        // Flatten the sequence (not necessary but keeps AST flat)
                        mkNode (AST.Expr.Seq (node :: nodes))
                               node.Pos.End node.Pos.Begin node2.Pos.End
                    | _ ->
                        mkNode (AST.Expr.Seq [node; node2])
                               node.Pos.End node.Pos.Begin node2.Pos.End
            preturn node
        ]
    pSimpleExpr >>= fun node ->
        // Check whether there is a semicolon and an expression after the simple
        // expression: if so, 'node' is part of a sequence. Otherwise, return
        // 'node' as it is.
        choice [
            pToken SEMI ->>- pExpr
                |>> fun (tok, node2) ->
                    match node2.Expr with
                    | AST.Expr.Seq nodes ->
                        // Flatten the sequence (not necessary but keeps AST flat)
                        mkNode (AST.Expr.Seq (node :: nodes))
                               tok.Begin node.Pos.Begin node2.Pos.End
                    | _ ->
                        mkNode (AST.Expr.Seq [node; node2])
                               tok.Begin node.Pos.Begin node2.Pos.End
            preturn node
        ]
]


/// Parse a 'let' binding without type annotation.
let pLet =
    pToken LET ->>- pIdent ->>-
        (pToken EQ >>- pSimpleExpr) ->>-
        (pToken SEMI >>- pExpr)
            |>> fun (((tok, (_, name)), init), scope) ->
                mkNode (AST.Expr.Let (name, init, scope))
                       tok.Begin tok.Begin scope.Pos.End


/// Parse a 'let' binding with a type annotation.
let pLetT =
    pToken LET ->>- pIdent ->>-
        (pToken COLON >>- pPretype) ->>-
        (pToken EQ >>- pSimpleExpr) ->>-
        (pToken SEMI >>- pExpr)
            |>> fun ((((tok, (_, name)), tpe), init), scope) ->
                mkNode (AST.Expr.LetT (name, tpe, init, scope))
                       tok.Begin tok.Begin scope.Pos.End


/// Parse any Hygge expression.
let pExpr' = choice [
    pType
    pLetT
    pLet
    pSequenceExpr
]


// Initialize the forward references defined at the beginning of this file.
pPretypeRef.Value <- pPretype'
pExprRef.Value <- pExpr'
pSequenceExprRef.Value <- pSequenceExpr'
pSimpleExprRef.Value <- pSimpleExpr'
pUnaryExprRef.Value <- pUnaryExpr'


/// Parse a whole Hygge program (i.e., an expression followed by EOF); if the
/// parsing fails, report a summary of all errors.
let pProgram: TokenStream -> Result<AST.UntypedAST, Lexer.Position * string> =
    pExpr ->> pToken EOF  |>  summarizeErrors


/// Parse the given array of 'tokens' with positions and return either Ok
/// UntypedAST, or an Error with a position and a message.
let parse (tokens: array<TokenWithPos>): Result<AST.UntypedAST, Lexer.Position * string> =
    let stream = {
        // TODO: use immarray when available. See:
        // https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1094-immarray.md
        Tokens = tokens
        Index = 0
        Expected = ResizeArray 512
    }
    pProgram stream

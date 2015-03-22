module IMP_Lexer where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
    emptyDef { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.identStart      = letter
             , Token.identLetter     = alphaNum
             , Token.reservedNames   = [ "if"
                                       , "then"
                                       , "else"
                                       , "while"
                                       , "do"
                                       , "skip"
                                       , "true"
                                       , "false"
                                       , "not"
                                       , "and"
                                       , "or"
                                       , "print"
                                       , "var"
                                       ]
             , Token.reservedOpNames = [ "+"
                                       , "-"
                                       , "*"
                                       , "/"
                                       , ":="
                                       , "<"
                                       , ">"
                                       , "="
                                       , "++"
                                       , "and"
                                       , "or"
                                       , "not"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
integer       = Token.integer       lexer
semi          = Token.semi          lexer
comma         = Token.comma         lexer
whiteSpace    = Token.whiteSpace    lexer
stringLiteral = Token.stringLiteral lexer

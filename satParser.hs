-- | Parser for a small propositional logic LANGUAGE
-- | implemented using the tutorial at http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements

module SatParser 
    ( parseSystem
    ) where

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import SatDefs

def :: LanguageDef ()
def = emptyDef { commentStart = "/*"
               , commentEnd = "*/"
               , identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "~&|>="
               , opLetter = oneOf "~&|>="
               , reservedOpNames = ["~", "&", "|", "=>", "==" ]
               , reservedNames = ["true", "false" ] 
               }

mainParser :: Parser System
mainParser = m_whiteSpace >> multiParser <* eof
    where multiParser = m_semiSep1 exprParser
          exprParser  = buildExpressionParser table term <?> "expression"
          table = [ [Prefix (m_reservedOp "~" >> return NotS)]
                  , [Infix (m_reservedOp "&" >> return AndS) AssocLeft]
                  , [Infix (m_reservedOp "|" >> return OrS) AssocLeft]
                  , [Infix (m_reservedOp "=>" >> return ImplS) AssocRight]
                  , [Infix (m_reservedOp "==" >> return EqS) AssocRight]
                  ]
          term = m_parens exprParser
                 <|> fmap VarS m_identifier
                 <|> (m_reserved "true" >> return TrueS)
                 <|> (m_reserved "false" >> return FalseS)
          TokenParser { parens = m_parens
                      , identifier = m_identifier
                      , reservedOp = m_reservedOp
                      , reserved = m_reserved
                      , semiSep1 = m_semiSep1
                      , whiteSpace = m_whiteSpace
                      } = makeTokenParser def

parseSystem :: String -> Either String System
parseSystem input = case parse mainParser "" input of
                        Left err  -> Left $ show err
                        Right ans -> Right ans
import Text.XML.HXT.Core
import MathML
import UncertML
import Control.Monad
import Text.XML.HXT.Arrow.ParserInterface

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xshow $ (\v -> (v, v)) ^>> parseXmlDoc >>> propagateNamespaces >>>
                               (hasQName (mkNsName "mml:mathml" mathmlNS) /> xmlToMathML2 >>> arr mmlToUnAST >>> unlistA >>> uncertmlASTToXML))

mmlToUnAST = error "TODO: Implement mmlToUnAST"
uncertmlASTToXML = error "TODO: Implement uncertmlASTToXML"

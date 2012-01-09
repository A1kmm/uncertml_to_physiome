import Text.XML.HXT.Core
import MathML
import UncertML

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xshow $ (\v -> (v, v)) ^>> parseXmlDoc >>> propagateNamespaces >>>
                               (hasQName (mkNsName "mml:mathml" mathmlNS) /> xmlToMathML2 >>> arr mmlToUnAST >>> unlistA >>> uncertmlASTToXML))

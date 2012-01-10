import Text.XML.HXT.Core
import MathML
import UncertML
import Control.Monad
import Text.XML.HXT.Arrow.ParserInterface
import Data.Maybe
import ParsingSupport

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xshow $ (\v -> (v, v)) ^>> parseXmlDoc >>> propagateNamespaces >>> hasQName (mkNsName "mml:mathml" mathmlNS) /> xmlToMathML2 >>> arrMaybe mmlToUnAST >>> unlistA >>> unToXML)

mmlToUnAST (M2Apply M2Eq [l, M2Apply (M2Csymbol upd) [d]])
  | upd == "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution" =
    mmlDistToUnAST d
mmlToUnAST _ = Nothing

mmlDistToUnAST (M2Apply (M2Csymbol dtype) [v])
  | dtype == "http://www.cellml.org/uncertainty-1#distributionFromRealisations" =
    mmlRealisationsToUnAST v
  | dtype == "http://www.cellml.org/uncertainty-1#distributionFromDensity" =
    mmlDensityToUnAST v
  | dtype == "http://www.cellml.org/uncertainty-1#distributionFromMass" =
    mmlMassToUnAST v
mmlDistToUnAST _ = Nothing

mmlExtractVector (M2Vector l) = Just l
mmlExtractVector _ = Nothing
mmlRealisationsToUnAST (M2Vector l) = mapMaybe (mmlExtractVector >> m2TryEval) l

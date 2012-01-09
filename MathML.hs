{-# LANGUAGE NoMonomorphismRestriction #-}
module MathML (MathML2Expression(..), MathML2Op(..), m2ToXML, mathml2ToXML, mathmlNS, cellmlNS, xmlToMathML2)
where

import Control.Monad
import Text.XML.HXT.Core hiding(trace)
import Data.List
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.XML.HXT.Arrow.ParserInterface
import Data.Maybe
import ParsingSupport

data MathML2Expression = M2Apply MathML2Op [MathML2Expression] | M2Ci String | M2Cn String Double | M2Lambda String MathML2Expression |
                         M2Vector [MathML2Expression] | M2True | M2False | M2Infinity | M2Pi | M2EulerGamma | M2ExponentialE |
                         M2Piecewise [(MathML2Expression, MathML2Expression)] (Maybe MathML2Expression) deriving (Eq, Ord, Show)
data MathML2Op = M2Quotient | M2Factorial | M2Divide | M2Max | M2Min | M2Minus | M2Plus | M2Power | M2Rem |
                 M2Times | M2Root { m2rootDegree :: Maybe MathML2Expression } | M2Gcd | M2And | M2Or | M2Xor |
                 M2Not | M2Implies | M2Abs | M2Lcm | M2Floor | M2Ceiling | M2Eq | M2Neq | M2Gt | M2Lt | M2Geq |
                 M2Leq | M2Factorof | M2Int { m2intLowlimit :: Maybe MathML2Expression, m2intUplimit :: Maybe MathML2Expression,
                                              m2intDegree :: Maybe MathML2Expression, m2intBvar :: String } |
                 M2Diff { m2diffBvar :: String } | M2Exp | M2Ln |
                 M2Log { m2logLogbase :: Maybe MathML2Expression } | M2Csymbol String  deriving (Eq, Ord, Show)

mathmlNS = "http://www.w3.org/1998/Math/MathML"
cellmlNS = "http://www.cellml.org/cellml/1.1#"

m2ToXML = (\v -> (mathml2ToXML v, v)) ^>> app

mathml2ToXML :: ArrowXml a => MathML2Expression -> a b XmlTree
mathml2ToXML (M2Apply op subexprs) =
  mkqelem (mkNsName "mml:apply" mathmlNS) []
          (mathml2OpToXML op ++ map mathml2ToXML subexprs)
mathml2ToXML (M2Ci ciName) =
  mkqelem (mkNsName "mml:ci" mathmlNS) [] [txt ciName]
  
mathml2ToXML (M2Cn unitsName cnValue) =
  mkqelem (mkNsName "mml:cn" mathmlNS)
    [sqattr (mkNsName "cellml:units" cellmlNS) unitsName] [txt $ show cnValue]
  
mathml2ToXML (M2Lambda str expr) =
  mkqelem (mkNsName "mml:lambda" mathmlNS) []
    [mkqelem (mkNsName "mml:bvar" mathmlNS) []
       [mkqelem (mkNsName "mml:ci" mathmlNS) [] [txt str]],
     mathml2ToXML expr]
  
mathml2ToXML (M2Vector parts) =
  mkqelem (mkNsName "mml:vector" mathmlNS) [] $
    map mathml2ToXML parts

mathml2ToXML (M2True) =
  mkqelem (mkNsName "mml:true" mathmlNS) [] []
  
mathml2ToXML (M2False) =
    mkqelem (mkNsName "mml:false" mathmlNS) [] []

mathml2ToXML (M2Infinity) =
  mkqelem (mkNsName "mml:infinity" mathmlNS) [] []
  
mathml2ToXML (M2Pi) =
    mkqelem (mkNsName "mml:pi" mathmlNS) [] []

mathml2ToXML (M2EulerGamma) =
  mkqelem (mkNsName "mml:eulergamma" mathmlNS) [] []
  
mathml2ToXML (M2ExponentialE) =
    mkqelem (mkNsName "mml:exponentiale" mathmlNS) [] []

mathml2ToXML (M2Piecewise pieces maybeOtherwise) =
    mkqelem (mkNsName "mml:piecewise" mathmlNS) []
      ((map (\(val, cond) -> mkqelem (mkNsName "mml:piece" mathmlNS) [] [mathml2ToXML val, mathml2ToXML cond]) pieces) ++
       (map (mkqelem (mkNsName "mml:otherwise" mathmlNS) [] . (:[]) . mathml2ToXML) (maybeToList maybeOtherwise)))

mathml2OpToXML M2Quotient = [mkqelem (mkNsName "mml:quotient" mathmlNS) [] []]
mathml2OpToXML M2Factorial = [mkqelem (mkNsName "mml:factorial" mathmlNS) [] []]
mathml2OpToXML M2Divide = [mkqelem (mkNsName "mml:divide" mathmlNS) [] []]
mathml2OpToXML M2Max = [mkqelem (mkNsName "mml:max" mathmlNS) [] []]
mathml2OpToXML M2Min = [mkqelem (mkNsName "mml:min" mathmlNS) [] []]
mathml2OpToXML M2Minus = [mkqelem (mkNsName "mml:minus" mathmlNS) [] []]
mathml2OpToXML M2Plus = [mkqelem (mkNsName "mml:plus" mathmlNS) [] []]
mathml2OpToXML M2Power = [mkqelem (mkNsName "mml:power" mathmlNS) [] []]
mathml2OpToXML M2Rem = [mkqelem (mkNsName "mml:rem" mathmlNS) [] []]
mathml2OpToXML M2Times = [mkqelem (mkNsName "mml:times" mathmlNS) [] []]
mathml2OpToXML (M2Root { m2rootDegree = mex }) =
  [mkqelem (mkNsName "mml:root" mathmlNS) [] [],
   mkqelem (mkNsName "mml:degree" mathmlNS) [] (map (\ex -> mathml2ToXML ex) $
                                                    maybeToList mex)]
mathml2OpToXML M2Gcd = [mkqelem (mkNsName "mml:gcd" mathmlNS) [] []]
mathml2OpToXML M2And = [mkqelem (mkNsName "mml:and" mathmlNS) [] []]
mathml2OpToXML M2Or = [mkqelem (mkNsName "mml:or" mathmlNS) [] []]
mathml2OpToXML M2Xor = [mkqelem (mkNsName "mml:xor" mathmlNS) [] []]
mathml2OpToXML M2Not = [mkqelem (mkNsName "mml:not" mathmlNS) [] []]
mathml2OpToXML M2Implies = [mkqelem (mkNsName "mml:implies" mathmlNS) [] []]
mathml2OpToXML M2Abs = [mkqelem (mkNsName "mml:abs" mathmlNS) [] []]
mathml2OpToXML M2Lcm = [mkqelem (mkNsName "mml:lcm" mathmlNS) [] []]
mathml2OpToXML M2Floor = [mkqelem (mkNsName "mml:floor" mathmlNS) [] []]
mathml2OpToXML M2Ceiling = [mkqelem (mkNsName "mml:ceiling" mathmlNS) [] []]
mathml2OpToXML M2Eq = [mkqelem (mkNsName "mml:eq" mathmlNS) [] []]
mathml2OpToXML M2Neq = [mkqelem (mkNsName "mml:neq" mathmlNS) [] []]
mathml2OpToXML M2Gt = [mkqelem (mkNsName "mml:gt" mathmlNS) [] []]
mathml2OpToXML M2Lt = [mkqelem (mkNsName "mml:lt" mathmlNS) [] []]
mathml2OpToXML M2Geq = [mkqelem (mkNsName "mml:geq" mathmlNS) [] []]
mathml2OpToXML M2Leq = [mkqelem (mkNsName "mml:leq" mathmlNS) [] []]
mathml2OpToXML M2Factorof = [mkqelem (mkNsName "mml:factorof" mathmlNS) [] []]
mathml2OpToXML (M2Int { m2intLowlimit = mll, m2intUplimit = mul, m2intDegree = mid,
                        m2intBvar = ib } ) =
  [mkqelem (mkNsName "mml:int" mathmlNS) [] [],
   mkqelem (mkNsName "mml:lowlimit" mathmlNS) [] (map (\ex -> mathml2ToXML ex) $ maybeToList mll),
   mkqelem (mkNsName "mml:uplimit" mathmlNS) [] (map (\ex -> mathml2ToXML ex) $ maybeToList mul),
   mkqelem (mkNsName "mml:degree" mathmlNS) [] (map (\ex -> mathml2ToXML ex) $ maybeToList mid),
   mkqelem (mkNsName "mml:bvar" mathmlNS)
     [] [mkqelem (mkNsName "mml:ci" mathmlNS) [] [txt ib]]
   ]
mathml2OpToXML (M2Diff { m2diffBvar = db }) =
  [mkqelem (mkNsName "mml:diff" mathmlNS) [] [],
   mkqelem (mkNsName "mml:bvar" mathmlNS)
     [] [mkqelem (mkNsName "mml:ci" mathmlNS) [] [txt db]]
  ]

mathml2OpToXML M2Exp = [mkqelem (mkNsName "mml:exp" mathmlNS) [] []]
mathml2OpToXML M2Ln = [mkqelem (mkNsName "mml:ln" mathmlNS) [] []]
mathml2OpToXML (M2Log { m2logLogbase = mlb }) =
  [mkqelem (mkNsName "mml:log" mathmlNS) [] [],
   mkqelem (mkNsName "mml:logbase" mathmlNS) [] (map (\ex -> mathml2ToXML ex) $ maybeToList mlb)]
mathml2OpToXML (M2Csymbol cs) = [mkqelem (mkNsName "mml:csymbol" mathmlNS) [sattr "definitionURL" cs] []]

xmlToMathML2 :: ArrowXml a => a XmlTree MathML2Expression
xmlToMathML2 =
  (hasQName (mkNsName "mml:apply" mathmlNS) >>>
   liftArrow2 M2Apply (getChildren >>> xmlToM2Op)
                      (listA $ listA (getChildren >>> isElem) >>> tail ^>> unlistA >>> xmlToMathML2))
  
xmlToM2Op = m2SimpleOp "quotient" M2Quotient <+>
            m2SimpleOp "factorial" M2Factorial <+>
            m2SimpleOp "divide" M2Divide <+>
            m2SimpleOp "max" M2Max <+>
            m2SimpleOp "min" M2Min <+>
            m2SimpleOp "minus" M2Minus <+>
            m2SimpleOp "plus" M2Plus <+>
            m2SimpleOp "power" M2Power <+>
            m2SimpleOp "rem" M2Rem <+>
            m2SimpleOp "times" M2Times <+>
            m2SimpleOp "gcd" M2Gcd <+>
            m2SimpleOp "and" M2And <+>
            m2SimpleOp "or" M2Or <+>
            m2SimpleOp "xor" M2Xor <+>
            m2SimpleOp "not" M2Not <+>
            m2SimpleOp "implies" M2Implies <+>
            m2SimpleOp "abs" M2Abs <+>
            m2SimpleOp "lcm" M2Lcm <+>
            m2SimpleOp "floor" M2Floor <+>
            m2SimpleOp "ceiling" M2Ceiling <+>
            m2SimpleOp "eq" M2Eq <+>
            m2SimpleOp "neq" M2Neq <+>
            m2SimpleOp "gt" M2Gt <+>
            m2SimpleOp "lt" M2Lt <+>
            m2SimpleOp "geq" M2Geq <+>
            m2SimpleOp "leq" M2Leq <+>
            m2SimpleOp "factorof" M2Factorof <+>
            m2SimpleOp "exp" M2Exp <+>
            m2SimpleOp "ln" M2Ln

m2SimpleOp n c = hasQName (mkNsName ("mml:" ++ n) mathmlNS) >>^ const c

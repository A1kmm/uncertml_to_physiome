{-# LANGUAGE NoMonomorphismRestriction #-}
module MathML (MathML2Expression(..), MathML2Op(..), m2ToXML, mathml2ToXML, mathmlNS, cellmlNS, xmlToMathML2, m2TryEval)
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
  catMaybes $ [Just $ mkqelem (mkNsName "mml:root" mathmlNS) [] [],
               liftM (\a -> mkqelem (mkNsName "mml:degree" mathmlNS) [] [mathml2ToXML a]) mex]
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
  catMaybes [Just $ mkqelem (mkNsName "mml:int" mathmlNS) [] [],
             liftM (\ex -> mkqelem (mkNsName "mml:lowlimit" mathmlNS) [] [mathml2ToXML ex]) mll,
             liftM (\ex -> mkqelem (mkNsName "mml:uplimit" mathmlNS) [] [mathml2ToXML ex]) mul,
             liftM (\ex -> mkqelem (mkNsName "mml:degree" mathmlNS) [] [mathml2ToXML ex]) mid,
             Just $ mkqelem (mkNsName "mml:bvar" mathmlNS)
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
  catMaybes $ [Just $ mkqelem (mkNsName "mml:log" mathmlNS) [] [],
               liftM (\ex -> mkqelem (mkNsName "mml:logbase" mathmlNS) [] [mathml2ToXML ex]) mlb
              ]
mathml2OpToXML (M2Csymbol cs) = [mkqelem (mkNsName "mml:csymbol" mathmlNS) [sattr "definitionURL" cs] []]

mathmlFloat = do
  mathmlWhitespace
  m <- (char '-' >> return (-1)) <|> (return 1)
  v <- naturalOrFloat haskell
  case v of
       Left n -> return $ m * fromIntegral n
       Right f -> return $ m * f
mathmlWhitespace = many (oneOf " \t\r\n")

xmlToMathML2 :: ArrowXml a => a XmlTree MathML2Expression
xmlToMathML2 =
  (hasQName (mkNsName "mml:apply" mathmlNS) >>>
   liftArrow2 M2Apply (xmlToM2Op)
                      (listA $ listA (getChildren >>> isElem) >>> tail ^>> unlistA >>> xmlToMathML2)) <+>
  (hasQName (mkNsName "mml:ci" mathmlNS) >>> liftArrow M2Ci combinedChildText) <+>
  (hasQName (mkNsName "mml:cn" mathmlNS) >>>
   liftArrow2 M2Cn (getQAttrValue (mkNsName "cellml:units" cellmlNS))
                   (parseCombinedChildText mathmlFloat)) <+>
  (hasQName (mkNsName "mml:lambda" mathmlNS) >>> liftArrow2 M2Lambda (getChildren >>> hasQName (mkNsName "mml:bvar" mathmlNS) 
                                                                      /> hasQName (mkNsName "mml:ci" mathmlNS)
                                                                      >>> combinedChildText)
                       (getChildren >>> xmlToMathML2)) <+>
  (hasQName (mkNsName "mml:vector" mathmlNS) >>> liftArrow M2Vector (listA $ getChildren >>> xmlToMathML2)) <+>
  xml2M2Constant "true" M2True <+>
  xml2M2Constant "false" M2False <+>
  xml2M2Constant "infinity" M2Infinity <+>
  xml2M2Constant "pi" M2Pi <+>
  xml2M2Constant "eulergamma" M2EulerGamma <+>
  xml2M2Constant "exponentiale" M2ExponentialE <+>
  (hasQName (mkNsName "mml:piecewise" mathmlNS) >>>
   liftArrow2 M2Piecewise
      (listA $ getChildren >>> hasQName (mkNsName "mml:piece" mathmlNS) >>>
               listA (getChildren >>> xmlToMathML2) >>?
               (\v -> liftM2 (,) (v!!?0) (v!!?1)))
      (maybeA $ getChildren >>> hasQName (mkNsName "mml:otherwise" mathmlNS) />
                xmlToMathML2))

xml2M2Constant n c = ((hasQName (mkNsName ("mml:" ++ n) mathmlNS)) `guards` (arr $ const c))

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
            m2SimpleOp "ln" M2Ln <+>
            ((getChildren >>> hasQName (mkNsName "mml:root" mathmlNS)) `guards`
              liftArrow M2Root (m2MaybeExpression "degree")) <+>
            ((getChildren >>> hasQName (mkNsName "mml:int" mathmlNS)) `guards`
              (liftArrow4 M2Int
                 (m2MaybeExpression "lowlimit")
                 (m2MaybeExpression "uplimit")
                 (m2MaybeExpression "degree")
                 m2BvarChild
              )) <+>
            ((getChildren >>> hasQName (mkNsName "mml:diff" mathmlNS)) `guards`
              (liftArrow M2Diff m2BvarChild)) <+>
            ((getChildren >>> hasQName (mkNsName "mml:log" mathmlNS)) `guards`
              (liftArrow M2Log
                 (m2MaybeExpression "logbase")
              )) <+>
            (getChildren >>> hasQName (mkNsName "mml:csymbol" mathmlNS) >>> liftArrow M2Csymbol (getAttrValue "definitionURL"))

m2MaybeExpression n = (maybeA (getChildren >>> hasQName (mkNsName ("mml:" ++ n) mathmlNS) /> xmlToMathML2))
m2SimpleOp n c = getChildren >>> (hasQName (mkNsName ("mml:" ++ n) mathmlNS) >>^ const c)
m2BvarChild = getChildren >>> hasQName (mkNsName "mml:bvar" mathmlNS) /> hasQName (mkNsName "mml:ci" mathmlNS) >>> combinedChildText

m2TryEval :: MathML2Expression -> Maybe Double
m2TryEval (M2Apply M2Quotient [ea, eb]) = do
  a <- m2TryEval ea
  b <- m2TryEval eb
  return $ fromIntegral $ (floor a) `div` (floor b)
m2TryEval (M2Apply M2Factorial [ea]) = do
  a <- m2TryEval ea
  let f x | x <= 0 = 1
          | otherwise = x * f (x - 1)
  return $ fromIntegral . f. floor $ a
m2TryEval (M2Apply M2Divide [ea, eb]) =
  liftM2 (/) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Max l) =
  liftM maximum (sequence (map m2TryEval l))
m2TryEval (M2Apply M2Min l) =
  liftM minimum (sequence (map m2TryEval l))
m2TryEval (M2Apply M2Minus [ea]) = liftM (0-) (m2TryEval ea)
m2TryEval (M2Apply M2Minus [ea, eb]) = liftM2 (-) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Plus l) = liftM sum (sequence (map m2TryEval l))
m2TryEval (M2Apply M2Power [ea, eb]) = liftM2 (**) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Rem [ea, eb]) = do
  a <- m2TryEval ea
  b <- m2TryEval eb
  return $ fromIntegral $ (floor a) `rem` (floor b)
m2TryEval (M2Apply M2Times l) = liftM product (sequence (map m2TryEval l))
m2TryEval (M2Apply (M2Root mdeg) [ea]) =
  liftM2 (**) (m2TryEval ea) (liftM (1/) (maybe (Just 2) m2TryEval mdeg))

-- To do: Implement m2TryEval (M2Apply M2Gcd [ea, eb]) =
-- To do: Implement m2TryEval (M2Apply M2Lcm [ea, eb]) =
m2TryEval (M2Apply M2And l) =
  liftM (fromIntegral . fromEnum . and . map (/=0)) (sequence . map m2TryEval $ l)
m2TryEval (M2Apply M2Or l) =
  liftM (fromIntegral . fromEnum . or . map (/=0)) (sequence . map m2TryEval $ l)
m2TryEval (M2Apply M2Xor l) =
  let
    a `xor` b = (a && not b) || (not a && b)
  in
   liftM (fromIntegral . fromEnum . or . map (/=0)) (sequence . map m2TryEval $ l)
m2TryEval (M2Apply M2Not [ea]) =
  liftM (fromIntegral . fromEnum . not . (/=0)) (m2TryEval ea)
m2TryEval (M2Apply M2Implies [ea, eb]) = do
  a <- liftM (/=0) $ m2TryEval ea
  b <- liftM (/=0) $ m2TryEval eb
  return . fromIntegral . fromEnum $ not a || (a && b)
m2TryEval (M2Apply M2Abs [ea]) = liftM abs (m2TryEval ea)
m2TryEval (M2Apply M2Floor [ea]) = liftM (fromIntegral . floor) (m2TryEval ea)
m2TryEval (M2Apply M2Ceiling [ea]) = liftM (fromIntegral . ceiling) (m2TryEval ea)
m2TryEval (M2Apply M2Eq [ea, eb]) = fmap (fromIntegral . fromEnum) $ liftM2 (==) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Neq [ea, eb]) = fmap (fromIntegral . fromEnum) $ liftM2 (/=) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Gt [ea, eb]) = fmap (fromIntegral . fromEnum) $ liftM2 (>) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Lt [ea, eb]) = fmap (fromIntegral . fromEnum) $ liftM2 (<) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Leq [ea, eb]) = fmap (fromIntegral . fromEnum) $ liftM2 (<=) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Geq [ea, eb]) = fmap (fromIntegral . fromEnum) $ liftM2 (>=) (m2TryEval ea) (m2TryEval eb)
m2TryEval (M2Apply M2Factorof [ea, eb]) = do
  a <- m2TryEval ea
  b <- m2TryEval eb
  return . fromIntegral . fromEnum $ (floor b) `rem` (floor a) == 0
-- To do: Definite integrals: m2TryEval (M2Apply M2Int ...)
m2TryEval (M2Apply M2Exp [ea]) = liftM exp (m2TryEval ea)
m2TryEval (M2Apply M2Ln [ea]) = liftM log (m2TryEval ea)
m2TryEval (M2Apply (M2Log mlb) [ea]) =
  liftM2 logBase (maybe (Just 10) m2TryEval mlb) (m2TryEval ea)

m2TryEval (M2Cn _ v) = Just v
m2TryEval M2True = Just 1
m2TryEval M2False = Just 0
m2TryEval M2Infinity = Nothing
m2TryEval M2Pi = Just pi
m2TryEval M2EulerGamma = Just 0.57721566490153286060651209008240243104215933593992
m2TryEval M2ExponentialE = Just (exp 1)
m2TryEval (M2Piecewise pw ow) = 
  (listToMaybe . flip mapMaybe pw $ \(mvalue, mcond) ->
    case m2TryEval mcond 
      of
        Just v | v /= 0 -> m2TryEval mvalue
        _ -> Nothing
  ) `mplus` (ow >>= m2TryEval)
m2TryEval _ = Nothing

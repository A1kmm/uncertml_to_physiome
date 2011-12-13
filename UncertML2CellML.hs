{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Text.XML.HXT.Core
import Data.List
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.XML.HXT.Arrow.ParserInterface
import Data.Maybe

data UncertMLDistribution =
  AsSamples [[Double]] |
  DirichletDistribution { dirichletConcentration :: [Double] } |
  ExponentialDistribution { exponentialRate :: Double } |
  GammaDistribution { gammaShape :: Double, 
                      gammaScale :: Double } |
  InverseGammaDistribution { invGammaShape :: Double, 
                             invGammaScale :: Double } |
  NormalInverseGammaDistribution {
    normalInvGammaMean :: Double,
    normalInvGammaVarianceScaling :: Double,
    normalInvGammaShape :: Double,
    normalInvGammaScale :: Double } |
  PoissonDistribution { poissonRate :: Double } |
  NormalDistribution { normalMean :: Double, 
                       normalVariance :: Double } |
  BinomialDistribution { binomialNumTrials :: Int,
                         binomialPSuccess :: Double } |
  MultinomialDistribution { multinomNumTrials :: Int,
                            multinomProbabilities :: [Double]} |
  LogNormalDistribution { logNormalLogScale :: Double,
                          logNormalShape :: Double } |
  StudentTDistribution { studentTLocation :: Double,
                         studentTScale :: Double,
                         studentTDegF :: Int } |
  UniformDistribution { uniformMinimum :: Double,
                        uniformMaximum :: Double } |
  MixtureModel { mixtureComponent :: [(Double, UncertMLDistribution)] } |
  MultivariateNormalDistribution { mvnormMean :: [Double],
                                   mvnormCov :: [[Double]] } |
  MultivariateStudentTDistribution { mvtMean :: [Double],
                                     mvtCov :: [[Double]],
                                     mvtDegF :: Int } |
  BetaDistribution { betaAlpha :: Double, betaBeta :: Double } |
  LaplaceDistribution { laplaceAlpha :: Double, laplaceBeta :: Double } |
  CauchyDistribution { cauchyLocation :: Double, cauchyScale :: Double } |
  WeibullDistribution { weibullScale :: Double, weibullShape :: Double } |
  LogisticDistribution { logisticLocation :: Double, logisticScale :: Double } |
  ChiSquareDistribution { chiSqDegF :: Int } |
  GeometricDistribution { geoProbability :: Double } |
  HypergeometricDistribution { hypergeoNumSuccess :: Int,
                               hypergeoNumTrials :: Int,
                               hypergeoPopSize :: Int } |
  FDistribution { fdistDenominator :: Double, fdistNumerator :: Double } |
  NegativeBinomialDistribution { negbinNumFailures :: Int, negbinProb :: Double } |
  ParetoDistribution { paretoScale :: Double, paretoSlope :: Double } |
  WishartDistribution { wishartDegF :: Double, wishartScale :: [[Double]] } |
  BernoulliDistribution { bernoulliProb :: Double } deriving(Eq, Ord, Show)

data MathML2Expression = M2Apply MathML2Op [MathML2Expression] | M2Ci String | M2Cn String Double | M2Lambda String MathML2Expression | M2Vector [MathML2Expression] | M2True |
                         M2False | M2Infinity | M2Pi | M2EulerGamma | M2ExponentialE | M2Piecewise [(MathML2Expression, MathML2Expression)] (Maybe MathML2Expression) deriving (Eq, Ord, Show)
data MathML2Op = M2Quotient | M2Factorial | M2Divide | M2Max | M2Min | M2Minus | M2Plus | M2Power | M2Rem | M2Times | M2Root { m2rootDegree :: Maybe MathML2Expression } | M2Gcd | M2And | M2Or | M2Xor | M2Not | M2Implies | M2Abs | M2Lcm | M2Floor | M2Ceiling | M2Eq | M2Neq | M2Gt | M2Lt | M2Geq | M2Leq | M2Factorof | M2Int { m2intLowlimit :: Maybe MathML2Expression, m2intUplimit :: Maybe MathML2Expression, m2intDegree :: Maybe MathML2Expression, m2intBvar :: String } | M2Diff { m2diffBvar :: String } | M2Exp | M2Ln | M2Log { m2logLogbase :: Maybe MathML2Expression } | M2Csymbol String  deriving (Eq, Ord, Show)

liftArrow :: Arrow a => (c -> d) -> a b c -> a b d
liftArrow = flip (>>^)
liftArrow2 :: Arrow a => (c -> c' -> d) -> a b c -> a b c' -> a b d
liftArrow2 f a1 a2 = arr (\a -> (a, a)) >>> (a1 *** a2) >>> arr (\(a, b) -> f a b)
liftArrow3 :: Arrow a => (c -> c' -> c'' -> d) -> a b c -> a b c' -> a b c'' -> a b d
liftArrow3 f a1 a2 a3 = arr (\a -> (a, (a, a))) >>> (a1 *** a2 *** a3) >>> arr (\(a, (b, c)) -> f a b c)
liftArrow4 :: Arrow a => (c -> c' -> c'' -> c''' -> d) -> a b c -> a b c' -> a b c'' -> a b c''' -> a b d
liftArrow4 f a1 a2 a3 a4 = arr (\a -> (a, (a, (a, a)))) >>> (a1 *** a2 *** a3 *** a4) >>> arr (\(a, (b, (c, d))) -> f a b c d)
liftArrow5 :: Arrow a => (c -> c' -> c'' -> c''' -> c'''' -> d) -> a b c -> a b c' -> a b c'' -> a b c''' -> a b c'''' -> a b d
liftArrow5 f a1 a2 a3 a4 a5 = arr (\a -> (a, (a, (a, (a, a))))) >>> (a1 *** a2 *** a3 *** a4 *** a5) >>> arr (\(a, (b, (c, (d, e)))) -> f a b c d e)

combinedChildText :: ArrowXml a => a XmlTree String
combinedChildText = getChildren >>> getText >. concat

readCombinedChildText :: (ArrowXml a, Read b) => a XmlTree b
readCombinedChildText = combinedChildText >>^ read

rightOrFail msg (Left err) = error $ msg ++ show err
rightOrFail _ (Right v) = v

parseCombinedChildText :: ArrowXml a => Parsec String () o -> a XmlTree o
parseCombinedChildText p = combinedChildText >>^ (\v -> rightOrFail "Parsing text node: " (parse p "XML text node" v))

uncertmlFloat = naturalOrFloat haskell >>=
              (\v ->
                case v of
                  Left n -> return $ fromIntegral n
                  Right f -> return f)
uncertmlWhitespace = many (oneOf " \t\r\n")
uncertmlList a = sepBy a uncertmlWhitespace
uncertmlListOfFloat = uncertmlList uncertmlFloat

xmlToUncertMLDistribution :: ArrowXml a => a XmlTree UncertMLDistribution
xmlToUncertMLDistribution =
  (hasQName (mkNsName "un:RandomSample" uncertmlNS) >>>
   liftArrow AsSamples (listA $ getChildren >>> hasQName (mkNsName "un:Realisation" uncertmlNS) />
                                hasQName (mkNsName "un:values" uncertmlNS) >>> parseCombinedChildText uncertmlListOfFloat)) <+>
  (hasQName (mkNsName "un:DirichletDistribution" uncertmlNS) >>>
   liftArrow DirichletDistribution (getChildren >>> hasQName (mkNsName "un:concentration" uncertmlNS) >>> parseCombinedChildText uncertmlListOfFloat)) <+>
  (hasQName (mkNsName "un:ExponentialDistribution" uncertmlNS) >>>
   liftArrow ExponentialDistribution (getChildren >>> hasQName (mkNsName "un:rate" uncertmlNS) >>> readCombinedChildText)) <+>
  (hasQName (mkNsName "un:GammaDistribution" uncertmlNS) >>>
   liftArrow2 GammaDistribution (getChildren >>> hasQName (mkNsName "un:shape" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:InverseGammaDistribution" uncertmlNS) >>>
   liftArrow2 InverseGammaDistribution (getChildren >>> hasQName (mkNsName "un:shape" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:NormalInverseGammaDistribution" uncertmlNS) >>>
   liftArrow4 NormalInverseGammaDistribution 
     (getChildren >>> hasQName (mkNsName "un:mean" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:varianceScaling" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:shape" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:PoissonDistribution" uncertmlNS) >>>
   liftArrow PoissonDistribution (getChildren >>> hasQName (mkNsName "un:rate" uncertmlNS) >>> readCombinedChildText)) <+>
  (hasQName (mkNsName "un:NormalDistribution" uncertmlNS) >>>
   liftArrow2 NormalDistribution
     (getChildren >>> hasQName (mkNsName "un:mean" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:variance" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:BinomialDistribution" uncertmlNS) >>>
   liftArrow2 BinomialDistribution
     (getChildren >>> hasQName (mkNsName "un:numberOfTrials" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:probabilityOfSuccess" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:MultinomialDistribution" uncertmlNS) >>>
   liftArrow2 MultinomialDistribution
     (getChildren >>> hasQName (mkNsName "un:numberOfTrials" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:probabilities" uncertmlNS) >>> parseCombinedChildText uncertmlListOfFloat)
  ) <+>
  (hasQName (mkNsName "un:LogNormalDistribution" uncertmlNS) >>>
   liftArrow2 LogNormalDistribution
     (getChildren >>> hasQName (mkNsName "un:logScale" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:shape" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:StudentTDistribution" uncertmlNS) >>>
   liftArrow3 StudentTDistribution
     (getChildren >>> hasQName (mkNsName "un:location" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:degreesOfFreedom" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:UniformDistribution" uncertmlNS) >>>
   liftArrow2 UniformDistribution
     (getChildren >>> hasQName (mkNsName "un:minimum" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:maximum" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:MixtureModel" uncertmlNS) >>>
   liftArrow MixtureModel
     (listA (getChildren >>> hasQName (mkNsName "un:component" uncertmlNS) >>> ((getAttrValue "weight" >>^ read) &&& (getChildren >>> xmlToUncertMLDistribution))))
  ) <+>
  (hasQName (mkNsName "un:MultivariateNormalDistribution" uncertmlNS) >>>
   liftArrow2 MultivariateNormalDistribution
     (getChildren >>> hasQName (mkNsName "un:mean" uncertmlNS) >>> parseCombinedChildText uncertmlListOfFloat)
     (getChildren >>> readCovarianceMatrix)
  ) <+>
  (hasQName (mkNsName "un:MultivariateStudentTDistribution" uncertmlNS) >>>
   liftArrow3 MultivariateStudentTDistribution
     (getChildren >>> hasQName (mkNsName "un:mean" uncertmlNS) >>> parseCombinedChildText uncertmlListOfFloat)
     (getChildren >>> readCovarianceMatrix)
     (getChildren >>> hasQName (mkNsName "un:degreesOfFreedom" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:BetaDistribution" uncertmlNS) >>>
   liftArrow2 BetaDistribution
     (getChildren >>> hasQName (mkNsName "un:alpha" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:beta" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:LaplaceDistribution" uncertmlNS) >>>
   liftArrow2 LaplaceDistribution
     (getChildren >>> hasQName (mkNsName "un:location" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:CauchyDistribution" uncertmlNS) >>>
   liftArrow2 CauchyDistribution
     (getChildren >>> hasQName (mkNsName "un:location" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:WeibullDistribution" uncertmlNS) >>>
   liftArrow2 WeibullDistribution
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:shape" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:LogisticDistribution" uncertmlNS) >>>
   liftArrow2 LogisticDistribution
     (getChildren >>> hasQName (mkNsName "un:location" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:ChiSquareDistribution" uncertmlNS) >>>
   liftArrow ChiSquareDistribution (getChildren >>> hasQName (mkNsName "un:degreesOfFreedom" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:GeometricDistribution" uncertmlNS) >>>
   liftArrow GeometricDistribution (getChildren >>> hasQName (mkNsName "un:probability" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:HypergeometricDistribution" uncertmlNS) >>>
   liftArrow3 HypergeometricDistribution
     (getChildren >>> hasQName (mkNsName "un:numberOfSuccesses" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:numberOfTrials" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:populationSize" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:FDistribution" uncertmlNS) >>>
   liftArrow2 FDistribution
     (getChildren >>> hasQName (mkNsName "un:denominator" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:numerator" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:NegativeBinomialDistribution" uncertmlNS) >>>
   liftArrow2 NegativeBinomialDistribution
     (getChildren >>> hasQName (mkNsName "un:numberOfFailures" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:probability" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:ParetoDistribution" uncertmlNS) >>>
   liftArrow2 ParetoDistribution
     (getChildren >>> hasQName (mkNsName "un:scale" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:slope" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:WishartDistribution" uncertmlNS) >>>
   liftArrow2 WishartDistribution
     (getChildren >>> hasQName (mkNsName "un:degreesOfFreedom" uncertmlNS) >>> readCombinedChildText)
     (getChildren >>> hasQName (mkNsName "un:scaleMatrix" uncertmlNS) >>> readCombinedChildText)
  ) <+>
  (hasQName (mkNsName "un:BernoulliDistribution" uncertmlNS) >>>
   liftArrow BernoulliDistribution
     (getChildren >>> hasQName (mkNsName "un:probabilities" uncertmlNS) >>> readCombinedChildText)
  )

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
  
To do next: MathML2ToXML for
M2Lambda String MathML2Expression | M2Vector [MathML2Expression] | M2True |
                         M2False | M2Infinity | M2Pi | M2EulerGamma | M2ExponentialE | M2Piecewise [(MathML2Expression, MathML2Expression)] (Maybe MathML2Expression) deriving (Eq, Ord, Show)



mathml2OpToXML M2Quotient = [mkqelem (mkNsName "mml:quotient" mathmlNS) [] []]
mathml2OpToXML M2Factorial = [mkqelem (mkNsName "mml:factorial" mathmlNS) [] []]
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
mathml2OpToXML (M2Csymbol cs) = [mkqelem (mkNsName "mml:csymbol" mathmlNS) [] [txt cs]]

readCovarianceMatrix = hasQName (mkNsName "un:covarianceMatrix" uncertmlNS) >>>
                       liftArrow2 chunkList (getAttrValue "dimension" >>^ read) (getChildren >>> hasQName (mkNsName "un:values" uncertmlNS) >>>
                                                                                 parseCombinedChildText uncertmlListOfFloat)
chunkList _ [] = []
chunkList dim l = let
    (h, t) = splitAt dim l
  in
     h:(chunkList dim t)

uncertmlNS = "http://www.uncertml.org/2.0"
mathmlNS = "http://www.w3.org/1998/Math/MathML"
cellmlNS = "http://www.cellml.org/cellml/1.1#"
randomSampleName = mkNsName "un:RandomSample" uncertmlNS
realisationName = mkNsName "un:Realisation" uncertmlNS

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA ((\v -> (v, v)) ^>> parseXmlDoc >>> propagateNamespaces >>> (xmlToUncertMLDistribution >>> arr unToMMLAST >>> unlistA >>> m2ToXML >>^ show))

unIsDiscrete (GeometricDistribution {}) = True
unIsDiscrete (HypergeometricDistribution {}) = True
unIsDiscrete (NegativeBinomialDistribution {}) = True
unIsDiscrete (BernoulliDistribution {}) = True
unIsDiscrete (PoissonDistribution {}) = True
unIsDiscrete (BinomialDistribution {}) = True
unIsDiscrete _ = False

sampleOutput ([]:_) = M2Ci "outvar"
sampleOutput ((a:[]):_) = M2Ci "outvar"
sampleOutput (l:_) = M2Vector $ map (\i -> M2Ci ("outvar" ++ show i)) [1..length l]
sampleOutput _ = M2Ci "outvar"

unToMMLAST (AsSamples v) = [M2Apply M2Eq [
                               sampleOutput v,
                               M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromRealisations")
                               [M2Vector (map (\r -> M2Vector (map (M2Cn "dimensionless") r)) v)]]]
unToMMLAST (DirichletDistribution conc) = error "Multivariate dirichlet distrib generator - todo"

unToMMLAST (NormalInverseGammaDistribution mean varScal shape scale) =
  let
    lambda = M2Cn "dimensionless" mean
  in
   error "Normal Inverse Gamma Distribution: To do"
   
unToMMLAST v = [M2Apply M2Eq [M2Ci "outvar",
                              unToMMLExprAST v
                             ]
               ]

unToMMLExprAST v | unIsDiscrete v =
    M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromMass") [M2Lambda "massBvar" $ unToMMLASTPMF (M2Ci "massBvar") v]
                 | otherwise = M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromDensity") [M2Lambda "densityBvar" $ unToMMLASTPDF (M2Ci "densityBvar") v]

unToMMLASTPDF x (ExponentialDistribution rate) =
  M2Piecewise [(M2Cn "dimensionless" 0, M2Apply M2Lt [x, (M2Cn "dimensionless" 0)])] $ Just $
    M2Apply M2Times [M2Cn "dimensionless" rate,
                     M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Times [M2Cn "dimensionless" rate, x]]]]
unToMMLASTPDF x (GammaDistribution shape scale) =
  let
    shapeP = M2Cn "dimensionless" shape
    scaleP = M2Cn "dimensionless" scale
  in
   M2Apply M2Times [M2Apply M2Power [x, M2Apply M2Minus [M2Apply M2Minus [shapeP, M2Cn "dimensionless" 1]]],
                    M2Apply M2Divide [M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Divide [x, scaleP]]],
                                      M2Apply M2Times [M2Apply M2Power [scaleP, shapeP], mmlGammaFunc shapeP]]]
  
unToMMLASTPDF x (InverseGammaDistribution shape scale) =
  let
    alpha = M2Cn "dimensionless" shape
    beta = M2Cn "dimensionless" scale
  in
   M2Apply M2Times [
     M2Apply M2Divide [M2Apply M2Power [beta, alpha], mmlGammaFunc alpha],
     M2Apply M2Power [x, M2Apply M2Minus [M2Apply M2Minus [alpha],
                                          M2Cn "dimensionless" 1]],
     M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Divide [beta, x]]]
     ]

unToMMLASTPDF x (NormalInverseGammaDistribution mean varScal shape scale) =
  error "Normal Inverse Gamma Distribution cannot currently be used in a mixture model"

unToMMLASTPDF x (NormalDistribution mean var) =
  let
    mu = M2Cn "dimensionless" mean
    sigma2 = M2Cn "dimensionless" var
  in
   M2Apply M2Times [
     M2Apply M2Divide [M2Cn "dimensionless" 1,
                       M2Apply (M2Root Nothing) [
                         M2Apply M2Times [M2Cn "dimensionless" 2,
                                          M2Pi, sigma2]]
                      ],
     M2Apply M2Exp [M2Apply M2Minus [
                       M2Apply M2Divide [
                          M2Apply M2Power [M2Apply M2Minus [x, mu], M2Cn "dimensionless" 2],
                          M2Apply M2Times [M2Cn "dimensionless" 2, sigma2]
                                        ]
                                    ]
                   ]
     ]

unToMMLASTPDF x (MultinomialDistribution numTrials pSuccesses) = error "Multinomial isn't supported yet"

unToMMLASTPDF x (LogNormalDistribution logScale shape) =
  let
    mu = M2Cn "dimensionless" logScale
    sigma = M2Cn "dimensionless" shape
  in
   M2Piecewise [(M2Cn "dimensionless" 0, M2Apply M2Leq [x, M2Cn "dimensionless" 0])] $ Just $
   M2Apply M2Times [
       M2Apply M2Divide [
          M2Cn "dimensionless" 1,
          M2Apply M2Times [
            x, sigma, M2Apply (M2Root Nothing) [
               M2Cn "dimensionless "2,
               M2Pi
                 ]
            ]
       ],
       M2Apply M2Exp [
           M2Apply M2Minus [
              M2Apply M2Divide [
                 M2Apply M2Power [
                    M2Apply M2Minus [M2Apply M2Ln [x], mu],
                    M2Cn "dimensionless" 2
                   ],
                 M2Apply M2Times [
                     M2Cn "dimensionless" 2,
                     M2Apply M2Power [ sigma, M2Cn "dimensionless" 2 ]
                   ]
               ]
            ]
         ]
     ]

unToMMLASTPDF x (StudentTDistribution loc scale degf) =
  let
    t = M2Apply M2Divide [M2Apply M2Minus [x, M2Cn "dimensionless" loc],
                          M2Cn "dimensionless" scale]
    nu = M2Cn "dimensionless" (fromIntegral degf)
    nuP1Half = M2Apply M2Divide [M2Apply M2Plus [nu, M2Cn "dimensionless" 1],
                                 M2Cn "dimensionless" 2]
  in
   M2Apply M2Times [
     M2Apply M2Divide [
        mmlGammaFunc nuP1Half,
        M2Apply M2Times [
            M2Apply (M2Root Nothing) [
               M2Apply M2Times [nu, M2Pi]
                           ],
            mmlGammaFunc (M2Apply M2Divide [nu, M2Cn "dimensionless" 2])
          ]
      ],
     M2Apply M2Power [
         M2Apply M2Plus [
            M2Cn "dimensionless" 1,
            M2Apply M2Divide [
                M2Apply M2Power [x, M2Cn "dimensionless" 2],
                nu
              ]
           ]
       ],
       M2Apply M2Minus [
           M2Apply M2Divide [
              M2Apply M2Plus [nu, M2Cn "dimensionless" 1],
              M2Cn "dimensionless" 2
            ]
         ]
     ]

unToMMLASTPDF x (UniformDistribution min max) =
  let
    a = M2Cn "dimensionless" min
    b = M2Cn "dimensionless" max
  in
   M2Piecewise [(M2Apply M2Divide [M2Cn "dimensionless" 1, M2Apply M2Minus [b, a]],
                 M2Apply M2And [M2Apply M2Geq [x, a], M2Apply M2Leq [x, b]])] $ Just
     (M2Cn "dimensionless" 0)

unToMMLASTPDF x (MixtureModel components) =
  M2Apply M2Plus $ flip map components $ \(weight, component) -> 
    M2Apply M2Times [M2Cn "dimensionless" weight, unToMMLExprAST component]

unToMMLASTPDF x (MultivariateNormalDistribution mean cov) = error "Multivariate normal distribution cannot currently be used in a mixture model"

unToMMLASTPDF x (MultivariateStudentTDistribution mean cov degf) = error "Multivariate student-T distribution cannot currently be used in a mixture model"

unToMMLASTPDF x (BetaDistribution alphaP betaP) =
  let
    alpha = M2Cn "dimensionless" alphaP
    beta = M2Cn "dimensionless" betaP
  in
   M2Apply M2Divide [
     M2Apply M2Times [
        M2Apply M2Power [x, M2Apply M2Minus [alpha, M2Cn "dimensionless" 1]],
        M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, x],
                         M2Apply M2Minus [beta, M2Cn "dimensionless" 1]]
        ],
     mmlBetaFunc alpha beta
     ]

unToMMLASTPDF x (LaplaceDistribution locP scaleP) =
  let
    loc = M2Cn "dimensionless" locP
    scale = M2Cn "dimensionless" scaleP
  in
    M2Apply M2Times
      [
        M2Apply M2Divide [M2Cn "dimensionless" 1,
                          M2Apply M2Times [M2Cn "dimensionless" 2, scale]],
        M2Apply M2Exp
          [
            M2Apply M2Minus
              [
                M2Apply M2Divide [ M2Apply M2Abs [ M2Apply M2Minus [x, loc] ], scale ]
              ]
          ]
      ]

unToMMLASTPDF x (CauchyDistribution locP scaleP) =
  let
    loc = M2Cn "dimensionless" locP
    scale = M2Cn "dimensionless" scaleP
  in
   M2Apply M2Divide
     [
       M2Cn "dimensionless" 1,
       M2Apply M2Times
         [
           M2Pi,
           scale,
           M2Apply M2Plus
             [
               M2Cn "dimensionless" 1,
               M2Apply M2Power
                 [
                   M2Apply M2Divide
                     [
                       M2Apply M2Minus [x, loc],
                       scale
                     ],
                   M2Cn "dimensionless" 2
                 ]
             ]
         ]
     ]

unToMMLASTPDF x (WeibullDistribution scaleP shapeP) =
  let
    scale = M2Cn "dimensionless" scaleP
    shape = M2Cn "dimensionless" shapeP
  in
   M2Apply M2Times
     [
       M2Apply M2Divide [shape, scale],
       M2Apply M2Power [M2Apply M2Divide [x, scale], M2Apply M2Minus [shape, M2Cn "dimensionless" 1]],
       M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Power [M2Apply M2Divide [x, scale], shape]]]
     ]
    
unToMMLASTPDF x (LogisticDistribution locP scaleP) =
  let
    loc = M2Cn "dimensionless" locP
    scale = M2Cn "dimensionless" scaleP
  in
   M2Apply M2Divide
     [
       M2Apply M2Exp [M2Apply M2Divide [M2Apply M2Minus [M2Apply M2Minus [x, loc]], scale]],
       M2Apply M2Times
         [
           scale,
           M2Apply M2Power
             [
               M2Apply M2Plus
                 [
                   M2Cn "dimensionless" 1,
                   M2Apply M2Exp [M2Apply M2Divide [M2Apply M2Minus [M2Apply M2Minus [x, loc]], scale]]
                 ],
               M2Cn "dimensionless" 2
             ]
         ]
     ]

unToMMLASTPDF x (ChiSquareDistribution degfP) =
  let
    degf = M2Cn "dimensionless" (fromIntegral degfP)
  in
   M2Apply M2Times
     [
       M2Apply M2Divide
         [
           M2Cn "dimensionless" 1,
           M2Apply M2Times
             [
               M2Apply M2Power [ M2Cn "dimensionless" 2, M2Apply M2Divide [degf, M2Cn "dimensionless" 2] ],
               mmlGammaFunc (M2Apply M2Divide [degf, M2Cn "dimensionless" 2])
             ]
         ],
       M2Apply M2Power
         [
           x, M2Apply M2Minus [M2Apply M2Divide [degf, M2Cn "dimensionless" 2], M2Cn "dimensionless" 1]
         ],
       M2Apply M2Exp
         [
           M2Apply M2Divide [M2Apply M2Minus [x], M2Cn "dimensionless" 2]
         ]
     ]
   
unToMMLASTPDF x (FDistribution denom num) =
  let
    d1 = M2Cn "dimensionless" num
    d2 = M2Cn "dimensionless" denom
  in
    M2Apply M2Divide
      [
        M2Apply (M2Root Nothing)
          [
            M2Apply M2Divide
              [
                M2Apply M2Times [ M2Apply M2Power [M2Apply M2Times [d1, x], d1], M2Apply M2Power [d2, d2] ],
                M2Apply M2Power [M2Apply M2Plus [M2Apply M2Times [d1, x], d2], M2Apply M2Plus [d1, d2]]
              ]
          ],
        M2Apply M2Times
          [
            x, mmlBetaFunc (M2Apply M2Divide [d1, M2Cn "dimensionless" 2]) (M2Apply M2Divide [d2, M2Cn "dimensionless" 2])
          ]
      ]

unToMMLASTPDF x (ParetoDistribution scale slope) =
  let
    xm = M2Cn "dimensionless" scale
    alpha = M2Cn "dimensionless" slope
  in
   M2Piecewise [
     (M2Apply M2Divide
      [
        M2Apply M2Times [ alpha, M2Apply M2Power [xm, alpha] ],
        M2Apply M2Power [ x, M2Apply M2Plus [ alpha, M2Cn "dimensionless" 1 ]]
      ],
      M2Apply M2Geq [x, xm])] (Just $ M2Cn "dimensionless" 0)

unToMMLASTPDF x (WishartDistribution degf scales) = error "Wishart distribution is not yet supported"

unToMMLASTPDF _ _ = error "PDF requested for unexpected UncertML distribution"

unToMMLASTPMF k (BernoulliDistribution prob) =
  let
    p = M2Cn "dimensionless" prob
  in
   M2Piecewise
     [(p, M2Apply M2Eq [k, M2Cn "dimensionless" 1]),
      (M2Apply M2Minus [M2Cn "dimensionless" 1, p], M2Apply M2Eq [k, M2Cn "dimensionless" 1])]
     (Just $ M2Cn "dimensionless" 0)

unToMMLASTPMF k (PoissonDistribution poissonRate) =
  let
    lambda = M2Cn "dimensionless" poissonRate
  in
    M2Apply M2Times [
      M2Apply M2Divide [M2Apply M2Power [lambda, k], M2Apply M2Factorial [k]],
      M2Apply M2Exp [M2Apply M2Minus [lambda]]
                    ]

unToMMLASTPMF k (GeometricDistribution prob) =
  let
    p = M2Cn "dimensionless" prob
  in
    M2Apply M2Times [M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, p], M2Apply M2Minus [k, M2Cn "dimensionless" 1]], p]

unToMMLASTPMF k (HypergeometricDistribution nsuccess ntrials npop) =
  let
    m = M2Cn "dimensionless" (fromIntegral nsuccess)
    bigN = M2Cn "dimensionless" (fromIntegral npop)
    smalln = M2Cn "dimensionless" (fromIntegral ntrials)
  in
   M2Apply M2Divide
     [
       M2Apply M2Times [mmlBinCoeff m k, mmlBinCoeff (M2Apply M2Minus [bigN, m]) (M2Apply M2Minus [m, k])],
       mmlBinCoeff bigN smalln
     ]

unToMMLASTPMF k (NegativeBinomialDistribution numFailures prob) =
  let
    r = M2Cn "dimensionless" (fromIntegral numFailures)
    p = M2Cn "dimensionless" prob
  in
   M2Apply M2Times
     [
       mmlBinCoeff (M2Apply M2Minus [M2Apply M2Plus [k, r], M2Cn "dimensionless" 1]) k,
       M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, p], r],
       M2Apply M2Power [p, k]
     ]

unToMMLASTPMF k (BinomialDistribution numTrials pSuccess) =
  let
    n = M2Cn "dimensionless" (fromIntegral numTrials)
    p = M2Cn "dimensionless" pSuccess
  in
   M2Apply M2Times [
     M2Apply M2Divide [
        M2Apply M2Factorial [n],
        M2Apply M2Times [M2Apply M2Factorial [k],
                         M2Apply M2Factorial [M2Apply M2Minus [n, k]]]
        ],
     M2Apply M2Power [p, k],
     M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, p],
                      M2Apply M2Minus [n, k]]
                   ]

mmlGammaFunc z =
  let
    t = M2Ci "gammaBvar"
  in
   M2Apply (M2Int { m2intDegree = Nothing, m2intLowlimit = Just (M2Cn "dimensionless" 0), m2intUplimit = Just M2Infinity, m2intBvar = "gammaBvar" })
     [M2Apply M2Times [M2Apply M2Power [t, M2Apply M2Minus [z, M2Cn "dimensionless" (-1)]],
                       M2Apply M2Exp [M2Apply M2Minus [t]]]]

mmlBetaFunc alpha beta =
  let
    u = M2Ci "betaBvar"
  in
   M2Apply (M2Int { m2intDegree = Nothing, m2intLowlimit = Just (M2Cn "dimensionless" 0), m2intUplimit = Just (M2Cn "dimensionless" 1), m2intBvar = "betaBvar" }) [
     M2Apply M2Times [
        M2Apply M2Power [u, M2Apply M2Minus [alpha, M2Cn "dimensionless" 1]],
        M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, u], M2Apply M2Minus [beta, M2Cn "dimensionless" 1]]
      ]]

mmlBinCoeff n k = M2Apply M2Divide [M2Apply M2Factorial [n],
                                    M2Apply M2Times [M2Apply M2Factorial [k], M2Apply M2Factorial [M2Apply M2Minus [n, k]]]]

arrowSum :: ArrowPlus a => [a b c] -> a b c
arrowSum = foldl' (<+>) zeroArrow

convertMultipleDistributions =
  arrowSum [convertRealisations]

convertRealisations = undefined

{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Text.XML.HXT.Core
import Data.List
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

data UncertMLDistribution =
  AsSamples [[Double]] |
  DirichletDistribution { dirichletConcentration :: Double } |
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
  MixtureModel { mixtureComponent :: [UncertMLDistribution] } |
  MultivariateNormalDistribution { mvnormMean :: [Double],
                                   mvnormCov :: [[Double]] } |
  MultivariateStudentTDistribution { mvtMean :: [Double],
                                     mvtCov :: [[Double]],
                                     mvtDegF :: Int } |
  BetaDistribution { betaAlpha :: Double, betaBeta :: Double } |
  LaplaceDistribution { laplaceAlpha :: Double, laplaceBeta :: Double } |
  CauchyDistribution { cauchyLocation :: Double, cauchyScale :: Double } |
  WeibullDistribution { weibullLocation :: Double, weibullScale :: Double } |
  LogisticDistribution { logisticLocation :: Double, logisticScale :: Double } |
  ChiSquareDistribution { chiSqDegF :: Int } |
  GeometricDistribution { geoProbability :: Double } |
  HypergeometricDistribution { hypergeoNumSuccess :: Int,
                               hypergeoNumTrials :: Int,
                               hypergeoPopSize :: Int } |
  FDistribution { fdistDenominator :: Double, fdistNumerator :: Double } |
  NegativeBinomialDistribution { negbinNumFailures :: Int, negbinProb :: Double } |
  ParetoDistribution { paretoScale :: Double, paretoShape :: Double } |
  WishartDistribution { wishartDegF :: Double, wishartScale :: [[Double]] } |
  BernoulliDistribution { bernoulliProb :: Double } deriving(Eq, Ord, Show)

data MathML2Expression = M2Apply MathML2Op [MathML2Expression] | M2Ci String | M2Cn String Double | M2Vector [MathML2Expression] | M2True |
                         M2False | M2Pi | M2EulerGamma | M2ExponentialE | M2Piecewise [(MathML2Expression, MathML2Expression)] deriving (Eq, Ord, Show)
data MathML2Op = M2Quotient | M2Factorial | M2Divide | M2Max | M2Min | M2Minus | M2Plus | M2Power | M2Rem | M2Times | M2Root { m2rootDegree :: Maybe MathML2Expression } | M2Gcd | M2And | M2Or | M2Xor | M2Not | M2Implies | M2Abs | M2Lcm | M2Floor | M2Ceiling | M2Eq | M2Neq | M2Gt | M2Lt | M2Geq | M2Leq | M2Factorof | M2Int { m2intLowlimit :: Maybe MathML2Expression, m2intUplimit :: Maybe MathML2Expression, m2intDegree :: Maybe MathML2Expression, m2intBvar :: String } | M2Diff { m2intBvar :: String } | M2Exp | M2Ln | M2Log { m2logLogbase :: Maybe MathML2Expression } | M2Csymbol String  deriving (Eq, Ord, Show)

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
   liftArrow DirichletDistribution (getChildren >>> hasQName (mkNsName "un:concentration" uncertmlNS) >>> readCombinedChildText)) <+>
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
     (listA (getChildren >>> hasQName (mkNsName "un:component" uncertmlNS) /> xmlToUncertMLDistribution))
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

readCovarianceMatrix = hasQName (mkNsName "un:covarianceMatrix" uncertmlNS) >>>
                       liftArrow2 chunkList (getAttrValue "dimension" >>^ read) (getChildren >>> hasQName (mkNsName "un:values" uncertmlNS) >>>
                                                                                 parseCombinedChildText uncertmlListOfFloat)
chunkList _ [] = []
chunkList dim l = let
    (h, t) = splitAt dim l
  in
     h:(chunkList dim t)

uncertmlNS = "http://www.uncertml.org/2.0"
randomSampleName = mkNsName "un:RandomSample" uncertmlNS
realisationName = mkNsName "un:Realisation" uncertmlNS

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xread >>> propagateNamespaces >>> (xshow this <+> constA "Out: " <+> (xmlToUncertMLDistribution >>^ show)))

unToMMLAST (AsSamples v) = M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromRealisations")
                             [M2Vector (map (\r -> M2Vector (map (M2Cn "dimensionless") r)) v)]
{-
  DirichletDistribution { dirichletConcentration :: Double } |
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
  MixtureModel { mixtureComponent :: [UncertMLDistribution] } |
  MultivariateNormalDistribution { mvnormMean :: [Double],
                                   mvnormCov :: [[Double]] } |
  MultivariateStudentTDistribution { mvtMean :: [Double],
                                     mvtCov :: [[Double]],
                                     mvtDegF :: Int } |
  BetaDistribution { betaAlpha :: Double, betaBeta :: Double } |
  LaplaceDistribution { laplaceAlpha :: Double, laplaceBeta :: Double } |
  CauchyDistribution { cauchyLocation :: Double, cauchyScale :: Double } |
  WeibullDistribution { weibullLocation :: Double, weibullScale :: Double } |
  LogisticDistribution { logisticLocation :: Double, logisticScale :: Double } |
  ChiSquareDistribution { chiSqDegF :: Int } |
  GeometricDistribution { geoProbability :: Double } |
  HypergeometricDistribution { hypergeoNumSuccess :: Int,
                               hypergeoNumTrials :: Int,
                               hypergeoPopSize :: Int } |
  FDistribution { fdistDenominator :: Double, fdistNumerator :: Double } |
  NegativeBinomialDistribution { negbinNumFailures :: Int, negbinProb :: Double } |
  ParetoDistribution { paretoScale :: Double, paretoShape :: Double } |
  WishartDistribution { wishartDegF :: Double, wishartScale :: [[Double]] } |
  BernoulliDistribution { bernoulliProb :: Double } deriving(Eq, Ord, Show)
-}


arrowSum :: ArrowPlus a => [a b c] -> a b c
arrowSum = foldl' (<+>) zeroArrow

convertMultipleDistributions =
  arrowSum [convertRealisations]
convertRealisations = undefined

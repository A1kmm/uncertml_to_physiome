{-# LANGUAGE NoMonomorphismRestriction #-}
module UncertML (UncertMLDistribution(..), xmlToUncertMLDistribution,
                 uncertmlASTToXML, unToXML, uncertmlNS)
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
  ParetoDistribution { paretoScale :: Double, paretoShape :: Double } |
  WishartDistribution { wishartDegF :: Double, wishartScale :: [[Double]] } |
  BernoulliDistribution { bernoulliProb :: Double } deriving(Eq, Ord, Show)

uncertmlNS = "http://www.uncertml.org/2.0"

uncertmlFloat = do
  uncertmlWhitespace
  m <- (char '-' >> return (-1)) <|> (return 1)
  v <- naturalOrFloat haskell
  case v of
       Left n -> return $ m * fromIntegral n
       Right f -> return $ m * f

uncertmlWhitespace = many (oneOf " \t\r\n")
uncertmlList a = sepBy a uncertmlWhitespace
uncertmlListOfFloat = uncertmlList uncertmlFloat

xmlToUncertMLDistribution :: ArrowXml a => a XmlTree UncertMLDistribution
xmlToUncertMLDistribution =
  (hasQName (uelName "RandomSample") >>>
   liftArrow AsSamples (listA $ getChildren >>> hasQName (uelName "Realisation") />
                                hasQName (uelName "values") >>> parseCombinedChildText uncertmlListOfFloat)) <+>
  (hasQName (uelName "DirichletDistribution") >>>
   liftArrow DirichletDistribution (getChildren >>> hasQName (uelName "concentration") >>> parseCombinedChildText uncertmlListOfFloat)) <+>
  (hasQName (uelName "ExponentialDistribution") >>>
   liftArrow ExponentialDistribution (getChildren >>> hasQName (uelName "rate") >>> readCombinedChildText)) <+>
  (hasQName (uelName "GammaDistribution") >>>
   liftArrow2 GammaDistribution (getChildren >>> hasQName (uelName "shape") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "InverseGammaDistribution") >>>
   liftArrow2 InverseGammaDistribution (getChildren >>> hasQName (uelName "shape") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "NormalInverseGammaDistribution") >>>
   liftArrow4 NormalInverseGammaDistribution 
     (getChildren >>> hasQName (uelName "mean") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "varianceScaling") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "shape") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "PoissonDistribution") >>>
   liftArrow PoissonDistribution (getChildren >>> hasQName (uelName "rate") >>> readCombinedChildText)) <+>
  (hasQName (uelName "NormalDistribution") >>>
   liftArrow2 NormalDistribution
     (getChildren >>> hasQName (uelName "mean") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "variance") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "BinomialDistribution") >>>
   liftArrow2 BinomialDistribution
     (getChildren >>> hasQName (uelName "numberOfTrials") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "probabilityOfSuccess") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "MultinomialDistribution") >>>
   liftArrow2 MultinomialDistribution
     (getChildren >>> hasQName (uelName "numberOfTrials") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "probabilities") >>> parseCombinedChildText uncertmlListOfFloat)
  ) <+>
  (hasQName (uelName "LogNormalDistribution") >>>
   liftArrow2 LogNormalDistribution
     (getChildren >>> hasQName (uelName "logScale") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "shape") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "StudentTDistribution") >>>
   liftArrow3 StudentTDistribution
     (getChildren >>> hasQName (uelName "location") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "degreesOfFreedom") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "UniformDistribution") >>>
   liftArrow2 UniformDistribution
     (getChildren >>> hasQName (uelName "minimum") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "maximum") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "MixtureModel") >>>
   liftArrow MixtureModel
     (listA (getChildren >>> hasQName (uelName "component") >>> ((getAttrValue "weight" >>^ read) &&& (getChildren >>> xmlToUncertMLDistribution))))
  ) <+>
  (hasQName (uelName "MultivariateNormalDistribution") >>>
   liftArrow2 MultivariateNormalDistribution
     (getChildren >>> hasQName (uelName "mean") >>> parseCombinedChildText uncertmlListOfFloat)
     (getChildren >>> readCovarianceMatrix)
  ) <+>
  (hasQName (uelName "MultivariateStudentTDistribution") >>>
   liftArrow3 MultivariateStudentTDistribution
     (getChildren >>> hasQName (uelName "mean") >>> parseCombinedChildText uncertmlListOfFloat)
     (getChildren >>> readCovarianceMatrix)
     (getChildren >>> hasQName (uelName "degreesOfFreedom") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "BetaDistribution") >>>
   liftArrow2 BetaDistribution
     (getChildren >>> hasQName (uelName "alpha") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "beta") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "LaplaceDistribution") >>>
   liftArrow2 LaplaceDistribution
     (getChildren >>> hasQName (uelName "location") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "CauchyDistribution") >>>
   liftArrow2 CauchyDistribution
     (getChildren >>> hasQName (uelName "location") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "WeibullDistribution") >>>
   liftArrow2 WeibullDistribution
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "shape") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "LogisticDistribution") >>>
   liftArrow2 LogisticDistribution
     (getChildren >>> hasQName (uelName "location") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "ChiSquareDistribution") >>>
   liftArrow ChiSquareDistribution (getChildren >>> hasQName (uelName "degreesOfFreedom") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "GeometricDistribution") >>>
   liftArrow GeometricDistribution (getChildren >>> hasQName (uelName "probability") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "HypergeometricDistribution") >>>
   liftArrow3 HypergeometricDistribution
     (getChildren >>> hasQName (uelName "numberOfSuccesses") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "numberOfTrials") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "populationSize") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "FDistribution") >>>
   liftArrow2 FDistribution
     (getChildren >>> hasQName (uelName "denominator") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "numerator") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "NegativeBinomialDistribution") >>>
   liftArrow2 NegativeBinomialDistribution
     (getChildren >>> hasQName (uelName "numberOfFailures") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "probability") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "ParetoDistribution") >>>
   liftArrow2 ParetoDistribution
     (getChildren >>> hasQName (uelName "scale") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "shape") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "WishartDistribution") >>>
   liftArrow2 WishartDistribution
     (getChildren >>> hasQName (uelName "degreesOfFreedom") >>> readCombinedChildText)
     (getChildren >>> hasQName (uelName "scaleMatrix") >>> readCombinedChildText)
  ) <+>
  (hasQName (uelName "BernoulliDistribution") >>>
   liftArrow BernoulliDistribution
     (getChildren >>> hasQName (uelName "probabilities") >>> readCombinedChildText)
  )

readCovarianceMatrix = hasQName (uelName "covarianceMatrix") >>>
                       liftArrow2 chunkList (getAttrValue "dimension" >>^ read) (getChildren >>> hasQName (uelName "values") >>>
                                                                                 parseCombinedChildText uncertmlListOfFloat)

uelName n = mkNsName ("un:" ++ n) uncertmlNS

makeUncertMLList n v = mkqelem (uelName n) [] [txt . intercalate " " $ map show v]
makeUncertMLNamedShow n v = mkqelem (uelName n) [] [txt $ show v]

unToXML = (\v -> (uncertmlASTToXML v, v)) ^>> app

uncertmlASTToXML (AsSamples s) =
  mkqelem (uelName "RandomSample") []
    (map (\rls -> mkqelem (uelName "Realisation") [] [makeUncertMLList "values" rls]) s)
uncertmlASTToXML (DirichletDistribution dirconc) =
  mkqelem (uelName "DirichletDistribution") [] [
      makeUncertMLNamedShow "concentration" dirconc
    ]
uncertmlASTToXML (ExponentialDistribution exprate) =
  mkqelem (uelName "ExponentialDistribution") [] [
      makeUncertMLNamedShow "rate" exprate
    ]
uncertmlASTToXML (GammaDistribution shape scale) =
  mkqelem (uelName "GammaDistribution") [] [
      makeUncertMLNamedShow "shape" shape,
      makeUncertMLNamedShow "scale" scale
    ]
uncertmlASTToXML (InverseGammaDistribution shape scale) =
  mkqelem (uelName "InverseGammaDistribution") [] [
      makeUncertMLNamedShow "shape" shape,
      makeUncertMLNamedShow "scale" scale
    ]
uncertmlASTToXML (NormalInverseGammaDistribution mean varscal shape scale) =
  mkqelem (uelName "NormalInverseGammaDistribution") [] [
      makeUncertMLNamedShow "mean" mean,
      makeUncertMLNamedShow "varianceScaling" varscal,
      makeUncertMLNamedShow "shape" shape,
      makeUncertMLNamedShow "scale" scale
    ]
uncertmlASTToXML (PoissonDistribution rate) =
  mkqelem (uelName "PoissonDistribution") [] [
      makeUncertMLNamedShow "rate" rate
    ]
uncertmlASTToXML (NormalDistribution mean var) =
  mkqelem (uelName "NormalDistribution") [] [
      makeUncertMLNamedShow "mean" mean,
      makeUncertMLNamedShow "variance" var
    ]
uncertmlASTToXML (BinomialDistribution ntrials psuccess) =
  mkqelem (uelName "BinomialDistribution") [] [
      makeUncertMLNamedShow "numberOfTrials" ntrials,
      makeUncertMLNamedShow "probabilityOfSuccess" psuccess
  ]
uncertmlASTToXML (MultinomialDistribution ntrials probs) =
  mkqelem (uelName "MultinomialDistribution") [] [
      makeUncertMLNamedShow "numberOfTrials" ntrials,
      makeUncertMLList "probabilities" probs
    ]
uncertmlASTToXML (LogNormalDistribution logscale shape) =
  mkqelem (uelName "LogNormalDistribution") [] [
      makeUncertMLNamedShow "logScale" logscale,
      makeUncertMLNamedShow "shape" shape
  ]
uncertmlASTToXML (StudentTDistribution loc scal df) =
  mkqelem (uelName "StudentTDistribution") [] [
      makeUncertMLNamedShow "location" loc,
      makeUncertMLNamedShow "scale" scal,
      makeUncertMLNamedShow "degreesOfFreedom" df
  ]
uncertmlASTToXML (UniformDistribution minv maxv) =
  mkqelem (uelName "UniformDistribution") [] [
      makeUncertMLNamedShow "minimum" minv,
      makeUncertMLNamedShow "maximum" maxv
  ]
uncertmlASTToXML (MixtureModel l) =
  mkqelem (uelName "MixtureModel") [] (map (\(w,d) -> mkqelem (uelName "component") [sattr "weight" (show w)] [uncertmlASTToXML d]) l)
uncertmlASTToXML (MultivariateNormalDistribution mean cov) =
  mkqelem (uelName "MultivariateNormalDistribution") [] [
      makeUncertMLNamedShow "mean" mean,
      mkqelem (uelName "covarianceMatrix")
        [sattr "dimension" (show . length . head $ cov)]
        [makeUncertMLList "values" $ concat cov]
    ]
uncertmlASTToXML (MultivariateStudentTDistribution mean cov df) =
  mkqelem (uelName "MultivariateStudentTDistribution") [] [
      makeUncertMLNamedShow "mean" mean,
      mkqelem (uelName "covarianceMatrix")
        [sattr "dimension" (show . length . head $ cov)]
        [makeUncertMLList "values" $ concat cov],
      makeUncertMLNamedShow "degreesOfFreedom" df
    ]
uncertmlASTToXML (BetaDistribution alpha beta) =
  mkqelem (uelName "BetaDistribution") [] [
      makeUncertMLNamedShow "alpha" alpha,
      makeUncertMLNamedShow "beta" beta
    ]
uncertmlASTToXML (LaplaceDistribution loc scal) =
  mkqelem (uelName "LaplaceDistribution") [] [
      makeUncertMLNamedShow "location" loc,
      makeUncertMLNamedShow "scale" scal
    ]
uncertmlASTToXML (CauchyDistribution loc scal) =
  mkqelem (uelName "CauchyDistribution") [] [
      makeUncertMLNamedShow "location" loc,
      makeUncertMLNamedShow "scale" scal
    ]
uncertmlASTToXML (WeibullDistribution scal shape) =
  mkqelem (uelName "WeibullDistribution") [] [
      makeUncertMLNamedShow "scale" scal,
      makeUncertMLNamedShow "shape" shape
    ]
uncertmlASTToXML (LogisticDistribution loc scal) =
  mkqelem (uelName "LogisticDistribution") [] [
      makeUncertMLNamedShow "location" loc,
      makeUncertMLNamedShow "scale" scal
    ]
uncertmlASTToXML (ChiSquareDistribution df) =
  mkqelem (uelName "ChiSquareDistribution") [] [
      makeUncertMLNamedShow "degreesOfFreedom" df
    ]
uncertmlASTToXML (GeometricDistribution prob) =
  mkqelem (uelName "GeometricDistribution") [] [
      makeUncertMLNamedShow "probability" prob
    ]
uncertmlASTToXML (HypergeometricDistribution nsuc ntri npop) =
  mkqelem (uelName "HypergeometricDistribution") [] [
      makeUncertMLNamedShow "numberOfSuccesses" nsuc,
      makeUncertMLNamedShow "numberOfTrials" ntri,
      makeUncertMLNamedShow "populationSize" npop
    ]
uncertmlASTToXML (FDistribution denom num) =
  mkqelem (uelName "FDistribution") [] [
      makeUncertMLNamedShow "denominator" denom,
      makeUncertMLNamedShow "numerator" num
    ]
uncertmlASTToXML (NegativeBinomialDistribution numFails prob) =
  mkqelem (uelName "NegativeBinomialDistribution") [] [
      makeUncertMLNamedShow "numberOfFailures" numFails,
      makeUncertMLNamedShow "probability" prob
    ]
uncertmlASTToXML (ParetoDistribution scal shape) =
  mkqelem (uelName "ParetoDistribution") [] [
      makeUncertMLNamedShow "scale" scal,
      makeUncertMLNamedShow "shape" shape
    ]
uncertmlASTToXML (WishartDistribution df scalMat) =
  mkqelem (uelName "WishartDistribution") [] [
      makeUncertMLNamedShow "degreesOfFreedom" df,
      mkqelem (uelName "scaleMatrix")
        [sattr "dimension" (show . length . head $ scalMat)]
        [makeUncertMLList "values" $ concat scalMat]
    ]
uncertmlASTToXML (BernoulliDistribution prob) =
  mkqelem (uelName "BernoulliDistribution") [] [
      makeUncertMLNamedShow "probabilities" prob
    ]

import Control.Monad
import Text.XML.HXT.Core

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
  BernoulliDistribution { bernoulliProb :: Double }
  
                            
{-
            DirichletDistribution concentration,
            ExponentialDistribution rate,
            GammaDistribution shape scale,
            InverseGammaDistribution shape scale,
            NormalInverseGammaDistribution mean varianceScaling shape scale,
            PoissonDistribution rate,
            NormalDistribution mean variance,
            BinomialDistribution numberOfTrials probabilityOfSuccess,
            MultinomialDistribution numberOfTrials probabilities,
            LogNormalDistribution logScale shape,
            StudentTDistribution location scale degreesOfFreedom,
            UniformDistribution minimum maximum | numberOfClasses,
            MixtureModel (component (anyDistribution weight))*,
            MultivariateNormalDistribution mean covarianceMatrix,
            MultivariateStudentTDistribution mean covarianceMatrix degreesOfFreedom,
            BetaDistribution alpha beta,
            LaplaceDistribution location scale,
            CauchyDistribution location scale,
            WeibullDistribution scale shape,
            LogisticDistribution location scale,
            ChiSquareDistribution degreesOfFreedom,
            GeometricDistribution probability,
            HypergeometricDistribution numberOfSuccesses numberOfTrials populationSize,
            FDistribution denominator numerator,
            NegativeBinomialDistribution numberOfFailures probability
            ParetoDistribution scale shape,
            WishartDistribution degreesOfFreedom scaleMatrix,
            BernoulliDistribution probabilities
 -}


uncertmlNS = "http://www.uncertml.org/2.0"
randomSampleName = mkNsName "un:RandomSample" uncertmlNS
realisationName = mkNsName "un:Realisation" uncertmlNS
realisationName = mkNsName "un:Realisation" uncertmlNS

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xshow (xread >>> propagateNamespaces >>> convertMultipleDistributions))

arrowSum :: ArrowPlus a => [a b c] -> a b c
arrowSum = foldl' (<+>) zeroArrow l


convertMultipleDistributions =
  arrowSum [convertRealisations,
           ]
import Control.Monad
import Text.XML.HXT.Core
import Data.List
import Text.XML.HXT.Arrow.ParserInterface
import Data.Maybe
import Data.Packed.Matrix
import UncertML
import MathML
import Numeric.LinearAlgebra.Algorithms
import Numeric.Container

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xshow $ mkqelem (mkNsName "mml:math" mathmlNS) [sattr "xmlns:mml" mathmlNS, sattr "xmlns:cellml" cellmlNS] [(\v -> (v, v)) ^>> parseXmlDoc >>> propagateNamespaces >>> (xmlToUncertMLDistribution >>> arr unToMMLAST >>> unlistA >>> m2ToXML)])

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
                               M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
                                 [
                                   M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromRealisations")
                                     [M2Vector (map (\r -> M2Vector (map (M2Cn "dimensionless") r)) v)]
                                 ]
                               ]
                           ]

unToMMLAST (DirichletDistribution alphafull@(alpha1:(alphatail@(_:_)))) =
  let
      x = M2Ci "x"
      initialAlpha = M2Cn "dimensionless" alpha1
      initialBeta = M2Cn "dimensionless" (sum alphatail)
      k = length alphafull
      out1 = M2Ci "out1"
      outk = M2Ci ("out" ++ show k)
      alphatt = tail alphatail
      partialSums = take (k - 2) $ scanl1 (+) alphafull
      partialSumsBack = reverse (scanl1 (+) (reverse alphatt))
      intEqns = map (\(i, alpha, beta) ->
                      let
                        a = M2Cn "dimensionless" alpha
                        b = M2Cn "dimensionless" beta
                        tmp = M2Ci $ "dirichletIntermediate" ++ (show i)
                        in
                         M2Apply M2Eq
                           [tmp,
                            M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
                              [
                                M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromDensity")
                                  [
                                    M2Lambda "x" $
                                      M2Apply M2Times
                                        [M2Apply M2Divide [M2Cn "dimensionless" 1, mmlBetaFunc a b],
                                         M2Apply M2Power [x, M2Apply M2Minus [a, M2Cn "dimensionless" 1]],
                                         M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, x], M2Apply M2Minus [b, M2Cn "dimensionless" 1]]
                                        ]
                                  ]
                              ]
                           ]
                    ) $ zip3 [2..] alphatt partialSumsBack
      varEqns = map (\(i, v) ->
                      let
                        outi = M2Ci $ "out" ++ show i
                        tmpi = M2Ci $ "dirichletIntermediate" ++ show i
                      in
                       M2Apply M2Eq [outi,
                                     M2Apply M2Times [M2Cn "dimensionless" (1 - v), tmpi]]
                    ) $ zip [2..] partialSums
   in
    [M2Apply M2Eq
      [out1,
       M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
         [
           M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromDensity")
             [
               M2Lambda "x" $
                 M2Apply M2Times
                   [M2Apply M2Divide [M2Cn "dimensionless" 1, mmlBetaFunc initialAlpha initialBeta],
                    M2Apply M2Power [x, M2Apply M2Minus [initialAlpha, M2Cn "dimensionless" 1]],
                    M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, x], M2Apply M2Minus [initialBeta, M2Cn "dimensionless" 1]]
                   ]
             ]
         ]
      ],
     M2Apply M2Eq
       [
         outk,
         M2Apply M2Minus ((M2Cn "dimensionless" 1):(map (\i -> M2Ci ("out" ++ show i)) [1..(k - 1)]))
       ]
    ] ++ intEqns ++ varEqns
   
unToMMLAST (DirichletDistribution _) = error "Invalid Dirichlet distribution - at least two concentration parameters required."

unToMMLAST (NormalInverseGammaDistribution mean varScal shape scale) =
  let
    x = M2Ci "x"
    lambda = M2Cn "dimensionless" mean
    nu = M2Cn "dimensionless" varScal
    alpha = M2Cn "dimensionless" shape
    beta = M2Cn "dimensionless" scale
    sigma2 = M2Apply M2Divide [ M2Ci "outvar", nu ]
  in
   [M2Apply M2Eq
       [M2Ci "outvar",
        M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
          [
            M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromDensity")
              [M2Lambda "x" $
                M2Apply M2Times [
                  M2Apply M2Divide [M2Apply M2Power [beta, alpha], mmlGammaFunc alpha],
                  M2Apply M2Power [x, M2Apply M2Minus [M2Apply M2Minus [alpha],
                                                       M2Cn "dimensionless" 1]],
                  M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Divide [beta, x]]]
                ]
              ]
          ]
       ],
    M2Apply M2Eq
      [M2Ci "outmean",
       M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
         [M2Lambda "x" $
            M2Apply M2Times [
              M2Apply M2Divide [M2Cn "dimensionless" 1,
                                M2Apply (M2Root Nothing) [
                                  M2Apply M2Times [M2Cn "dimensionless" 2,
                                                   M2Pi, sigma2]]
                               ],
              M2Apply M2Exp [M2Apply M2Minus [
                                M2Apply M2Divide [
                                   M2Apply M2Power [M2Apply M2Minus [x, lambda], M2Cn "dimensionless" 2],
                                   M2Apply M2Times [M2Cn "dimensionless" 2, sigma2]
                                   ]
                                ]
                            ]
              ]
         ]
      ]
   ]

unToMMLAST (MultinomialDistribution numTrials pvals) =
  let
    n = M2Cn "dimensionless" (fromIntegral numTrials)
    recursivelyBuildEquations i nexpr (pval:prest) =
      let
        xvar = M2Ci ("out" ++ show i)
        pexpr = M2Cn "dimensionless" pval
        massBvar = M2Ci "massBvar"
      in
       (M2Apply M2Eq
          [xvar,
           M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
           [M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromMass") [
             M2Lambda "massBvar" $
               M2Apply M2Times [
                 M2Apply M2Divide [
                    M2Apply M2Factorial [nexpr],
                    M2Apply M2Times [M2Apply M2Factorial [massBvar],
                                     M2Apply M2Factorial [M2Apply M2Minus [nexpr, massBvar]]]
                    ],
                 M2Apply M2Power [pexpr, massBvar],
                 M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, pexpr],
                                  M2Apply M2Minus [nexpr, massBvar]]
                 ]
             ]
          ]]):
       (recursivelyBuildEquations (i + 1) (M2Apply M2Minus [nexpr, xvar]) prest)
    recursivelyBuildEquations _ _ [] = []
  in
    recursivelyBuildEquations 1 n pvals

unToMMLAST (MultivariateNormalDistribution mean cov) = 
  let
      n = length mean
      (_, _, dists') = foldl' addOneDist (mean, cov, []) [1..n]
      addOneDist (mu:[], (sigma2:[]):[], dists) i = ([], [], (oneNormalDist i (M2Cn "dimensionless" mu) sigma2):dists)
      addOneDist (mu:remainingMu, ((sigma2val:sigmarow1):sigmarow2plus), dists) i =
        let
          sigmaRowP = (1 >< (n - i)) sigmarow1
          sigmaColumnP = ((n - i) >< 1) (map head sigmarow2plus)
          sigmaBlock = map (drop 1) sigmarow2plus
          sigmaBlockP = fromLists sigmaBlock
          sigmaBlockInv = inv sigmaBlockP
          sigmaFactorP = sigmaRowP <> sigmaBlockP
          sigmaFactor = head . toLists $ sigmaFactorP
          variance = sigma2val - (sigmaFactorP <> sigmaColumnP)@@>(0, 0)
          mu = M2Apply M2Plus $
            map (\(sf, muj, j) -> M2Apply M2Times
                                       [M2Cn "dimensionless" sf,
                                        M2Apply M2Minus [M2Ci ("outvar" ++ show j), M2Cn "dimensionless" muj]]) $
               zip3 sigmaFactor remainingMu [(i + 1)..]
        in
          (remainingMu, sigmaBlock, (oneNormalDist i mu variance):dists)
      addOneDist (mu, sigma, dists) i = (mu, sigma, dists)
      oneNormalDist i mu sigma2v =
        let
          sigma2 = M2Cn "dimensionless" sigma2v
          x = M2Ci "densityBvar"
        in
         M2Apply M2Eq [M2Ci ("outvar" ++ show i),
                       M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution") [
                         M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromDensity") [
                            M2Lambda "densityBvar" $
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
                            ]
                         ]
                       ]
  in
   dists'

unToMMLAST (MultivariateStudentTDistribution mean cov degf) = error "Multivariate student-T distribution not yet supported"

unToMMLAST v = [M2Apply M2Eq [M2Ci "outvar",
                              M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution")
                                [unToMMLExprAST v]
                             ]
               ]

unToMMLExprAST v | unIsDiscrete v =
    M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromMass") [M2Lambda "massBvar" $ unToMMLASTPMF (M2Ci "massBvar") v]
                 | otherwise = M2Apply (M2Csymbol "http://www.cellml.org/uncertainty-1#distributionFromDensity") [M2Lambda "densityBvar" $ unToMMLASTPDF (M2Ci "densityBvar") v]

unToMMLASTPDF x (ExponentialDistribution rate) =
  M2Piecewise [(M2Cn "dimensionless" 0, M2Apply M2Lt [x, (M2Cn "dimensionless" 0)])] $ Just $
    M2Apply M2Times [M2Cn "dimensionless" rate,
                     M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Times [M2Cn "dimensionless" rate, x]]]]
    
unToMMLASTPDF x (DirichletDistribution _) =
  error "DirichletDistribution cannot be used in a mixture model"
  
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

unToMMLASTPDF x (MultinomialDistribution numTrials pSuccesses) = error "Multinomial distribution cannot be used from a mixture model"

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

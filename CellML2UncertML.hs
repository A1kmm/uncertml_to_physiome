import Text.XML.HXT.Core
import MathML
import UncertML
import Control.Monad
import Text.XML.HXT.Arrow.ParserInterface
import Data.Maybe
import ParsingSupport
import Debug.Trace

main = do
  liftM doConversion getContents >>= putStr

doConversion :: String -> String
doConversion = concat . runLA (xshow $ (\v -> (v, v)) ^>> parseXmlDoc >>> propagateNamespaces >>> hasQName (mkNsName "mml:math" mathmlNS) /> xmlToMathML2 >>> arrMaybe mmlToUnAST >>> unToXML)

mmlToUnAST (M2Apply M2Eq [l, M2Apply (M2Csymbol upd) [d]])
  | upd == "http://www.cellml.org/uncertainty-1#uncertainParameterWithDistribution" =
    Debug.Trace.trace (show d) $! mmlDistToUnAST d
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
mmlRealisationsToUnAST :: MathML2Expression -> Maybe UncertMLDistribution
mmlRealisationsToUnAST (M2Vector l) =
  liftM AsSamples $
      (
        sequence $ map (\v -> mmlExtractVector v >>= sequence . map m2TryEval) l
      ) `mplus` (
        sequence . map (liftM (:[]) . m2TryEval) $ l
      )
mmlRealisationsToUnAST _ = Nothing

mmlMassToUnAST (M2Lambda k v) = mmlMassFToUnAST k v

mmlMassFToUnAST k (M2Apply M2Times [M2Apply M2Divide [M2Apply M2Power [lambda, M2Ci k1], M2Apply M2Factorial [M2Ci k2]],
                                    M2Apply M2Exp [M2Apply M2Minus [lambda2]]])
  | lambda == lambda2 && k1 == k && k2 == k = liftM PoissonDistribution (m2TryEval lambda)

mmlMassFToUnAST k (M2Apply M2Times [M2Apply M2Power [M2Apply M2Minus [M2Cn _ one1, p],
                                                     M2Apply M2Minus [M2Ci k1, M2Cn _ one2]], p1])
  | one1 == 1 && one2 == 1 && k1 == k && p1 == p = liftM GeometricDistribution (m2TryEval p)

mmlMassFToUnAST k (M2Apply M2Divide [
                      M2Apply M2Times [
                         M2Apply M2Divide [M2Apply M2Factorial [m],
                                           M2Apply M2Times [M2Apply M2Factorial [M2Ci k1], M2Apply M2Factorial [M2Apply M2Minus [m1, M2Ci k2]]]],
                         M2Apply M2Divide [M2Apply M2Factorial [M2Apply M2Minus [bigN, m2]],
                                           M2Apply M2Times [M2Apply M2Factorial [M2Apply M2Minus [m3, M2Ci k3]],
                                                            M2Apply M2Factorial [M2Apply M2Minus [M2Apply M2Minus [bigN1, m4], M2Apply M2Minus [m5, M2Ci k4]]]]]],
                      M2Apply M2Divide [M2Apply M2Factorial [bigN2],
                                        M2Apply M2Times [M2Apply M2Factorial [smalln], M2Apply M2Factorial [M2Apply M2Minus [bigN3, smalln2]]]]
                      ])
  | k == k1 && k == k2 && k == k3 && k == k4 && bigN == bigN1 && bigN == bigN2 && bigN == bigN3 &&
    m == m1 && m == m2 && m == m3 && m == m4 && m == m5 && smalln == smalln2 = 
      liftM3 HypergeometricDistribution (liftM floor $ m2TryEval m) (liftM floor $ m2TryEval bigN) (liftM floor $ m2TryEval smalln)

mmlMassFToUnAST k (M2Apply M2Times [
                      M2Apply M2Divide [M2Apply M2Factorial [M2Apply M2Minus [M2Apply M2Plus [M2Ci k1, r], M2Cn _ one1]],
                                        M2Apply M2Times [M2Apply M2Factorial [r1],
                                                         M2Apply M2Factorial [M2Apply M2Minus [M2Apply M2Minus [M2Apply M2Plus [M2Ci k2, r2], M2Cn _ one2],
                                                                                               M2Ci k3]]]],
                      M2Apply M2Power [M2Apply M2Minus [M2Cn _ one3, p], r3],
                      M2Apply M2Power [p1, M2Ci k4]
                      ])
  | one1 == 1 && one2 == 1 && one3 == 1 && k1 == k && k2 == k && k3 == k && k4 == k && r1 == r && r2 == r && p1 == p =
    liftM2 NegativeBinomialDistribution (liftM floor $ m2TryEval r) (m2TryEval p)

mmlMassFToUnAST k (M2Apply M2Times [
                      M2Apply M2Divide [
                         M2Apply M2Factorial [n],
                         M2Apply M2Times [M2Apply M2Factorial [M2Ci k1],
                                          M2Apply M2Factorial [M2Apply M2Minus [n1, M2Ci k2]]]
                         ],
                      M2Apply M2Power [p, M2Ci k3],
                      M2Apply M2Power [M2Apply M2Minus [M2Cn "dimensionless" 1, p1],
                                       M2Apply M2Minus [n2, M2Ci k4]]
                      ])
  | k1 == k && k2 == k && k3 == k && k4 == k && n == n1 && n == n2 && p == p1 =
    liftM2 BinomialDistribution (liftM floor $ m2TryEval n) (m2TryEval p)

mmlDensityToUnAST (M2Lambda x v) = mmlDensityFToUnAST x v
mmlDensityToUnAST _ = Nothing

mmlDensityFToUnAST x (M2Piecewise [(M2Cn _ zero1, M2Apply M2Lt [M2Ci x1, M2Cn _ zero2])]
                        (Just (M2Apply M2Times [exprate,
                                               M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Times [exprate1, M2Ci x2]]]]
                              )))
  | zero1 == 0 && zero2 == 0 && x1 == x && x2 == x && exprate == exprate1 =
    liftM ExponentialDistribution (m2TryEval exprate)

mmlDensityFToUnAST x (M2Apply M2Times [M2Apply M2Power [M2Ci x1, M2Apply M2Minus [M2Apply M2Minus [shape, M2Cn _ zero1]]],
                                       M2Apply M2Divide [M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Divide [M2Ci x2, scale]]],
                                                         M2Apply M2Times [M2Apply M2Power [scale1, shape1], gamma]]])
  | zero1 == 0 && x1 == x && shape == shape1 && scale == scale1 && isGammaFunc shape gamma =
    liftM2 GammaDistribution (m2TryEval shape) (m2TryEval scale)

mmlDensityFToUnAST x (M2Apply M2Times [M2Apply M2Divide [M2Apply M2Power [beta, alpha], gamma],
                                       M2Apply M2Power [M2Ci x1, M2Apply M2Minus [M2Apply M2Minus [alpha1],
                                                                                  M2Cn _ one1]],
                                       M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Divide [beta1, M2Ci x2]]]
                                      ])
  | one1 == 1 && alpha1 == alpha && beta1 == beta && x1 == x && x2 == x && isGammaFunc alpha gamma =
    liftM2 InverseGammaDistribution (m2TryEval alpha) (m2TryEval beta)

mmlDensityFToUnAST x (M2Apply M2Times [
                         M2Apply M2Divide [M2Cn _ one1,
                                           M2Apply (M2Root Nothing) [
                                               M2Apply M2Times [M2Cn _ two1, M2Pi, sigma2]
                                             ]
                                          ],
                         M2Apply M2Exp [M2Apply M2Minus [
                                           M2Apply M2Divide [
                                              M2Apply M2Power [M2Apply M2Minus [M2Ci x1, mu], M2Cn _ two2],
                                              M2Apply M2Times [M2Cn _ two3, sigma2_2]
                                                            ]
                                                        ]]
                                       ])
  | one1 == 1 && two1 == 2 && two2 == 2 && two3 == 2 && sigma2 == sigma2_2 && x == x1 =
    liftM2 NormalDistribution (m2TryEval mu) (m2TryEval sigma2)

mmlDensityFToUnAST x (M2Piecewise [(M2Cn _ zero1, M2Apply M2Leq [M2Ci x1, M2Cn _ zero2])]
                        (Just (
                            M2Apply M2Times [
                               M2Apply M2Divide [
                                  M2Cn _ one1,
                                  M2Apply M2Times [
                                    M2Ci x2, sigma, M2Apply (M2Root Nothing) [
                                       M2Cn _ two1,
                                       M2Pi
                                       ]
                                    ]
                                  ],
                               M2Apply M2Exp [
                                 M2Apply M2Minus [
                                    M2Apply M2Divide [
                                       M2Apply M2Power [ M2Apply M2Minus [M2Apply M2Ln [M2Ci x3], mu], M2Cn _ two2 ],
                                       M2Apply M2Times [
                                         M2Cn _ two3,
                                         M2Apply M2Power [ sigma1, M2Cn _ two4 ]
                                         ]
                                       ]
                                    ]
                                 ]
                               ])))
  | one1 == 1 && two1 == 2 && two2 == 2 && two3 == 2 && two4 == 2 && x1 == x && x2 == x && x3 == x &&
    sigma1 == sigma = liftM2 LogNormalDistribution (m2TryEval mu) (m2TryEval sigma)

mmlDensityFToUnAST x (M2Apply M2Divide [
                         M2Apply M2Plus [
                            (M2Apply M2Times [
                                M2Apply M2Divide [
                                   gamma1,
                                   M2Apply M2Times [
                                     M2Apply (M2Root Nothing) [
                                        M2Apply M2Times [nu, M2Pi]
                                        ],
                                     gamma2
                                     ]
                                   ],
                                M2Apply M2Power [
                                  M2Apply M2Plus [
                                     M2Cn _ one1,
                                     M2Apply M2Divide [
                                       M2Apply M2Power [x1, M2Cn _ two1],
                                       nu1
                                       ]
                                     ]
                                  ],
                                M2Apply M2Minus [
                                  M2Apply M2Divide [
                                     M2Apply M2Plus [nu2, M2Cn _ one2],
                                     M2Cn _ two2
                                     ]
                                  ]
                                ]), loc],
                         scal])
  | one1 == 1 && one2 == 1 && two1 == 2 && two2 == 2 && nu == nu1 && nu == nu2 &&
    isGammaFunc (M2Apply M2Divide [M2Apply M2Plus [nu, M2Cn "dimensionless" 1],
                                   M2Cn "dimensionless" 2]) gamma1 &&
    isGammaFunc (M2Apply M2Divide [nu, M2Cn "dimensionless" 2]) gamma2 =
      liftM3 StudentTDistribution(m2TryEval loc) (m2TryEval scal) (liftM floor $ m2TryEval nu)

mmlDensityFToUnAST x (M2Piecewise [(M2Apply M2Divide [M2Cn _ one1, M2Apply M2Minus [b, a]],
                                    M2Apply M2And [M2Apply M2Geq [M2Ci x1, a1], M2Apply M2Leq [M2Ci x2, b1]]
                                   )] (Just (M2Cn _ zero1)))
  | one1 == 1 && x1 == x && x2 == x && a1 == a && b1 == b && zero1 == 0 =
    liftM2 UniformDistribution (m2TryEval a) (m2TryEval b)

mmlDensityFToUnAST x (M2Apply M2Plus l) =
  let
    extractMixtureComponent x (M2Apply M2Times [M2Cn "dimensionless" weight, compPDF]) = do
      v <- mmlDensityFToUnAST x compPDF
      return (weight, v)
  in
   liftM MixtureModel (sequence $ map (extractMixtureComponent x) l)

mmlDensityFToUnAST x (M2Apply M2Divide [
                         M2Apply M2Times [
                            M2Apply M2Power [M2Ci x1, M2Apply M2Minus [a, M2Cn _ matchOne1]],
                            M2Apply M2Power [M2Apply M2Minus [M2Cn _ matchOne2, M2Ci x2],
                                             M2Apply M2Minus [b, M2Cn _ matchOne3]]],
                         betaAB])
  | matchOne1 == 1 && matchOne2 == 1 && matchOne3 == 1 &&
    isBetaFunc a b betaAB && x == x1 && x == x2 =
      liftM2 BetaDistribution (m2TryEval a) (m2TryEval b)

mmlDensityFToUnAST x (M2Apply M2Times [
                         M2Apply M2Divide [M2Cn _ one1,
                                           M2Apply M2Times [M2Cn _ two1, scale]],
                         M2Apply M2Exp [
                           M2Apply M2Minus [
                              M2Apply M2Divide [ M2Apply M2Abs [ M2Apply M2Minus [M2Ci x1, loc] ], scale1 ]
                              ]
                           ]
                         ])
  | one1 == 1 && two1 == 2 && x1 == x && scale1 == scale = liftM2 LaplaceDistribution (m2TryEval loc) (m2TryEval scale)

mmlDensityFToUnAST x (M2Apply M2Divide [
                         M2Cn _ one1,
                         M2Apply M2Times [
                           M2Pi, scale,
                           M2Apply M2Plus [
                             M2Cn _ one2,
                             M2Apply M2Power [ M2Apply M2Divide [ M2Apply M2Minus [M2Ci x1, loc], scale1 ], M2Cn _ two1]
                             ]
                           ]
                         ])
  | one1 == 1 && one2 == 1 && two1 == 2 && x1 == x && scale1 == scale =
    liftM2 CauchyDistribution (m2TryEval loc) (m2TryEval scale)

mmlDensityFToUnAST x (M2Apply M2Times [
                         M2Apply M2Divide [shape, scale],
                         M2Apply M2Power [M2Apply M2Divide [M2Ci x1, scale1], M2Apply M2Minus [shape1, M2Cn _ one1]],
                         M2Apply M2Exp [M2Apply M2Minus [M2Apply M2Power [M2Apply M2Divide [M2Ci x2, scale2], shape2]]]
                         ])
  | x1 == x && x2 == x && shape == shape1 && shape == shape2 && scale == scale1 && scale == scale2 && one1 == 1 =
    liftM2 WeibullDistribution (m2TryEval shape) (m2TryEval scale)

mmlDensityFToUnAST x (M2Apply M2Divide [
                         M2Apply M2Exp [M2Apply M2Divide [M2Apply M2Minus [M2Apply M2Minus [M2Ci x1, loc]], scale]],
                         M2Apply M2Times [ scale1,
                                           M2Apply M2Power [
                                             M2Apply M2Plus [
                                                M2Cn _ one1,
                                                M2Apply M2Exp [M2Apply M2Divide [M2Apply M2Minus [M2Apply M2Minus [M2Ci x2, loc1]], scale2]]
                                                ],
                                             M2Cn _ two1
                                             ]
                                         ]
                         ])
  | x1 == x && x2 == x && scale == scale1 && scale == scale2 && loc1 == loc && one1 == 1 && two1 == 2 =
    liftM2 LogisticDistribution (m2TryEval loc) (m2TryEval scale)
    
mmlDensityFToUnAST x (M2Apply M2Times [
                         M2Apply M2Divide [
                            M2Cn _ one1,
                            M2Apply M2Times [
                              M2Apply M2Power [ M2Cn _ two1, M2Apply M2Divide [degf, M2Cn _ two2] ],
                              gamma
                              ]
                            ],
                         M2Apply M2Power [
                           M2Ci x1, M2Apply M2Minus [M2Apply M2Divide [degf1, M2Cn _ two3], M2Cn _ one2]
                                         ],
                         M2Apply M2Exp [
                           M2Apply M2Divide [M2Apply M2Minus [M2Ci x2], M2Cn _ two4]
                                       ]
                         ])
  | one1 == 1 && one2 == 1 && two1 == 2 && two2 == 2 && two3 == 2 && two4 == 2 && x1 == x && x2 == x &&
    isGammaFunc (M2Apply M2Divide [degf, M2Cn "dimensionless" 2]) gamma = liftM (ChiSquareDistribution . floor) (m2TryEval degf)

mmlDensityFToUnAST x (M2Apply M2Divide [
                         M2Apply (M2Root Nothing) [
                            M2Apply M2Divide [
                               M2Apply M2Times [ M2Apply M2Power [M2Apply M2Times [num, M2Ci x1], num1], M2Apply M2Power [denom, denom1] ],
                               M2Apply M2Power [M2Apply M2Plus [M2Apply M2Times [num2, M2Ci x2], denom2], M2Apply M2Plus [num3, denom3]]
                               ]
                            ],
                         M2Apply M2Times [
                           M2Ci x3, beta
                           ]
                         ])
  | x1 == x && x2 == x && x3 == x && num == num1 && num == num2 && num == num3 &&
    denom == denom1 && denom == denom2 && denom == denom3 &&
    isBetaFunc (M2Apply M2Divide [num, M2Cn "dimensionless" 2]) (M2Apply M2Divide [denom, M2Cn "dimensionless" 2]) beta =
      liftM2 FDistribution (m2TryEval denom) (m2TryEval num)

mmlDensityFToUnAST x (M2Piecewise [(M2Apply M2Divide
                                    [ M2Apply M2Times [ slope, M2Apply M2Power [scale, slope1] ],
                                      M2Apply M2Power [ M2Ci x1, M2Apply M2Plus [ slope2, M2Cn _ one1 ]]
                                    ],
                                    M2Apply M2Geq [M2Ci x2, scale1])] (Just (M2Cn _ zero1)))
  | zero1 == 0 && one1 == 1 && scale1 == scale && slope1 == slope && slope2 == slope && x1 == x && x2 == x =
    liftM2 ParetoDistribution (m2TryEval scale) (m2TryEval slope)


mmlDensityFToUnAST _ _ = Nothing

isGammaFunc alpha (M2Apply (M2Int { m2intDegree = Nothing, m2intLowlimit = Just (M2Cn _ zero1), m2intUplimit = Just M2Infinity,
                                    m2intBvar = bvarName }) [M2Apply M2Times [
                                                                M2Apply M2Power [M2Ci bvarName1, M2Apply M2Minus [alpha1, M2Cn _ minusOne1]],
                                                                M2Apply M2Exp [M2Apply M2Minus [M2Ci bvarName2]]]])
  | alpha == alpha1 && minusOne1 == -1 && bvarName1 == bvarName && bvarName2 == bvarName = True

isGammaFunc _ _ = False

isBetaFunc alpha beta (M2Apply (M2Int { m2intDegree = Nothing, m2intLowlimit = Just (M2Cn _ zero1), m2intUplimit = Just (M2Cn _ one1),
                                        m2intBvar = bvName }) [
                            M2Apply M2Times [
                               M2Apply M2Power [M2Ci bvName1, M2Apply M2Minus [alpha1, M2Cn _ one2]],
                               M2Apply M2Power [M2Apply M2Minus [M2Cn _ one3, M2Ci bvName2], M2Apply M2Minus [beta1, M2Cn _ one4]]
                              ]
                          ])
  | zero1 == 0 && one1 == 1 && one2 == 1 && one3 == 1 && one4 == 1 &&
    bvName == bvName1 && bvName == bvName2 && alpha1 == alpha && beta1 == beta = True
isBetaFunc _ _ _ = False

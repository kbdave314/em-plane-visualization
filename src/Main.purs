module Main (main) where

import Prelude (map, negate, ($), (>>=), (+), (*), (-), (<>), bind, pure)
import Control.Monad.Eff (Eff)
import Data.Foreign (toForeign)
import Data.Function.Uncurried (mkFn5, runFn3)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Math (cos, sin, pi)

import Mathbox.Classes (mkCamera, mkCartesian, mkAxis, mkInterval, mkLine,
                       mkVector)
import Mathbox.Field  (Field(..))
import Mathbox.Mathbox (MATHBOX, Mathbox, MathboxPrimitive(..), addAll,
  mkMathbox, toJs , orbitControls)
  
import Mathbox.Types (mkVec2, mkVec3, unsafeMkColor, axis1, axis2, axis3,
                     In5, Ch3, It2, N, Color)

data Polarization = Polarization {
  lag   :: N, --phase lag
  plane :: N  --fraction of a turn about the propogation axis
  }

data EMFieldType = EField | BField

type FieldEmitter = In5 Ch3 It2

emitEMField :: EMFieldType -> Polarization -> FieldEmitter
emitEMField ft (Polarization {lag, plane}) = mkFn5 f
  where
    f emit x (_) t (_) =
      let phi l      = sin(2.0*pi * (0.5*x - 0.5* t + l))
          c          = cos (2.0*pi*plane)
          s          = sin (2.0*pi*plane)
          val1       = phi 0.0
          val2       = phi lag
          y          = case ft of
                         EField -> c * val1
                         BField -> (-1.0)* s * val2
          z          = case ft of
                         EField -> s * val2
                         BField -> c * val1
          vectorTail = runFn3 emit x 0.0 0.0
          vectorHead = runFn3 emit x y z in
      vectorTail /\ vectorHead

axesSpec :: List MathboxPrimitive
axesSpec = do
  whichAxis <- axis1 : axis2 : axis3 : Nil
  pure (Axis $ mkAxis {
    width = Val 12.0,
    axis  = Val whichAxis,
    start = Val true
  })

emFieldSpec :: Field Color -> FieldEmitter -> List MathboxPrimitive
emFieldSpec colour emitter = (Interval $ mkInterval {
        range    = Just $ Val [(-1.0), 3.0],
        width    = Just $ Val 40,
        expr     = Just $ Val $ toForeign emitter,
        channels = Val 3,
        items    = Val 2
      }) :
      (Vector $ mkVector {
        color = colour,
        size  = Val 40.0,
        end   = Val true
      }) :
      (Line $ mkLine {
        color = colour,
        size  = Val 20.0
      }) : 
      Nil 

withViewboxSpec :: List MathboxPrimitive -> List MathboxPrimitive
withViewboxSpec graphicsPrims = (Camera $ mkCamera {
    proxy    = Val true,
    position = Just $ Val $ mkVec3 4 1 1,
    lookAt   = Just $ Val $ mkVec3 0 0 0
  }) :
  (Cartesian $ mkCartesian { 
    range = Val [
      mkVec2 (-1) 3,
      mkVec2 (-1) 1,
      mkVec2 (-1) 1
      ], 
    scale = Val (mkVec3 2 1 1)
    }) graphicsPrims : Nil

mathboxSpec :: Polarization -> List MathboxPrimitive
mathboxSpec polarization = withViewboxSpec (
      axesSpec <>
      electricFieldSpec <>
      magneticFieldSpec ) where
        electricFieldColor = Val $ unsafeMkColor "#FFFF00" --yellow
        emitE              = emitEMField EField polarization
        electricFieldSpec  = emFieldSpec electricFieldColor emitE
        magneticFieldColor = Val $ unsafeMkColor "#0000FF" --blue
        emitB              = emitEMField BField polarization
        magneticFieldSpec  = emFieldSpec magneticFieldColor emitB
  
initMathbox :: forall eff. Eff ( mathbox :: MATHBOX | eff ) Mathbox
initMathbox = mkMathbox { 
             plugins: ["core"] <> ["controls"]
           , controls: {
               klass: orbitControls
             }
           , fullscreen: true
           }

populateWith :: 
  List MathboxPrimitive ->
  Mathbox ->
  (forall eff.
    Eff ( mathbox :: MATHBOX | eff ) Mathbox
  )
populateWith graphicsPrimitives = addAll (map toJs graphicsPrimitives)

main :: forall eff. Eff ( mathbox :: MATHBOX | eff ) Mathbox
main = initMathbox >>= (populateWith (mathboxSpec thePolarization)) where
  thePolarization = Polarization { lag: 0.0  , plane: 0.0   } --linear
--thePolarization = Polarization { lag: 0.125, plane: 0.125 } --elliptic
--thePolarization = Polarization { lag: 0.25, plane: 0.125   } --circular

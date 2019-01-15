module Wasm.Util.NFData where

import           Control.DeepSeq

rnfLift :: (NFData a, NFData1 f1) => f1 a -> ()
rnfLift = liftRnf rnf

rnfLiftLift :: (NFData a, NFData1 f1, NFData1 f2) => f1 (f2 a) -> ()
rnfLiftLift = liftRnf rnfLift

rnfLiftLiftLift
  :: (NFData a, NFData1 f1, NFData1 f2, NFData1 f3) => f1 (f2 (f3 a)) -> ()
rnfLiftLiftLift = liftRnf rnfLiftLift

rnfLiftLiftLiftLift
  :: (NFData a, NFData1 f1, NFData1 f2, NFData1 f3, NFData1 f4)
  => f1 (f2 (f3 (f4 a)))
  -> ()
rnfLiftLiftLiftLift = liftRnf rnfLiftLiftLift

rnfLiftLiftLiftLiftLift
  :: (NFData a, NFData1 f1, NFData1 f2, NFData1 f3, NFData1 f4, NFData1 f5)
  => f1 (f2 (f3 (f4 (f5 a))))
  -> ()
rnfLiftLiftLiftLiftLift = liftRnf rnfLiftLiftLiftLift

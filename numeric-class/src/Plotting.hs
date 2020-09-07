module Plotting
  ( plotX
  , plotX'
  )
where

import           Graphics.EasyPlot

-- | small wrapper around plot to use X11, ignored the results too, I take that no display is failure
plotX :: Plot a => a -> IO ()
plotX p = plot X11 p >> pure ()

-- | small wrapper around plot' to use X11, ignored the results too, I take that no display is failure
-- Interactive mode is on in this case, useful for 3D plots
plotX' :: Plot a => a -> IO ()
plotX' p = plot' [Interactive] X11 p >> pure ()

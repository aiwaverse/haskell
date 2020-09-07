module Plotting
  ( plotX
  , plotX'
  )
where

import           Graphics.EasyPlot

-- | small wrapper around plot to use X11
plotX :: Plot a => a -> IO Bool
plotX = plot X11

-- | small wrapper around plot' to use X11
-- Interactive mode is on in this case, useful for 3D plots
plotX' :: Plot a => a -> IO Bool
plotX' = plot' [Interactive] X11

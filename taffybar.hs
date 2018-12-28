-- -*- mode:haskell -*-
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Control.Monad                  ( void )
import           System.Taffybar
import           System.Taffybar.Util
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph
import           Control.Monad.IO.Class         ( liftIO )


transparent = (0.0, 0.0, 0.0, 0.0)

yellow1, yellow2 :: (Double, Double, Double, Double)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig = defaultGraphConfig { graphPadding         = 0
                                   , graphBorderWidth     = 0
                                   , graphWidth           = 75
                                   , graphBackgroundColor = transparent
                                   }

memCfg = myGraphConfig { graphDataColors = [taffyBlue], graphLabel = Nothing }

cpuCfg =
  myGraphConfig { graphDataColors = [green1, green2], graphLabel = Nothing }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  let
    myWorkspacesConfig = defaultWorkspacesConfig { minIcons         = 1
                                                 , widgetGap        = 20
                                                 , showWorkspaceFn  = hideEmpty
                                                 , underlinePadding = 0
                                                 }
    workspaces    = workspacesNew myWorkspacesConfig
    cpu           = pollingGraphNew cpuCfg 0.5 cpuCallback
    tcpu          = textCpuMonitorNew "$total$%" 0.5
    mem           = pollingGraphNew memCfg 1 memCallback
    tmem          = textMemoryMonitorNew "$used$ $free$" 1.0
    netm          = networkMonitorNew "▼ $inAuto$ ▲ $outAuto$" Nothing
    clock         = textClockNew Nothing "%Y-%_m-%_d %X" 1
    layout        = layoutNew defaultLayoutConfig
    myWindows     = windowsNew defaultWindowsConfig
        -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
        -- for a better way to set up the sni tray
    tray          = sniTrayNew
    bat           = textBatteryNew "$percentage$% $time$ $status$"
    notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]
    myConfig      = defaultSimpleTaffyConfig
      { startWidgets  = workspaces
                          : map (>>= buildContentsBox) [layout, myWindows]
      , endWidgets    = map
                          (>>= buildContentsBox)
                          [ tray
                          , clock
                          , batteryIconNew
                          , bat
                          , cpu
                          , liftIO tcpu
                          , mem
                          , liftIO tmem
                          , netm
                          , mpris2New
                          ]
      , barPosition   = Bottom
      , barPadding    = 0
      , barHeight     = 40
      , widgetSpacing = 20
      }
  dyreTaffybar
    $ appendHook notifySystemD
    $ withBatteryRefresh
    $ withLogServer
    $ withToggleServer
    $ toTaffyConfig myConfig

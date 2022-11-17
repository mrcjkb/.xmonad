module Config (myConfig) where

import Xmobar

myConfig :: Config
myConfig = defaultConfig 
   { 
   -- appearance
     font =         "xft:JetBrains Mono:size=9:bold:antialias=true"
   , bgColor =      "#202020"
   , fgColor =      "white"
   , position =     Top
   , border =       BottomB
   , borderColor =  "#BC96DA"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "%XMonadLog% }{ %battery% | %memory% | %dynnetwork% || %LSZH% | %date% || %kbd% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands = 

        -- weather monitor
        [ Run $ Weather "LSZH" [ "--template"
                               , "<skyCondition> | <station>: <fc=#FAC536><tempC></fc>°C | <fc=#FAC536><rh></fc>% | <fc=#FAC536><pressure></fc>hPa"
                               ] 36000

        -- XMonad logs
        , Run $ XMonadLog

        -- , Run $ Volume "default" "Master" [] 10

        -- network activity monitor (dynamic interface resolution)
        , Run $ DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                               , "--Low"      , "1000"       -- units: B/s
                               , "--High"     , "5000"       -- units: B/s
                               , "--low"      , okColour
                               , "--normal"   , warnColour
                               , "--high"     , criticalColour
                               ] 10

        -- cpu activity monitor
        , Run $ MultiCpu       [ "--template" , "Cpu: <total0>%|<total1>%"
                               , "--Low"      , "50"         -- units: %
                               , "--High"     , "85"         -- units: %
                               , "--low"      , okColour
                               , "--normal"   , warnColour
                               , "--high"     , criticalColour
                               ] 10

        -- cpu core temperature monitor
        , Run $ CoreTemp       [ "--template" , "Temp: <core0>°C|<core1>°C"
                               , "--Low"      , "70"        -- units: °C
                               , "--High"     , "80"        -- units: °C
                               , "--low"      , okColour
                               , "--normal"   , warnColour
                               , "--high"     , criticalColour
                               ] 50
                          
        -- memory usage monitor
        , Run $ Memory         [ "--template" ,"Mem: <usedratio>%"
                               , "--Low"      , "20"        -- units: %
                               , "--High"     , "90"        -- units: %
                               , "--low"      , okColour
                               , "--normal"   , warnColour
                               , "--high"     , criticalColour
                               ] 10

        -- battery monitor
        , Run $ Battery        [ "--template" , "Batt: <acstatus>"
                               , "--Low"      , "10"        -- units: %
                               , "--High"     , "80"        -- units: %
                               , "--low"      , criticalColour
                               , "--normal"   , warnColour
                               , "--high"     , okColour

                               , "--" -- battery specific options
                                      -- discharging status
                               , "-o" , "<left>% (<timeleft>)"
                               -- AC "on" status
                               , "-O" , "<fc=" <> warnColour <> ">Charging</fc>"
                               -- charged status
                               , "-i" , "<fc=" <> okColour <> ">Charged</fc>"
                               ] 50

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10

        -- keyboard layout indicator
        , Run $ Kbd            [ ("us(altgr-intl)" , "<fc=" <> okColour <> ">US(altgr-intl)</fc>")
                               , ("de(ch)"         , "<fc=" <> warnColour <> ">DE_CH</fc>")
                               ]
        ]
  }
  where
    criticalColour = "#F25056"
    warnColour = "#FAC536"
    okColour = "#39EA49"


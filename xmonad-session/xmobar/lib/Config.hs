module Config (myConfig) where

import Xmobar

myConfig :: Config
myConfig = defaultConfig 
   { 
   -- appearance
     font =         "xft:JetBrains Mono Nerd Font Mono:size=14:bold:antialias=true"
   , bgColor =      "#212121"
   , fgColor =      "white"
   , position =     TopH 30
   -- , border =       BottomB
   -- , borderWidth = 0
   -- , borderColor =  "#B480D6"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "\62227 \57533 \59255 \57533 %XMonadLog% }{ %battery% \57533 %memory% \57533 %dynnetwork% \57533 %LSZH% \57533 %date% \57533 %kbd% "

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
                               , "<station>: <fc=#E6B455><tempC> °C</fc> <skyCondition> <fc=#6E98EB><rh> \58227</fc>"
                               ] 36000

        -- XMonad logs
        , Run XMonadLog

        -- , Run $ Volume "default" "Master" [] 10

        -- network activity monitor (dynamic interface resolution)
        , Run $ DynNetwork     [ "--template" , "<dev>: \60064<tx> kB/s \60061<rx> kB/s"
                               , "--Low"      , "1000"       -- units: B/s
                               , "--High"     , "5000"       -- units: B/s
                               , "--low"      , okColour
                               , "--normal"   , warnColour
                               , "--high"     , criticalColour
                               ] 10

        -- cpu activity monitor
        -- , Run $ MultiCpu       [ "--template" , "Cpu: <total> %"
        --                        , "--Low"      , "50"         -- units: %
        --                        , "--High"     , "85"         -- units: %
        --                        , "--low"      , okColour
        --                        , "--normal"   , warnColour
        --                        , "--high"     , criticalColour
        --                        ] 10

        -- cpu core temperature monitor
        -- , Run $ CoreTemp       [ "--template" , "Temp: <core>°C"
        --                        , "--Low"      , "70"        -- units: °C
        --                        , "--High"     , "80"        -- units: °C
        --                        , "--low"      , okColour
        --                        , "--normal"   , warnColour
        --                        , "--high"     , criticalColour
        --                        ] 50
                          
        -- memory usage monitor
        , Run $ Memory         [ "--template" ,"\63578 <usedratio> %"
                               , "--Low"      , "20"        -- units: %
                               , "--High"     , "90"        -- units: %
                               , "--low"      , okColour
                               , "--normal"   , warnColour
                               , "--high"     , criticalColour
                               ] 10

        -- battery monitor
        , Run $ Battery        [ "--template" , "<acstatus>"
                               , "--Low"      , "10"        -- units: %
                               , "--High"     , "80"        -- units: %
                               , "--low"      , criticalColour
                               , "--normal"   , warnColour
                               , "--high"     , okColour

                               , "--" -- battery specific options
                                      -- discharging status
                               , "-o" , "\62845 <left>% (<timeleft>)"
                               -- AC "on" status, charging 
                               , "-O" , "<fc=" <> warnColour <> ">\62851</fc>"
                               -- charged status 
                               , "-i" , "<fc=" <> okColour <> ">\62840</fc>"
                               ] 50

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date           "<fc=#FFFFFF>%F (%a) %T</fc>" "date" 10

        -- keyboard layout indicator
        , Run $ Kbd            [ ("us(altgr-intl)" , "<fc=" <> okColour <> ">US(altgr-intl)</fc>")
                               , ("de(ch)"         , "<fc=" <> warnColour <> ">DE_CH</fc>")
                               ]
        ]
  }
  where
    criticalColour = "#FF5370"
    warnColour = "#E6B455"
    okColour = "#B480D6"



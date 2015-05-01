import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo (raiseMaybe, runOrRaise)
import XMonad.Hooks.DynamicLog (dzenPP, statusBar, dynamicLog, dynamicLogWithPP, ppCurrent, PP(..), wrap, pad, trim, dzenColor, dzenEscape, dzenStrip, shorten)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.TabBarDecoration
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell (shellPrompt, safePrompt)
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.Run (safeSpawn, runInTerm, spawnPipe, hPutStrLn)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    simpleMenu <- spawnPipe dzenMenu
    workspaceNTitle <- spawnPipe dzenTitleBar
    informasiConky  <- spawnPipe dzenConky
    mulai  <- spawnPipe "~/.xmonad/autostart.sh"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { modMask           = mod4Mask
        , terminal          = "urxvtc"
        , borderWidth       = 0
        , normalBorderColor = "#AAAAAA"
        , focusedBorderColor= "#333333"
        , workspaces        = myWorkspace
        , keys              = myKeys
        , handleEventHook   = fullscreenEventHook
        , layoutHook        = myLayout
        , manageHook        = manageDocks <+> myManageHook <+> manageHook  defaultConfig
        , logHook           = myLogHook workspaceNTitle }

myWorkspace :: [String]
myWorkspace  = clickable . (map dzenEscape) $ ["1","2","3","4","5","6","7"]
  where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                      (i,ws) <- zip [1..] l,
                      let n = i ]

myLayout = avoidStruts $ smartBorders ( layarPenuh ||| kiriKanan ||| atasBawah ||| ikan ||| siklik )
  where
    ikan       = named " <>< "  $ simpleFloat
    kiriKanan  = named " ||| "  $ Tall 1 (3/100) (5/9)
    atasBawah  = named " === "  $ Mirror kiriKanan 
    layarPenuh = named " [ ] "  $ Full
    siklik     = named " +++ "  $ Circle

myXPConfig :: XPConfig
myXPConfig =
    defaultXPConfig { font      = fontDzen
                    , bgColor   = "#000000"
                    , fgColor   = "#FFFFFF"
                    , bgHLight  = "#FFFFFF"
                    , fgHLight  = "#000000"
                    , promptBorderWidth = 0
                    , height    = 16
                    }

myManageHook = composeAll
    [ className =? "mpv" --> doFloat              
    , className =? "MPV" --> doFloat              
    , className =? "feh" --> doFloat
    ]

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenPP = dzenPP
    { ppCurrent         = dzenColor "#00FFFF" ""
    , ppHidden          = dzenColor "#0000FF" ""
    , ppHiddenNoWindows = dzenColor "#FFFFFF" ""
    , ppUrgent          = dzenColor "#FF0000" ""
    , ppSep             = ""
    , ppLayout          = dzenColor "#FFFFFF" "" . wrap "^ca(1,xdotool key super+space)" "^ca()"
    , ppTitle           = dzenColor "#FFFFFF" ""
                            . wrap "^ca(1,xdotool key super+j)^ca(3,xdotool key super+k)^ca(5, xdotool key super+shift+j)^ca(4, xdotool key super+shift+k)" "^ca()^ca()^ca()^ca()" . shorten 100 . dzenEscape
    }

dzenMenu        = "(zsh ~/.xmonad/menu.sh) | (dzen2 -w '80' -y '740' " ++ konfigurasiDzen ++ eksekusiMenu
eksekusiMenu    = " -m -p -l 7  -e 'button3=togglecollapse;leaveslave=collapse;button1=menuexec')"
dzenTitleBar    = "dzen2 -x '80' -y '740' -w '700' "++ konfigurasiDzen
dzenConky       = "conky -c ~/.xmonad/conkyrc | dzen2 -x '780' -y '740' -w '586' " ++ konfigurasiDzen
konfigurasiDzen = "-ta 'l' -h '28' -fg '#9f9fa2' -bg '#2b2b28' -fn '" ++ fontDzen ++ "'"
fontDzen        = "-*-source code pro-medium-r-normal-*-18-*-*-*-*-*-*-*"
myKeys conf@(XConfig {XMonad.modMask = mod}) = M.fromList $
    [ ((mod .|. shiftMask       , xK_Return     ), spawn $ XMonad.terminal conf)
    , ((mod .|. shiftMask       , xK_q          ), spawn "killall udiskie ; killall dzen2; killall compton; killall urxvtd" >> io (exitWith ExitSuccess))
    , ((mod .|. shiftMask       , xK_o          ), safePrompt "google-chrome-stable" myXPConfig)
    , ((mod .|. shiftMask       , xK_p          ), shellPrompt myXPConfig)
    , ((mod .|. shiftMask       , xK_n          ), raiseMaybe (runInTerm "-title lagu" "ncmpcpp") (title =? "ncmpcpp"))
    , ((mod .|. shiftMask       , xK_x          ), kill)
    , ((mod .|. shiftMask       , xK_space      ), setLayout $ XMonad.layoutHook conf)
    , ((mod .|. shiftMask       , xK_s          ), spawn "xfce4-screenshooter")
    , ((mod .|. shiftMask       , xK_Right      ), shiftToNext)
    , ((mod .|. shiftMask       , xK_j          ), nextWS)
    , ((mod .|. shiftMask       , xK_Left       ), shiftToPrev)
    , ((mod .|. shiftMask       , xK_k          ), prevWS)
    , ((mod .|. shiftMask       , xK_a          ), spawn $ XMonad.terminal conf)
    , ((mod .|. shiftMask       , xK_Tab        ), windows W.focusUp)
    , ((mod .|. controlMask     , xK_Right      ), shiftToNext >> nextWS)
    , ((mod .|. controlMask     , xK_Left       ), shiftToPrev >> prevWS)
    , ((mod                     , xK_q          ), spawn "if type xmonad; then killall dzen2 && xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\";fi")
    , ((mod                     , xK_Tab        ), windows W.focusDown)
    , ((mod                     , xK_comma      ), sendMessage (IncMasterN 1))
    , ((mod                     , xK_period     ), sendMessage (IncMasterN (-1)))
    , ((mod                     , xK_t          ), withFocused $ windows . W.sink)
    , ((mod                     , xK_l          ), sendMessage Expand)
    , ((mod                     , xK_j          ), windows W.focusDown)
    , ((mod                     , xK_k          ), windows W.focusUp)
    , ((mod                     , xK_h          ), sendMessage Shrink)
    , ((mod                     , xK_Return     ), windows W.swapMaster)
    , ((mod                     , xK_m          ), windows W.swapDown)
    , ((mod                     , xK_F1         ), manPrompt            myXPConfig)
    , ((mod                     , xK_F2         ), xmonadPrompt         myXPConfig)
    , ((mod                     , xK_F6         ), spawn "/usr/bin/pulseaudio-ctl up")
    , ((mod                     , xK_F5         ), spawn "/usr/bin/pulseaudio-ctl down")
    , ((mod                     , xK_F7         ), spawn "/usr/bin/pulseaudio-ctl mute")
    , ((mod                     , xK_F8         ), windowPromptBring    myXPConfig)
    , ((mod                     , xK_F10        ), windowPromptGoto     myXPConfig)
    , ((mod                     , xK_Right      ), nextWS)
    , ((mod                     , xK_Left       ), prevWS)
    , ((mod                     , xK_Next       ), nextWS)
    , ((mod                     , xK_Prior      ), prevWS)
    , ((mod                     , xK_z          ), spawn "thunar")
    , ((mod                     , xK_o          ), spawn "google-chrome-stable")
    , ((mod                     , xK_space      ), sendMessage NextLayout)
    ]
    ++
    [ ((m .|. mod               , k             ), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf ) [xK_1 .. xK_7]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

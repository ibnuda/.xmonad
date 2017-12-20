import qualified Data.Map                       as M
import           Data.Monoid
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWindows
import           XMonad.Actions.CycleWS
import           XMonad.Actions.WindowGo        (raiseMaybe, runOrRaise)
import           XMonad.Hooks.DynamicLog        (PP (..), dynamicLog,
                                                 dynamicLogWithPP, dzenColor,
                                                 dzenEscape, dzenPP, dzenStrip,
                                                 pad, ppCurrent, shorten,
                                                 statusBar, trim, wrap)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Circle
import           XMonad.Layout.Grid
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.TabBarDecoration
import           XMonad.Prompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.Shell            (safePrompt, shellPrompt)
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                as W
import           XMonad.Util.Run                (hPutStrLn, runInTerm,
                                                 safeSpawn, spawnPipe)

main :: IO ()
main = do
  simpleMenu <- spawnPipe dzenMenu
  workspaceNTitle <- spawnPipe dzenTitleBar
  informasiConky <- spawnPipe dzenConky
  mulai <- spawnPipe "~/.xmonad/autostart.sh"
  xmonad $
    withUrgencyHook NoUrgencyHook $
    def
    { modMask = mod4Mask
    , terminal = "urxvtc"
    , borderWidth = 0
    , normalBorderColor = "#AAAAAA"
    , focusedBorderColor = "#333333"
    , workspaces = myWorkspace
    , keys = myKeys
    , handleEventHook = fullscreenEventHook
    , layoutHook = myLayout
    , manageHook = manageDocks <+> myManageHook <+> manageHook def
    , logHook = myLogHook workspaceNTitle
    }

myWorkspace :: [String]
myWorkspace = clickable . (map dzenEscape) $ map show [1 .. 10]
  where
    clickable :: [[Char]] -> [[Char]]
    clickable l =
      [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()"
      | (i, ws) <- zip [1 ..] l
      , let n = i
      ]

myLayout = avoidStruts $ smartBorders (layarPenuh ||| kiriKanan ||| atasBawah ||| ikan ||| siklik)
  where
    ikan = named " <>< " $ simpleFloat
    kiriKanan = named " ||| " $ Tall 1 (3 / 100) (5 / 9)
    atasBawah = named " === " $ Mirror kiriKanan
    layarPenuh = named " [ ] " $ Full
    siklik = named " +++ " $ Circle

myXPConfig :: XPConfig
myXPConfig =
  def
  { font = fontDzen
  , bgColor = "#000000"
  , fgColor = "#FFFFFF"
  , bgHLight = "#FFFFFF"
  , fgHLight = "#000000"
  , promptBorderWidth = 0
  , height = 20
  }

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [className =? "mpv" --> doFloat, className =? "MPV" --> doFloat, className =? "feh" --> doFloat]

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenPP :: PP
myDzenPP =
  dzenPP
  { ppCurrent = dzenColor "#00FFFF" ""
  , ppHidden = dzenColor "#0000FF" ""
  , ppHiddenNoWindows = dzenColor "#FFFFFF" ""
  , ppUrgent = dzenColor "#FF0000" ""
  , ppSep = ""
  , ppLayout = dzenColor "#FFFFFF" "" . wrap "^ca(1,xdotool key super+space)" "^ca()"
  , ppTitle =
      dzenColor "#FFFFFF" "" .
      wrap
        "^ca(1,xdotool key super+j)^ca(3,xdotool key super+k)^ca(5, xdotool key super+shift+j)^ca(4, xdotool key super+shift+k)"
        "^ca()^ca()^ca()^ca()" .
      shorten 100 . dzenEscape
  }

dzenMenu :: [Char]
dzenMenu = "(zsh ~/.xmonad/menu.sh) | (dzen2 -w '30' -y '748' " ++ konfigurasiDzen ++ eksekusiMenu

eksekusiMenu :: [Char]
eksekusiMenu = " -m -p -l 7  -e 'button3=togglecollapse;leaveslave=collapse;button1=menuexec')"

dzenTitleBar :: [Char]
dzenTitleBar = "dzen2 -x '30' -y '748' -w '1068' " ++ konfigurasiDzen

dzenConky :: [Char]
dzenConky = "conky -c ~/.xmonad/conkyrc | dzen2 -x '1068' -y '748' -w '300' " ++ konfigurasiDzen

konfigurasiDzen :: [Char]
konfigurasiDzen = "-ta 'l' -h '20' -fg '#9f9fa2' -bg '#2b2b28' -fn '" ++ fontDzen ++ "'"

fontDzen :: [Char]
fontDzen = "-*-fira code-medium-r-normal-*-12-*-*-*-*-*-*-*"

restartWM :: [Char]
restartWM ="killall udiskie ; killall dzen2; killall compton; killall urxvtd"

googleChrome :: [Char]
googleChrome = "google-chrome-stable"

xfceScreenshooter :: [Char]
xfceScreenshooter = "xfce4-screenshooter"

recompileThisWM :: [Char]
recompileThisWM =
  "if type xmonad;" ++
  "then killall dzen2 && xmonad --recompile && xmonad --restart;" ++
  "else xmessage xmonad not in \\$PATH: \"$PATH\";" ++
  "fi"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = mod}) =
  M.fromList $
  [ ((mod .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((mod .|. shiftMask, xK_q), spawn restartWM >> io (exitWith ExitSuccess))
  , ((mod .|. shiftMask, xK_r), safePrompt googleChrome myXPConfig)
  , ((mod .|. shiftMask, xK_l), shellPrompt myXPConfig)
  , ((mod .|. shiftMask, xK_b), raiseMaybe (runInTerm "-title lagu" "ncmpcpp") (title =? "ncmpcpp"))
  , ((mod .|. shiftMask, xK_k), kill)
  , ((mod .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  , ((mod .|. shiftMask, xK_o), spawn xfceScreenshooter)
  , ((mod .|. shiftMask, xK_Right), shiftToNext)
  , ((mod .|. shiftMask, xK_h), nextWS)
  , ((mod .|. shiftMask, xK_Left), shiftToPrev)
  , ((mod .|. shiftMask, xK_t), prevWS)
  , ((mod .|. shiftMask, xK_a), spawn $ XMonad.terminal conf)
  , ((mod .|. shiftMask, xK_Tab), windows W.focusUp)
  , ((mod .|. controlMask, xK_Right), shiftToNext >> nextWS)
  , ((mod .|. controlMask, xK_Left), shiftToPrev >> prevWS)
  , ((mod, xK_p), spawn recompileThisWM)
  , ((mod, xK_Tab), windows W.focusDown)
  , ((mod, xK_comma), sendMessage (IncMasterN 1))
  , ((mod, xK_period), sendMessage (IncMasterN (-1)))
  , ((mod, xK_y), withFocused $ windows . W.sink)
  , ((mod, xK_n), sendMessage Expand)
  , ((mod, xK_h), windows W.focusDown)
  , ((mod, xK_t), windows W.focusUp)
  , ((mod, xK_d), sendMessage Shrink)
  , ((mod, xK_Return), windows W.swapMaster)
  , ((mod, xK_m), windows W.swapDown)
  , ((mod, xK_F1), manPrompt myXPConfig)
  , ((mod, xK_F2), xmonadPrompt myXPConfig)
  , ((mod, xK_F6), spawn "/usr/bin/pulseaudio-ctl up")
  , ((mod, xK_F5), spawn "/usr/bin/pulseaudio-ctl down")
  , ((mod, xK_F7), spawn "/usr/bin/pulseaudio-ctl mute")
  , ((mod, xK_F8), windowPromptBring myXPConfig)
  , ((mod, xK_F10), windowPromptGoto myXPConfig)
  , ((mod, xK_Right), nextWS)
  , ((mod, xK_Left), prevWS)
  , ((mod, xK_Next), nextWS)
  , ((mod, xK_Prior), prevWS)
  , ((mod, xK_z), spawn "thunar")
  , ((mod, xK_o), spawn "firefox")
  , ((mod, xK_space), sendMessage NextLayout)
  ] ++
  [ ((m .|. mod, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_7]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

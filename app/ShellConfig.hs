module ShellConfig
  ( defShellCfg,
    shCfg,
    pythonCfg,
    shhCfg,
    zshCfg,
    rangerCfg,
    errorCfg,
    getPuppetCfg,
  )
where

import qualified Data.ByteString as BS
import Data.Map as Map
import Tshsh.Lang.Coroutine.CPS
import Protolude
import Tshsh.Puppet
import Tshsh.Matcher

defShellCfg :: PuppetCfg
defShellCfg =
  PuppetCfg
    { _pc_cmd = "",
      _pc_cmdArgs = [],
      _pc_promptMatcher = mkBracketMatcher "sh-" "$ ",
      _pc_getCwdCmd = GetCwdFromProcess,
      _pc_mkCdCmd = (\dir -> "cd '" <> dir <> "'"),
      _pc_switchEnterHook = pure (),
      _pc_switchExitHook = pure (),
      _pc_cleanPromptP =
        ( \pp ->
            liftP_ (BS.hPut (_pp_inputH pp) "\ETX") $ -- Ctrl-C
              waitInputC_
                finishP
        )
    }

shCfg :: PuppetCfg
shCfg =
  defShellCfg
    { _pc_cmd = "sh",
      _pc_cleanPromptP =
        ( \pp ->
            liftP_
              ( do
                  BS.hPut (_pp_inputH pp) "\NAK" -- Ctrl-U
                  BS.hPut (_pp_inputH pp) "\n"
              )
              $ waitInputC_
                finishP
        )
    }

pythonCfg :: PuppetCfg
pythonCfg =
  defShellCfg
    { _pc_cmd = "python3",
      _pc_promptMatcher = mkSeqMatcher ">>> ",
      _pc_mkCdCmd = (\dir -> "import os; os.chdir('" <> dir <> "')"),
      _pc_cleanPromptP =
        ( \pp ->
            liftP_
              ( do
                  BS.hPut (_pp_inputH pp) "\NAK" -- Ctrl-U
                  BS.hPut (_pp_inputH pp) "\n"
              )
              $ waitInputC_
                finishP
        )
    }

shhCfg :: PuppetCfg
shhCfg =
  defShellCfg
    { _pc_cmd = "shh",
      _pc_promptMatcher = mkBracketMatcher "\ESC[1;36m\206\187\ESC[m  \ESC[1;32m" "\ESC[m  ",
      _pc_getCwdCmd = GetCwdCommand "pwd",
      _pc_mkCdCmd = (\dir -> "cd \"" <> dir <> "\""),
      _pc_switchEnterHook = BS.hPut stdout "\x1b[?2004l" -- disable bracket paste mode
    }

zshCfg :: PuppetCfg
zshCfg =
  defShellCfg
    { _pc_cmd = "zsh",
      _pc_promptMatcher = mkSeqMatcher "\ESC[K\ESC[?2004h"
    }

rangerCfg :: PuppetCfg
rangerCfg =
  PuppetCfg
    { _pc_cmd = "ranger",
      _pc_cmdArgs = [],
      -- TODO: need a dummy matcher
      _pc_promptMatcher = mkSeqMatcher "\ESC[K\ESC[?2004h",
      _pc_getCwdCmd = GetCwdFromProcess,
      -- TODO: need a program here
      _pc_mkCdCmd = (\_ -> ""),
      _pc_cleanPromptP = \_ -> Finish (Right ()),
      _pc_switchEnterHook = pure (),
      _pc_switchExitHook = pure ()
    }

errorCfg :: PuppetCfg
errorCfg =
  defShellCfg
    { _pc_cmd = "non existing"
    }

pupppetConfigs :: Map Text PuppetCfg
pupppetConfigs =
  Map.fromList
    [ ("python", pythonCfg),
      ("ranger", rangerCfg),
      ("shh", shhCfg),
      ("zsh", zshCfg),
      ("sh", shCfg),
      ("error", errorCfg)
    ]

getPuppetCfg :: Text -> PuppetCfg -> PuppetCfg
getPuppetCfg cmd def = fromMaybe def $ (Map.lookup cmd pupppetConfigs)

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skelly.CLI.Command (
  CommandSpec (..),
  ParsedCommand (..),
  parseCommand,
  genManDocs,
  genWebDocs,
) where

import Data.Type.Map (Map)
import Options.Applicative qualified as Opt
import Skelly.CLI.Service qualified as CLI

data CommandSpec parentOpts
  = forall opts service. IsService (opts ': parentOpts) service =>
    CommandSpec
    { cmdName :: String
    , cmdDesc :: String
    , cmdImpl :: (service -> opts -> IO ())
    , cmdOptions :: Opt.Parser opts
    }
  | forall extraOpts.
    CommandGroupSpec
    { cmdName :: String
    , cmdDesc :: String
    , cmdChildren :: [CommandSpec (HAppendListR parentOpts extraOpts)]
    , cmdExtraOptions :: Opt.Parser (HList extraOpts)
    }

data ParsedCommand =
  forall opts allOpts service.
  IsService allOpts service =>
  ParsedCommand
    { cmdAction :: service -> IO ()
    , cmdParsedOptions :: HList allOpts
    }

parseCommand :: CommandSpec '[] -> IO ParsedCommand
parseCommand cmd = Opt.execParser $ toParserInfo True (pure HNil) cmd
  where
    cmdName = \case
      CommandSpec{cmdName = x} -> x
      CommandGroupSpec{cmdName = x} -> x
    cmdDesc = \case
      CommandSpec{cmdDesc = x} -> x
      CommandGroupSpec{cmdDesc = x} -> x

    toParserInfo ::
      Bool
      -> Opt.Parser (HList parentOpts)
      -> CommandSpec parentOpts
      -> Opt.ParserInfo ParsedCommand
    toParserInfo isRoot parentOpts cmd =
      Opt.info (fromCommand parentOpts cmd Opt.<**> Opt.helper) . mconcat $
        [ if isRoot then Opt.header "skelly - An opinionated Haskell build system" else mempty
        , Opt.progDesc (cmdDesc cmd)
        ]

    fromCommand ::
      Opt.Parser (HList parentOpts)
      -> CommandSpec parentOpts
      -> Opt.Parser ParsedCommand
    fromCommand parentOptions = \case
      CommandSpec{cmdImpl, cmdOptions} -> do
        opts <- cmdOptions
        parentOpts <- parentOptions
        pure
          ParsedCommand
            { cmdAction = \service -> cmdImpl service opts
            , cmdParsedOptions = HCons opts parentOpts
            }
      CommandGroupSpec{cmdChildren, cmdExtraOptions} ->
        Opt.subparser . mconcat $
          [ Opt.command (cmdName cmd) $
              toParserInfo False (hAppendList <$> parentOptions <*> cmdExtraOptions) cmd
          | cmd <- cmdChildren
          ]

-- TODO: man page generator
genManDocs :: CommandSpec '[] -> Text
genManDocs = error "TODO"

-- TODO: web docs generator
genWebDocs :: CommandSpec '[] -> Text
genWebDocs = error "TODO"

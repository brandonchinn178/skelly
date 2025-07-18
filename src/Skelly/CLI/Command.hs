{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Skelly.CLI.Command (
  CommandSpec (..),
  ParsedCommand (..),
  parseCommand,
  genManDocs,
  genWebDocs,
) where

import Data.HList (HList (..))
import Data.HList qualified as HList
import Data.Text (Text)
import Options.Applicative qualified as Opt
import Skelly.Core.Service (IsService)

-- | The specification for a Skelly command.
--
-- Currently, parentOpts is a type-level list of list of types. We do this so that
-- at any level, the parent can pass down multiple option types to the children.
data CommandSpec parentOpts
  = forall opts service. IsService (opts ': HList.HConcatR parentOpts) service =>
    CommandSpec
    { cmdName :: String
    , cmdDesc :: String
    , cmdImpl :: (service -> opts -> IO ())
    , cmdOptions :: Opt.Parser opts
    }
  | forall extraOpts. HList.HConcat (HList extraOpts ': parentOpts) =>
    CommandGroupSpec
    { cmdName :: String
    , cmdDesc :: String
    , cmdChildren :: [CommandSpec (HList extraOpts ': parentOpts)]
    , cmdExtraOptions :: Opt.Parser (HList extraOpts)
    }

data ParsedCommand =
  forall allOpts service.
  IsService allOpts service =>
  ParsedCommand
    { cmdAction :: service -> IO ()
    , cmdParsedOptions :: HList allOpts
    }

parseCommand :: CommandSpec '[] -> IO ParsedCommand
parseCommand = Opt.execParser . toParserInfo True (pure HNil)
  where
    cmdName = \case
      CommandSpec{cmdName = x} -> x
      CommandGroupSpec{cmdName = x} -> x
    cmdDesc = \case
      CommandSpec{cmdDesc = x} -> x
      CommandGroupSpec{cmdDesc = x} -> x

    toParserInfo ::
      (HList.HConcat parentOpts) =>
      Bool
      -> Opt.Parser (HList parentOpts)
      -> CommandSpec parentOpts
      -> Opt.ParserInfo ParsedCommand
    toParserInfo isRoot cmdParentOpts cmd =
      Opt.info (fromCommand cmdParentOpts cmd Opt.<**> Opt.helper) . mconcat $
        [ if isRoot then Opt.header "skelly - An opinionated Haskell build system" else mempty
        , Opt.progDesc (cmdDesc cmd)
        ]

    fromCommand ::
      (HList.HConcat parentOpts) =>
      Opt.Parser (HList parentOpts)
      -> CommandSpec parentOpts
      -> Opt.Parser ParsedCommand
    fromCommand cmdParentOpts = \case
      CommandSpec{cmdImpl, cmdOptions} -> do
        opts <- cmdOptions
        parentOpts <- HList.hConcat <$> cmdParentOpts
        pure
          ParsedCommand
            { cmdAction = \service -> cmdImpl service opts
            , cmdParsedOptions = HCons opts parentOpts
            }
      CommandGroupSpec{cmdChildren, cmdExtraOptions} ->
        Opt.subparser . mconcat $
          [ Opt.command (cmdName cmd) $
              toParserInfo False (HCons <$> cmdExtraOptions <*> cmdParentOpts) cmd
          | cmd <- cmdChildren
          ]

-- TODO: man page generator
genManDocs :: CommandSpec '[] -> Text
genManDocs = error "TODO"

-- TODO: web docs generator
genWebDocs :: CommandSpec '[] -> Text
genWebDocs = error "TODO"

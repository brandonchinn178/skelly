{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Skelly.CLI.Command (
  CommandSpec (..),
  ParsedCommand (..),
  parseCommand,
  genManDocs,
  genWebDocs,
) where

import Data.HList (HList)
import Data.HList qualified as HList
import Data.Kind (Type)
import Data.Text (Text)
import Options.Applicative qualified as Opt
import Skelly.Core.Service (IsService)

type family HFlatConcat (xs :: [Type]) (ys :: [Type]) :: [Type] where
  HFlatConcat '[] ys = ys
  HFlatConcat (HList xs1 ': xs) ys = HFlatConcat xs1 (HFlatConcat xs ys)
  HFlatConcat (x ': xs) ys = x ': (HFlatConcat xs ys)

class HFlatConcatImpl xs ys where
  hFlatConcat :: HList xs -> HList ys -> HList (HFlatConcat xs ys)
instance HFlatConcatImpl '[] ys where
  hFlatConcat _ ys = ys
instance (HFlatConcatImpl xs1 (HFlatConcat xs ys), HFlatConcatImpl xs ys) => HFlatConcatImpl (HList xs1 ': xs) ys where
  hFlatConcat (xs1 `HList.HCons` xs) ys = hFlatConcat xs1 (hFlatConcat xs ys)
instance HFlatConcatImpl xs ys => HFlatConcatImpl (x ': xs) ys where
  hFlatConcat (x `HList.HCons` xs) ys = x `HList.HCons` hFlatConcat xs ys

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
    , cmdChildren :: [CommandSpec (HFlatConcat parentOpts extraOpts)]
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
parseCommand cmd = Opt.execParser $ toParserInfo True (pure HList.HNil) cmd
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
            , cmdParsedOptions = HList.HCons opts parentOpts
            }
      CommandGroupSpec{cmdChildren, cmdExtraOptions} ->
        Opt.subparser . mconcat $
          [ Opt.command (cmdName cmd) $
              toParserInfo False (HList.hAppendList <$> parentOptions <*> cmdExtraOptions) cmd
          | cmd <- cmdChildren
          ]

-- TODO: man page generator
genManDocs :: CommandSpec '[] -> Text
genManDocs = error "TODO"

-- TODO: web docs generator
genWebDocs :: CommandSpec '[] -> Text
genWebDocs = error "TODO"

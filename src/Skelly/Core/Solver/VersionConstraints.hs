module Skelly.Core.Solver.VersionConstraints (
) where

import Data.List (foldl')
import Skelly.Core.Utils.PackageId (PackageName)
import Skelly.Core.Utils.Version (CompiledVersionRange, Version)

-- IDEA:
--   1. constraints (pkg + range) and conditional constraints (pkg + range => pkg + range)
--   2. over time, constraints all become ==
--   3. as versions get picked / constraints get tightened, promote conditional constraints to constraints
--   4. when conflict arises, add all dependencies involved to conditional constraints + backtrack to most recent dependency in conflict
data VersionConstraints = VersionConstraints
  { constraints :: Constraints
  , conflictSets :: [ConflictSet]
  }

-- | A conflict set represents a set of package version ranges
-- that cannot all be true at once
type ConflictSet = [(PackageName, CompiledVersionRange)]

newtype Constraints = Constraints
  ( HashMap PackageName
      -- (pkg, range) -> [(depPkg, depRange)]
      [ (CompiledVersionRange, [(PackageName, CompiledVersionRange)])
      ]
      -- NOT (pkg, range) -> NOT [(parentPkg, parentRange)]
      [ (CompiledVersionRange, [(PackageName, CompiledVersionRange)])
      ]
  )

-- -- | A list representing the disjunction of version constraints
-- data VersionConstraintClause
--   = Clause
-- type VersionConstraintClause = [VersionConstraint]

-- data VersionConstraint = VersionConstraint
--   { constraintPackage :: PackageName
--   , constraintRange :: CompiledVersionRange
--   }

newtype VersionCandidates = VersionCandidates (HashMap PackageName [Version])

-- newtype VersionAssignments = VersionAssignments (HashMap PackageName Version)

-- propagateUnits :: VersionConstraints -> VersionAssignments -> VersionAssignments
-- propagateUnits (VersionConstraints clauses0) (VersionAssignments assignments0) =
--   VersionAssignments $ foldl' go assignments0 clauses0
--   where
--     go clause assignments =
--       case filter _ clause of
--         [constraint] -> _

# constraints-emerge: defer instance lookups until runtime

## Dedication

> Failure should be our teacher, not our undertaker. Failure is delay, not
> defeat. It is a temporary detour, not a dead end. Failure is something we can
> avoid only by saying nothing, doing nothing, and being nothing.
>
> Denis Waitley


## Synopsis

```haskell
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fplugin=Data.Constraint.Emerge.Plugin #-}

module Test where

import Data.Constraint.Emerge

showAnything :: forall c. Emerge (Show c) => c -> String
showAnything c =
  case emerge @(Show c) of
    Just Dict -> show c
    Nothing -> "<<unshowable>>"


showBool = showAnything True  -- "True"
showId   = showAnything id    -- "<<unshowable>>"
```


## Known Bugs

* `constraints-emerge` fails to provide `Emerge c` dictionaries at runtime.
* `constraints-emerge` will generate type-equality dictionaries any types (even
    ones that aren't equal!!!)

If someone wants to pick it up from here, that’d be great!


## Related Work

 * [union-constraints](https://github.com/rampion/constraint-unions)
 * [Data.Constraint.Deferrable](https://github.com/ekmett/constraints/)


## Contact

Please reports bugs and missing features at the [GitHub bugtracker][issuses]. This is
also where you can find the [source code][source].

`constraints-emerge` was written by [Sandy Maguire][me] and is licensed under a
permissive MIT [license][lic].

[me]: http://reasonablypolymorphic.me
[lic]: https://github.com/isovector/constraints-emerge/blob/LICENSE
[issues]: https://github.com/isovector/constraints-emerge/issues
[source]: https://github.com/isovector/constraints-emerge


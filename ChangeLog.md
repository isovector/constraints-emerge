# Revision history for constraints-emerge

## 0.1.2 -- 2018-04-19

* Significantly better dictionary generation; the plugin will now build
    dictionaries that depend on subdicts.
* Fixed a bug where dictionaries depending on subdicts would cause segfaults at
    runtime.
* The plugin will now refuse to discharge 'Emerge c' constraints if 'c' is not
    fully monomorphic.
* The plugin will now fail to generate equality dicts, even for types that are,
    in fact, equal. This is bad, but much less disastrous than the old behavior
    which would generate dictionaries for non-equal types...

## 0.1.1 -- 2018-04-18

* Updated some misleading error messages.

## 0.1 -- 2018-04-18

* First version. Released on an unsuspecting world.

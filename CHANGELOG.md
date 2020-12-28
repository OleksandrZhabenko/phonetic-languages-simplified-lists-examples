# Revision history for phonetic-languages-simplified-lists-examples

## 0.1.0.0 -- 2020-11-29

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-12-01

* Second version. Added a new module Phonetic.Languages.Simplified.Lists.DeEnCoding for dealing with intersections using heaps functionality.
For lineVariantsG2:
 ** Added for this heaps as a new dependency (a lightweight one).

 ** Added the possibilities to leave the last word in the line on its place (this can lead to preserving rhymes in poetry, for example),
to print either metrices information or not.

 ** Added the possibility to use multiple metrices at once by using +M ... -M blocks of command line arguments. The type of metrics is the first argument and
the numeric arguments for it (as usual) are all further, then again you can specify up to two additional metrices with arguments enclosed by the block
+M and -M delimiters.

## 0.3.0.0 -- 2020-12-03

* Third version. Extended the multiple properties mode up to 4 different properties. Added the possibility to use more
intercation by interactive mode and to write the single line result to file in file writing mode.
Some documentation improvements.

## 0.4.0.0 -- 2020-12-03

* Fourth version. Added the new properties ralated to the uzpp2Durat3 function -- 03y, y3 (and yy2 related to
uzpp2Durat2 function).

## 0.5.0.0 -- 2020-12-05

* Fifth version. Switched to the Double instead of Float whenever possible. Some dependencies changes for this.

## 0.6.0.0 -- 2020-12-07

* Sixth version. Added multiproperties mode for propertiesTextG2 executable. Added constraint of ++B to it, too. Extended
up to 5 possible properties for the lineVariantsG2 executable. Added 'whitelines' modes. Some code and documentation
improvements. Added a new module Phonetic.Languages.Simplified.Lists.SimpleConstraints.


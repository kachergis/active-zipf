
FILES

  * README
  * sundb_fulldata.dat (3.9M)


NOTES

  * Each line contains information about an image
  * Separated by "|"
    COLUMN
       1   scene category name
       2   path to the xml file
       3   #images (always = 1 here)
       4   object category name
       5   frequency of the object category in the image
       6   same as 4th
       7   same as 5th
       8   same as 4th
       9   same as 5th
       and so on


DATA CLEANING

  * Plural to singular using NLTK + WordNet
    * Every words composing a category name
    * E.g. "apples bags" --> "apple bag"
  * Remove the special keyword "crop"
    * E.g. "human crop" means a part of a man in a picture
  * Remove sentence annotations
    * Composed of >= 4 words
    * Contain "is", "are", or "."
  * Remove numbering annotations, end with digits
    * E.g. "car1", "car2", etc


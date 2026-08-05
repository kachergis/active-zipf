require("stringr")

scn_names = read.csv("sundb_fulldata/sundb_fulldata_scenes.txt")

scn = read.csv("sundb_fulldata/sundb_fulldata.txt")

str_split(as.character(scn[1,]), "|")
  

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


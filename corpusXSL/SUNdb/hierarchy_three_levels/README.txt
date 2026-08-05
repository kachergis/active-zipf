This folder contains the three-level scene hierarchy used in the following paper. Please cite this paper if you use this file in your research:

J. Xiao, J. Hays, K. Ehinger, A. Oliva, and A. Torralba
SUN Database: Large-scale Scene Recognition from Abbey to Zoo
Proceedings of 23rd IEEE Conference on Computer Vision and Pattern Recognition (CVPR2010) 

The three_levels.xlsx is an Microsoft Excel files containing two different hierarchies:
1. One with 397 categories used for our benchmark evaluation.
2. One with 908 categories, i.e. all our SUN scene categories.

The SUN397 hierarchy is used for human AFC experiment in the paper. The idea is to perform a series of three questions so that the worker can select the appropriate category without having to memorize hundreds of categories.

We show one image and we ask the next questions:

First question:  Which type of scene is in the image:
1) An indoor scene
2) A natural outdoor scene
3) A man-made outdoor scene

then, if the answer is 1, then we show the second question:

Does the image correspond to a:
1) shopping and dining
2) workplace
3) ...

Then, if the answer is 1) then, we ask to select on of the possible scenes. For each combination there is about 30 possible scenes.

The questions are also already defined in the first row of the excel file.

The table in the Excel file indicates the answers to each question that lead to each category. One important note is that in some cases it is possible to arrive to a given category with different answers. This is to account for the fact that some categories might be ambiguous with respect to the first question. For instance, c/cottage_garden/ is both A "natural outdoor scene" and also a "man-made outdoor scene".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

BUG REPORT

Please email to jxiao@csail.mit.edu for reporting any bug. We can only provide minimal support for academic researchers only.
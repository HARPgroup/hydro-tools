# Generate word doc from latex
# Note: this is an excerpt from the modeling appendices in the 2020 state plan.
#       in order to render the images, I simply had to copy the image folder path from
#       the overleaf project into the sub-directory where the .text file resides.
#       See hydrotools projecdt for example.
pandoc_convert(input="C:/usr/local/home/git/hydro-tools/data/OWS_summaries/consumptive_use.tex", output = "C:/Workspace/tmp/cu.docx")
import os
import xlrd

import pandas as pd

# get the file list in datasets/abs/2005
file_list = os.listdir('datasets/abs/2005')
print(file_list)
# read the first file's first sheet
# df = pd.read_excel('datasets/abs/2005/' + file_list[0], sheet_name=0)

book = xlrd.open_workbook('datasets/abs/2005/' + file_list[0])
print("The number of worksheets is {0}".format(book.nsheets))
#!/usr/bin/env python
# -*- coding: utf-8 -*-

from util import *

DATA = "../data/dan_0002.csv"
lines = list()

with open(DATA,'r') as data_file:
    lines = data_file.read().splitlines()

case_ls = []
for line in lines:
    case_ls.append(line.split(',')[-4])

preprocess(case_ls)
# with open('all_case','w') as f_sink:
    # for case in case_ls:
        # f_sink.write('{}\n====\n'.format(case))

test(case_ls[0])

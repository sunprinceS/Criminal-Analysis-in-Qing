#!/usr/bin/env python
# -*- coding: utf-8 -*-
import re

def preprocess(case_ls):
    for i in range(len(case_ls)):
        case_ls[i] = re.sub('[_0-9a-zA-Z.-]+',' ',case_ls[i])

def norm_num(case_ls):
    for i in range(len(case_ls)):
        case_ls[i] = (re.sub('N+','N',case_ls[i]))

if __name__ == "__main__":
    lines = list()
    with open('all_case_normalize','r') as f_src:
        lines = f_src.read().splitlines()
    norm_num(lines)
    with open('all_case_normalize_num','w') as f_sink:
        for line in lines:
            f_sink.write('{}\n'.format(line))



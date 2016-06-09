#!/usr/bin/env python
# -*- coding: utf-8 -*-

from util import *

obj_cnt = {}
obj_ls = list()

with open('../data/object_analysis/theft_obj.txt','r') as f:
    tmp = f.read().splitlines()
    obj_ls = tmp[0].split(',')
    obj_ls[0] = obj_ls[0]
    for obj in obj_ls:
        obj_cnt[obj] = 0

case_ls = list()
with open('../data/all_case_normalize_num') as f:
    case_ls = f.read().splitlines()
    for obj in obj_ls:
        for case in case_ls:
            if obj in case:
                obj_cnt[obj] += 1

category = {}
category_cnt = {}
with open('../data/object_analysis/category.txt','r') as f:
    tmp = f.read().splitlines()
    for lala in tmp :
        oao = lala.split(':')
        c_name = oao[0]
        category_cnt[c_name] = 0
        category[c_name] = oao[1].split(',')

with open('../data/all_case_normalize_num','r') as f:
    lines = f.read().splitlines()
    for line in lines:
        for c_name,obj_list in category.items():
            for obj in obj_list:
                if obj in line:
                    category_cnt[c_name] += 1
                    break
print(category_cnt)

# summation over whole obj list  (not used)
# category_cnt = {}
# for category_name,obj_ls in category.items():
    # category_cnt[category_name] = 0 
    # for obj in obj_ls:
        # category_cnt[category_name] += obj_cnt[obj]
# for key,times in category_cnt.items():
    # print(key,times)

# draw word cloud
freq_ls = list()
for key,freq in obj_cnt.items():
    print(key,freq)
    freq_ls.append((key,freq))
draw_word_cloud(freq_ls)

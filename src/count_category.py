#!/usr/bin/env python
# -*- coding: utf-8 -*-

# collect category
category = {}
with open('../data/object_analysis/category.txt','r') as f:
    tmp = f.read().splitlines()
    for lala in tmp :
        oao = lala.split(':')
        c_name = oao[0]
        category[c_name] = oao[1].split(',')
for name,obj_ls in category.items():
    print(name,len(obj_ls))

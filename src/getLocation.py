#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import xml.dom.minidom
import os

from xml.dom.minidom import parse

"""
Some observations:
    * only 咸豐-光緒
    * build a hash table (key is 年份，value為一個hash_table(key為location,value為num_happen>))
    * Need some Squeez method to reduce # locations
"""

def get_location(filename):

# Open XML document using minidom parser
if __name__ == '__main__' :
    base_dir = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
    data_dir = os.path.join(base_dir, 'data')
    file_list = [f for f in os.listdir(data_dir) if os.path.isfile(os.path.join(data_dir,f))]
    
    # loc_list = []
    # for file in file_list:
        # loc_list.extend(get_location(file))

#!/usr/bin/env python
# -*- coding: utf-8 -*-
import re
from wordcloud import WordCloud
import matplotlib.pyplot as plt

def preprocess(case_ls):
    for i in range(len(case_ls)):
        case_ls[i] = re.sub('[_0-9a-zA-Z.-]+',' ',case_ls[i])

def draw_word_cloud( freq_list , file_name='word_cloud.png' ):
    # Generate a word cloud image
    wordcloud = WordCloud(
        font_path='NotoSansCJKtc-Regular.otf',
        width=960,
        height=480,
        relative_scaling=.5
    ).generate_from_frequencies( freq_list )
    wordcloud.to_file( file_name )


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


